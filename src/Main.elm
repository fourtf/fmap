module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events as Events
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (classList, src, style, tabindex)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Unique as Unique exposing (Unique)


type alias Model =
    { root : Node
    }


newModel : Model
newModel =
    { root = Unique.run newNodeGen }


type alias Node =
    { text : String
    , color : String
    , children : NodeChildren
    , selected : Bool
    , id : Unique.Id
    }


type alias NodeGen =
    Unique.Unique Node


type NodeChildren
    = NodeChildren (List Node)


newNodeGen : NodeGen
newNodeGen =
    Unique.map
        (\id ->
            { text = "new node"
            , color = ""
            , children = NodeChildren []
            , selected = True
            , id = id
            }
        )
        Unique.unique


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( newModel, Cmd.none )
        , view = viewDocument
        , update = update
        , subscriptions = \_ -> Sub.batch [ Events.onKeyPress onKeyPress ]
        }


type Msg
    = CreateChild
    | SelectNode Unique.Id
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateChild ->
            ( { model | root = addChildNode model.root }, Cmd.none )

        SelectNode id ->
            ( { model | root = selectNode id model.root }, Cmd.none )

        _ ->
            ( model, Cmd.none )


addChildNode : Node -> Node
addChildNode node =
    Unique.run <| addChildNodeRec node


addChildNodeRec : Node -> NodeGen
addChildNodeRec node =
    let
        newNode =
            maybeIf node.selected newNodeGen
    in
    case node.children of
        NodeChildren nodeChildren ->
            Unique.map2
                (\id children ->
                    { node
                        | children = NodeChildren children
                        , selected = False
                        , id = id
                    }
                )
                Unique.unique
                (Unique.sequence <| (List.map addChildNodeRec nodeChildren ++ maybeToList newNode))


selectNode : Unique.Id -> Node -> Node
selectNode id node =
    { node
        | selected = node.id == id
        , children = mapChildren (List.map (selectNode id)) node.children
    }


mapChildren : (List Node -> List Node) -> NodeChildren -> NodeChildren
mapChildren func nc =
    case nc of
        NodeChildren children ->
            NodeChildren (func children)



-- View


viewDocument : Model -> Document Msg
viewDocument model =
    { title = "fmap"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    -- let root = Unique.run <| model.root in
    -- div [ style "overflow-x" "auto" ] [ viewNode model root ]
    div [ style "overflow-x" "auto" ] [ viewNode model.root ]


type NodePosition
    = First
    | Last
    | Middle
    | OnlyChild
    | Root


viewNode : Node -> Html Msg
viewNode =
    viewNodeRec Root


viewNodeRec : NodePosition -> Node -> Html Msg
viewNodeRec pos node =
    case node.children of
        NodeChildren children ->
            let
                hasChildren =
                    List.length children /= 0

                isRoot =
                    pos == Root
            in
            div
                -- surrounding border
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "height" "100%"
                ]
                -- border around the object
                [ lineForPos pos
                , elementIf (not isRoot) smolLine
                , viewNodeContent node

                -- children
                , elementIf hasChildren smolLine
                , viewNodeChildren children
                ]


viewNodeContent : Node -> Html Msg
viewNodeContent node =
    div
        [ style "border"
            (if node.selected then
                "1px solid red"

             else
                "1px solid black"
            )
        , style "align-self" "center"
        , style "margin" "0.1rem 0"
        , style "padding" "0 0.2rem"
        , style "white-space" "nowrap"
        , style "font" "10px sans-serif"
        , onClick (SelectNode node.id)
        ]
        [ text node.text ]


viewNodeChildren : List Node -> Html Msg
viewNodeChildren children =
    div
        [ style "display" "flex", style "flex-direction" "column" ]
        (List.indexedMap
            (\i childNode ->
                viewNodeRec
                    (if List.length children == 1 then
                        OnlyChild

                     else if i == 0 then
                        First

                     else if i == List.length children - 1 then
                        Last

                     else
                        Middle
                    )
                    childNode
            )
            children
        )


lineForPos : NodePosition -> Html Msg
lineForPos pos =
    case pos of
        First ->
            bottomLine

        Last ->
            topLine

        Middle ->
            middleLine

        OnlyChild ->
            div [] []

        Root ->
            div [] []


{-| Small horizontal line.
-}
smolLine : Html Msg
smolLine =
    div
        [ style "width" "6px"
        , style "border-top" "1px solid black"
        , style "align-self" "center"
        ]
        []


{-| Vertical line but only lower half.
-}
bottomLine : Html Msg
bottomLine =
    div
        [ style "border-left" "1px solid black"
        , style "height" "calc(50% + 0.5px)"
        , style "align-self" "flex-end"
        ]
        []


{-| Vertical line but only upper half.
-}
topLine : Html Msg
topLine =
    div
        [ style "border-left" "1px solid black"
        , style "height" "calc(50% + 0.5px)"
        ]
        []


{-| A full vertical line.
-}
middleLine : Html Msg
middleLine =
    div
        [ style "border-left" "1px solid black"
        ]
        []


onKeyPress : Decode.Decoder Msg
onKeyPress =
    Decode.map keyToMsg (Decode.field "key" Decode.string)


keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "Enter" ->
            CreateChild

        _ ->
            Nop



-- HELPERS


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just val ->
            [ val ]

        _ ->
            []


maybeIf : Bool -> a -> Maybe a
maybeIf cond x =
    if cond then
        Just x

    else
        Nothing


elementIf : Bool -> Html msg -> Html msg
elementIf cond el =
    if cond then
        el

    else
        div [] []
