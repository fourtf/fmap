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
        nodes =
            if node.selected then
                [ newNodeGen ]

            else
                []
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
                (Unique.sequence <| (List.map addChildNodeRec nodeChildren ++ nodes))


withId : Node -> Unique.Id -> Node
withId node id =
    { node | id = id }


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


viewDocument : Model -> Document Msg
viewDocument model =
    { title = "fmap"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    -- let root = Unique.run <| model.root in
    -- div [ style "overflow-x" "auto" ] [ viewNode model root ]
    div [ style "overflow-x" "auto" ] [ viewNode model model.root ]


viewNode : Model -> Node -> Html Msg
viewNode model node =
    case node.children of
        NodeChildren children ->
            div
                -- surrounding border
                [ style "border" "1px solid grey"
                , inlineBlock
                ]
                -- border around the object
                [ div
                    [ style "border"
                        (if node.selected then
                            "1px solid red"

                         else
                            "1px solid black"
                        )
                    , inlineBlock
                    , onClick (SelectNode node.id)
                    ]
                    [ text node.text ]

                -- children
                , div
                    [ inlineBlock ]
                    (List.map
                        (viewNode model)
                        children
                    )
                ]


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


inlineBlock =
    style "display" "inline-block"
