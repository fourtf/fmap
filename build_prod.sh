#!/bin/sh

set -e

srcfile="src/Main.elm"
js="tmp.js"
min="prod/app.min.js"

elm make --optimize --output=$js $srcfile || elm.exe make --optimize --output=$js $srcfile

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle > $min

rm tmp.js

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"