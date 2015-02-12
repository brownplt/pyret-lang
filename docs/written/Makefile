all:
	scribble \
    ++style styles.css \
    ++style ../../node_modules/codemirror/lib/codemirror.css \
    ++style editor.css \
    --prefix myprefix.html \
    ++extra ../../node_modules/codemirror/mode/pyret/pyret.js \
    ++extra ../../node_modules/codemirror/lib/codemirror.js \
    ++extra ../../node_modules/codemirror/addon/runmode/runmode.js \
    ++extra hilite.js \
    --dest ../../build/ \
    --dest-name docs \
    ++arg "$(VERSION)" \
    --htmls index.scrbl
