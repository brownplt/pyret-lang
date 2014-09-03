all:
	scribble \
    ++style styles.css \
    ++style ../../lib/CodeMirror/lib/codemirror.css \
    ++style ../../src/web/editor.css \
    --prefix myprefix.html \
    ++extra ../../lib/CodeMirror/mode/pyret/pyret.js \
    ++extra ../../lib/CodeMirror/lib/codemirror.js \
    ++extra ../../lib/CodeMirror/addon/runmode/runmode.js \
    ++extra hilite.js \
    --dest ../../build/ \
    --dest-name docs \
    ++arg $(VERSION) \
    --htmls index.scrbl
