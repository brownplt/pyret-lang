{
  nativeRequires: ["source-map"],
  requires: [],
  provides: {
    values: {
      "to-string-with-source-map": "tany",
      "source-node": "tany",
      "is-source-node": "tany",
      "new-map": "tany"
    }
  },
  theModule: function(runtime, namespace, uri, sourceMap) {
    const SN = sourceMap.SourceNode;
    function newMap(line, col, file, name) {
      var curString = "";
      var cur = {line: line, col: col, file: file, name: name, elts: []};
      var stack = [cur];
      function startNode(line, col, file, name) {
        cur = {line: line, col: col, file: file, name: name, elts: []};
        stack.push(cur);
        return runtime.nothing;
      }
      function endNode() {
        var elt = stack.pop();
        cur = stack[stack.length - 1];
        cur.elts.push(new SN(elt.line, elt.col, elt.file, elt.elts, elt.name));
        return runtime.nothing;
      }
      function string(s) {
        var elt = stack[stack.length - 1];
        var lastix = elt.elts.length - 1;
        if(elt.elts.length === 0) {
          elt.elts.push(s);
        }
        else if(typeof elt.elts[lastix] === "string") {
          elt.elts[lastix] += s;
        }
        else {
          elt.elts.push(s);
        }
        return runtime.nothing;
      }
      function get() {
        return runtime.makeOpaque(new SN(cur.line, cur.col, cur.file, cur.elts, cur.name));
      }
      return runtime.makeObject({
        "start-node": runtime.makeFunction(startNode),
        "end-node": runtime.makeFunction(endNode),
        "string": runtime.makeFunction(string),
        "get": runtime.makeFunction(get)
      });
    }
    function sourceNode(uri, line, col, pieces, name) {
      return [ uri, line, col, pieces, name ];
    }
    function isSourceNode(maybeSn) {
      return Array.isArray(maybeSn);
    }
    function toStringWithSourceMap(sm, name) {
      var mapped = sm.val.toStringWithSourceMap({
          file: name
        });
      return runtime.makeObject({
        code: mapped.code,
        map: mapped.map.toString()
      });
    }

    return runtime.makeModuleReturn({
        "new-map": runtime.makeFunction(newMap, "new-map"),
        "source-node": runtime.makeFunction(sourceNode, "source-node"),
        "is-source-node": runtime.makeFunction(isSourceNode, "is-source-node"),
        "to-string-with-source-map":
          runtime.makeFunction(toStringWithSourceMap, "to-string-with-source-map")
      },
      {});
  }
}
