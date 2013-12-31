CodeMirror.registerHelper("fold", "indent", function(cm, start) {
  var tabSize = cm.getOption("tabSize"), firstLine = cm.getLine(start.line);
  var myIndent = CodeMirror.countColumn(firstLine, null, tabSize);

  if (! (/\S/.test(cm.getLine(start.line)))) { return; }

  for (var i = start.line + 1, end = cm.lineCount(); i < end; i ++) {
    var curLine = cm.getLine(i);
    if (CodeMirror.countColumn(curLine, null, tabSize) > myIndent) {
      break;
    }
    if (!(/\S/.test(curLine))) {
      continue;
    }
    return;
  }
  for (var i = start.line + 1, end = cm.lineCount(); i < end; ++i) {
    var curLine = cm.getLine(i);
    if (CodeMirror.countColumn(curLine, null, tabSize) > myIndent) {
      foundIndentedLine = true;
    }
    if (CodeMirror.countColumn(curLine, null, tabSize) <= myIndent &&
        CodeMirror.countColumn(cm.getLine(i-1), null, tabSize) > myIndent &&
        /\S/.test(curLine))
      return {from: CodeMirror.Pos(start.line, firstLine.length),
              to: CodeMirror.Pos(i - 1, cm.getLine(i - 1).length)};
  }
});
CodeMirror.indentRangeFinder = CodeMirror.fold.indent; // deprecated
