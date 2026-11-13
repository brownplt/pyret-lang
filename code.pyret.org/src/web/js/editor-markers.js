// Pass pyretPreserveOnRun in markText options to keep editor state across Run.
function clearOnRun(cm) {
  cm.getAllMarks().forEach(function(mark) {
    // The fold addon marks its markers with __isFold; it doesn't accept markText options.
    if (!mark.pyretPreserveOnRun && !mark.__isFold) {
      mark.clear();
    }
  });
}

module.exports = { clearOnRun };
