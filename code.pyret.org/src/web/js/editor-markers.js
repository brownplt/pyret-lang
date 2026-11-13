// Run clears untagged text markers (for example, old error highlights).
// Features whose markers describe editor state should call preserveOnRun when
// creating them. Keep this tag on the marker, not in its DOM attributes.
function preserveOnRun(mark) {
  mark.pyretPreserveOnRun = true;
  return mark;
}

function clearOnRun(cm) {
  cm.getAllMarks().forEach(function(mark) {
    if (mark.pyretPreserveOnRun !== true) {
      mark.clear();
    }
  });
}

function preserveFolds(cm) {
  // CodeMirror creates the fold marker before emitting "fold". Translate its
  // addon-specific tag here so Run only needs to know our preservation policy.
  cm.on("fold", function(cm, from) {
    cm.findMarksAt(from).forEach(function(mark) {
      if (mark.__isFold) {
        preserveOnRun(mark);
      }
    });
  });
}

module.exports = { preserveOnRun, clearOnRun, preserveFolds };
