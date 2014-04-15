define(["trove/image-lib"], function(imageLib) {

  // Because some finicky functions (like images and CodeMirrors), require
  // extra events to happen for them to show up, we provide this as an
  // imperative API: the DOM node created will be appended to the output
  // and also returned
  function renderPyretValue(output, runtime, answer) {
    var image = imageLib(runtime, runtime.namespace);
    if(runtime.isOpaque(answer) && image.isImage(answer.val)) {
      var container = $("<div>").addClass('replOutput');
      output.append(container);
      var imageDom = container.append(answer.val.toDomNode());
      $(imageDom).trigger({type: 'afterAttach'});
      $('*', imageDom).trigger({type : 'afterAttach'});
      return imageDom;
    } else {
      var echoContainer = $("<div>").addClass("replTextOutput");
      var text = runtime.toReprJS(answer, "_torepr");
      var echo = $("<textarea class='CodeMirror'>");
      output.append(echoContainer);
      echoContainer.append(echo);
      var echoCM = CodeMirror.fromTextArea(echo[0], { readOnly: 'nocursor' });
      echoCM.setValue(text);
      return echoContainer;
    }
  }
  return { renderPyretValue: renderPyretValue };

})
