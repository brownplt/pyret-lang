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
      var imageDom;
      var maxWidth = output.width() * .75;
      var maxHeight = $(document).height() * .6;
      var realWidth = answer.val.getWidth();
      var realHeight = answer.val.getHeight();
      if(answer.val.getWidth() > maxWidth || answer.val.getHeight() > maxHeight) {
        container.addClass("replImageThumbnail");
        container.attr("title", "Click to see full image");
        var scaleFactorX = 100 / realWidth;
        var scaleFactorY = 200 / realHeight;
        var scaleFactor = scaleFactorX < scaleFactorY ? scaleFactorX : scaleFactorY;
        var scaled = image.makeScaleImage(scaleFactor, scaleFactor, answer.val);
        imageDom = scaled.toDomNode();
        container.append(imageDom);
        $(imageDom).trigger({type: 'afterAttach'});
        $('*', imageDom).trigger({type : 'afterAttach'});
        var originalImageDom = answer.val.toDomNode();
        $(imageDom).on("click", function() {
          var dialog = $("<div>");
          dialog.dialog({
            modal: true,
            height: $(document).height() * .9,
            width: $(document).width() * .9,
            resizable: true
          });
          dialog.css({"overflow": "scroll"});
          dialog.append($(originalImageDom));
          $(originalImageDom).trigger({type: 'afterAttach'});
          $('*', originalImageDom).trigger({type : 'afterAttach'});
        });

      } else {
        imageDom = answer.val.toDomNode();
        container.append(imageDom);
        $(imageDom).trigger({type: 'afterAttach'});
        $('*', imageDom).trigger({type : 'afterAttach'});
        return imageDom;
      }
    } else {
      if (!runtime.isNothing(answer)) {
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
  }
  return { renderPyretValue: renderPyretValue };

})
