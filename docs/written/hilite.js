  $(function(){
    $("pre.pyret-highlight, span.pyret-highlight").each(function(_,code) {
      CodeMirror.runMode($(code).text(), "pyret", code);
      $(code).addClass("cm-s-default");
    });
  });
