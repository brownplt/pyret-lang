/*global define */
/*jslint unparam: true, todo: true, node: true*/

define([], function() {
  function renderValue(runtime, val) {
    if(runtime.isPyretVal(val)) {
      return runtime.toReprJS(val, "_torepr");
    }
    return String(val);
  }

  return {
    renderValue : renderValue
  };
});
