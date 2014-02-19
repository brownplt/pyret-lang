define(["./runtime-util", "../trove/list"], function(util, listLib) {
  console.log("loading ffi-helpers");
  return util.memoModule("ffi-helpers", function(RUNTIME, NAMESPACE) {
      var L = RUNTIME.getField(listLib(RUNTIME, NAMESPACE), "provide");
      return {
          makeList: function(arr) {
            var lst = RUNTIME.getField(L, "empty");
            for(var i = arr.length - 1; i >= 0; i--) {
              lst = RUNTIME.getField(L, "link").app(arr[i], lst); 
            }
            return lst;
          },
          toArray: function(list) {
            var isList = RUNTIME.getField(L, "List");
            var isEmpty = RUNTIME.getField(L, "is-empty");
            var isLink = RUNTIME.getField(L, "is-link");
            if(!(RUNTIME.unwrap(isList.app(list)) === true)) {
              throw "Non-list given to toArray " + String(list);
            }
            var arr = [];
            while(!(RUNTIME.unwrap(isEmpty.app(list)) === true)) {
              arr.push(RUNTIME.getField(list, "first"));
              list = RUNTIME.getField(list, "rest");
            }
            return arr;
          }
        }

    });
});
