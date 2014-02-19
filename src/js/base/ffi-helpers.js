define(["./runtime-util", "../trove/list"], function(util, listLib) {
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
            // console.error("list is " + JSON.stringify(list).substr(0, 100));
            // console.error("list is Object? " + RUNTIME.isObject(list));
            // console.error("list.brands is " + JSON.stringify(list.brands));
            if(!(RUNTIME.unwrap(isList.app(list)) === true)) {
              throw "Non-list given to toArray " + String(list);
            }
            var arr = [];
            try {
              while(!(RUNTIME.unwrap(isEmpty.app(list)) === true)) {
                try {
                  arr.push(RUNTIME.getField(list, "first"));
                } catch(e) {
                  console.error("***** getField first failed on list: " + JSON.stringify(list));
                  console.error(e);
                  throw e;
                }
                try {
                  list = RUNTIME.getField(list, "rest");
                } catch(e) {
                  console.error("***** getField rest failed on list: " + JSON.stringify(list));
                  console.error(e);
                  throw e;
                }
              }
            } catch(e) {
              console.error("******* Calling isEmpty failed on list: " + JSON.stringify(list));
              console.error(e);
              throw e;
            }
            return arr;
          },
          isList: function(list) { return RUNTIME.unwrap(RUNTIME.getField(L, "List").app(list)); }
        }

    });
});
