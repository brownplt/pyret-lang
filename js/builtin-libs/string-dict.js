
define(["../namespace"], function(Namespace) {
  return function(RUNTIME, NAMESPACE) {
    var unwrap = RUNTIME.unwrap;
    function ImmutableStringDict(d) {
      this.d = d;
    }

    function StringDict(d) {
      this.d = d;
    }

    function isStringDict(sd) {
      return (sd instanceof ImmutableStringDict) || (sd instanceof StringDict);
    }

    function getJSDict(sd) {
      if (isStringDict(sd)) {
        return sd.d;
      } else {
        throw RUNTIME.makeMessageException("Non-StringDict in getJSDict");
      }
    }

    function equalsDict(d1, d2) {
      if(((d1 instanceof StringDict) && (d2 instanceof StringDict)) ||
         ((d1 instanceof ImmutableStringDict) && (d2 instanceof ImmutableStringDict))) {
        var names1 = d1.getNames();
        var names2 = d2.getNames();
        if (names1.length !== names2.length) { return false; }
        for(var i = 0; i < names1.length; i++) {
          if(!(RUNTIME.same(d1.get(names1[i]), d2.get(names2[i])))) {
            return false;
          }
        }
      }
    }

    function stringDictObj(aDict) {
      return RUNTIME.makeObject({
          'the-dict': RUNTIME.makeOpaque(aDict, equalsDict),
          'has-key': RUNTIME.makeMethodFromFun(function(self, str) {
              RUNTIME.checkIf(str, RUNTIME.isString);
              var s = unwrap(str); 
              return RUNTIME.makeBoolean(RUNTIME.getField(self, "the-dict").val.hasBinding(s));
            }),
          'get': RUNTIME.makeMethodFromFun(function(self, str) {
              RUNTIME.checkIf(str, RUNTIME.isString);
              var s = unwrap(str); 
              return RUNTIME.getField(self, "the-dict").val.get(s);
            }),
          'set': RUNTIME.makeMethodFromFun(function(self, str, val) {
              RUNTIME.checkIf(str, RUNTIME.isString);
              var s = unwrap(str);
              return stringDictObj(RUNTIME.getField(self, "the-dict").val.set(s, val));
            }),
          'keys': RUNTIME.makeMethodFromFun(function(self, str, val) {
              throw RUNTIME.makeMessageException("Cannot get keys of dict yet");
            })
        });
    }

    return RUNTIME.makeObject({
        provide: RUNTIME.makeObject({
          'StringDict': RUNTIME.makeFunction(function() { throw RUNTIME.makeMessageException("Cannot check StringDict yet") } ),
          'to-dict': RUNTIME.makeFunction(function() { throw RUNTIME.makeMessageException("Cannot to-dict yet"); }),
          'immutable-string-dict': RUNTIME.makeFunction(function() {
              return stringDictObj(Namespace({}));
            }),
          'string-dict': RUNTIME.makeFunction(function() {
              throw RUNTIME.makeMessageException("Cannot string-dict yet");
            }),
        }),
        answer: NAMESPACE.get("nothing")
      });
    
  };
});

