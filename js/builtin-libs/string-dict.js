
define(["../namespace", "./ffi-helpers"], function(Namespace, ffi) {
  var memo = false;
  return function(RUNTIME, NAMESPACE) {
    if(memo) { return memo; }
    else {

      var F = ffi(RUNTIME, NAMESPACE);
      var unwrap = RUNTIME.unwrap;
      var ImmutableStringDict = Namespace.Namespace;

      function StringDict(d) {
        this.d = d;
      }
      StringDict.prototype.get = function(s) { return this.d[" " + s]; }
      StringDict.prototype.set = function(s, val) { this.d[" " + s] = val; }
      StringDict.prototype.getNames = function() { return Object.keys(this.d); }
      StringDict.prototype.hasBinding = function(s) {
        
        var result = Object.prototype.hasOwnProperty.call(this.d, " " + s);
        return result;
      };

      function isStringDict(sd) {
        return (sd instanceof ImmutableStringDict) || (sd instanceof StringDict);
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
                var dict = RUNTIME.getField(self, "the-dict");
                if(dict.val instanceof ImmutableStringDict) {
                  return dict.val.get(s);
                }
                else {
                  return dict.val.get(s);
                }
              }),
            'set': RUNTIME.makeMethodFromFun(function(self, str, val) {
                RUNTIME.checkIf(str, RUNTIME.isString);
                var s = unwrap(str);
                var dict = RUNTIME.getField(self, "the-dict");
                if(dict.val instanceof ImmutableStringDict) {
                  return stringDictObj(dict.val.set(s, val));
                }
                else {
                  dict.val.set(s, val);
                  return self;
                }
              }),
            'keys': RUNTIME.makeMethodFromFun(function(self) {
                var dict = RUNTIME.getField(self, "the-dict");
                return F.makeList(dict.val.getNames().map(RUNTIME.makeString));
              })
          });
      }

      memo = RUNTIME.makeObject({
          provide: RUNTIME.makeObject({
            'StringDict': RUNTIME.makeFunction(function() { throw RUNTIME.makeMessageException("Cannot check StringDict yet") } ),
            'to-dict': RUNTIME.makeFunction(function(dict) {
                RUNTIME.checkIf(dict, RUNTIME.isObject);
                var fields = RUNTIME.getFields(dict);
                var ns = new ImmutableStringDict(Object.create({}));
                fields.forEach(function(f) { ns = ns.set(f, RUNTIME.getField(dict, f)); });
                return stringDictObj(ns);
              }),
            'immutable-string-dict': RUNTIME.makeFunction(function() {
                return stringDictObj(new ImmutableStringDict(Object.create(null)));
              }),
            'string-dict': RUNTIME.makeFunction(function() {
                return stringDictObj(new StringDict(Object.create(null)));
              }),
          }),
          answer: NAMESPACE.get("nothing")
        });
    }
    return memo;
  };
});

