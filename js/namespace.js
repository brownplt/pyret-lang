"use strict";
var Namespace = (function() {
  var noProto = {};
  function Namespace(initialBindings) {
    if (typeof initialBindings !== "object") {
      throw new Error("Non-object " + initialBindings + " given to Namespace constructor");
    }
    this.bindings = initialBindings;
    this.proto = noProto;
  }
  Namespace.prototype = {
    merge: function(other) {
      var combined = Object.create(this.bindings);
      for(var k in other.bindings) {
        combined[k] = other.bindings[k];
      }
      var newNamespace = new Namespace(combined);
      if (other.proto !== noProto) {
        newNamespace.proto = other.proto;
      } else {
        newNamespace.proto = this.proto;
      }
      return newNamespace;
    },
    get: function(key) {
      if (key === "__proto__") {
        if (this.proto === noProto) {
          throw new Error("Looked up __proto__, not bound in namespace");
        }
        return this.proto;
      }
      else {
        if (!(key in this.bindings)) {
          throw new Error("Looked up " + key + ", not bound in namespace");
        }
        return this.bindings[key];
      }
    },
    set: function(key, value) {
      if (key === "__proto__") {
        var newNamespace = new Namespace(this.bindings);
        newNamespace.proto = value;
        return newNamespace;
      } else {
        var o = Object.create(null);
        o[key] = value;
        return this.merge(new Namespace(o));
      }
    },
    hasBinding: function(key) {
      if (key === "__proto__") {
        return this.proto !== noProto;
      }
      else {
        return key in this.bindings;
      }
    }
  };
  var makeNamespace = function(bindingsObj) {
    var bindings = Object.create(null);
    Object.keys(bindingsObj).forEach(function(k) {
      bindings[k] = bindingsObj[k];
    });
    return new Namespace(bindings);
  }
  
  return makeNamespace;
})();

