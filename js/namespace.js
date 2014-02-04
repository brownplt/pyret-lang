"use strict";

define(function() {
    /**@type {!Object}*/
    var noProto = {};

    /**
        A Namespace contains the bindings currenlty defined in the runtime
        @constructor
        @template T 
        ^the type of values stored in the namespace

        @param {!Object.<string, T>} initialBindings
    */
    function Namespace(initialBindings) {
      if (typeof initialBindings !== "object") {
          throw new Error("Non-object " + initialBindings + " given to Namespace constructor");
      }
      /**@type {!Object.<string, T>}*/
      this.bindings = initialBindings;

      /**@type {T}*/
      this.proto = noProto;
    }

      /**
        Merges this namesspace with another
        @param {!Namespace.<T>} other

        @return {!Namespace.<T>} A namespace that is the result of merging this namespace with the other namespace
      */
      Namespace.prototype.merge = function(other) {
          var combined = Object.create(this.bindings);
          for (var k in other.bindings) {
            combined[k] = other.bindings[k];
          }
          var newNamespace = new Namespace(combined);
          if (other.proto !== noProto) {
            newNamespace.proto = other.proto;
          }
          else {
            newNamespace.proto = this.proto;
          }
          return newNamespace;
      };

      /**
        Gets the value of the specified binding with key 'key'
        Throws an error if binding not bound

        @param {string} key the key to look up
        @return {T} 
      */
      Namespace.prototype.get = function(key) {
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
      };

      /**
        Sets the value of the specified binding with key 'key'

        @param {string} key the key to bind
        @param {T} value the value to set
        @return {!Namespace.<T>} the updated namespace
      */
      Namespace.prototype.set = function(key, value) {
          if (key === "__proto__") {
            var newNamespace = new Namespace(this.bindings);
            newNamespace.proto = value;
            return newNamespace;
          } 
          else {
            var o = Object.create(null);
            o[key] = value;
            return this.merge(new Namespace(o));
          }
      };

      /**
        Checks whether the namespace has 'key' bound 

        @param {string} key the key to check
        @return {boolean} whether or not key is bound
      */
      Namespace.prototype.hasBinding = function(key) {
          if (key === "__proto__") {
            return this.proto !== noProto;
          }
          else {
            return key in this.bindings;
          }
      };

      /**
        Gives an array of all names bound

        @return {Array.<string>}
      */
      Namespace.prototype.getNames = function() {
        var keys = [];
        if (this.proto !== noProto) { keys.push("__proto__"); }
        for (var key in this.bindings) {
          keys.push(key);
        }
        return keys;
      };

    var makeNamespace = function(bindingsObj) {
      var bindings = Object.create(null);
      Object.keys(bindingsObj).forEach(function(k) {
          bindings[k] = bindingsObj[k];
      });
      return new Namespace(bindings, 0);
    }
    
    return {
        namespace: makeNamespace,
        Namespace: Namespace
      };
});
