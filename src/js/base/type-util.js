define("pyret-base/js/type-util", [], function() {

  var any = { "tag": "any" };
  var string = { "tag": "name", "module": "builtin", "name": "String" }
  var number = { "tag": "name", "module": "builtin", "name": "Number" }
  var boolean = { "tag": "name", "module": "builtin", "name": "Boolean" }
  var nothing = { "tag": "name", "module": "builtin", "name": "Nothing" }

  function forall(args, onto) {
    if(!Array.isArray(args)) { throw "Expected list for args, but got " + String(args); }
    args.forEach(function(a) {
      if(typeof a !== "string") { throw "Type args must be strings, but got: " + String(a); }
    });
    return {
      tag: "forall",
      args: args,
      onto: onto
    }
  }

  function arrow(args, ret) {
    if(!Array.isArray(args)) { throw "Expected list for arrow args, but got " + String(args); }
    return {
      tag: "arrow",
      args: args,
      ret: ret
    };
  }

  function tyapp(onto, args) {
    if(!Array.isArray(args)) { throw "Expected list for tyapp args, but got " + String(args); }
    return {
      tag: "tyapp",
      onto: onto,
      args: args
    };
  }

  function tyvar(name) {
    return {
      tag: "tyvar",
      name: name
    };
  }

  function builtinName(name) {
    return {
      tag: "name",
      module: "builtin",
      name: name
    };
  }

  function libName(lib, name) {
    return {
      tag: "name",
      module: "pyret-builtin://" + lib,
      name: name
    };
  }

  function localType(name) {
    return {
      tag: "name",
      module: "LOCAL",
      name: name
    };
  }

  function record(fields) {
    return {
      tag: "record",
      fields: fields
    };
  }

  function dataRefinement(basetype, variant) {
    return {
      tag: "data-refinement",
      basetype: basetype,
      variant: variant
    };
  }

  function dataType(origin, name, params, variants, methods) {
    return {
      tag: "data",
      origin: origin,
      name: name,
      params: params,
      variants: variants,
      methods: methods
    };
  }

  function variant(name, vmembers, withmembers) {
    return {
      tag: "variant",
      name: name,
      vmembers: vmembers,
      withmembers: withmembers
    };
  }

  function singletonVariant(name, withmembers) {
    return {
      tag: "singleton-variant",
      name: name,
      withmembers: withmembers
    }
  }

  function variantMember(name, kind, typ) {
    return {
      tag: "variant-member",
      kind: kind,
      typ: typ
    };
  }

  function bindToPyret(runtime, value, shorthands) {
    var origin = runtime.makeObject({ provided: false });
    if(!value.bind) {
      return runtime.makeObject({
        origin: origin,
        bind: "let",
        typ: toPyretType(runtime, expandType(value, shorthands))
      });
    }
    else {
      if(typeof value.origin === "object") {
        origin = value.origin;
        origin.provided = true;
        origin = runtime.makeObject(origin);
      }
      if(value.bind === "let") {
        return runtime.makeObject({
          origin: origin,
          typ: toPyretType(runtime, expandType(value.typ, shorthands)),
          bind: "let"
        });
      }
      else if(value.bind === "alias") {
        return runtime.makeObject({
          'original-name': value["original-name"],
          origin: origin,
          bind: "alias",
          typ: false
        });
      }
      else if(value.bind === "fun") {
        var flatnessInt = parseInt(value.flatness);
        // Flatness had better be (a) an integer and (b) a non-negative number,
        // otherwise treat it as an infinitely-deep function
        var flatness = flatnessInt === value.flatness && flatnessInt >= 0 ? flatnessInt : false;
        return runtime.makeObject({
          origin: origin,
          bind: "fun",
          name: value.name || "",
          flatness: flatness,
          typ: toPyretType(runtime, expandType(value.typ, shorthands))
        });
      }
      else if(value.bind === "var") {
        return runtime.makeObject({
          origin: origin,
          bind: "var",
          typ: toPyretType(runtime, expandType(value.typ, shorthands))
        });
      }
      else {
        console.error("Bad value provide format:", value);
        throw new Error("Bad value provide format");
      }
    }
  }


  function toPyretType(runtime, typ) {
    var O = runtime.makeObject;
    var L = runtime.ffi.makeList;
    var tp = function(thing) { return toPyretType(runtime, thing); };
    function provided(o) { o.provided = true; return O(o); }
    if(typ === "tany") { return O({ tag: "any" }); }
    if(typ === "tbot") { return O({ tag: "bot" }); }
    switch(typ.tag) {
      case "any":
        return O({ tag: "any"});
      case "data-alias":
        return O({ tag: "data-alias", origin: provided(typ.origin), name : typ.name });
      case "data":
        var methods = Object.keys(typ.methods).map(function(k) {
          return O({ name: k, value: tp(typ.methods[k]) });
        });
        var origin;
        if(typeof typ.origin === "object" && typeof typ.origin.provided !== "boolean") { origin = provided(typ.origin); }
        else { origin = O({provided: false}); }
        return O({
          tag: "data",
          origin: origin,
          name: typ.name,
          params: L(typ.params),
          variants: L(typ.variants.map(tp)),
          methods: L(methods)
        });
      case "variant":
        var methods = Object.keys(typ.withmembers).map(function(k) {
          return O({ name: k, value: tp(typ.withmembers[k]) });
        });
        return O({
          tag: "variant",
          name: typ.name,
          vmembers: L(typ.vmembers.map(tp)),
          withmembers: L(methods)
        });
      case "singleton-variant":
        var methods = Object.keys(typ.withmembers).map(function(k) {
          return O({ name: k, value: tp(typ.withmembers[k]) });
        });
        return O({
          tag: "singleton-variant",
          name: typ.name,
          withmembers: L(methods)
        });
      case "variant-member":
        return O({
          tag: "variant-member",
          name: typ.name,
          kind: typ.kind,
          typ: tp(typ.typ),
        });
      case "record":
        return O({
          tag: "record",
          fields: L(Object.keys(typ.fields).map(function(f) { return O({ tag: "member", name: f, value: tp(typ.fields[f]) }); })),
        });
      case "data-refinement":
        return O({
          tag: "data-refinement",
          basetype: tp(typ.basetype),
          variant: typ.variant
        });
      case "name":
        return O({
          tag: "name",
          origin: O(typ.origin),
          name: typ.name
        });
      case "forall":
        return O({
          tag: "forall",
          args: L(typ.args),
          onto: tp(typ.onto)
        });
      case "tyvar":
        return O({
          tag: "tyvar",
          name: typ.name
        });
      case "arrow":
        return O({
          tag: "arrow",
          args: L(typ.args.map(tp)),
          ret: tp(typ.ret)
        });
      case "tuple":
        return O({
          tag: "tuple",
          elts: L(typ.elts.map(tp)),
        });
      case "tyapp":
        return O({
          tag: "tyapp",
          args: L(typ.args.map(tp)),
          onto: tp(typ.onto)
        });
      default:
        console.error(typ);
        throw new Error("No such tag: " + typ.tag);
    }
  }

  function expandType(typ, shorthands) {
    if(typ.bind == 'fun') {
      return {
        bind: typ.bind,
        origin: typ.origin,
        flatness: typ.flatness,
        name: typ.name,
        typ: expandType(typ.typ, shorthands)
      };
    }
    else if (typ.bind === 'var') {
      return { bind: typ.bind, origin: typ.origin, typ: expandType(typ.typ, shorthands) };
    }
    var fromGlobal = { "import-type": "uri", uri: "builtin://global" };
    var prims = ["Number", "String", "Boolean", "Nothing", "Any"];
    function mkName(origin, name) {
      return { tag: "name", origin: origin, name: name };
    }
    function p(name) { return mkName(fromGlobal, name); }
    function mkApp1(tycon, arg) {
      return {
        tag: "tyapp",
        onto: expandType(tycon, shorthands),
        args: [ expandType(arg, shorthands) ]
      };
    }
    var constrs = {
      "Array": function(name, arg) { 
        return mkApp1(mkName({ "import-type": "uri", uri: "builtin://arrays" }, name), arg);
      },
      "RawArray": function(name, arg) { return mkApp1(mkName(fromGlobal, name), arg); },
      "List": function(name, arg) { 
        return mkApp1(mkName({ "import-type": "uri", uri: "builtin://lists" }, name), arg); 
      },
      "Option": function(name, arg) { 
        return mkApp1(mkName({ "import-type": "uri", uri: "builtin://option" }, name), arg); 
      },
      "Maker": function(_, arg, ret) {
        var maker = {
          "make":  ["arrow", [["RawArray", arg]], ret],
          "make0": ["arrow", [], ret],
          "make1": ["arrow", [arg], ret],
          "make2": ["arrow", [arg, arg], ret],
          "make3": ["arrow", [arg, arg, arg], ret],
          "make4": ["arrow", [arg, arg, arg, arg], ret],
          "make5": ["arrow", [arg, arg, arg, arg, arg], ret]
        };
        return expandType(["record", maker], shorthands);
      }
    };
    var iA = Array.isArray;
    var iO = function(o) { return typeof o === "object" && o !== null && !(iA(o)); };


    function expandMember(m, shorthands) {
      if(!iA(m)) {
        throw new Error("Serialized members should be arrays, got: " + String(m));
      }
      if(m.length === 2) {
        return {
          tag: "variant-member",
          kind: "normal",
          name: m[0],
          typ: expandType(m[1], shorthands)
        };
      }
      else if(m.length === 3) {
        return {
          tag: "variant-member",
          kind: "ref",
          name: m[1],
          typ: expandType(m[2], shorthands)
        };
      }
      else {
        throw new Error("Bad serialized member: " + String(m));
      }
    }

    function expandVariant(v, shorthands) {
      if(!iA(v)) {
        throw new Error("Serialized variant types should be arrays, got: " + String(v));
      }
      else {
        if(v.length === 1) {
          return singletonVariant(v[0], {});
        }
        else if(v.length === 2) {
          if(Array.isArray(v[1])) {
            return variant(v[0], v[1].map(function(m) { return expandMember(m, shorthands); }), {});
          }
          else {
            return singletonVariant(v[0], expandRecord(v[1], shorthands));
          }
        }
        else if(v.length === 3) {
          return variant(v[0],
            v[1].map(function(m) { return expandMember(m, shorthands); }),
            expandRecord(v[2], shorthands));
        }
        else {
          throw new Error("Bad serialized variant: " + String(v));
        }
      }
    }

    if(typeof typ === "string") {
      if(typ === "tany") {
        return "tany";
      }
      else if(typ === "tbot") {
        return "tbot";
      }
      else if(prims.indexOf(typ) !== -1) {
        return p(typ);
      }
      else if(typ in shorthands) {
        return shorthands[typ];
      }
      else {
        throw new Error("Unknown prim type or shorthand: " + typ);
      }
    }
    else if(Array.isArray(typ)) {
      var head = typ[0];
      if (head in constrs) {
        var constr = constrs[head];
        if(typ.length !== constr.length) {
          throw new Error("Bad tail for type constructor " + head + ": " + String(typ));
        }
        return constr.apply(null, typ);
      }
      else {
        if(head === "arrow" && typ.length === 3 && Array.isArray(typ[1])) {
          return {
            tag: "arrow",
            args: typ[1].map(function(t) { return expandType(t, shorthands); }),
            ret: expandType(typ[2], shorthands)
          };
        }
        else if(head === "tuple" && typ.length === 2 && iA(typ[1])) {
          return {
            tag: "tuple",
            elts: typ[1].map(function(t) { return expandType(t, shorthands); })
          };
        }
        else if(head === "data" && typ.length === 6 && iA(typ[3]) && iA(typ[4]) && iO(typ[5])) {
          return {
            tag: "data",
            origin: typ[1],
            name: typ[2],
            params: typ[3],
            variants: typ[4].map(function(v) { return expandVariant(v, shorthands); }),
            methods: expandRecord(typ[5], shorthands)
          };
        }
        else if(head === "data" && typ.length === 5 && iA(typ[2]) && iA(typ[3]) && iO(typ[4])) {
          return {
            tag: "data",
            origin: {provided: false},
            name: typ[1],
            params: typ[2],
            variants: typ[3].map(function(v) { return expandVariant(v, shorthands); }),
            methods: expandRecord(typ[4], shorthands)
          };
        }
        else if(head === "data-alias" && typ.length === 3) {
          return {
            tag: "data-alias",
            origin: typ[1],
            name: typ[2]
          };
        }
        else if(head === "tid" && typ.length === 2) {
          return {
            tag: "tyvar",
            name: typ[1]
          };
        }
        else if(head === "forall" && typ.length === 3) {
          return {
            tag: "forall",
            args: typ[1],
            onto: expandType(typ[2], shorthands)
          };
        }
        else if(head === "local") {
          return {
            tag: "name",
            origin: {"import-type": "$ELF"},
            name: typ[1]
          };
        }
        else if(head === "record") {
          return {
            tag: "record",
            fields: expandRecord(typ[1], shorthands)
          };
        }
        else if(head === "data%") {
          return {
            tag: "data-refinement",
            basetype: expandType(typ[1], shorthands),
            variant: typ[2]
          };
        }
        else if(head === "tyapp") {
          return {
            tag: "tyapp",
            onto: expandType(typ[1], shorthands),
            args: typ[2].map(function(t) { return expandType(t, shorthands); })
          };
        }
        else {
          throw new Error("Unknown shape or head tag for serialized type: " + String(typ));
        }
      }
    }
    else if(iO(typ)) {
      // If the object a ValueExport, we want to return an object that's
      // exactly the same, but with the typ field expanded.
      if (typ.bind == "fun") {
        var o = {};
        Object.keys(typ).forEach(function(k) {
          if (k == "typ") {
            o[k] = expandType(typ.typ, shorthands);
          }
          else {
            o[k] = typ[k];
          }
        });
        return o;
      }
      return typ;
    }
    else {
      throw new Error("Unknown description for serialized type: " + String(typ));
    }
  }

  function expandRecord(r, shorthands) {
    var o = {};
    Object.keys(r).forEach(function(k) {
      o[k] = expandType(r[k], shorthands);
    });
    return o;
  }


  return {
    any: any,
    string: string,
    number: number,
    boolean: boolean,
    nothing: nothing,
    forall: forall,
    arrow: arrow,
    tyapp: tyapp,
    tyvar: tyvar,
    builtinName: builtinName,
    libName: libName,
    localType: localType,
    record: record,
    dataType: dataType,
    toPyretType: toPyretType,
    bindToPyret: bindToPyret,
    expandType: expandType,
    expandRecord: expandRecord
  };

});
