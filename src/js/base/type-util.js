define([], function() {

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

  function dataType(name, params, variants, methods) {
    return {
      tag: "data",
      name: name,
      params: params,
      variants: variants,
      methods: methods
    };
  }

  function variant(name, vmembers) {
    return {
      tag: "variant",
      name: name,
      vmembers: vmembers
    };
  }

  function singletonVariant(name) {
    return {
      tag: "singleton-variant",
      name: name
    }
  }

  function variantMember(name, kind, typ) {
    return {
      tag: "variant-member",
      kind: kind,
      typ: typ
    };
  }

  function toPyret(runtime, typ) {
    var O = runtime.makeObject;
    var L = runtime.ffi.makeList;
    var tp = function(thing) { return toPyret(runtime, thing); };
    if(typ === "tany") { return O({ tag: "any" }); }
    switch(typ.tag) {
      case "any":
        return O({ tag: "any"});
      case "data":
        var methods = Object.keys(typ.methods).map(function(k) {
          return O({ name: k, value: tp(typ.methods[k]) });
        });
        return O({
          tag: "data",
          name: typ.name,
          params: L(typ.params),
          variants: L(typ.variants.map(tp)),
          methods: L(methods)
        });
      case "variant":
        return O({
          tag: "variant",
          name: typ.name,
          vmembers: L(typ.vmembers.map(tp)),
        });
      case "singleton-variant":
        return O({
          tag: "singleton-variant",
          name: typ.name
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

  function providesToPyret(runtime, provides) {
    if(Array.isArray(provides.values)) {
      var values = provides.values;
    }
    else {
      var values = Object.keys(provides.values).map(function(k) {
        return runtime.makeObject({
          name: k,
          typ: toPyret(runtime, provides.values[k])
        });
      });
    }
    if(Array.isArray(provides.types)) {
      var aliases = provides.types;
    }
    else if(typeof provides.aliases === "object") {
      var aliases = Object.keys(provides.aliases).map(function(k) {
        return runtime.makeObject({
          name: k,
          typ: toPyret(runtime, provides.aliases[k])
        });
      });
    }
    if(provides.datatypes) {
      if(Array.isArray(provides.datatypes)) {
        var datatypes = provides.datatypes;
      }
      else if(typeof provides === "object") {
        var datatypes = Object.keys(provides.datatypes).map(function(k) {
          return runtime.makeObject({
            name: k,
            typ: toPyret(runtime, provides.datatypes[k])
          });
        });
      }
    }
    else {
      var datatypes = [];
    }
    return runtime.makeObject({
      values: runtime.ffi.makeList(values),
      aliases: runtime.ffi.makeList(aliases),
      datatypes: runtime.ffi.makeList(datatypes)
    });
  }

  function expandType(typ, shorthands) {
    var fromGlobal = { "import-type": "uri", uri: "builtin://global" };
    var prims = ["Number", "String", "Boolean", "Nothing", "Any"];
    var singles = {
      "Array": { "import-type": "uri", uri: "builtin://arrays" },
      "RawArray": fromGlobal,
      "List": { "import-type": "uri", uri: "builtin://lists" },
      "Option": { "import-type": "uri", uri: "builtin://option" }
    };
    function p(name) {
      return {
        tag: "name",
        origin: fromGlobal,
        name: name
      };
    }
    function singleApp(name, arg) {
      return {
        tag: "tyapp",
        onto: {
          tag: "name",
          origin: singles[name],
          name: name
        },
        args: [ expandType(arg, shorthands) ]
      };
    }
    /*
    { "import-type": "dependency",
      "protocol": "js-file",
      "args": ["./image-lib"] }
    */
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
          return singletonVariant(v[0]);
        }
        else if(v.length === 2) {
          return variant(v[0], v[1].map(function(m) { return expandMember(m, shorthands); }));
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
      if (head in singles) {
        if(typ.length !== 2) {
          throw new Error("Bad tail for type constructor " + head + ": " + String(typ));
        }
        return singleApp(head, typ[1]);
      }
      else {
        if(head === "arrow" && typ.length === 3 && Array.isArray(typ[1])) {
          return {
            tag: "arrow",
            args: typ[1].map(function(t) { return expandType(t, shorthands); }),
            ret: expandType(typ[2], shorthands)
          };
        }
        else if(head === "data" && typ.length === 5 && iA(typ[2]) && iA(typ[3]) && iO(typ[4])) {
          return {
            tag: "data",
            name: typ[1],
            params: typ[2],
            variants: typ[3].map(function(v) { return expandVariant(v, shorthands); }),
            methods: expandRecord(typ[4], shorthands)
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
    toPyret: toPyret,
    providesToPyret: providesToPyret,
    expandType: expandType,
    expandRecord: expandRecord
  };

});
