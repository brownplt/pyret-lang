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
          module: typ.module,
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
    providesToPyret: providesToPyret
  };

});
