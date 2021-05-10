import * as J from 'estree';
import type * as Escodegen from 'escodegen';
import type * as A from './ts-ast';
import { assert } from 'console';

({ 
  requires: [],
  nativeRequires: [],
  provides: {
    values: {
      "compile-program": "tany"
    }
  },
  theModule: function(runtime, _, ___) {
    const escodegen : typeof Escodegen = require('escodegen');
    // Pretty-print JS asts
    // Return a PyretObject
    // Type enough AST to get to s-num

    class ExhaustiveSwitchError extends Error {
      constructor(v: never, message?: string) {
        super(`Switch is not exhaustive on \`${JSON.stringify(v)}\`: ${message}`);
      }
    }
    class InternalCompilerError extends Error {
      constructor(message: string) {
        super(`Internal compile error (compiler bug/broken invariant): ${message}`);
      }
    }
    class TODOError extends Error {
      constructor(message: string) {
        super(`Incomplete feature in compiler: ${message}`);
      }
    }

    type PyretObject = {
      dict: any
    };

    function ExpressionStatement(e : J.Expression) : J.Statement {
      return { type: "ExpressionStatement", expression: e };
    }
    function CallExpression(callee : J.Expression, args: Array<J.Expression>) : J.Expression {
      return {
        type: "CallExpression",
        callee: callee,
        arguments: args,
        optional: false
      };
    }
    function DotExpression(object : J.Expression, property : string) : J.Expression {
      return {
        type: "MemberExpression",
        object: object,
        property: Literal(property),
        computed: true,
        optional: false
      }
    }
    function Literal(a : any) : J.Expression {
      return {
        type: "Literal",
        value: a
      };
    }
    function Identifier(id : A.Name) : J.Identifier {
      return {
        type: "Identifier",
        name: nameToSourceString(id)
      };
    }
    function ReturnStatement(e : J.Expression) : J.Statement {
      return {
        type: "ReturnStatement",
        argument: e
      }
    }
    function Var(id : A.Name, expr : J.Expression) : J.Declaration {
      return {
        type: "VariableDeclaration",
        kind: "var",
        declarations: [ { type: "VariableDeclarator", id: Identifier(id), init: expr }]
      };
    }
    function ObjectExpression(properties : Array<J.Property>) : J.Expression {
      return {
        type: "ObjectExpression",
        properties: properties
      };
    }
    function Property(name : string, value : J.Expression) : J.Property {
      return {
        type: "Property",
        kind: "init",
        key: Literal(name),
        value: value,
        method: false,
        shorthand: false,
        computed: true
      };
    }
    function Program(body : Array<J.Statement>) : J.Program {
      return {
        sourceType: "module",
        type: "Program",
        body: body
      }
    }

    function MakeName(start : number) {
      var count = start;
      function atom(base : string) : A.Name {
        count = count + 1;
        return { $name: "s-atom", dict: { base: base, serial: count }};
      }
      return {
        reset: () => count = start,
        sUnderscore: (l : A.Srcloc) : A.Name => ({ $name: "s-underscore", dict: { l }}),
        sName: (s : string, l : A.Srcloc) : A.Name => ({ $name: "s-name", dict: { s, l }}),
        sGlobal: (s : string) : A.Name => ({ $name: "s-global", dict: { s }}),
        sModuleGlobal: (s : string) : A.Name => ({ $name: "s-module-global", dict: { s }}),
        sTypeGlobal: (s : string) : A.Name => ({ $name: "s-type-global", dict: { s }}),
        makeAtom : atom,
        isSUnderscore: v => v.$name === "s-underscore",
        isSName: v => v.$name === "s-name",
        isSGlobal: v => v.$name === "s-global",
        isSModuleGlobal: v => v.$name === "s-module-global",
        isSAtom: v => v.$name === "s-atom",
      };
    }

    // NOTE(joe): Function version of to-sourcestring() method on Name in ast.arr
    function nameToSourceString(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "_";
        case "s-name": return name.dict.s;
        case "s-global": return name.dict.s;
        case "s-module-global": return "$module$" + name.dict.s;
        case "s-type-global": return "$type$" + name.dict.s;
        case "s-atom": return name.dict.base + String(name.dict.serial);
      }
    }

    // NOTE(joe): Function version of toname() method on Name in ast.arr
    function nameToName(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "_";
        case "s-atom": return name.dict.base;
        default: return name.dict.s;
      }
    }

    function nameToKey(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "underscore#";
        case "s-name": return "name#" + name.dict.s;
        case "s-global": return "global#" + name.dict.s;
        case "s-module-global": return "mglobal#" + name.dict.s;
        case "s-type-global": return "tglobal#" + name.dict.s;
        case "s-atom": return "atom#" + name.dict.base + "#" + String(name.dict.serial);
      }
    }

    const dummyLoc : A.Srcloc = {
      $name: "builtin",
      dict: { 'module-name': "dummy location" }
    };

    const jsnames = MakeName(0);
    const jsIds = new Map<string, A.Name>();
    const effectiveIds = new Map<string, boolean>();

    function freshId(id : A.Name) : A.Name {
      let n;
      do {
        const baseName = id.$name === "s-type-global" ? nameToSourceString(id) : nameToName(id);
        const noHyphens = baseName.replace("-", "$");
        n = jsnames.makeAtom(noHyphens);
      } while(effectiveIds.has(nameToSourceString(n)));
      effectiveIds.set(nameToSourceString(n), true);
      return n;
    }
    function jsIdOf(id : A.Name) : A.Name {
      const s = nameToKey(id);
      if (jsIds.has(s)) { return jsIds.get(s); }
      else {
        const safeId = freshId(id);
        jsIds.set(s, safeId);
        return safeId;
      }
    }
    function constId(name : string) : A.Name {
      return {
        $name: "s-name",
        dict: {
          l: dummyLoc,
          s: name
        }
      }
    }
    function compilerName(id: string) : A.Name {
      return constId("$" + id)
    }

    function compileList(context, exprs: A.List<A.Expr>) : [ Array<J.Expression>, Array<J.Statement> ] {
      if(exprs.$name === 'empty') { return [[], []]; }
      else {
        const [ firstAns, startStmts ] = compileExpr(context, exprs.dict.first);
        const [ restAns, restStmts ] = compileList(context, exprs.dict.rest);
        return [ [firstAns, ...restAns], [...startStmts, ...restStmts] ];
      }
    }

    function compileSeq(context, exprs : A.List<A.Expr>) : [J.Expression, Array<J.Statement>] {
      if(exprs.$name === 'empty') { throw new InternalCompilerError("Empty block reached codegen"); }
      else if(exprs.dict.rest.$name === 'link') {
        return compileExpr(context, exprs.dict.first);
      }
      else {
        const [ firstAns, startStmts ] = compileExpr(context, exprs.dict.first);
        const [ ans, restStmts ] = compileSeq(context, exprs.dict.rest);
        return [ ans, [...startStmts, ExpressionStatement(firstAns), ...restStmts]];
      }
    }

    function listToArray<T>(l : A.List<T>) : Array<T> {
      return runtime.ffi.toArray(l);
    }

    function compileObj(context, expr : A.Expr & { $name: 's-obj' }) : [J.Expression, Array<J.Statement>] {
      const tmpBind = freshId(compilerName("temporary"));
      const fieldsAsArray = listToArray(expr.dict.fields);

      const fieldvs = [], stmts = [], binds = [];
      fieldsAsArray.forEach(f => {
        if(f.$name !== "s-data-field") { throw new TODOError("Not yet (or maybe ever) implemented: non-data fields"); }
        const [val, compiledStmts] = compileExpr(context, f.dict.value);
        switch(f.dict.value.$name) {
          case 's-method':
            throw new TODOError("s-method not yet implemented");
          default:
            fieldvs.push(Property(f.dict.name, val));
            stmts.push(...compiledStmts);

        }
      });
      const varObj = Var(tmpBind, ObjectExpression(fieldvs));
      const initStmts = [...stmts, varObj];
      const orderedStmts = [...initStmts, ...binds];
      return [ Identifier(tmpBind), orderedStmts ];
    }

    function pyretLookup(l : A.Srcloc, obj : J.Expression, field : string) : J.Expression {
      return DotExpression(obj, field);
    }

    function compileExpr(context, expr : A.Expr) : [J.Expression, Array<J.Statement>] {
      switch(expr.$name) {
        case 's-module':
          return compileExpr(context, expr.dict.answer);
        case 's-block':
          return compileSeq(context, expr.dict.stmts);
        case 's-num':
          const numAns = Literal(expr.dict.n);
          return [numAns, []];
        case 's-prim-app':
          const [argvs, argstmts] = compileList(context, expr.dict.args);
          const primAns = CallExpression(DotExpression(Identifier(constId("_runtime")), expr.dict._fun), argvs);
          return [primAns, argstmts];
        case 's-srcloc':
          return [Literal("srcloc"), []];
        case 's-obj':
          return compileObj(context, expr);
        case 's-dot':
          const [objV, objStmts] = compileExpr(context, expr.dict.obj);
          return [pyretLookup(expr.dict.l, objV, expr.dict.field), objStmts]
        case 's-id':
          // TODO(joe): freeBindings
          return [Identifier(jsIdOf(expr.dict.id)), []];
        case 's-let-expr':
          const prelude = [];
          listToArray(expr.dict.binds).forEach(v => {
            const [ val, vStmts ] = compileExpr(context, v.dict.value);
            switch(v.dict.b.$name) {
              case "s-tuple-bind":
                throw new InternalCompilerError("Broken invariant: s-tuple-bind in codegen");
              case "s-bind":
                prelude.push(...vStmts);
                prelude.push(Var(jsIdOf(v.dict.b.dict.id), val));
            }
          });
          const [ bv, bodyStmts ] = compileExpr(context, expr.dict.body);
          return [ bv, [...prelude, ...bodyStmts]];
        default:
          throw new TODOError("Unhandled expression type: " + expr.$name);
      }
    }

    function createPrelude(prog, provides, env, freeBindings, options, importFlags) : Array<J.Statement> {
      const runtimePath = "./../builtin/runtime.js";
      const runtimeImport = Var(constId("_runtime"), CallExpression(Identifier(constId("require")), [Literal(runtimePath)]));
      return [runtimeImport];
    }

    function visit<T extends { $name: string, dict: {} }>(v : Partial<Record<T["$name"], any>>, d : T) {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Visit failed: " + JSON.stringify(d)); }
      if(d.$name in v) { v[d.$name](v, d); }
      else {
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            visit(v, subd as any);
          }
        }
      }
    }

    function map<T extends { $name: string, dict: {} }, A extends T>(v : Partial<Record<T["$name"], any>>, d : A) : A {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Map failed: " + JSON.stringify(d)); }
      if(d.$name in v) { return v[d.$name](v, d); }
      else {
        const newObj : typeof d = Object.create(Object.getPrototypeOf(d));
        for(const [k, meta] of Object.entries(d)) {
          if(k !== "dict") { newObj[k] = meta; }
        }
        newObj.dict = Object.create(Object.getPrototypeOf(d.dict));
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            const result = map(v, subd as any);
            newObj.dict[k] = result;
          }
          else {
            newObj.dict[k] = subd;
          }
        }
        return newObj;
      }
    }

    function assertMapIsDoingItsJob(prog : A.Program) {
      const after = map<A.Program, A.Program>({}, prog);
      assert(prog !== after);
      return after;
    }

    function compileProgram(prog : A.Program, uri : string, env : any, postEnv : any, provides : any, options : any) : PyretObject {
      const importFlags = {};                    // TODO(joe) set up import-flags
      const translatedDatatypeMap = new Map();   // TODO(joe) process from stringdict
      const fromUri = provides.dict['from-uri']; // TODO(joe) handle phases builtin-stage*
      const freeBindings = new Map();            // NOTE(joe) this starts empty in the mainline compiler

      // TODO(joe): remove this when we are 100% confident that map doesn't fudge with the AST
      prog = assertMapIsDoingItsJob(prog);

      process.stdout.write(JSON.stringify(prog));

      const [ans, stmts] = compileExpr({
        uri: fromUri,
        options: options,
        provides: provides,
        datatypes: translatedDatatypeMap,
        env: env,
        postEnv: postEnv,
        freeBindings: freeBindings
      }, prog.dict.block);

      const prelude = createPrelude(prog, provides, env, freeBindings, options, importFlags);

      const moduleBody = Program([...prelude, ...stmts, ReturnStatement(ans)]);
      const moduleAndMap = {
        map: "",
        code: escodegen.generate(moduleBody)
      };
      return runtime.makeObject({
        theModule: moduleAndMap.code,
        theMap: moduleAndMap.map
      });
    }

    return runtime.makeModuleReturn({
      'compile-program': runtime.makeFunction(compileProgram)
    }, {});

  }
})