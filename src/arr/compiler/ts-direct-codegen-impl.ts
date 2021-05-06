import * as J from 'estree';
import type * as Escodegen from 'escodegen';
import type * as A from './ts-ast';

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

    type PyretObject = {
      dict: any
    };

    type Compiled = {

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
    function Identifier(s : string) : J.Identifier {
      return {
        type: "Identifier",
        name: s
      };
    }
    function ReturnStatement(e : J.Expression) : J.Statement {
      return {
        type: "ReturnStatement",
        argument: e
      }
    }
    function Var(id : string, expr : J.Expression) : J.Declaration {
      return {
        type: "VariableDeclaration",
        kind: "var",
        declarations: [ { type: "VariableDeclarator", id: Identifier(id), init: expr }]
      };
    }
    function Program(body : Array<J.Statement>) : J.Program {
      return {
        sourceType: "module",
        type: "Program",
        body: body
      }
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
      if(exprs.$name === 'empty') { throw new Error("Empty block reached codegen"); }
      else if(exprs.dict.rest.$name === 'link') {
        return compileExpr(context, exprs.dict.first);
      }
      else {
        const [ firstAns, startStmts ] = compileExpr(context, exprs.dict.first);
        const [ ans, restStmts ] = compileSeq(context, exprs.dict.rest);
        return [ ans, [...startStmts, ExpressionStatement(firstAns), ...restStmts]];
      }
    }

    function compileExpr(context, expr : A.Expr) : [J.Expression, Array<J.Statement>] {
      switch(expr.$name) {
        case 's-module':
          return compileExpr(context, expr.dict.answer);
        case 's-block':
          return compileSeq(context, expr.dict.stmts);
        case 's-num':
          const numAns = CallExpression(DotExpression(Identifier("console"), "log"), [Literal(expr.dict.n)])
          return [numAns, []];
        case 's-prim-app':
          const [argvs, argstmts] = compileList(context, expr.dict.args);
          const primAns = CallExpression(DotExpression(Identifier("_runtime"), expr.dict._fun), argvs);
          return [primAns, argstmts];
        case 's-srcloc':
          return [Literal("srcloc"), []];
        default:
          throw new Error("Unhandled expression type: " + expr.$name);
      }
    }

    function createPrelude(prog, provides, env, freeBindings, options, importFlags) : Array<J.Statement> {
      const runtimePath = "./../builtin/runtime.js";
      const runtimeImport = Var("_runtime", CallExpression(Identifier("require"), [Literal(runtimePath)]));
      return [runtimeImport];
    }

    function compileProgram(prog : A.Program, uri : string, env : any, postEnv : any, provides : any, options : any) : PyretObject {
      const importFlags = {};                    // TODO(joe) set up import-flags
      const translatedDatatypeMap = new Map();   // TODO(joe) process from stringdict
      const fromUri = provides.dict['from-uri']; // TODO(joe) handle phases builtin-stage*
      const freeBindings = new Map();            // NOTE(joe) this starts empty in the mainline compiler

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