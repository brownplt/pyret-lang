import * as J from 'estree';
import type * as Escodegen from 'escodegen';
import type * as Path from 'path';
import type * as A from './ts-ast';
import type * as T from './ts-impl-types';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as PS from './provide-serialization';
import type { Variant } from './ts-codegen-helpers';
import type * as TJSP from './ts-js-of-pyret';
import { CompileOptions } from './ts-compiler-lib-impl';

export type Exports = {
  dict: {
    values: {
      dict: {
        'compile-program': T.PFunction<(prog : A.Program, uri : string, env : CS.CompileEnvironment, postEnv : CS.ComputedEnvironment, provides : CS.Provides, options : CompileOptions) => TJSP.CCPDict>,
      }
    }
  }
}

({ 
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['provide-serialization']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
   ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "compile-program": "tany"
    }
  },
  theModule: function(runtime, _, ___, tj : TJ.Exports, TCSH : (TCSH.Exports), ps : PS.Exports, A : (A.Exports), escodegen : (typeof Escodegen), P : (typeof Path)) {
    // Pretty-print JS asts
    // Return a PyretObject
    // Type enough AST to get to s-num

    const {
      ArrayExpression,
      AssignmentExpression,
      BinaryExpression,
      BlockStatement,
      BracketExpression,
      BreakStatement,
      CallExpression,
      Case,
      ConditionalExpression,
      Default,
      DotExpression,
      ExhaustiveSwitchError,
      ExpressionStatement,
      FunctionExpression,
      Getter,
      Identifier,
      IfStatement,
      InternalCompilerError,
      Literal,
      LogicalExpression,
      MakeName,
      MethodCallExpression,
      ObjectExpression,
      Program,
      Property,
      ReturnStatement,
      ShouldHaveDesugared,
      SwitchStatement,
      TODOError,
      UnaryExpression,
      Var,
      bindToName,
      dummyLoc,
      listToArray,
      nameToKey,
      nameToName,
      nameToSourceString,
      formatSrcloc,
      map,
      visit,
    } = tj;

    const { unwrap } = TCSH;

    const { compileProvides, compileProvidesOverrideUri } = ps;

    const jsnames = MakeName(0);
    const jsIds = new Map<string, A.Name>();
    const effectiveIds = new Map<string, boolean>();

    function freshOrReuseId(expr : J.Expression, name : string) : [J.Identifier, J.Statement[]] {
      if(expr.type === "Identifier") {
        return [expr, []];
      }
      else {
        const idName = freshId(compilerName(name));
        return [Identifier(idName), [Var(idName, expr)]];
      }
    }
    function freshId(id : A.Name) : A.Name {
      let n;
      do {
        const baseName = id.$name === "s-type-global" ? nameToSourceString(id) : nameToName(id);
        const noHyphens = (baseName as any).replaceAll("-", "$");
        n = jsnames.makeAtom(noHyphens);
      } while(effectiveIds.has(nameToSourceString(n)));
      effectiveIds.set(nameToSourceString(n), true);
      return n;
    }
    function jsIdOf(id : A.Name) : A.Name {
      const s = nameToKey(id);
      if (jsIds.has(s)) { return jsIds.get(s)!; }
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

    const RUNTIME = constId("_runtime");
    const CHECKER = constId("_checker");
    const TABLE = constId("_table")
    const NUMBER_ERR_CALLBACKS = "$errCallbacks"
    const EQUAL_ALWAYS = "$equalAlways"
    const EQUAL_NOW = "$equalNow"
    const IDENTICAL = "$identical"
    const TOREPR = "$torepr"
    const UNDEFINED = Identifier(constId("undefined"));

    const OP_TO_FUNCTION = {
      'op<=>': IDENTICAL,
      'op==': EQUAL_ALWAYS,
      'op=~': EQUAL_NOW
    };

    function compressRuntimeName(name : string) { return name; }
    
    function rtField(name : string) {
      return DotExpression(Identifier(RUNTIME), name);
    }

    function rtMethod(name : string, args : Array<J.Expression>) {
      return CallExpression(DotExpression(Identifier(RUNTIME), compressRuntimeName(name)), args);
    }
    
    function chooseSrcloc(l : A.Srcloc, context: Context) {
      switch(l.$name) {
        case "builtin": return l;
        case "srcloc": {
          const override : A.Srcloc = {
            $name: 'srcloc', dict: { ...l.dict, source: context.uri }
          };
          const mode = (context.options.dict['compile-mode'] as CS.CompileMode);
          switch(mode.$name) {
            case 'cm-normal': return l;
            case 'cm-builtin-stage-1': return override;
            case 'cm-builtin-general': return override;
            default:
              throw new ExhaustiveSwitchError(mode);
          }
        }
        default: throw new ExhaustiveSwitchError(l);
      }

    }

    function compileList(context : Context, exprs: T.List<A.Expr>) : [ Array<J.Expression>, Array<J.Statement> ] {
      if(exprs.$name === 'empty') { return [[], []]; }
      else {
        const [ firstAns, startStmts ] = compileExpr(context, exprs.dict.first);
        const [ restAns, restStmts ] = compileList(context, exprs.dict.rest);
        return [ [firstAns, ...restAns], [...startStmts, ...restStmts] ];
      }
    }

    function compileSeq(context : Context, exprs : T.List<A.Expr>) : [J.Expression, Array<J.Statement>] {
      if(exprs.$name === 'empty') { 
        throw new InternalCompilerError("Empty block reached codegen"); 
      } else {
        let ans: J.Expression = Literal('placeholder'), stmts: J.Statement[] = [];
        let cur: T.List<A.Expr> = exprs;
        while (cur.$name === 'link') {
          const [first, firstStmts] = compileExpr(context, cur.dict.first);
          ans = first;
          stmts.push(...firstStmts);
          if (cur.dict.rest.$name !== 'empty' && first !== undefined && !(first.type === 'Identifier')) {
            stmts.push(ExpressionStatement(first));
          }
          cur = cur.dict.rest;
        }
        return [ans, stmts];
      }
    }

    function arrayToList<T>(arr : T[]) : T.List<T> {
      return runtime.ffi.makeList(arr);
    }

    function compileMethodDefinition(context : Context, method : Variant<A.Member, "s-method-field">) {
      const args = method.dict.args as Variant<T.List<Variant<A.Bind, "s-bind">>, "link">
      const self = jsIdOf(args.dict.first.dict.id);
      const lamProps = method.dict;
      // This removes the `self` argument from the args list for use in the curried method
      // From well-formed, we know methods have at least one argument
      lamProps.args = args.dict.rest;
      const [methodBody, _] = compileExpr(context, { $name: "s-lam", dict: lamProps });
      const install = rtMethod("$installMethod", [Identifier(self), Literal(method.dict.name), methodBody])
      const body = [
        Var(self, { type: "ThisExpression"}),
        ReturnStatement(install)
      ];
      const f = FunctionExpression(jsIdOf(constId(`getWrapper_${method.dict.name}`)), [], BlockStatement(body));
      return { ...f, leadingComments: [{ type: "Block" as "Block", value: ` @stopify flat ` }] };
    }

    function compileObj(context : Context, expr : Variant<A.Expr, 's-obj'>) : [J.Expression, Array<J.Statement>] {
      const fieldsAsArray = listToArray(expr.dict.fields);
      const fieldvs : Array<J.Property> = [], stmts: Array<J.Statement> = [], methods : Array<[string, J.Property]> = [];
      fieldsAsArray.forEach(f => {
        switch(f.$name) {
          case 's-method-field':
            const method = compileMethodDefinition(context, f);
            methods.push([f.dict.name, Property(f.dict.name, method)]);
            return;
          case 's-data-field':
            const [val, compiledStmts] = compileExpr(context, f.dict.value);
            fieldvs.push(Property(f.dict.name, val));
            stmts.push(...compiledStmts);
            return;
          default:
            throw new InternalCompilerError("Not yet implemented: " + f.$name);
        }
      });
      fieldvs.push(Property("$methods", ObjectExpression(methods.map(m => m[1]))));
      const obj = ObjectExpression(fieldvs);
      const result = methods.length === 0 ? obj : rtMethod("$setupMethodGetters", [obj]);
      return [  result, stmts ];
    }

    function getVariantMemberId(m : A.VariantMember) : A.Name {
      switch(m.$name) {
        case 's-variant-member':
          // NOTE(joe/ben): with mutable/non-mutable fields having the same
          // runtime semantics (restricted by earlier TC/wrapping), no need
          // to switch on member-type here
          switch(m.dict.bind.$name) {
            case 's-tuple-bind': throw new InternalCompilerError(m.dict.bind.$name);
            case 's-bind':
              return m.dict.bind.dict.id;
          }
      }
    }

    function compileData(context : Context, expr : Variant<A.Expr, 's-data-expr'>) : [J.Expression, Array<J.Statement>] {
      const [sharedBaseObj, sharedBaseStmts] = compileObj(context, {$name: 's-obj', dict: { l: expr.dict.l, fields: expr.dict['shared-members'] } });
      const sharedBaseName = freshId(constId(`sharedBase_${expr.dict.name}`));
      const variants = listToArray(expr.dict.variants);
      const variantBaseObjs : Array<[A.Name, J.Expression, Array<J.Statement>]> = variants.map(v => {
        const [extensionV, extensionStmts] = compileExpr(context, { $name: "s-obj", dict: { l: v.dict.l, fields: v.dict['with-members'] } });
        const variantId = freshId(constId(`variantBase_${v.dict.name}`));
        const dataField = Property("$data", Identifier(sharedBaseName));
        const nameField = Property("$name", Literal(v.dict.name));
        let fieldNamesField;
        switch(v.$name) {
          case 's-singleton-variant': fieldNamesField = Literal(null); break;
          case 's-variant': {
            const binds = listToArray(v.dict.members).map(m => m.dict.bind);
            fieldNamesField = ArrayExpression(binds.map((b) => Literal(nameToName(bindToName(b)))));
            break;
          }
        }
        const meta = ObjectExpression([dataField, nameField, Property("$fieldNames", fieldNamesField)]);
        return [variantId, rtMethod("$createVariant", [Identifier(sharedBaseName), extensionV, meta]), extensionStmts];
      });
      const variantBaseStmts = variantBaseObjs.map(v => {
        return [ ...v[2], Var(v[0], v[1]) ]
      }).flat();
      const sharedPrelude = [
        ...sharedBaseStmts,
        Var(sharedBaseName, sharedBaseObj),
        ...variantBaseStmts
      ];

      const variantConstructors = variants.map((v, i) => {
        const base = Identifier(variantBaseObjs[i][0]);
        switch(v.$name) {
          case 's-singleton-variant':
            return Property(v.dict.name, base);
          case 's-variant':
            const argList = listToArray(v.dict.members).map(getVariantMemberId);
            const fields = argList.map(a => Property(nameToName(a), Identifier(jsIdOf(a))));
            const extension = ObjectExpression(fields);
            const body = BlockStatement([ReturnStatement(rtMethod("$makeDataValue", [base, extension]))]);
            const constructor = FunctionExpression(jsIdOf(constId(v.dict.name)), argList.map(jsIdOf), body);
            return Property(v.dict.name, constructor);
        }
      });

      function recognizer(name, compareVal, field) {
        const isname = `is-${name}`;
        const arg = constId("val");
        const argIsObject = BinaryExpression("===", UnaryExpression("typeof", Identifier(arg)), Literal("object"));
        const argIsNotNull = BinaryExpression("!==", Identifier(arg), Literal(null));
        const argIsVariant = BinaryExpression("===", DotExpression(Identifier(arg), field), compareVal);
        const check = LogicalExpression("&&", LogicalExpression("&&", argIsObject, argIsNotNull), argIsVariant);
        const checker = FunctionExpression(jsIdOf(constId(isname)), [arg], BlockStatement([ ReturnStatement(check) ]));
        return Property(isname, checker);
      }

      const variantRecognizers = variants.map((v, i) => {
        const base = Identifier(variantBaseObjs[i][0]);
        return recognizer(v.dict.name, DotExpression(base, "$variant"), "$variant");
      });

      const dataRecognizer = recognizer(expr.dict.name, Identifier(sharedBaseName), "$data");

      const dataPackage = ObjectExpression([...variantConstructors, ...variantRecognizers, dataRecognizer]);
      return [dataPackage, sharedPrelude];
    }

    function pyretLookup(l : A.Srcloc, obj : J.Expression, field : string) : J.Expression {
      return DotExpression(obj, field);
    }

    type CompileResult = [J.Expression, Array<J.Statement>];

    

    function compileModule(context : Context, expr: Variant<A.Expr, "s-module">) : CompileResult {
      const fields: J.Property[] = [];
      const stmts: J.Statement[] = [];
      const locs: J.ObjectExpression[] = [];
      listToArray(expr.dict['defined-values']).forEach(dv => {
        switch(dv.$name) {
          case 's-defined-value': {
            const [ val, fieldStmts ] = compileExpr(context, dv.dict['value']);
            const sloc = context.compileSrcloc(dv.dict.value.dict.l);
            fields.push(Property(dv.dict.name, val));
            stmts.push(...fieldStmts);
            locs.push(ObjectExpression([Property("name", Literal(dv.dict.name)), Property("srcloc", sloc)]));
            return;
          }
          case 's-defined-var': {
            const sloc = context.compileSrcloc(dv.dict.loc);
            fields.push(Property(dv.dict.name, Identifier(jsIdOf(dv.dict.id))));
            locs.push(ObjectExpression([Property("name", Literal(dv.dict.name)), Property("srcloc", sloc)]));
            return;
          }
        }
      });

      const [aExp, aStmts] = compileExpr(context, expr.dict.answer);
      const checkResults = (!context.importFlags['checker-import']) ?
        ArrayExpression([]) :
        CallExpression(DotExpression(Identifier(context.curCheckContext), 'results'), []);
      const traces = rtMethod("$getTraces", [Literal(context.uri)]);

      const answer1 = freshId(compilerName("answer"))
      const answerVar = Var(answer1, aExp)

      const ans = ObjectExpression([
        ...fields,
        Property("$answer", Identifier(answer1)),
        Property("$checks", checkResults),
        Property("$traces", traces),
        Property("$locations", ArrayExpression(locs))
      ]);
      context.options.dict.log.app("\nrunChecks check option: \n\n<" + context.options.dict.checks + ">\n\n", runtime.ffi.makeNone());
      // NOTE(Ben): We can't use compileExprScopedChecks to handle module compilation,
      // because the list of defined values isn't neatly block-scoped, and we can't
      // run the checks at the very end after the module's been put together.
      const callRunChecks = (!context.importFlags['checker-import']) ? ExpressionStatement(Literal("Skipped runChecks")) :
        ExpressionStatement(
          CallExpression(DotExpression(Identifier(context.curCheckContext), 'runChecks'),
            [Literal(context.provides.dict['from-uri']), ArrayExpression(context.checkBlockTestCalls)]
          ));
      const postLoadHook = rtMethod("$postLoadHook", [Literal(context.provides.dict['from-uri']), ans]);
      const assignAns = AssignmentExpression(DotExpression(Identifier(constId("module")), "exports"), postLoadHook);
      // NOTE(Ben): answerVar must come before callRunChecks, in case answerVar has side effects
      // This matches current Pyret semantics, e.g. in
      // var x = 1
      // check: x is 1 end # fails
      // x := 2
      // check: x is 2 end # fails
      // x := 3
      // answerVar here is compiled as `answer = $traceVal(x = 3)`, which would otherwise happen
      // after the callRunChecks, which would make the second test case unwantedly pass.
      return [assignAns, [...aStmts, answerVar, callRunChecks, ...stmts]];
    }

    function compileSOp(context : Context, op: string, lv: J.Expression, rv: J.Expression): J.Expression {
      switch(op) {
        case "op+": return rtMethod("_plus", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op-": return rtMethod("_minus", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op*": return rtMethod("_times", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op/": return rtMethod("_divide", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op<": return rtMethod("_lessthan", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op>": return rtMethod("_greaterthan", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op<=": return rtMethod("_lessequal", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op>=": return rtMethod("_greaterequal", [lv, rv, rtField(NUMBER_ERR_CALLBACKS)]); break;
        case "op==": return CallExpression(rtField(EQUAL_ALWAYS), [lv, rv]); break;
        case "op=~": return CallExpression(rtField(EQUAL_NOW), [lv, rv]); break;
        case "op<>": return UnaryExpression("!", CallExpression(rtField(EQUAL_ALWAYS), [lv, rv])); break;
        case "op<=>": return CallExpression(rtField(IDENTICAL), [lv, rv]); break;
        case "opor": return LogicalExpression("||", lv, rv); break;
        case "opand": return LogicalExpression("&&", lv, rv); break;
        case "op^": return CallExpression(rv, [lv]); break;
        default: throw new TODOError(`Not yet implemented: ${op}`);        
      }
    }

    function compileOp(context : Context, expr : Variant<A.Expr, "s-op">) : CompileResult {
      const [lv, lStmts] = compileExpr(context, expr.dict.left);
      const [rv, rStmts] = compileExpr(context, expr.dict.right);
      return [compileSOp(context, expr.dict.op, lv, rv), [...lStmts, ...rStmts]];
    }

    function compileIf(context : Context, branches: (A.IfBranch | A.IfPipeBranch)[], compiledElse: CompileResult) : CompileResult {
      const ans = Identifier(freshId(compilerName('ans')));
      const [elseV, elseStmts] = compiledElse;
      const elseBlock = BlockStatement([
        ...elseStmts,
        ExpressionStatement(AssignmentExpression(ans, elseV))
      ]);
      let block = elseBlock;
      for (let i = branches.length - 1; i >= 0; i--) {
        const branch = branches[i];
        const [testV, testStmts] = compileExpr(context, branch.dict.test);
        const [bodyV, bodyStmts] = compileExprScopedChecks(context, branch.dict.body);
        block = BlockStatement([
          ...testStmts,
          IfStatement(testV, BlockStatement([
            ...bodyStmts,
            ExpressionStatement(AssignmentExpression(ans, bodyV))
          ]),
          block)
        ]);
      }
      return [ans, block.body];
    }

    function compileCases(context : Context, expr : Variant<A.Expr, "s-cases-else">) : CompileResult {
      const ans = freshId(compilerName("ans"));
      const [val, valStmts] = compileExpr(context, expr.dict.val);
      const switchBlocks = listToArray(expr.dict.branches).map(b => {
        const [bodyVal, bodyStmts] = compileExprScopedChecks(context, b.dict.body);
        switch(b.$name) {
          case 's-cases-branch':
            const argBinds = listToArray(b.dict.args).map((a, i) => {
              const b = a.dict.bind as Variant<A.Bind, "s-bind">;
              return Var(jsIdOf(b.dict.id), BracketExpression(val, BracketExpression(DotExpression(val, "$fieldNames"), Literal(i))))
            });
            const assignAnswer = ExpressionStatement(AssignmentExpression(Identifier(ans), bodyVal));
            return Case(Literal(b.dict.name), [...argBinds, ...bodyStmts, assignAnswer, BreakStatement]);
          case 's-singleton-cases-branch': {
            const assignAnswer = ExpressionStatement(AssignmentExpression(Identifier(ans), bodyVal));
            return Case(Literal(b.dict.name), [...bodyStmts, assignAnswer, BreakStatement]);
          }
        }
      });
      const [elseV, elseStmts] = compileExprScopedChecks(context, expr.dict._else);
      const elseCase = Default([...elseStmts, ExpressionStatement(AssignmentExpression(Identifier(ans), elseV))])
      return [
        Identifier(ans),
        [...valStmts, Var(ans, undefined), SwitchStatement(DotExpression(val, "$name"), [...switchBlocks, elseCase])]
      ];
    }

    function compileSFor(context : Context, expr : Variant<A.Expr, "s-for">): CompileResult {
      const binds: A.Bind[] = [];
      const args: A.Expr[] = [];
      listToArray(expr.dict.bindings).forEach((b) => {
        binds.push(b.dict.bind);
        args.push(b.dict.value);
      });
      const lam: Variant<A.Expr, 's-lam'> = {
        $name: 's-lam',
        dict: {
          l: expr.dict.body.dict.l,
          name: 'loop',
          params: runtime.ffi.makeList([]),
          args: arrayToList(binds),
          ann: expr.dict.ann,
          body: expr.dict.body,
          doc: '',
          _check: runtime.ffi.makeNone(),
          "_check-loc": runtime.ffi.makeNone(),
          blocky: true,
        },
      };
      const call: Variant<A.Expr, 's-app'> = {
        $name: 's-app',
        dict: {
          l: expr.dict.l,
          _fun: expr.dict.iterator,
          args: runtime.ffi.makeList([lam, ...args]),
        },
      };
      return compileExpr(context, call);
    }

    function compileCheckBlock(context : Context, expr : Variant<A.Expr, "s-check">) : CompileResult {
      context.options.dict.log.app("Checks: " + context.options.dict.checks, runtime.ffi.makeNone());
      if(context.options.dict.checks === "none") {
        return [Literal("Skipped check blocks"),[]];
      }
      context.importFlags["checker-import"] = true;
      const [ checkBlockVal, checkBlockStmts ] = compileExpr(context, expr.dict.body);
      let jsCheckBlockFuncName: A.Name;
      let testBlockName: J.Expression;
      const name = expr.dict.name;
      jsCheckBlockFuncName = freshId(compilerName(expr.dict['keyword-check'] ? "check-block" : "examples-block"));
      switch(name.$name) {
        case 'none':
          testBlockName = Literal(nameToSourceString(jsCheckBlockFuncName));
          break;
        case 'some':
          testBlockName = Literal(name.dict.value);
          break;
      }
      const jsCheckBlockFuncBlock = BlockStatement([...checkBlockStmts, ExpressionStatement(checkBlockVal)]);
      const jsCheckBlockFunc = FunctionExpression(jsCheckBlockFuncName, [], jsCheckBlockFuncBlock);
      const blockLoc = context.compileSrcloc(chooseSrcloc(expr.dict.l, context));
      //const testerCall = ExpressionStatement(rtMethod("$checkBlock", [blockLoc, testBlockName, jsCheckBlockFunc]));
      const testerCall = ObjectExpression([
          Property('keywordCheck', Literal(expr.dict['keyword-check'])),
          Property('location', blockLoc),
          Property('name', testBlockName),
          Property('run', jsCheckBlockFunc)
        ]);
      context.checkBlockTestCalls.push(testerCall);
      return [UNDEFINED, []];
    }

    /**
     * 
      Emits:
        _checkTest(lh-func: () -> any, rh-func: () -> any,
                   test-func: (check-expr-result, check-expr-result) -> check-op-result,
                   loc: String) -> void
      
        _checkTest(function lh-func() {},
                   function rh-func() {},
                   function test-func(lhs, rhs) {}, loc);
      
        _checkTest: (test-thunk: () -> check-op-result, loc: string) -> void
        
        check-expr-result = {
          value: any,
          exception: bool
          exception_val: object;
        }
        
        check-op-result = {
          success: boolean,
          lhs: check-expr-result,
          rhs: check-expr-result,
          exception: object | undefined,
        }
        
        Individual tests are wrapped in functions to allow individual tests to fail
        but still possible to run other tests
     */
    function compileCheckTest(context : Context, expr : Variant<A.Expr, "s-check-test">) : CompileResult {
      function thunk(name: string, expr: A.Expr): J.FunctionExpression {
        const [exp, expStmts] = compileExpr(context, expr);
        const body = BlockStatement([...expStmts, ReturnStatement(exp)]);
        return FunctionExpression(compilerName(name), [], body);
      }
      function maybeThunk(name: string, optExp: A.Option<A.Expr>): J.FunctionExpression | undefined {
        switch(optExp.$name) {
          case 'none': return undefined;
          case 'some': return thunk(name, optExp.dict.value);
        }
      }
      function locOf(exp: A.Expr): J.Expression {
        return context.compileSrcloc(chooseSrcloc(exp.dict.l, context));
      }
      function maybeLocOf(exp: A.Option<A.Expr>): J.Expression {
        switch (exp.$name) {
          case 'none': return UNDEFINED;
          case 'some': return locOf(exp.dict.value);
        }
      }
      const {l, op, refinement, left, right: rightOpt, cause} = expr.dict;
      const leftThunk = thunk("LHS", left);
      const rightThunk = maybeThunk("RHS", rightOpt);
      const causeThunk = maybeThunk("CAUSE", cause);
      const refinementThunk = maybeThunk("REFINE", refinement);
      const loc = locOf(expr);
      const metadata = ObjectExpression([
        Property('opName', Literal(op.$name)),
        Property('loc', loc),
        Property('on-left', locOf(left)),
        Property('on-right', maybeLocOf(rightOpt)),
        Property('on-cause', maybeLocOf(cause)),
        Property('on-refinement', maybeLocOf(refinement))
      ]);
      let testCall: J.Expression;
      let checkContext: J.Expression = Identifier(context.curCheckContext);
      switch(op.$name) {
        case 's-op-is':
          if (refinementThunk && causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRefinementCause'), 
                                      [refinementThunk, leftThunk, rightThunk!, causeThunk, metadata]);
          } else if (refinementThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRefinement'), 
                                      [refinementThunk, leftThunk, rightThunk!, metadata]);
          } else if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsCause'), 
                                      [leftThunk, rightThunk!, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIs'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-is-not':
          if (refinementThunk && causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRefinementCause'), 
                                      [refinementThunk, leftThunk, rightThunk!, causeThunk, metadata]);
          } else if (refinementThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRefinement'), 
                                      [refinementThunk, leftThunk, rightThunk!, metadata]);
          } else if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotCause'), 
                                      [leftThunk, rightThunk!, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNot'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-is-op': {
          const newRefinement = FunctionExpression(compilerName("REFINE"), [], BlockStatement([ReturnStatement(rtField(OP_TO_FUNCTION[op.dict.op]))]));
          metadata.properties.push(Property("opIsName", Literal(op.dict.op)))
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRefinementCause'), 
                                      [newRefinement, leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRefinement'), 
                                      [newRefinement, leftThunk, rightThunk!, metadata]);
          }
          break;
        }
        case 's-op-is-not-op': {
          const newRefinement = FunctionExpression(compilerName("REFINE"), [], BlockStatement([ReturnStatement(rtField(OP_TO_FUNCTION[op.dict.op]))]));
          metadata.properties.push(Property("opIsName", Literal(op.dict.op)))
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRefinementCause'), 
                                      [newRefinement, leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRefinement'), 
                                      [newRefinement, leftThunk, rightThunk!, metadata]);
          }
          break;
        }
        case 's-op-is-roughly':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRoughlyCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsRoughly'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-is-not-roughly':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRoughlyCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkIsNotRoughly'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-raises':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesStrCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesStr'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-raises-not':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesNotCause'), 
                                      [leftThunk, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesNot'), 
                                      [leftThunk, metadata]);
          }
          break;
        case 's-op-raises-other':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesOtherStrCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesOtherStr'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-raises-satisfies':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesSatisfiesCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesSatisfies'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-raises-violates':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesViolatesCause'), 
                                      [leftThunk, rightThunk!, causeThunk, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkRaisesViolates'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-satisfies':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkSatisfiesDelayedCause'), 
                                      [leftThunk, rightThunk!, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkSatisfiesDelayed'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        case 's-op-satisfies-not':
          if (causeThunk) {
            testCall = CallExpression(DotExpression(checkContext, 'checkSatisfiesNotDelayedCause'), 
                                      [leftThunk, rightThunk!, metadata]);
          } else {
            testCall = CallExpression(DotExpression(checkContext, 'checkSatisfiesNotDelayed'), 
                                      [leftThunk, rightThunk!, metadata]);
          }
          break;
        default: throw new ExhaustiveSwitchError(op);
      }
      return [testCall, []];
    }

    function compileTable(context : Context, expr : Variant<A.Expr, 's-table'>): CompileResult {
      context.importFlags['table-import'] = true;

      const func = BracketExpression(Identifier(TABLE), Literal("_makeTable"));

      const jsHeaders = listToArray(expr.dict.headers).map(h => Literal(h.dict.name));

      const rows: J.Expression[] = [];
      const rowsStmts: J.Statement[] = [];
      listToArray(expr.dict.rows).forEach((r) => {
        const rowElems = r.dict.elems;
        const elemValues: J.Expression[] = [];
        const elemStmts: J.Statement[] = [];
        listToArray(rowElems).forEach((re) => {
          const [v, vStmts] = compileExpr(context, re);
          elemValues.push(v);
          elemStmts.push(...vStmts);
        });
        const jsRow = ArrayExpression(elemValues);
        rows.push(jsRow);
        rowsStmts.push(...elemStmts);
      });
      const args = [ArrayExpression(jsHeaders), ArrayExpression(rows)];

      return [CallExpression(func, args), rowsStmts];      
    }

    function compileLoadTable(context : Context, expr : Variant<A.Expr, 's-load-table'>): CompileResult {
      // This case handles `loadTable` syntax. The lines in the following Pyret
      // code,
      //
      // | myTable = loadTable: a, b, c
      // |   source: csvOpen('myTable.csv')
      // | end
      //
      // compile into JavaScript code that resembles the following:
      //
      // | var myTable = _makeTableFromTableSkeleton(
      // |                 _tableSkeletonChangeHeaders(
      // |                   csvOpen('csv.txt'),
      // |                   ["a", "b", "

      // NOTE(michael):
      //  sLoadTable is currently implemented for a single LoadTableSpec of type
      //  sTableSrc, meaning that using one or more `sanitize` forms will result in
      //  a notYetImplemented error.

      const spec = listToArray(expr.dict.spec);
      if (spec.length !== 1) {
        throw new TODOError("sLoadTable with != 1 spec");
      } else {
        switch(spec[0].$name) {
          case 's-sanitize':
            throw new TODOError("sLoadTable with a sanitize spec");
          case 's-table-src': {
            // Set the tableImport flag
            context.importFlags['table-import'] = true;

            const tableId = Identifier(TABLE);
            const makeTableFunc =
              BracketExpression(tableId, Literal("_makeTableFromTableSkeleton"));
            const changeHeadersFunc =
              BracketExpression(tableId, Literal("_tableSkeletonChangeHeaders"));

            const [headersExprArgs, headersExprStmts] = compileExpr(context, spec[0].dict.src);

            const headerStringsList: J.Expression[] = [];
            listToArray(expr.dict.headers).forEach((fieldName) => {
              headerStringsList.push(Literal(fieldName.dict.name));
            });

            const headerStrings = ArrayExpression(headerStringsList);

            const changeHeadersExpr = CallExpression(changeHeadersFunc, [headersExprArgs, headerStrings]);

            const exprArgs = [changeHeadersExpr];
            const makeTableExpr = CallExpression(makeTableFunc, exprArgs);

            return [makeTableExpr, headersExprStmts];
          }
        }
      }
    }

    function compileTableExtend(context : Context, expr: Variant<A.Expr, 's-table-extend'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      // This case handles `extend` syntax. The starred lines in the following
      // Pyret code,
      //
      //        | my-table = table: a, b, c
      //        |   row: 1, 2, 3
      //        |   row: 4, 5, 6
      //        |   row: 7, 8, 9
      //        | end
      //        |
      // *      | my-extended-table = extend my-table using a, b
      // *(Map) |   d: a / 2,           // a "Mapping" extension
      // *(Red) |   e: running-sum of b // a "Reducer"
      // *      | end
      //
      // compile into JavaScript code that resembles the following:
      //
      // *      | var myExtendedTable = _tableReduce(
      // *      |   myTable,
      // *      |   [
      // *(Map) |     { "type": "map",
      // *(Map) |       "reduce": (rowNumber) => {
      // *(Map) |         var columnNumberB = _tableGetColumnIndex(myTable, "b");
      // *(Map) |         var b = myTable["_rows"][rowNumber][columnNumberB];
      // *(Map) |         var columnNumberA = _tableGetColumnIndex(myTable, "a");
      // *(Map) |         var a = myTable["_rows"][rowNumber][columnNumberA];
      // *(Map) |         return a / 2; },
      // *(Map) |       "extending": "d" },
      // *(Red) |     { "type": "reduce",
      // *(Red) |       "one": runningSum["one"],
      // *(Red) |       "reduce": runningSum["reduce"],
      // *(Red) |       "using": "b",
      // *(Red) |       "extending": "e" }
      // *      |   ]);
      //
      // The actual "extending" work is done by _tableReduce at runtime.

      const columnBindsL = expr.dict['column-binds'].dict.l;
      const columnBindsBinds = listToArray(expr.dict['column-binds'].dict.binds);
      const columnBindsTable = expr.dict['column-binds'].dict.table;
      const [tableExpr, tableStmts] = compileExpr(context, columnBindsTable);

      const reducersExprs: J.Expression[] = [];
      const reducersStmts: J.Statement[] = [];
      listToArray(expr.dict.extensions).forEach((extension) => {
        switch(extension.$name) {
          case 's-table-extend-reducer': {
            // Handles Reducer forms, like `e: runningSum of b`.

            const [reducerExpr, reducerStmts] = compileExpr(context, extension.dict.reducer);

            const typeFieldName = "type";
            const typeFieldValue = Literal("reduce");
            const typeField = Property(typeFieldName, typeFieldValue);

            const oneFieldName = "one";
            const oneFieldValueObj = reducerExpr;
            const oneFieldValueField = Literal("one");
            const oneFieldValue =
              BracketExpression(oneFieldValueObj, oneFieldValueField);
            const oneField = Property(oneFieldName, oneFieldValue);

            const reduceFieldName = "reduce";
            const reduceFieldValueObj = reducerExpr;
            const reduceFieldValueField = Literal("reduce");
            const reduceFieldValue =
              BracketExpression(reduceFieldValueObj, reduceFieldValueField);
            const reduceField = Property(reduceFieldName, reduceFieldValue)

            const usingFieldName = "using";
            const usingFieldValue = Literal(nameToName(extension.dict.col));
            const usingField = Property(usingFieldName, usingFieldValue);

            const extendingFieldName = "extending";
            const extendingFieldValue = Literal(extension.dict.name);
            const extendingField = Property(extendingFieldName, extendingFieldValue);

            const reducerObjectFields = [
              typeField,
              oneField,
              reduceField,
              usingField,
              extendingField,
            ];
            const reducerObject = ObjectExpression(reducerObjectFields);

            reducersExprs.push(reducerObject);
            reducersStmts.push(...reducerStmts);
            break;
          }
          case 's-table-extend-field': {
            // Handles Mapping forms, like `d: a / 2`.

            const typeFieldName = "type";
            const typeFieldValue = Literal("map");
            const typeField = Property(typeFieldName, typeFieldValue);

            const reduceFieldName = "reduce";

            const funName = freshId(compilerName("s-table-extendfield"));
            const rowNumberName = freshId(compilerName("rowNumber"));
            const funArgs = [rowNumberName];
            const indexingStmts: J.Statement[] = [];
            listToArray(expr.dict['column-binds'].dict.binds).forEach((bind) => {

              const bindId = bindToName(bind);
              
              const getIndexName = freshId(compilerName("columnNumber"));
              const getColumnIndex = BracketExpression(Identifier(TABLE), Literal("_tableGetColumnIndex"));
              const columnIndexArgs = [tableExpr, Literal(nameToName(bindId))];
              const getIndexRhs = CallExpression(getColumnIndex, columnIndexArgs);
              const getIndexStmt = Var(getIndexName, getIndexRhs);
              
              const indexName = bindId
              const tableRows = BracketExpression(tableExpr, Literal("_rows"));
              const currentRow = BracketExpression(tableRows, Identifier(rowNumberName));
              const indexRhs = BracketExpression(currentRow, Identifier(getIndexName));
              const assignIndexStmt = Var(jsIdOf(indexName), indexRhs)
              
              indexingStmts.push(getIndexStmt, assignIndexStmt);
            });

            const [returnExpr, returnCompiledStmts] = compileExpr(context, extension.dict.value);
            const returnStmt = ReturnStatement(returnExpr);

            const bodyStmts = [...indexingStmts, returnStmt];
            const funBody = BlockStatement(bodyStmts);

            const reduceFieldValue = FunctionExpression(funName, funArgs, funBody);
            const reduceField = Property(reduceFieldName, reduceFieldValue);

            const extendingFieldName = "extending";
            const extendingFieldValue = Literal(extension.dict.name);
            const extendingField = Property(extendingFieldName, extendingFieldValue);

            const mappingObjectFields = [
              typeField,
              reduceField,
              extendingField,
            ];
            const mappingObject = ObjectExpression(mappingObjectFields);

            // NOTE(Ben): this is different than the code in direct-codegen:
            // it put the returnCompiledStmts in accExprs, I think by mistake
            reducersExprs.push(mappingObject);
            reducersStmts.push(...returnCompiledStmts);
            break;
          }
        }
      });

      const exprFuncObj = Identifier(TABLE);
      const exprFuncField = Literal("_tableReduce");
      const applyExprFunc = BracketExpression(exprFuncObj, exprFuncField);
      const applyExprArgs = [tableExpr,  ArrayExpression(reducersExprs)];
      const applyExpr = CallExpression(applyExprFunc, applyExprArgs);

      const applyStmts = [...tableStmts, ...reducersStmts];

      return [applyExpr, applyStmts]
    }

    function compileTableUpdate(context : Context, expr : Variant<A.Expr, 's-table-update'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      // This case handles `transform` syntax. The starred lines in the following
      // Pyret code,
      //
      //   | my-table = table: name, age, favorite-color
      //   |   row: "Bob", 12, "blue"
      //   |   row: "Alice", 17, "green"
      //   |   row: "Eve", 14, "red"
      //   | end
      //   |
      // * | age-fixed = transform my-table using age:
      // * |   age: age + 1
      // * | end
      //
      // compile into JavaScript code that resembles the following:
      //
      // * | var ageFixed = _tableTransform(
      // * |   myTable,
      // * |   ["age"],
      // * |   []
      // * | );
      //
      // The actual "transforming" work is done by _tableTransform at runtime.

      const [tableExpr, tableStmts] = compileExpr(context, expr.dict['column-binds'].dict.table);

      // makes a list of strings (column names)
      const updates = listToArray(expr.dict.updates)
      const listColnames = updates.map((u) => Literal(u.dict.name));

      const columnUpdateZip: Array<[A.Bind, A.Expr]> = listToArray(expr.dict['column-binds'].dict.binds)
        .map((cb, i) => {
          const update = updates[i];
          switch(update.$name) {
            case 's-data-field': return [ cb, update.dict.value];
            default: 
              throw new InternalCompilerError(`Invalid update type ${updates[i].$name} at index ${i}`);
          }
        });

      // makes a list of functions
      const funName = freshId(compilerName("sTableTransform"));
      const listUpdates = columnUpdateZip.map((cu) => {
        const [bind, updateExpr] = cu;

        // Use the Bind in ColumnBind as the parameter in the generated function
        const funArgs = [jsIdOf(bindToName(bind))];

	      const [uValueExpr, uValueStmts] = compileExpr(context, updateExpr);
        const blockReturnStmt = ReturnStatement(uValueExpr);
        const blockStmts = [...tableStmts, blockReturnStmt];
        const funBody = BlockStatement(blockStmts);
        const uFun = FunctionExpression(funName, funArgs, funBody);
        return uFun;
      });

      const appFunc = BracketExpression(Identifier(TABLE), Literal("_tableTransform"));
      const appArgs = [tableExpr, ArrayExpression(listColnames), ArrayExpression(listUpdates)];

      const returnExpr = CallExpression(appFunc, appArgs);
      const returnStmts = tableStmts;

      // tableTansform(table, colnames, updates)
      return [returnExpr, returnStmts];
    }

    function compileTableSelect(context : Context, expr : Variant<A.Expr, 's-table-select'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      const func = BracketExpression(Identifier(TABLE), Literal("_selectColumns"));

      const jsColumns = listToArray(expr.dict.columns).map((c) => Literal(nameToName(c)));

      const [jsTable, jsTableStmts] = compileExpr(context, expr.dict.table);

      const args = [jsTable, ArrayExpression(jsColumns)];

      // selectColumns(table, colnames)
      return [CallExpression(func, args), jsTableStmts];
    }

    function compileTableFilter(context : Context, expr: Variant<A.Expr, 's-table-filter'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      // This case handles `sieve` syntax. The starred lines in the following
      // Pyret code,
      //
      //   | my-table = table: a, b, c
      //   |   row: 1, 2, 3
      //   |   row: 4, 5, 6
      //   |   row: 7, 8, 9
      //   | end
      //   |
      // * | my-filtered-table = sieve my-table using b:
      // * |   (b / 4) == 2
      // * | end
      //
      // compile into JavaScript code that resembles the following:
      //
      // * | var myFilteredTable = _tableFilter(myTable, function sTableFilter(row) {
      // * |     var index = _tableGetColumnIndex(myTable, "b");
      // * |     var b = row[index];
      // * |     return (b / 4) == 2;
      // * | }
      //
      // The actual "sieving" work is done by _tableFilter at runtime.

      const [tableExpr, tableStmts] = compileExpr(context, expr.dict['column-binds'].dict.table);

      const rowName = freshId(compilerName("row"));

      const blockRowElementStmts = listToArray(expr.dict['column-binds'].dict.binds).flatMap((bind) => {

        // generate the `var index = _tableGetColumnIndex(myTable, "b");` lines.
        const appFunc = BracketExpression(Identifier(TABLE), Literal("_tableGetColumnIndex"));
        const appArgs = [tableExpr, Literal(nameToName(bindToName(bind)))];
        
        // generate the `var b = row[index];` lines.
        const columnIndexExpr = CallExpression(appFunc, appArgs);
        const columnIndexId = freshId(compilerName("index"));
        const columnIndexStmt = Var(columnIndexId, columnIndexExpr);
        const varStmt = Var(jsIdOf(bindToName(bind)), BracketExpression(Identifier(rowName), Identifier(columnIndexId)));
        
        return [columnIndexStmt, varStmt];
      });

      const funName = freshId(compilerName("s-table-filter"))
      const funArgs = [rowName];

      const [predicateExpr, predicateStmts] = compileExpr(context, expr.dict.predicate);
      const blockReturnStmt = ReturnStatement(predicateExpr);
      const blockStmts = [...blockRowElementStmts, blockReturnStmt];
      const funBody = BlockStatement(blockStmts);
      const filterFun = FunctionExpression(funName, funArgs, funBody);

      const appFunc = BracketExpression(Identifier(TABLE), Literal("_tableFilter"));
      const appArgs = [tableExpr, filterFun];
      const apply = CallExpression(appFunc, appArgs);

      const returnExpr = apply;
      const returnStmts = [...predicateStmts, ...tableStmts];

      return [returnExpr, returnStmts];
    }

    function compileTableOrder(context : Context, expr: Variant<A.Expr, 's-table-order'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      // This case handles `order` syntax. The starred lines in the following
      // Pyret code,
      //
      //   | my-table = table: name, age, favorite-color
      //   |   row: "Bob", 12, "blue"
      //   |   row: "Alice", 12, "green"
      //   |   row: "Eve", 13, "red"
      //   | end
      //   |
      // * | name-ordered = order my-table:
      // * |   age descending,
      // * |   name ascending
      // * | end
      //
      // compile into JavaScript code that resembles the following:
      //
      // * | var nameOrdered = _tableOrder(
      // * |   myTable,
      // * |   [{"column": "age", "direction": "descending"},
      // * |    {"column": "name", "direction": "ascending"}]);
      //
      // The actual "ordering" work is done by _tableOrder at runtime.

      const [tableExpr, tableStmts] = compileExpr(context, expr.dict.table);

      const orderingListElements = listToArray(expr.dict.ordering).map((theOrder) => {
        const theOrderColumn = theOrder.dict.column;
        const theOrderDirection = theOrder.dict.direction;
        
        const orderColumnField = Property("column", Literal(nameToName(theOrderColumn)));
        const orderDirectionField = Property(
          "direction",
          Literal(theOrderDirection.$name.toLowerCase())
        );
        
        const orderFields = [orderColumnField, orderDirectionField];
        const orderObj = ObjectExpression(orderFields);
        
        return orderObj;
      });

      const orderingListExpr = ArrayExpression(orderingListElements);

      const appFunc = BracketExpression(Identifier(TABLE), Literal("_tableOrder"));
      const appArgs = [tableExpr, orderingListExpr];
      const apply = CallExpression(appFunc, appArgs);

      const returnExpr = apply;
      const returnStmts = tableStmts;

      return [returnExpr, returnStmts];

    }

    function compileTableExtract(context : Context, expr: Variant<A.Expr, 's-table-extract'>): CompileResult {
      // Set the table-import flag
      context.importFlags['table-import'] = true;

      // This case handles `extract` syntax. The starred line in the following
      // Pyret code,
      //
      //   | my-table = table: a, b, c
      //   |   row: 1, 2, 3
      //   |   row: 4, 5, 6
      //   |   row: 7, 8, 9
      //   | end
      //   |
      // * | column-b = extract b from my-table end
      //
      // compiles into JavaScript code that resembles the following:
      //
      // * | var columnB = _tableExtractColumn(myTable, "b");
      //
      // The actual "extracting" work is done by _tableExtractColumn at runtime.

      const [tableExpr, tableStmts] = compileExpr(context, expr.dict.table);

      const appFunc = BracketExpression(Identifier(TABLE), Literal("_tableExtractColumn"));
      const appArgs = [tableExpr, Literal(nameToName(expr.dict.column))];
      const apply = CallExpression(appFunc, appArgs);

      const returnExpr = apply;
      const returnStmts = tableStmts;

      return [returnExpr, returnStmts];
    }

    function compileSpy(context : Context, expr : Variant<A.Expr, 's-spy-block'>): CompileResult {
      // Model each spy block as a spy block object
      // SpyBlockObject {
      //   message: () -> String,
      //   loc: String,
      //   exprs: List<{ key: String, expr: () -> JSValue, loc: String }>
      // }
      //
      // Translate spy blocks into:
      //   builtinSpyFunction(SpyBlockObject)
      //
      // Push responsibility of runtime spy-enabling to the builtinSpyFunction
      if (context.options.dict['enable-spies']) {
        // Generate spy code

        // Generate message code
        let jsMessageValue: J.Expression;
        let jsMessageStmts: J.Statement[] = [];
        switch(expr.dict.message.$name) {
          case 'some': {
            [jsMessageValue, jsMessageStmts] = compileExpr(context, expr.dict.message.dict.value);
            break;
          }
          case 'none':
            // Use 'null' to signal the builtinSpyFunction that there was no spy block message
            jsMessageValue = Literal(null);
            break;
        }

        // Create the message generation function
        const jsMessageFuncName = freshId(compilerName("spy-message"));
        const jsMessageReturn = ReturnStatement(jsMessageValue);
        const jsMessageFuncBlock = BlockStatement([...jsMessageStmts, jsMessageReturn]);
        const jsMessageFunc = FunctionExpression(jsMessageFuncName, [], jsMessageFuncBlock);

        // Compile each spy expression into the expression list
        const jsSpyFields: J.Expression[] = [];
        listToArray(expr.dict.contents).forEach((pyretSpyField) => {

          const [jsSpyValue, jsSpyStmts] = compileExpr(context, pyretSpyField.dict.value);
          
          const jsSpyExprFuncName = freshId(compilerName("spyExpr"));
          
          const jsSpyReturn = ReturnStatement(jsSpyValue);
          const jsSpyExprFuncBlock = BlockStatement([...jsSpyStmts, jsSpyReturn]);
          const jsSpyExprFun = FunctionExpression(jsSpyExprFuncName, [], jsSpyExprFuncBlock);
          
          // Create the spy expression object
          const jsSpyKey = Property("key", Literal(pyretSpyField.dict.name));
          const jsSpyExpr = Property("expr", jsSpyExprFun);
          const jsSpyLoc = Property("loc", context.compileSrcloc(pyretSpyField.dict.l));
          const jsSpyExprObj = ObjectExpression([jsSpyKey, jsSpyExpr, jsSpyLoc]);
          
          jsSpyFields.push(jsSpyExprObj);
        });

        const jsSpyLoc = context.compileSrcloc(expr.dict.l);

        // Create the SpyBlockObject
        const jsSpyFieldsList = ArrayExpression(jsSpyFields);
        const spyBlockObj = ObjectExpression([
          Property("message", jsMessageFunc),
          Property("loc", jsSpyLoc),
          Property("exprs", jsSpyFieldsList)
        ]);

        // Builtin spy function call
        // Runtime is responsible for output
        const spyCall = ExpressionStatement(rtMethod("$spy", [spyBlockObj]));

        return [UNDEFINED, [spyCall]];
      } else {
        // Do NOT generate spy code
        return [UNDEFINED, []]
      }
    }

    function compileUpdate(context : Context, expr : Variant<A.Expr, 's-update'>) : CompileResult {
      const [objExpr, objStmts] = compileExpr(context, expr.dict.supe);
      const [objId, idStmts] = freshOrReuseId(objExpr, "update-obj");
      const stmts = [...objStmts, ...idStmts];
      const fields = listToArray(expr.dict.fields);
      fields.forEach(field => {
        if(field.$name === 's-method-field') {
          throw new InternalCompilerError('method field in mutable update');
        }
        const [fieldValue, fieldStmts] = compileExpr(context, field.dict.value);
        stmts.push(...fieldStmts);
        const fieldAccess = DotExpression(objId, field.dict.name);
        stmts.push(ExpressionStatement(AssignmentExpression(fieldAccess, fieldValue)));
      });
      return [objId, stmts];
    }

    function maybeMakeNumberFromString(n : any) {
      if(typeof n === "number") {
        return Literal(n);
      }
      else {
        return rtMethod("_makeNumberFromString", [Literal(n.toString()), rtField(NUMBER_ERR_CALLBACKS)]);
      }
    }

    function compileExprScopedChecks(context : Context, expr: A.Expr): CompileResult {
      const newContext: Context = { ...context, checkBlockTestCalls: [] }
      const [ans, ansStmts] = compileExpr(newContext, expr);
      const callRunChecks: J.Statement[] = [];
      if (newContext.checkBlockTestCalls.length > 0) {
        if (!context.importFlags['checker-import']) {
          callRunChecks.push(ExpressionStatement(Literal("Skipped runChecks")));
        } else {
          callRunChecks.push(ExpressionStatement(
            CallExpression(DotExpression(Identifier(context.curCheckContext), 'runChecks'),
              [Literal(context.provides.dict['from-uri']), 
                ArrayExpression(newContext.checkBlockTestCalls)]
            )));
        }
      }
      return [ans, [...ansStmts, ...callRunChecks]];
    }

    function compileExpr(context : Context, expr : A.Expr) : CompileResult {
      switch(expr.$name) {
        case 's-module':
          return compileModule(context, expr);
        case 's-block':
          return compileSeq(context, expr.dict.stmts);
        case 's-num': {
          let numAns = maybeMakeNumberFromString(expr.dict.n);
          return [numAns, []];
        }
        case 's-frac':  {
          const numstr = maybeMakeNumberFromString(expr.dict.num);
          const denstr = maybeMakeNumberFromString(expr.dict.den);
          return [rtMethod('$makeRational', [numstr, denstr, rtField(NUMBER_ERR_CALLBACKS)]), []];
        }
        case 's-rfrac': {
          const numstr = maybeMakeNumberFromString(expr.dict.num);
          const denstr = maybeMakeNumberFromString(expr.dict.den);
          return [rtMethod('$numToRoughnum', [rtMethod("_divide", [numstr, denstr, rtField(NUMBER_ERR_CALLBACKS)]), rtField(NUMBER_ERR_CALLBACKS)]), []];
        }
        case 's-str':
          return [Literal(expr.dict.s), []];
        case 's-bool':
          return [Literal(expr.dict.b), []];
        case 's-prim-val': {
          return [rtField(expr.dict.name), []];
        }
          
        case 's-id': {
          const b = context.postEnv.dict.bindings;
          const key = nameToKey(expr.dict.id);
          if(runtime.getField(b, "has-key-now").app(key) && 
             !(runtime.getField(b, "get-value-now").app(key).dict.origin.dict["new-definition"])) {
            context.freeBindings.set(key, runtime.getField(b, "get-value-now").app(key))
          }
          return [Identifier(jsIdOf(expr.dict.id)), []];
        }
        case 's-id-letrec': {
          const isSafe = expr.dict.safe;
          if(isSafe) {
            return [Identifier(jsIdOf(expr.dict.id)), []];
          }
          else {
            const id = Identifier(jsIdOf(expr.dict.id));
            return [ConditionalExpression(
              BinaryExpression("!==", id, UNDEFINED),
              id,
              rtMethod("$messageThrow", [context.compileSrcloc(expr.dict.l), Literal("Uninitialized letrec identifier")])
            ), []];
          }
        }
        case 's-id-modref': {
          const [objv, objStmts] = compileExpr(context, { $name: "s-id", dict: { l: expr.dict.l, id: expr.dict.id }});
          return [ DotExpression(objv, expr.dict.name), objStmts ];
        }
        case 's-id-var': {
          // TODO(Ben, Joe): This needs to change when we figure out how vars should be represented:
          // Vars that are provided from a module must be boxed, so we need to use them consistently 
          // as boxes rather than as mutable JS variables.
          return [Identifier(jsIdOf(expr.dict.id)), []];
        }
        case 's-prim-app':
          const [argvs, argstmts] = compileList(context, expr.dict.args);
          const primAns = CallExpression(rtField(expr.dict._fun), argvs);
          return [primAns, argstmts];
        case 's-app-enriched': // TODO(joe): use info
        case 's-app': {
          const [fv, fstmts] = compileExpr(context, expr.dict._fun);
          const [argvs, argstmts] = compileList(context, expr.dict.args);
          return [ CallExpression(fv, argvs), [...fstmts, ...argstmts]];
        }
        case 's-srcloc':
          return [context.compileSrcloc(expr.dict.loc), []];
        case 's-op':
          return compileOp(context, expr);
        case 's-lam': {
          const [ bodyVal, bodyStmts ] = compileExprScopedChecks(context, expr.dict.body);
          const bindArgs = expr.dict.args as T.List<Variant<A.Bind, "s-bind">>;
          const jsArgs = listToArray(bindArgs).map(a => jsIdOf(a.dict.id));
          return [FunctionExpression(jsIdOf(constId(`lam_${expr.dict.name}`)), jsArgs,
            BlockStatement([...bodyStmts, ReturnStatement(bodyVal)])), []]
        }
        case 's-letrec':
        case 's-let-expr': {
          const prelude: J.Statement[] = [];
          listToArray<A.LetrecBind | A.LetBind>(expr.dict.binds).forEach(v => {
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
        }
        case 's-type-let-expr':
          return compileExpr(context, expr.dict.body);
        case 's-data-expr':
          return compileData(context, expr);
        // NOTE(joe/ben): This is deliberately the same behavior as s-dot,
        // the intended semantics is that these will be identical and errors
        // will be caught at the type-checking level or wrapped by an earlier
        // contract/error checking pass.
        case 's-dot':
        case 's-get-bang': {
          const [objV, objStmts] = compileExpr(context, expr.dict.obj);
          return [pyretLookup(expr.dict.l, objV, expr.dict.field), objStmts]
        }
        case 's-cases-else':
          return compileCases(context, expr);
        case 's-cases':
          throw new ShouldHaveDesugared(expr.dict.l, 's-cases');
        case 's-obj':
          return compileObj(context, expr);
        case 's-array': {
          const [ eltsVals, eltsStmts ] = compileList(context, expr.dict.values);
          return [ ArrayExpression(eltsVals), eltsStmts ];
        }
        case 's-construct': {
          const [ cval, cstmts ] = compileExpr(context, expr.dict.constructor);
          const [ eltsVals, eltsStmts ] = compileList(context, expr.dict.values);
          return [
            CallExpression(DotExpression(cval, "make"), [ArrayExpression(eltsVals)]),
            [...cstmts, ...eltsStmts]
          ];
        }
        case 's-instantiate': 
          return compileExpr(context, expr.dict.expr);
        case 's-user-block': 
          return compileExprScopedChecks(context, expr.dict.body);
        case 's-template': 
          return [rtMethod("throwUnfinishedTemplate", [context.compileSrcloc(expr.dict.l)]), []];
        case 's-type': 
          throw new ShouldHaveDesugared(expr.dict.l, expr.$name);
        case 's-newtype': throw new TODOError(expr.$name);
        case 's-when': {
          return compileIf(context, 
            [{ $name: 's-if-branch', dict: { l: expr.dict.l, test: expr.dict.test, body: expr.dict.block }}],
            [rtField("$nothing"), []]
          );
        }
        case 's-if':
        case 's-if-pipe': {
          const srcloc = context.compileSrcloc(expr.dict.l);
          const type = (expr.$name === 's-if' ? 'if' : 'ask');
          const throwNoMatch = CallExpression(rtField("throwNoBranchesMatched"), [srcloc, Literal(type)]);
          return compileIf(context,
            listToArray<A.IfBranch | A.IfPipeBranch>(expr.dict.branches),
            [throwNoMatch, []]);
        }
        case 's-if-else': 
        case 's-if-pipe-else': {
          return compileIf(context,
            listToArray<A.IfBranch | A.IfPipeBranch>(expr.dict.branches),
            compileExprScopedChecks(context, expr.dict._else));
        }
        case 's-assign': {
          const [rhs, rhsStmts] = compileExpr(context, expr.dict.value);
          const assnStmt = AssignmentExpression(Identifier(jsIdOf(expr.dict.id)), rhs);
          return [assnStmt, rhsStmts];
        }
        case 's-bracket': {
          const [lhs, lhsStmts] = compileExpr(context, expr.dict.obj);
          const [key, keyStmts] = compileExpr(context, expr.dict.key);
          const bracketExpr = CallExpression(DotExpression(lhs, "get-value"), [key]);
          return [bracketExpr, [...lhsStmts, ...keyStmts]];
        }
        case 's-extend': {
          const [objV, objStmts] = compileExpr(context, expr.dict.supe);
          const [extensionV, extensionStmts] = compileExpr(context, { $name: "s-obj", dict: expr.dict});
          return [rtMethod("$extend", [objV, extensionV]), [...objStmts, ...extensionStmts]];
        }
        case 's-for': return compileSFor(context, expr);
        case 's-tuple': {
          const [vals, stmts] = compileList(context, expr.dict.fields);
          const maker = rtMethod("PTuple", [ArrayExpression(vals)]);
          return [maker, stmts];
        }
        case 's-tuple-get': {
          const [tup, tupstmts] = compileExpr(context, expr.dict.tup);
          return [BracketExpression(tup, Literal(expr.dict.index)), tupstmts]
        }
        case 's-paren': return compileExpr(context, expr.dict.expr);
          
        case 's-check-expr': return compileExpr(context, expr.dict.expr);
        case 's-check': return compileCheckBlock(context, expr);
        case 's-check-test': return compileCheckTest(context, expr);
        
        case 's-table': return compileTable(context, expr);
        case 's-load-table': return compileLoadTable(context, expr);
        case 's-table-extend': return compileTableExtend(context, expr);
        case 's-table-update': return compileTableUpdate(context, expr);
        case 's-table-filter': return compileTableFilter(context, expr);
        case 's-table-select': return compileTableSelect(context, expr);
        case 's-table-order': return compileTableOrder(context, expr);
        case 's-table-extract': return compileTableExtract(context, expr);
        
        case 's-spy-block': return compileSpy(context, expr);
        case 's-update': return compileUpdate(context, expr);
        case 's-id-var-modref': throw new TODOError(expr.$name);
        

        case 's-contract':
        case 's-rec':
        case 's-reactor':
        case 's-fun':
        case 's-data':
        case 's-let':
        case 's-var':
          throw new ShouldHaveDesugared(expr.dict.l, expr.$name);

        default:
          throw new ExhaustiveSwitchError(expr, "Reached exhaustiveness check");
      }
    }

    function getGlobals(prog : A.Program) : Set<string> {
      const globalNames = new Set<string>();
      visit<A.Name | A.Program>({
        "s-global": (_, g : (Variant<A.Name, "s-global">)) => {
          globalNames.add(g.dict.s);
        }
      }, prog);
      return globalNames;
    }

    function getModuleGlobals(prog : A.Program) : Set<[A.Name, string]> {
      const globalURIs = new Set<[A.Name, string]>();
      visit<A.Expr | A.Program>({
        "s-id-modref": (_, g : (Variant<A.Expr, "s-id-modref">)) => {
          globalURIs.add([ g.dict.id, g.dict.uri ]);
        }
      }, prog);
      return globalURIs;
    }


    function importToDep(i : A.ImportType) : CS.Dependency {
      switch(i.$name) {
        case 's-const-import': return { $name: 'builtin', dict: { modname: i.dict.mod }}
        case 's-special-import': return { $name: 'dependency', dict: {protocol: i.dict.kind, arguments: i.dict.args}}
      }
    }

    function depToKey(d : CS.Dependency) : string {
      switch(d.$name) {
        case 'dependency': return `${d.dict.protocol}(${listToArray(d.dict.arguments).join(", ")})`
        case 'builtin': return `builtin(${d.dict.modname})`;
      }
    }

    function createPrelude(context: Context, prog : A.Program, provides, env, U: A.Name, M: A.Name, C: A.Name, srclocs: J.Expression[], freeBindings : Map<string, CS.ValueBind>, options: CompileOptions, importFlags) : Array<J.Statement> {

      function getBaseDir(source : string, buildDir : string) : [ string, string ] {
        let sourceHead = source.indexOf("://") + 3;
        const sourcePath = source.substring(sourceHead)
        const shorter = Math.min(sourcePath.length, buildDir.length)
        let cutoffIndex = 0;
        for(let i = 0; i < shorter; i += 1) {
          if(sourcePath[i] !== buildDir[i]) { cutoffIndex = i; break; }
        }
        if(cutoffIndex === 0) { cutoffIndex = buildDir.length; }
        return [ buildDir.substring(0, cutoffIndex), sourcePath ];
      }

      function getCompiledRelativePath(baseDir : string, source : string) {
        baseDir = P.resolve(baseDir);
        source = P.resolve(source);
        let projectRelativePath = P.dirname(source.substring(baseDir.length + 1));
        if(projectRelativePath === ".") { return "./"; }
        let parentPath = projectRelativePath.split("/").map(v => "../").join("");
        return parentPath;
      }

      const [ baseDir, absoluteSource ] = getBaseDir(provides.dict["from-uri"], options.dict["base-dir"]);
      const preAppendDir = getCompiledRelativePath(baseDir, absoluteSource);
      const relativePath = preAppendDir; // NOTE(joe): commented out in direct codegen: `preAppendDir + options.dict["runtime-path"]`

      // NOTE(joe/ben): we think we can simplify all the stuff above with the
      // right composition of P.relative and P.dirname, but the stuff above
      // works and this doesn't
//      const baseDir = options.dict["base-dir"];
//      const relativePath = P.relative(baseDir, P.dirname(uriToRealFsPath(provides.dict["from-uri"])));

      const imports = prog.dict.imports;

      function uriToRealFsPath(uri : string) : string {
        const index = uri.indexOf("://") + 3;
        return uri.substring(index) + ".js";
      }

      const runtimeBuiltinRelativePath : A.Option<string> = options.dict["runtime-builtin-relative-path"];

      function uriToImport(uri : string, name : A.Name) : Array<J.Statement> {
        if(uri.startsWith("builtin://")) {
          const builtinName = uriToRealFsPath(uri + ".arr");
          let thePath;
          switch(runtimeBuiltinRelativePath.$name) {
            case 'some': 
              thePath = runtimeBuiltinRelativePath.dict.value + builtinName;
              break;
            case 'none': 
              thePath = relativePath + "../builtin/" + builtinName;
              break;
          }
          return [
            Var(jsIdOf(name), CallExpression(Identifier(constId("require")), [Literal(thePath)])),
            ExpressionStatement(rtMethod("addModule", [Literal(uri), Identifier(jsIdOf(name))]))
          ];
        }
        else if(uri.startsWith("jsfile://") || uri.startsWith("file://")) {
          const targetPath = uriToRealFsPath(uri);
          const thisPath = uriToRealFsPath(provides.dict["from-uri"]);
          const jsRequirePath = P.relative(P.dirname(thisPath), targetPath);
          return [
            Var(jsIdOf(name), CallExpression(Identifier(constId("require")), [Literal("./" + jsRequirePath)])),
            ExpressionStatement(rtMethod("addModule", [Literal(uri), Identifier(jsIdOf(name))]))
          ];
        }
        else {
          throw new InternalCompilerError("Path for uriToImport did not look like builtin://, file://, or jsfile://");
        }
      }

      const globalNames = getGlobals(prog);
      const globalModuleNames = getModuleGlobals(prog);
      const uriToLocalJsName = new Map<string, A.Name>();

      function importBuiltin(bindName : A.Name, name : string) {
        let thePath;
        switch(runtimeBuiltinRelativePath.$name) {
          case 'some': thePath = runtimeBuiltinRelativePath.dict.value + name; break;
          case 'none': thePath = relativePath + "../builtin/" + name; break;
        }
        return Var(bindName, CallExpression(Identifier(constId("require")), [Literal(thePath)]));
      }

      const runtimeImport = importBuiltin(RUNTIME, "runtime.js");
      const tableImport =  importBuiltin(TABLE, "tables.arr.js");

      const manualImports: J.Declaration[] = [];
      if(importFlags["table-import"]) { manualImports.push(tableImport); }

      const explicitImports = listToArray(imports).map(importStmt => {
        switch(importStmt.$name) {
          case 's-import':
            const depKey = depToKey(importToDep(importStmt.dict.file));
            // TODO(joe): this punched through "uri-by-dep-key" because of flatness concerns
            // This should use a clean CompileEnv interface method
            let uri = runtime.getField(env.dict["my-modules"], "get-value").app(depKey);
            switch(options.dict["compile-mode"].$name) {
              case 'cm-normal': break;
              case 'cm-builtin-stage-1':
              case 'cm-builtin-general': uri = "builtin://" + P.basename(uri, ".arr"); break;
            }
            uriToLocalJsName.set(uri, importStmt.dict.name);
            return uriToImport(uri, importStmt.dict.name);
          default:
            return [];
            //throw new InternalCompilerError("Codegen requires s-import only " + JSON.stringify(importStmt));
        }
      }).reduce((l1, l2) => l1.concat(l2), []);

      function envUriByValueNameValue(env, name) {
        // TODO(joe): this punched through "uri-by-value-name-value" because of flatness concerns
        // This should use a clean CompileEnv interface method
        return runtime.getField(env.dict.globals.dict.values, "get-value").app(name).dict["uri-of-definition"];
      }

      const nonImportedGlobalNames = [...(globalNames.keys())].filter(g => {
        return !(uriToLocalJsName.has(envUriByValueNameValue(env, g)));
      });

      const implicitImports : J.Statement[] = [];
      nonImportedGlobalNames.forEach(g => {
        const uri = envUriByValueNameValue(env, g);
        if (!uriToLocalJsName.has(uri)) {
          const newName = freshId(compilerName("G"));
          uriToLocalJsName.set(uri, newName);
          implicitImports.push(...uriToImport(uri, newName));
        }
      });

      const hasModuleImport = new Set<string>();
      for(let [name, uri] of globalModuleNames) {
        const key = uri + nameToKey(name);
        if(!hasModuleImport.has(key)) {
          implicitImports.push(...uriToImport(uri, name));
          hasModuleImport.add(key);
        }
      }

      const importStmts = [...manualImports, ...explicitImports, ...implicitImports];

      /* NOTE(joe): this was no-op code that produces a list and doesn't use it
      for CL.map_list(g from global-names.keys-list-now()):
        uri = env.uri-by-value-name-value(g)
        imported-as = uri-to-local-js-name.get-value-now(uri)
        J.j-var(js-id-of(A.s-global(g)), J.j-dot(j-id(js-id-of(imported-as)), g))
      end
      */

      const fromModules = [...freeBindings.values()].map(binding => {
        const uri = binding.dict["origin"].dict["uri-of-definition"];
        const name = nameToName(binding.dict["origin"].dict["original-name"]);
        return Var(jsIdOf(binding.dict["atom"]), rtMethod("getModuleValue", [Literal(uri), Literal(name)]));
      });

      const setupRuntime = [
        Var(U, Literal(provides.dict['from-uri'])),
        runtimeImport,
        ExpressionStatement(rtMethod("$claimMainIfLoadedFirst", [Identifier(U)])),
        Var(M, ArrayExpression(srclocs)),
        ExpressionStatement(rtMethod("$clearTraces", [Identifier(U)])),
        ExpressionStatement(rtMethod("$clearChecks", [Identifier(U)]))
      ];
      

      const checkerImport = importBuiltin(CHECKER, "checker.js");
      if(importFlags["checker-import"]) {
        setupRuntime.push(
          checkerImport,
          // TODO(Ben) -- Make this not be all=true!!!
          Var(C, rtMethod("$initializeCheckContext", [Identifier(U), Literal(true)])),
        );
      } else if (options.dict['check-mode'] === false) {
        setupRuntime.push(ExpressionStatement(rtMethod("$omitCheckResults", [Identifier(U)])));
      }

      return [...setupRuntime, ...importStmts, ...fromModules];
    }

    /**
     * Temporary function that recursively runs every relevant `is-*` checker from
     * ast.arr that it encounters in the given `val`.  (It dynamically extends 
     * the visitor with additional handlers as it encounters each new data value,
     * which is tantamount to having a `method-missing` proxy hook, essentially.)
     * Since `deepCheck` is called after `map` is called with a no-op visitor,
     * this is checking that the resulting object looks sufficiently like a Pyret
     * value that it satisfies the `is-*` predicates and can continue to be used
     * from Pyret code.  In theory we could enhance this with other Pyret checkers 
     * (e.g. lists), but it's harder to know which module to find the checkers in.
     */
    function deepCheck<T extends { $name: string, dict: {} }>(visitor: Partial<Record<T["$name"], any>>, val: T) {
      const checkerName = `is-${val.$name}`;
      if (checkerName in A.dict.values.dict) {
        if (!A.dict.values.dict[checkerName].app(val)) {
          throw new InternalCompilerError(`Value didn't satisfy ${checkerName}: ${val}`);
        } 
      }
      for (const [k, subd] of Object.entries(val.dict)) {
        if (typeof subd === 'object' && subd !== null && "$name" in subd) {
          visitor[subd['$name'] as any] = deepCheck;
          visit(visitor, subd as any);
        }
      }
    }

    function assertMapIsDoingItsJob(prog : A.Program) {
      const after = map<A.Program, A.Program>({}, prog);
      if(prog === after) { throw new InternalCompilerError("AST map visitor returned an identical object"); }
      deepCheck({}, after);
      return after;
    }


    function serializeBuiltinRequires(name: string, options: CompileOptions): J.Expression {
      return ObjectExpression([
        Property("import-type", Literal("builtin")),
        Property("name", Literal(name)),
      ]);
    }

    // NOTE(alex): In cm-builtin-stage-1 and cm-builtin-general, treat ALL
    //  dependencies as builtin modules
    function serializeFileRequires(name: string, uriKey: string, protocol: string, options): J.Expression {
      const mode = (options.dict['compile-mode'] as CS.CompileMode);
      switch(mode.$name) {
        case 'cm-normal': return ObjectExpression([
          Property("import-type", Literal("dependency")),
          // NOTE(alex): protocol comes from cli-module-loader.arr
          Property("protocol", Literal(protocol)),
          Property("args", ArrayExpression([Literal(uriKey)])),
        ]);
        case 'cm-builtin-stage-1': return serializeBuiltinRequires(name, options);
        case 'cm-builtin-general': return serializeBuiltinRequires(name, options);
        default:
          throw new ExhaustiveSwitchError(mode);
      }
    }

    function serializeRequires(env: CS.CompileEnvironment, options): J.Expression[] {
      // NOTE(alex): current implementation includes the entire dependency subgraph that
      //   was present while compiling the current module, not just the dependency subgrpah
      //   reachable from the current module
      //
      // For example: A depends on B, A depends on C
      //    B will still show up in the requires of C
      //    and vice versa if the compiler visits dependency C first
      const result: J.Expression[] = [];
      Object.keys(env.dict['all-modules'].$underlyingDict).forEach((uriKey : string) => {
        const name = P.basename(uriKey, ".arr")
        // TODO(alex): would be nice if CompileEnvironment stored the dependencies as an actual
        //  compile-structs:Dependency so we didn't have to parse the all-modules keys
        let req;
        if (uriKey.startsWith("builtin://")) {
          req = serializeBuiltinRequires(name, options);
        } else if (uriKey.startsWith("jsfile://")) {
          req = serializeFileRequires(name, uriKey, "js-file", options);
        } else if (uriKey.startsWith("file://")) {
          req = serializeFileRequires(name, uriKey, "file", options);
        } else {
          throw new InternalCompilerError(`Unknown uri kind: ${uriKey}`);
        }
        result.push(req);
      });
      return result;
    }

    let defaultImportFlags = {
      'table-import': false,
      'array-import': false,
      'reactor-import': false,
      'checker-import': false,
    };
    type Context = {
      uri: string,
      curCheckContext: A.Name,
      compileSrcloc: (l : A.Srcloc, cache?: boolean) => J.Expression,
      options: CompileOptions,
      provides: CS.Provides,
      datatypes: Map<string, any>,
      env: CS.CompileEnvironment,
      postEnv: Variant<CS.ComputedEnvironment, 'computed-env'>,
      freeBindings: Map<string, CS.ValueBind>,
      checkBlockTestCalls: J.Expression[],
      importFlags: typeof defaultImportFlags
    }
    function compileProgram(prog : A.Program, uri : string, env : CS.CompileEnvironment, postEnv : CS.ComputedEnvironment, provides : CS.Provides, options : CompileOptions) : TJSP.CCPDict {
      const translatedDatatypeMap = new Map();   // TODO(joe) process from stringdict
      const fromUri = provides.dict['from-uri']; // TODO(joe) handle phases builtin-stage*
      const freeBindings = new Map<string, CS.ValueBind>();            // NOTE(joe) this starts empty in the mainline compiler
      const importFlags = { ...defaultImportFlags };

      // TODO(joe): remove this when we are 100% confident that map doesn't fudge with the AST
      prog = assertMapIsDoingItsJob(prog);

      const srclocs: J.ArrayExpression[] = [];
      const srclocIndexMap: Map<string, number> = new Map();
      const U = freshId(compilerName('U'));
      const M = freshId(compilerName('L'));
      const C = freshId(compilerName('curChCtx'));

      const context: Context = {
        uri: fromUri,
        curCheckContext: C,
        compileSrcloc(l : A.Srcloc, cache: boolean = true): J.Expression {
          const locAsStr = formatSrcloc(l, true);
          let idx = srclocIndexMap.get(locAsStr);
          if (idx === undefined) {
            idx = srclocs.length;
            srclocIndexMap.set(locAsStr, idx);
            let ans: J.Expression;
            switch(l.$name) {
              case "builtin": 
                ans = ArrayExpression([Literal(l.dict['module-name'])]); 
                break;
              case "srcloc":
                ans = ArrayExpression([
                  Identifier(U),
                  Literal(l.dict['start-line']),
                  Literal(l.dict['start-column']),
                  Literal(l.dict['start-char']),
                  Literal(l.dict['end-line']),
                  Literal(l.dict['end-column']),
                  Literal(l.dict['end-char']),
                ], true);
                break;
            }  
            srclocs.push(ans);
          }
          if (!cache) {
            const existing = srclocs[idx].elements;
            const allButUri = existing.slice(1);
            const uri = (l.$name === 'builtin' ? l.dict['module-name'] : l.dict['source']);
            return ArrayExpression([Literal(uri), ...allButUri]);
          }
          return BracketExpression(Identifier(M), Literal(idx));
        },  
        options,
        provides,
        datatypes: translatedDatatypeMap,
        env,
        postEnv: postEnv as Variant<CS.ComputedEnvironment, 'computed-env'>,
        freeBindings,
        checkBlockTestCalls: [],
        importFlags
      };

      const [ans, stmts] = compileExpr(context, prog.dict.block);

      const prelude = createPrelude(context, prog, provides, env, U, M, C, srclocs, freeBindings, options, importFlags);

      let serializedProvides: string;
      const mode = (options.dict['compile-mode'] as CS.CompileMode);
      switch(mode.$name) {
        case 'cm-normal': {
          serializedProvides = compileProvides(context, provides);
          break;
        }
        case 'cm-builtin-stage-1': 
        case 'cm-builtin-general': {
          serializedProvides = compileProvidesOverrideUri(context, provides, true);
          break;
        }
        default:
          throw new ExhaustiveSwitchError(mode);
      }

      const moduleBody = Program([...prelude, ...stmts, ReturnStatement(ans)]);
      const jsonOptions : Escodegen.GenerateOptions = {
        format: { json: true, },
        comment: true,
        verbatim: 'x-verbatim-content',
      };
      return ({
        requires: escodegen.generate(ArrayExpression(serializeRequires(env, options)), jsonOptions),
        provides: serializedProvides,
        nativeRequires: escodegen.generate(ArrayExpression([]), jsonOptions),
        theModule: escodegen.generate(moduleBody, jsonOptions),
        theMap: escodegen.generate(Literal(""), jsonOptions),
      });
    }

    const exports : Exports['dict']['values']['dict'] = {
      'compile-program': runtime.makeFunction(compileProgram)
    };
    return runtime.makeModuleReturn(exports, {});

  }
})