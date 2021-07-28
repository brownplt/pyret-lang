import * as J from 'estree';
import type * as Escodegen from 'escodegen';
import type * as Path from 'path';
import type * as A from './ts-ast';
import type * as T from './ts-impl-types';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as PS from './provide-serialization';
import type { Variant, PyretObject } from './ts-codegen-helpers';

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
      compileSrcloc,
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

    const RUNTIME = constId("_runtime");
    const TABLE = constId("_table")
    const NUMBER_ERR_CALLBACKS = "$errCallbacks"
    const EQUAL_ALWAYS = "equal-always"
    const IDENTICAL = "identical"
    const TOREPR = "$torepr"
    const UNDEFINED = Identifier(constId("undefined"));

    function compressRuntimeName(name : string) { return name; }
    
    function rtField(name : string) {
      return DotExpression(Identifier(RUNTIME), name);
    }

    function rtMethod(name : string, args : Array<J.Expression>) {
      return CallExpression(DotExpression(Identifier(RUNTIME), compressRuntimeName(name)), args);
    }
    
    function chooseSrcloc(l : A.Srcloc, context) {
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

    function compileList(context, exprs: T.List<A.Expr>) : [ Array<J.Expression>, Array<J.Statement> ] {
      if(exprs.$name === 'empty') { return [[], []]; }
      else {
        const [ firstAns, startStmts ] = compileExpr(context, exprs.dict.first);
        const [ restAns, restStmts ] = compileList(context, exprs.dict.rest);
        return [ [firstAns, ...restAns], [...startStmts, ...restStmts] ];
      }
    }

    function compileSeq(context, exprs : T.List<A.Expr>) : [J.Expression, Array<J.Statement>] {
      if(exprs.$name === 'empty') { throw new InternalCompilerError("Empty block reached codegen"); }
      let ans, stmts = [];
      let cur: T.List<A.Expr> = exprs;
      while (cur.$name === 'link') {
        const [first, firstStmts] = compileExpr(context, cur.dict.first);
        ans = first;
        stmts.push(...firstStmts);
        if (first !== undefined && !(first.type === 'Identifier' && first.name === 'undefined')) {
          stmts.push(ExpressionStatement(first));
        }
        cur = cur.dict.rest;
      }
      return [ans, stmts]
    }

    function arrayToList<T>(arr : T[]) : T.List<T> {
      return runtime.ffi.makeList(arr);
    }

    function compileMethodDefinition(context, method : Variant<A.Member, "s-method-field">) {
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
      return FunctionExpression(jsIdOf(constId(`getWrapper_${method.dict.name}`)), [], BlockStatement(body));
    }

    function compileObj(context, expr : Variant<A.Expr, 's-obj'>) : [J.Expression, Array<J.Statement>] {
      const fieldsAsArray = listToArray(expr.dict.fields);
      const fieldvs : Array<J.Property> = [], stmts = [], methods : Array<[string, J.Property]> = [];
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
          switch(m.dict['member-type'].$name) {
            case 's-mutable': throw new TODOError(m.dict['member-type'].$name);
            case 's-normal':
              switch(m.dict.bind.$name) {
                case 's-tuple-bind': throw new InternalCompilerError(m.dict.bind.$name);
                case 's-bind':
                  return m.dict.bind.dict.id;
              }
          }
      }
    }

    function compileData(context, expr : Variant<A.Expr, 's-data-expr'>) : [J.Expression, Array<J.Statement>] {
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

    

    function compileModule(context, expr: Variant<A.Expr, "s-module">) : CompileResult {
      const fields = [];
      const stmts = [];
      const locs = [];
      listToArray(expr.dict['defined-values']).forEach(dv => {
        switch(dv.$name) {
          case 's-defined-value': {
            const [ val, fieldStmts ] = compileExpr(context, dv.dict['value']);
            const sloc = compileSrcloc(context, dv.dict.value.dict.l);
            fields.push(Property(dv.dict.name, val));
            stmts.push(...fieldStmts);
            locs.push(ObjectExpression([Property("name", Literal(dv.dict.name)), Property("srcloc", sloc)]));
            return;
          }
          case 's-defined-var': {
            const sloc = compileSrcloc(context, dv.dict.loc);
            fields.push(Property(dv.dict.name, Identifier(jsIdOf(dv.dict.id))));
            locs.push(ObjectExpression([Property("name", Literal(dv.dict.name)), Property("srcloc", sloc)]));
            return;
          }
        }
      });

      const [aExp, aStmts] = compileExpr(context, expr.dict.answer);
      const checkResults = rtMethod("$checkResults", [Literal(context.uri)]);
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

      const assignAns = AssignmentExpression(DotExpression(Identifier(constId("module")), "exports"), ans);
      return [assignAns, [...aStmts, ...context.checkBlockTestCalls, answerVar, ...stmts]];
    }

    function compileSOp(context, op: string, lv: J.Expression, rv: J.Expression): J.Expression {
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
        case "op<>": return UnaryExpression("!", CallExpression(rtField(EQUAL_ALWAYS), [lv, rv])); break;
        case "op<=>": return CallExpression(rtField(IDENTICAL), [lv, rv]); break;
        case "opor": return LogicalExpression("||", lv, rv); break;
        case "opand": return LogicalExpression("&&", lv, rv); break;
        case "op^": return CallExpression(rv, [lv]); break;
        default: throw new TODOError(`Not yet implemented: ${op}`);        
      }
    }

    function compileOp(context, expr : Variant<A.Expr, "s-op">) : CompileResult {
      const [lv, lStmts] = compileExpr(context, expr.dict.left);
      const [rv, rStmts] = compileExpr(context, expr.dict.right);
      return [compileSOp(context, expr.dict.op, lv, rv), [...lStmts, ...rStmts]];
    }

    function compileIf(context, branches: (A.IfBranch | A.IfPipeBranch)[], compiledElse: CompileResult) : CompileResult {
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
        const [bodyV, bodyStmts] = compileExpr(context, branch.dict.body);
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

    function compileCases(context, expr : Variant<A.Expr, "s-cases-else">) : CompileResult {
      const ans = freshId(compilerName("ans"));
      const [val, valStmts] = compileExpr(context, expr.dict.val);
      const switchBlocks = listToArray(expr.dict.branches).map(b => {
        const [bodyVal, bodyStmts] = compileExpr(context, b.dict.body);
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
      const [elseV, elseStmts] = compileExpr(context, expr.dict._else);
      const elseCase = Default([...elseStmts, ExpressionStatement(AssignmentExpression(Identifier(ans), elseV))])
      return [
        Identifier(ans),
        [...valStmts, Var(ans, undefined), SwitchStatement(DotExpression(val, "$name"), [...switchBlocks, elseCase])]
      ];
    }

    function compileSFor(context, expr : Variant<A.Expr, "s-for">): CompileResult {
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

    function compileCheckBlock(context, expr : Variant<A.Expr, "s-check">) : CompileResult {
      const [ checkBlockVal, checkBlockStmts ] = compileExpr(context, expr.dict.body);
      let jsCheckBlockFuncName;
      let testBlockName;
      const name = expr.dict.name;
      switch(name.$name) {
        case 'none':
          jsCheckBlockFuncName = freshId(compilerName("check-block"));
          testBlockName = Literal(nameToSourceString(jsCheckBlockFuncName));
          break;
        case 'some':
          jsCheckBlockFuncName = freshId(compilerName("check-block" + name.dict.value));
          testBlockName = Literal(name.dict.value);
          break;
      }
      const jsCheckBlockFuncBlock = BlockStatement([...checkBlockStmts, ExpressionStatement(checkBlockVal)]);
      const jsCheckBlockFunc = FunctionExpression(jsCheckBlockFuncName, [], jsCheckBlockFuncBlock);
      const blockLoc = Literal(formatSrcloc(chooseSrcloc(expr.dict.l, context), true));
      const testerCall = ExpressionStatement(rtMethod("$checkBlock", [blockLoc, testBlockName, jsCheckBlockFunc]));
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
    function compileCheckTest(context, expr : Variant<A.Expr, "s-check-test">) : CompileResult {
      type CheckOpDesugar =
        | { $name: "binop-result", op: string }
        | { $name: "expect-raises" }
        | { $name: "refinement-result", refinement: J.Expression, negate: boolean }
        | { $name: "predicate-result", predicate: any };
      const {l, op, refinement, left, right: rightOpt, cause} = expr.dict;
      function makeCheckOpResult(success : J.Expression, lhs : J.Expression, rhs: J.Expression) {
        return ObjectExpression([
          Property("success", success), Property("lhs", lhs), Property("rhs", rhs)
        ]);
      }

      function makeCheckExprResult(value : T.Either<J.Expression, J.Expression>) {
        switch(value.$name) {
          case "left": return ObjectExpression([
              Property("value", UNDEFINED),
              Property("exception", Literal(true)),
              Property("exception_value", value.dict.v)
            ]);
          case "right": return ObjectExpression([
              Property("value", value.dict.v),
              Property("exception", Literal(false)),
              Property("exception_value", UNDEFINED)
            ]);
        }
      }

      function thunkIt(name : string, val : J.Expression, stmts : J.Statement[]) {
        const body = BlockStatement([...stmts, ReturnStatement(val)]);
        return FunctionExpression(compilerName(name), [], body);
      }
      function exceptionCheck(exceptionFlag : J.Expression, lhs : J.Expression, rhs : J.Expression) {
        const checkBody = BlockStatement([
          ReturnStatement(makeCheckOpResult(Literal(false), lhs, rhs))
        ]);
        return IfStatement(exceptionFlag, checkBody, null);
      }

      const testLoc = Literal(formatSrcloc(chooseSrcloc(l, context), true));

      let checkOp: CheckOpDesugar, checkOpStmts: J.Statement[];
      switch(op.$name) {
        case "s-op-is": {
          switch(refinement.$name) {
            case "some":
              const [ refinementExpr, refinementStmts ] = compileExpr(context, refinement.dict.value);
              checkOp = { $name: 'refinement-result', refinement: refinementExpr, negate: false };
              checkOpStmts = refinementStmts;
              break;
            case "none":
              [checkOp, checkOpStmts] = [ {$name: 'binop-result', op: "op=="}, []];
              break;
          }
          break;
        }
        case "s-op-is-not": {
          switch(refinement.$name) {
            case "some":
              const [ refinementExpr, refinementStmts ] = compileExpr(context, refinement.dict.value);
              checkOp = { $name: 'refinement-result', refinement: refinementExpr, negate: true };
              checkOpStmts = refinementStmts;
              break;
            case "none":
              [checkOp, checkOpStmts] = [ {$name: 'binop-result', op: "op<>"}, []];
              break;
          }
          break;
        }
        case 's-op-is-roughly': {
          [checkOp, checkOpStmts] = [
            {$name: 'refinement-result', refinement: rtMethod("within", [Literal(0.000001)]), negate: false},
            []
          ]
          break;
        }
        case 's-op-raises': {
          [checkOp, checkOpStmts] = [ {$name: 'expect-raises'}, []];
          break;
        }
        default: throw new InternalCompilerError("Not yet implemented: " + op.$name);
      }

      function defineBinTest(rightExpr: A.Expr, binOp: (lhs: J.Expression, rhs: J.Expression) => J.Expression): CompileResult {
        // Thunk the lhs
        const [ lhs, lhsStmts ] = compileExpr(context, left);
        const lhFunc = thunkIt("LHS", lhs, lhsStmts);

        // Thunk the rhs
        const [ rhs, rhsStmts ] = compileExpr(context, rightExpr);
        const rhFunc = thunkIt("RHS", rhs, rhsStmts);

        // Thunk the binCheckOp
        const lhsParamName = freshId(compilerName("lhs"));
        const rhsParamName = freshId(compilerName("rhs"));

        const lhsValue = DotExpression(Identifier(lhsParamName), "value");
        // LHS exception check
        const lhsException = DotExpression(Identifier(lhsParamName), "exception");
        const lhsExceptionCheck = exceptionCheck(lhsException, Identifier(lhsParamName), Identifier(rhsParamName));

        const rhsValue = DotExpression(Identifier(rhsParamName), "value");
        // RHS exception check
        const rhsException = DotExpression(Identifier(rhsParamName), "exception");
        const rhsExceptionCheck = exceptionCheck(rhsException, Identifier(lhsParamName), Identifier(rhsParamName));

        const jTestVal = binOp(lhsValue, rhsValue);

        const successResult = makeCheckOpResult(jTestVal, Identifier(lhsParamName), Identifier(rhsParamName));

        const testBodyStmts = [lhsExceptionCheck, rhsExceptionCheck, ...checkOpStmts, ReturnStatement(successResult)];

        const testBody = BlockStatement(testBodyStmts);
        const testFunc = FunctionExpression(compilerName("TEST"), [lhsParamName, rhsParamName], testBody);

        const testerCallArgs = [lhFunc, rhFunc, testFunc, testLoc];
        const testerCall = ExpressionStatement(rtMethod("$checkTest", testerCallArgs));

        return [UNDEFINED, [testerCall]];
      }

      switch(checkOp.$name) {
        case 'binop-result': {
          const binOp = checkOp.op;
          const right = unwrap(rightOpt, 'Attempting to use a binary check op without the RHS');
          return defineBinTest(right, (left, right) => {
            return compileSOp(context, binOp, left, right);
          });
        }
        case 'expect-raises': {
          // Transforms the following Pyret test expression:
          //   `lhs raises rhs`
          // into
          // ```
          //   LHS = thunk(lhs)
          //   RHS = thunk(rhs)
          //   test = function(lhs, rhs) {
          //     let success = RUNTIME.exception && (RUNTIME.$torepr(RUNTIME.$raiseExtract(lhs.exception_val).index(rhs.value))
          //     );
          //     return testResult(success, lhs, asException(rhs));
          //   };
          //   RUNTIME.$checkTest(LHS, RHS, test)
          //
          // ```
          // where testResult() and asException() are conversions emitted in place
          //
          // The `raises` operator checks that the rhs is contained within the
          //   string representation of the lhs.
          //

          const [ lhs, lhsStmts ] = compileExpr(context, left);
          const lhFunc = thunkIt("LHS", lhs, lhsStmts);

          // Thunk the RHS
          let rhs: J.Expression, rhsStmts: J.Statement[];
          const right = unwrap(rightOpt, "`raises` checkop did not have a RHS, should be a parsing error");
          [rhs, rhsStmts] = compileExpr(context, right);
          const rhFunc = thunkIt("RHS", rhs, rhsStmts);

          // Thunk the binCheckOp
          const lhsParamName = freshId(compilerName("lhs"));
          const rhsParamName = freshId(compilerName("rhs"));

          const rhsValue = DotExpression(Identifier(rhsParamName), "value");
          const expectedRhs = makeCheckExprResult({ $name: 'left', dict: { v: rhsValue }});

          const lhsExceptionVal = DotExpression(Identifier(lhsParamName), "exception_val");
          const lhsExceptionExtract = rtMethod(TOREPR, [rtMethod("$raiseExtract", [lhsExceptionVal])]);
          // NOTE(Ben): I don't like this.
          const extractionResult = CallExpression(DotExpression(lhsExceptionExtract, "includes"), [rhsValue]);
          const lhsIsExceptionVal = DotExpression(Identifier(lhsParamName), "exception");
          const testResult = LogicalExpression("&&", lhsIsExceptionVal, extractionResult);

          const successResult = makeCheckOpResult(testResult, Identifier(lhsParamName), expectedRhs);

          const testBodyStmts = [...checkOpStmts, ReturnStatement(successResult)];
          const testBody = BlockStatement(testBodyStmts);
          const testFunc = FunctionExpression(compilerName("TEST"), [lhsParamName, rhsParamName], testBody);

          const testerCallArgs = [lhFunc, rhFunc, testFunc, testLoc];
          const testerCall = ExpressionStatement(rtMethod("$checkTest", testerCallArgs));
          
          return [UNDEFINED, [testerCall]];
        }
        case 'refinement-result': {
          const { negate, refinement } = checkOp;
          const right = unwrap(rightOpt, 'Attempting to use a predicate check without the RHS');
          return defineBinTest(right, (left, right) => {
            if (negate) {
              return UnaryExpression("!", CallExpression(refinement, [left, right]));
            } else {
              return CallExpression(refinement, [left, right]);
            }
          });
        }
        // case 'predicate-result': {

        // }
      }
    }

    function compileTable(context, expr : Variant<A.Expr, 's-table'>): CompileResult {
      importFlags['table-import'] = true;

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

    function compileLoadTable(context, expr : Variant<A.Expr, 's-load-table'>): CompileResult {
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
            importFlags['table-import'] = true;

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

    function compileTableExtend(context, expr: Variant<A.Expr, 's-table-extend'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

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

    function compileTableUpdate(context, expr : Variant<A.Expr, 's-table-update'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

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

    function compileTableSelect(context, expr : Variant<A.Expr, 's-table-select'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

      const func = BracketExpression(Identifier(TABLE), Literal("_selectColumns"));

      const jsColumns = listToArray(expr.dict.columns).map((c) => Literal(nameToName(c)));

      const [jsTable, jsTableStmts] = compileExpr(context, expr.dict.table);

      const args = [jsTable, ArrayExpression(jsColumns)];

      // selectColumns(table, colnames)
      return [CallExpression(func, args), jsTableStmts];
    }

    function compileTableFilter(context, expr: Variant<A.Expr, 's-table-filter'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

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

    function compileTableOrder(context, expr: Variant<A.Expr, 's-table-order'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

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

    function compileTableExtract(context, expr: Variant<A.Expr, 's-table-extract'>): CompileResult {
      // Set the table-import flag
      importFlags['table-import'] = true;

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

    function compileSpy(context, expr : Variant<A.Expr, 's-spy-block'>): CompileResult {
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
          const jsSpyLoc = Property("loc", Literal(formatSrcloc(pyretSpyField.dict.l, true)));
          const jsSpyExprObj = ObjectExpression([jsSpyKey, jsSpyExpr, jsSpyLoc]);
          
          jsSpyFields.push(jsSpyExprObj);
        });

        const jsSpyLoc = Literal(formatSrcloc(expr.dict.l, true));

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

    function compileExpr(context, expr : A.Expr) : CompileResult {
      switch(expr.$name) {
        case 's-module':
          return compileModule(context, expr);
        case 's-block':
          return compileSeq(context, expr.dict.stmts);
        case 's-num': {
          let numAns;
          if(typeof expr.dict.n === "number") {
            numAns = Literal(expr.dict.n);
          }
          else {
            numAns = rtMethod("_makeNumberFromString", [Literal(expr.dict.n.toString()), rtField(NUMBER_ERR_CALLBACKS)]);
          }
          return [numAns, []];
        }
        case 's-frac': 
          return [rtMethod('$makeRational', [Literal(expr.dict.num), Literal(expr.dict.den)]), []];
        case 's-rfrac':
          return [rtMethod('$makeRoughnum', [Literal(expr.dict.num / expr.dict.den)]), []];
        case 's-str':
          return [Literal(expr.dict.s), []];
        case 's-bool':
          return [Literal(expr.dict.b), []];
        case 's-prim-val': {
          return [rtField(expr.dict.name), []];
        }
        case 's-undefined': {
          return [UNDEFINED, []];
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
              rtMethod("$messageThrow", [compileSrcloc(context, expr.dict.l), Literal("Uninitialized letrec identifier")])
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
          return [compileSrcloc(context, expr.dict.loc), []];
        case 's-op':
          return compileOp(context, expr);
        case 's-lam': {
          const [ bodyVal, bodyStmts ] = compileExpr(context, expr.dict.body);
          const bindArgs = expr.dict.args as T.List<Variant<A.Bind, "s-bind">>;
          const jsArgs = listToArray(bindArgs).map(a => jsIdOf(a.dict.id));
          return [FunctionExpression(jsIdOf(constId(`lam_${expr.dict.name}`)), jsArgs,
            BlockStatement([...bodyStmts, ReturnStatement(bodyVal)])), []]
        }
        case 's-letrec':
        case 's-let-expr': {
          const prelude = [];
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
        case 's-dot': {
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
          return compileExpr(context, expr.dict.body);
        case 's-template': 
          return [rtMethod("throwUnfinishedTemplate", [compileSrcloc(context, expr.dict.l)]), []];
        case 's-method': 
          throw new ShouldHaveDesugared(expr.dict.l, expr.$name);
        case 's-type': 
          throw new ShouldHaveDesugared(expr.dict.l, expr.$name);
        case 's-newtype': throw new TODOError(expr.$name);
        case 's-when': {
          const nothing = compileExpr(context, { $name: 's-id', dict: { l: expr.dict.l, id: jsnames.sGlobal("nothing") }});
          return compileIf(context, 
            [{ $name: 's-if-branch', dict: { l: expr.dict.l, test: expr.dict.test, body: expr.dict.block }}],
            nothing
          );
        }
        case 's-if':
        case 's-if-pipe': {
          const srcloc = compileSrcloc(context, expr.dict.l);
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
            compileExpr(context, expr.dict._else));
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

        case 's-hint-exp': {
          return compileExpr(context, expr.dict.exp);
        }

        case 's-get-bang': throw new TODOError(expr.$name);
        case 's-update': throw new TODOError(expr.$name);
        case 's-ref': throw new TODOError(expr.$name);
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

    function createPrelude(prog : A.Program, provides, env, freeBindings : Map<string, CS.ValueBind>, options, importFlags) : Array<J.Statement> {

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
      }

      const globalNames = getGlobals(prog);
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

      const arrayImport =  importBuiltin(RUNTIME, "array.arr.js");
      const tableImport =  importBuiltin(TABLE, "tables.arr.js");
      const reactorImport =  importBuiltin(RUNTIME, "reactor.arr.js");

      const manualImports = [runtimeImport];
      if(importFlags["table-import"]) { manualImports.push(tableImport); }
      if(importFlags["reactor-import"]) {
        throw new TODOError("reactor.arr.js not implemented via flags");
          //manualImports.push(reactorImport);
      }
      if(importFlags["array-import"]) {
        throw new TODOError("array.arr.js not implemented via flags");
          //manualImports.push(arrayImport);
      }

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

      const implicitImports = [];
      nonImportedGlobalNames.forEach(g => {
        const uri = envUriByValueNameValue(env, g);
        if (!uriToLocalJsName.has(uri)) {
          const newName = freshId(compilerName("G"));
          uriToLocalJsName.set(uri, newName);
          implicitImports.push(...uriToImport(uri, newName));
        }
      });

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
        ExpressionStatement(rtMethod("$clearTraces", [Literal(provides.dict['from-uri'])])),
        ExpressionStatement(rtMethod("$clearChecks", [Literal(provides.dict['from-uri'])]))
      ];

      return [...importStmts, ...setupRuntime, ...fromModules];
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
        if (typeof subd === 'object' && "$name" in subd) {
          visitor[subd['$name']] = deepCheck;
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


    function serializeBuiltinRequires(name: string, options): J.Expression {
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

    let importFlags = {
      'table-import': false,
      'array-import': false,
      'reactor-import': false
    };
    function compileProgram(prog : A.Program, uri : string, env : any, postEnv : any, provides : any, options : any) : PyretObject {
      const translatedDatatypeMap = new Map();   // TODO(joe) process from stringdict
      const fromUri = provides.dict['from-uri']; // TODO(joe) handle phases builtin-stage*
      const freeBindings = new Map<string, CS.ValueBind>();            // NOTE(joe) this starts empty in the mainline compiler

      // TODO(joe): remove this when we are 100% confident that map doesn't fudge with the AST
      prog = assertMapIsDoingItsJob(prog);

      const [ans, stmts] = compileExpr({
        uri: fromUri,
        options: options,
        provides: provides,
        datatypes: translatedDatatypeMap,
        env: env,
        postEnv: postEnv,
        freeBindings: freeBindings,
        checkBlockTestCalls: []
      }, prog.dict.block);

      const prelude = createPrelude(prog, provides, env, freeBindings, options, importFlags);

      let serializedProvides: string;
      const mode = (options.dict['compile-mode'] as CS.CompileMode);
      switch(mode.$name) {
        case 'cm-normal': {
          serializedProvides = compileProvides(provides);
          break;
        }
        case 'cm-builtin-stage-1': 
        case 'cm-builtin-general': {
          serializedProvides = compileProvidesOverrideUri(provides, true);
          break;
        }
        default:
          throw new ExhaustiveSwitchError(mode);
      }

      const moduleBody = Program([...prelude, ...stmts, ReturnStatement(ans)]);
      const jsonOptions : Escodegen.GenerateOptions = {
        format: { json: true },
      };
      return runtime.makeObject({
        requires: escodegen.generate(ArrayExpression(serializeRequires(env, options)), jsonOptions),
        provides: serializedProvides,
        nativeRequires: escodegen.generate(ArrayExpression([]), jsonOptions),
        theModule: escodegen.generate(moduleBody, jsonOptions),
        theMap: escodegen.generate(Literal(""), jsonOptions),
      });
    }

    return runtime.makeModuleReturn({
      'compile-program': runtime.makeFunction(compileProgram)
    }, {});

  }
})