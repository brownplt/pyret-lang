import type * as TJ from './ts-codegen-helpers';
import type * as A from './ts-ast';
import type * as G from './ts-gensym';
import type { List, PFunction, } from './ts-impl-types';

export interface Exports {
  'desugar-check': PFunction<(prog: A.Program) => A.Program>,
  'desugar-no-checks': PFunction<(prog: A.Program) => A.Program>,
}

class CheckInfo {
  l: A.Srcloc;
  name: string;
  body: A.Expr;
  keywordIsCheck: boolean;

  constructor(
    l: A.Srcloc,
    name: string,
    body: A.Expr,
    keywordIsCheck: boolean,  
  ) {
    this.l = l;
    this.name = name;
    this.body = body;
    this.keywordIsCheck = keywordIsCheck;
  }
};

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-gensym']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
  ],
  provides: {
    values: {
      'desugar-check': "tany",
      'desugar-no-checks': "tany",
    }
  },
  nativeRequires: [],
  theModule: function(runtime, _, __, tj : TJ.Exports, Gin : G.Exports, Ain : A.Exports) {
    const A  = Ain.dict.values.dict;
    const G = Gin.dict.values.dict;
    const { InternalCompilerError, ExhaustiveSwitchError, listToArray, map } = tj;

    const mtList: List<any> = runtime.ffi.makeList([]);
    const none = runtime.ffi.makeNone();
    function astLam(ast: A.Expr): TJ.Variant<A.Expr, 's-lam'> {
      return makeLam(ast.dict.l, [], ast);
    }
    function makeLam(l: A.Srcloc, args: A.Bind[], body: A.Expr): TJ.Variant<A.Expr, 's-lam'> {
      return A['s-lam'].app(l, "", runtime.ffi.makeList([]), runtime.ffi.makeList(args), A['a-blank'], "", body, none, none, true);
    }

    const flatPrimApp = A['prim-app-info-c'].app(false);
    function astSrcloc(l: A.Srcloc): TJ.Variant<A.Expr, 's-prim-app'> {
      return A['s-prim-app'].app(l, "makeSrcloc", runtime.ffi.makeList([A['s-srcloc'].app(l, l)]), flatPrimApp);
    }

    function checkers(l: A.Srcloc): TJ.Variant<A.Expr, 's-prim-app'> {
      return A['s-prim-app'].app(l, "current-checker", mtList, flatPrimApp);
    }

    function getChecks(stmts: A.Expr[]): CheckInfo[] {
      let standaloneCounter = 0;
      const ret : CheckInfo[] = [];
      const checkStmtsVisitor: TJ.Visitor<A.Expr, A.Expr> = {
        's-check': (visitor, check) => {
          return map(visitor, check.dict.body);
        },
        's-check-test': (_visitor, checkTest: TJ.Variant<A.Expr, 's-check-test'>) => {
          const { l, op, refinement, left, right, cause } = checkTest.dict;
          function checkOp(fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkOp'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), astLam(right.dict.value), astSrcloc(l)]));
          }
          function checkOpCause(cause: A.Expr, fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkOpCause'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), astLam(right.dict.value), astLam(cause), astSrcloc(l)]));
          }
          function checkRefinement(refinement: A.Expr, fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkOpRefinemenet'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([refinement, astLam(left), astLam(right.dict.value), astSrcloc(l)]));
          }
          function checkRefinementCause(refinement: A.Expr, cause: A.Expr, fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkOpRefinementCause'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([refinement, astLam(left), astLam(right.dict.value), astLam(cause), astSrcloc(l)]));
          }
          function checkRaises(fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkRaises'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), right.dict.value, astSrcloc(l)]));
          }
          function checkRaisesNot(fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), astSrcloc(l)]));
          }
          function checkRaisesCause(cause: A.Expr, fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            if (right.$name === 'none') { throw new InternalCompilerError('right is none in checkRaises'); }
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), right.dict.value, astLam(cause), astSrcloc(l)]));
          }
          function checkRaisesNotCause(cause: A.Expr, fieldName: string): TJ.Variant<A.Expr, 's-app'> {
            return A['s-app'].app(
              l, 
              A['s-dot'].app(l, checkers(l), fieldName),
              runtime.ffi.makeList([astLam(left), astLam(cause), astSrcloc(l)]));
          }
          switch(op.$name) {
            case 's-op-is': {
              if (refinement.$name === 'none') {
                if (cause.$name === 'none') {
                  return checkOp("check-is");
                } else {
                  return checkOpCause(cause.dict.value, "check-is-cause");
                }
              } else {
                if (cause.$name === 'none') {
                  return checkRefinement(refinement.dict.value, "check-is-refinement");
                } else {
                  return checkRefinementCause(refinement.dict.value, cause.dict.value, "check-is-refinement-cause");
                }
              }
            }
            case 's-op-is-roughly': {
              if (cause.$name === 'none') {
                return checkOp('check-is-roughly');
              } else {
                return checkOpCause(cause.dict.value, 'check-is-roughly-cause');
              }
            }
            case 's-op-is-not-roughly': {
              if (cause.$name === 'none') {
                return checkOp('check-is-not-roughly');
              } else {
                return checkOpCause(cause.dict.value, 'check-is-roughly-not-cause');
              }
            }
            case 's-op-is-not': {
              if (refinement.$name === 'none') {
                if (cause.$name === 'none') {
                  return checkOp("check-is-not");
                } else {
                  return checkOpCause(cause.dict.value, "check-is-not-cause");
                }
              } else {
                if (cause.$name === 'none') {
                  return checkRefinement(refinement.dict.value, "check-is-not-refinement");
                } else {
                  return checkRefinementCause(refinement.dict.value, cause.dict.value, "check-is-not-refinement-cause");
                }
              }
            }
            case 's-op-is-op': {
              let opname = op.dict.op;
              let opFunName: string;
              if (opname === "op==") { opFunName = "equal-always3"; }
              else if (opname === "op=~") { opFunName = "equal-now3"; }
              else if (opname === "op<=>") { opFunName = "identical3"; }
              else { throw new InternalCompilerError("Unknown op: " + opname); }
          
              const refinement = A['s-id'].app(l, A['s-name'].app(l, opFunName));
              if (cause.$name === 'none') {
                return checkRefinement(refinement, 'check-is-refinement');
              } else {
                return checkRefinementCause(refinement, cause.dict.value, 'check-is-refinement-cause');
              }
            }
            case 's-op-is-not-op': {
              let opname = op.dict.op;
              let opFunName: string;
              if (opname === "op==") { opFunName = "equal-always3"; }
              else if (opname === "op=~") { opFunName = "equal-now3"; }
              else if (opname === "op<=>") { opFunName = "identical3"; }
              else { throw new InternalCompilerError("Unknown op: " + opname); }
          
              const refinement = A['s-id'].app(l, A['s-name'].app(l, opFunName));
              if (cause.$name === 'none') {
                return checkRefinement(refinement, 'check-is-not-refinement');
              } else {
                return checkRefinementCause(refinement, cause.dict.value, 'check-is-not-refinement-cause');
              }
            }
            case 's-op-satisfies': {
              if (cause.$name === 'none') {
                return checkOp('check-satisfies-delayed');
              } else {
                return checkOpCause(cause.dict.value, 'check-satisfies-delayed-cause');
              }
            }
            case 's-op-satisfies-not': {
              if (cause.$name === 'none') {
                return checkOp('check-satisfies-not-delayed');
              } else {
                return checkOpCause(cause.dict.value, 'check-satisfies-not-delayed-cause');
              }
            }
            case 's-op-raises': {
              if (cause.$name === 'none') {
                return checkRaises('check-raises-str');
              } else {
                return checkRaisesCause(cause.dict.value, 'check-raises-str-cause');
              }
            }
            case 's-op-raises-not': {
              if (cause.$name === 'none') {
                return checkRaisesNot('check-raises-not');
              } else {
                return checkRaisesNotCause(cause.dict.value, 'check-raises-not-cause');
              }
            }
            case 's-op-raises-other': {
              if (cause.$name === 'none') {
                return checkRaises('check-raises-other-str');
              } else {
                return checkRaisesCause(cause.dict.value, 'check-raises-other-str-cause');
              }
            }
            case 's-op-raises-satisfies': {
              if (cause.$name === 'none') {
                return checkRaises('check-raises-satisfies');
              } else {
                return checkRaisesCause(cause.dict.value, 'check-raises-satisfies');
              }
            }
            case 's-op-raises-violates': {
              if (cause.$name === 'none') {
                return checkRaises('check-raises-violates');
              } else {
                return checkRaisesCause(cause.dict.value, 'check-raises-violates');
              }
            }
            default: throw new ExhaustiveSwitchError(op);
          }
        }
      };

      for (const stmt of stmts) {
        switch(stmt.$name) {
          case 's-fun': 
          case 's-data': {
            const { l, name, _check } = stmt.dict;
            if (_check.$name === 'some') {
              ret.push(new CheckInfo(l, name, map(checkStmtsVisitor, _check.dict.value), true));
            }
            break;
          }
          case 's-check': {
            let checkName: string;
            const { l, name, body, "keyword-check": keywordIsCheck } = stmt.dict;
            if (name.$name === 'none') {
              standaloneCounter += 1;
              checkName = `${(keywordIsCheck ? "check-block-" : "examples-block-")}${standaloneCounter}`;
            } else {
              checkName = name.dict.value;
            }
            ret.push(new CheckInfo(l, checkName, map(checkStmtsVisitor, body), keywordIsCheck));
          }
          default: break;
        }
      }

      return ret;
    }

    function createCheckBlock(l : A.Srcloc, checksToPerform: CheckInfo[]): TJ.Variant<A.Expr, 's-block'> {
      const checkerExprs: TJ.Variant<A.Expr, 's-obj'>[] = [];
      for (const check of checksToPerform) {
        const { l, name, body, keywordIsCheck } = check;
        checkerExprs.push(A['s-obj'].app(l, runtime.ffi.makeList([
          A['s-data-field'].app(l, "name", A['s-str'].app(l, name)),
          A['s-data-field'].app(l, "run", makeLam(l, runtime.ffi.makeList([]), body)),
          A['s-data-field'].app(l, "keyword-check", A['s-bool'].app(l, keywordIsCheck)),
          A['s-data-field'].app(l, "location", astSrcloc(l))
        ])));
      }
      let filename: string;
      if (l.$name === 'builtin') {
        filename = l.dict['module-name'];
      } else {
        filename = l.dict.source;
      }
      return A['s-block'].app(l, runtime.ffi.makeList([
        A['s-app'].app(l, A['s-dot'].app(l, checkers(l), "run-checks"), runtime.ffi.makeList([
          A['s-str'].app(l, filename),
          // TODO(joe): need to make this a s-global somehow
          A['s-construct'].app(l, 
            A['s-construct-normal'], A['s-id'].app(l, A['s-name'].app(l, "list")), 
            runtime.ffi.makeList(checkerExprs))
        ]))
      ]));
    }

    /**
      Desugars all check blocks to be calls to the current checker

      Preconditions on prog:
        - well-formed

      Postconditions on prog:
        - contains no s-check or s-check-test statements
        - all where blocks on s-lam, s-fun, s-data, s-method are none
    */
    function desugarCheck(prog: A.Program): A.Program {
      const ret = map<A.Program | A.Expr, A.Program>({
        's-block': (visitor, block: TJ.Variant<A.Expr, 's-block'>) => {
          const l = block.dict.l;
          const stmts = listToArray(block.dict.stmts);
          const checksToPerform = getChecks(stmts);
          const dsStmts = stmts.map((s) => map<A.Expr>(visitor, s));
          const doChecks = createCheckBlock(l, checksToPerform);
          if (checksToPerform.length === 0) { 
            return A['s-block'].app(l, runtime.ffi.makeList(dsStmts));
          } else if (dsStmts.length === 0) {
            throw new InternalCompilerError("Empty block");
          } else {
            const idResult = A['s-name'].app(l, G['make-name'].app("$result-after-checks"))
            const lastExpr = dsStmts.pop()!;
            return A['s-block'].app(
              l,
              runtime.ffi.makeList([
                ...dsStmts,
                A['s-let'].app(l, A['s-bind'].app(l, true, idResult, A['a-blank']), lastExpr, false),
                doChecks,
                A['s-id'].app(l, idResult)
              ]));
          }
        }
      }, prog);
      return ret;
    }

    function desugarNoChecks(prog: A.Program): A.Program {
      return map<A.Program | A.Expr, A.Program>({
        's-block': (visitor, block: TJ.Variant<A.Expr, 's-block'>) => {
          const l = block.dict.l;
          const stmts = listToArray(block.dict.stmts);
          const newStmts: A.Expr[] = [];
          for (const stmt of stmts) {
            const newStmt = map<A.Expr>(visitor, stmt);
            if (A['is-s-id'].app(newStmt) 
                && A['is-s-name'].app(newStmt.dict.id)
                && newStmt.dict.id.dict.s === "$elidedCheckBlock") {
              continue;
            } else {
              newStmts.push(newStmt);
            }
          }
          return A['s-block'].app(l, runtime.ffi.makeList(newStmts));
        },
        's-fun': (_visitor, fun) => {
          const { l, name, params, args, ann, doc, body, blocky } = fun.dict;
          return A['s-fun'].app(l, name, params, args, ann, doc, body, none, none, blocky);
        },
        's-data': (_visitor, data) => {
          const { l, name, params, mixins, variants, "shared-members": sharedMembers } = data.dict;
          return A['s-data'].app(l, name, params, mixins, variants, sharedMembers, none, none);
        },
        's-lam': (_visitor, lam) => {
          const { l, name, params, args, ann, doc, body, blocky } = lam.dict;
          return A['s-fun'].app(l, name, params, args, ann, doc, body, none, none, blocky);
        },
        's-check': (_visitor, check) => {
          // Because we now weave contracts in, and because examples blocks can go between
          // mutually-recursive functions, we need to change our desugaring of elided check blocks
          // to be completely removed, rather than be a nilpotent expression      
          return A['s-id'].app(check.dict.l, A['s-name'].app(check.dict.l, "$elidedCheckBlock"));
        }
      }, prog);
    }


    const exports : Exports = {
      'desugar-check': runtime.makeFunction(desugarCheck),
      'desugar-no-checks': runtime.makeFunction(desugarNoChecks)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})