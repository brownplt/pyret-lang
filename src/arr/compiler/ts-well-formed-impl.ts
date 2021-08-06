import type * as A from './ts-ast';
import type * as C from './ts-compile-structs';
import type * as ED from './error-display';
import type * as TJ from './ts-codegen-helpers';
import type { List } from './ts-impl-types';

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['error-display.arr'] },
  ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "check-well-formed": "tany"
    }
  },
  theModule: function (runtime, _, __, tj: TJ.Exports, Ain: (A.Exports), Cin: (C.Exports), EDin: (ED.Exports)) {
    const A = Ain.dict.values.dict;
    const C = Cin.dict.values.dict;
    const ED = EDin.dict.values.dict;

    function checkWellFormed(ast: A.Program, options): C.CompileResult<A.Program> {
      const {
        visit,
        listToArray,
        nameToName,
        nameToSourceString,
        ExhaustiveSwitchError,
      } = tj;
      let errors: C.CompileError[] = [];
      let inCheckBlock = false;
      let allowSMethod = false;
      let curShared = [];

      function addError(err: C.CompileError): void {
        errors.push(err);
      }

      function wfError(msg: List<ED.ErrorDisplay>, loc: A.Srcloc): void {
        addError(C['wf-err'].app(msg, loc));
      }

      function wrapVisitAllowSMethod(visitor, target: A.Expr | A.Provide | A.Member, allow: boolean): void {
        let curAllow = allowSMethod;
        allowSMethod = allow;
        visit(visitor, target);
        allowSMethod = curAllow;
      }

      function isBinder(expr: A.Expr): boolean {
        return A['is-s-let'].app(expr) || A['is-s-fun'].app(expr) || A['is-s-var'].app(expr) || A['is-s-rec'].app(expr);
      }

      function isBlockAllowed(expr: A.Expr): boolean {
        return isBinder(expr) || A['is-s-spy-block'].app(expr);
      }

      function explicitlyBlockyBlock(block: TJ.Variant<A.Expr, 's-block'>): boolean {
        let seenNonLet = false;
        let isBlocky = false;
        let seenTemplate = false;
        listToArray(block.dict.stmts).forEach(stmt => {
          if (A['is-s-template'].app(stmt)) {
            seenTemplate = true;
          } else if (seenNonLet) {
            isBlocky = true;
          } else if (!isBlockAllowed(stmt)) {
            seenNonLet = true;
          }
        });
        return isBlocky && !seenTemplate;
      }

      function wfBlockyBlocks(l: A.Srcloc, blocks: TJ.Variant<A.Expr, 's-block'>[]): void {
        const explicitlyBlockyBlocks = blocks.filter(explicitlyBlockyBlock);
        if (explicitlyBlockyBlocks.length !== 0) {
          addError(C['block-needed'].app(l, runtime.ffi.makeList(explicitlyBlockyBlocks)));
        }
      }

      function ensureUniqueIdsOrBindings(bindings: A.Bind[], checkShadows: boolean): void {
        let ad = new Map<string, A.Srcloc>();
        function help(bind: A.Bind) {
          switch (bind.$name) {
            case 's-bind': {
              const { l, shadows, id } = bind.dict;
              if (id.$name === 's-underscore') {
                return;
              }
              if (shadows && checkShadows) {
                return
              }
              if (ad.has(nameToName(id))) {
                addError(C['duplicate-id'].app(nameToSourceString(id), l, ad.get(nameToName(id))));
              } else {
                ad.set(nameToName(id), l);
              }
              break;
            }
            case 's-tuple-bind': {
              switch (bind.dict['as-name'].$name) {
                case 'none': {
                  break;
                }
                case 'some': {
                  help(bind.dict['as-name'].dict.value);
                  break;
                }
                default: {
                  throw new ExhaustiveSwitchError(bind.dict['as-name']);
                }
              }
              listToArray(bind.dict.fields).forEach(help);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(bind);
            }
          }
        }
        bindings.forEach(help);
      }

      function checkUnderscoreName(fields: A.Member[], kindOfThing: string): void {
        const underscores = fields.filter(f => f.dict.name === '_');
        if (underscores.length !== 0) {
          addError(C['underscore-as'].app(underscores[0].dict.l, kindOfThing));
        }
      }

      function ensureDistinctLines(loc: A.Srcloc, prevIsTemplate: boolean, stmts: List<A.Expr>): void {
        switch (stmts.$name) {
          case 'empty': {
            return;
          }
          case 'link': {
            const { first, rest } = stmts.dict;
            switch (loc.$name) {
              case 'builtin': {
                ensureDistinctLines(first.dict.l, A['is-s-template'].app(first), rest);
                break;
              }
              case 'srcloc': {
                const endLine1 = loc.dict['end-line'];
                switch (first.dict.l.$name) {
                  case 'builtin': {
                    ensureDistinctLines(loc, prevIsTemplate, rest);
                    break;
                  }
                  case 'srcloc': {
                    const startLine2 = first.dict.l.dict['start-line'];
                    if (endLine1 === startLine2) {
                      if (A['is-s-template'].app(first) && prevIsTemplate) {
                        addError(C['template-same-line'].app(loc, first.dict.l));
                      } else if (!A['is-s-template'].app(first) && !prevIsTemplate) {
                        addError(C['same-line'].app(loc, first.dict.l, A['is-s-paren'].app(first)));
                      }
                    }
                    ensureDistinctLines(first.dict.l, A['is-s-template'].app(first), rest);
                    break;
                  }
                  default: {
                    throw new ExhaustiveSwitchError(first.dict.l);
                  }
                }
                break;
              }
              default: {
                throw new ExhaustiveSwitchError(loc);
              }
            }
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(stmts);
          }
        }
      }

      function fieldsToBinds(members: A.Member[]): A.Bind[] {
        return members.map(member => {
          return A['s-bind'].app(member.dict.l, false, A['s-name'].app(member.dict.l, member.dict.name), A['a-blank']);
        });
      }

      function opname(op: string): string {
        return op.substr(2);
      }

      function reachableOps(visitor, l: A.Srcloc, opL: A.Srcloc, op: string, expr: A.Expr) {
        switch (expr.$name) {
          case 's-op': {
            const {'l': l2, 'op-l': opL2, 'op': op2, 'left': left2, 'right': right2} = expr.dict;
            if (op === op2) {
              reachableOps(visitor, l, opL, op, left2);
              reachableOps(visitor, l, opL, op, right2);
            } else {
              addError(C['mixed-binops'].app(l, opname(op), opL, opname(op2), opL2));
            }
            break;
          }
          default: {
            wrapVisitAllowSMethod(visitor, expr, false);
            break;
          }
        }
      }

      function rejectStandaloneExprs(stmts: A.Expr[], ignoreLast: boolean): void {
        function badStmt(l: A.Srcloc, stmt: A.Expr) {
          if (stmt.$name === 's-op') {
            if (stmt.dict.op === 'op==') {
              const secondPara = inCheckBlock ?
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("To write an example or test case, use the "),
                  ED.code.app(ED.text.app("is")),
                  ED.text.app(" operator; "),
                  ED.text.app("to define a name, use the "),
                  ED.code.app(ED.text.app("=")),
                  ED.text.app(" operator instead.")
                ])) :
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("To define a name, use the "),
                  ED.code.app(ED.text.app("=")),
                  ED.text.app(" operator instead."),
                ]));
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone "),
                  ED.highlight.app(ED.code.app(ED.text.app("==")), runtime.ffi.makeList([stmt.dict['op-l']]), 1),
                  ED.text.app(" operator expression probably isn't intentional.")
                ])),
                secondPara
              ]),
                l);
            } else {
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone "),
                  ED.highlight.app(ED.code.app(ED.text.app(stmt.dict.op.substr(2))), runtime.ffi.makeList([stmt.dict['op-l']]), 1),
                  ED.text.app(" operator expression probably isn't intentional.")
                ]))
              ]),
                l);
            }
          } else if (stmt.$name === 's-id') {
            wfError(runtime.ffi.makeList([
              ED.paragraph.app(runtime.ffi.makeList([
                ED.text.app("A standalone variable name probably isn't intentional.")
              ]))
            ]),
              l);
          } else if (stmt.$name === 's-num' || stmt.$name === 's-frac' || stmt.$name === 's-rfrac' || stmt.$name === 's-bool' || stmt.$name === 's-str') {
            wfError(runtime.ffi.makeList([
              ED.paragraph.app(runtime.ffi.makeList([
                ED.text.app("A standalone value probably isn't intentional.")
              ]))
            ]),
              l);
          } else if (stmt.$name === 's-dot') {
            wfError(runtime.ffi.makeList([
              ED.paragraph.app(runtime.ffi.makeList([
                ED.text.app("A standalone field-lookup expression probably isn't intentional.")
              ]))
            ]),
              l);
          } else if (stmt.$name === 's-lam') {
            wfError(runtime.ffi.makeList([
              ED.paragraph.app(runtime.ffi.makeList([
                ED.text.app("A standalone anonymous function expression probably isn't intentional.")
              ]))
            ]),
              l)
          } else if (stmt.$name === 's-paren') {
            badStmt(l, stmt.dict.expr);
          }
        }
        if (stmts.find(stmt => A['is-s-template'].app(stmt)) !== undefined) {
          return;
        }
        const toExamineLength = ignoreLast ? stmts.length - 1 : stmts.length;
        for (let i = 0; i < toExamineLength; i++) {
          badStmt(stmts[i].dict.l, stmts[i]);
        }
      }

      function wfBlockStmts(visitor, l: A.Srcloc, stmts: List<A.Expr>, topLevel: boolean): void {
        function mapStmts(stmt: A.Expr): A.Bind | null {
          if (A['is-s-var'].app(stmt)) {
            return stmt.dict.name;
          } else if (A['is-s-let'].app(stmt)) {
            return stmt.dict.name;
          } else if (A['is-s-rec'].app(stmt)) {
            return stmt.dict.name;
          } else {
            return null;
          }
        }
        const bindStmts: A.Bind[] = listToArray(stmts).map(mapStmts).filter(stmt => stmt);
        ensureUniqueIdsOrBindings(bindStmts, true);
        ensureDistinctLines(A['dummy-loc'], false, stmts);
        if (!inCheckBlock && !topLevel) {
          rejectStandaloneExprs(listToArray(stmts), true);
        }
        listToArray(stmts).forEach(stmt => wrapVisitAllowSMethod(visitor, stmt, false));
      }

      const wellFormedVisitor: TJ.Visitor<A.Ann | A.Expr, void> = {
        'a-name': (visitor, expr: TJ.Variant<A.Ann, 'a-name'>) => {
          if (A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['underscore-as-ann'].app(expr.dict.l));
          }
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>) => {
          const {l, 'op-l': opL, op, left, right} = expr.dict;
          reachableOps(visitor, l, opL, op, left);
          reachableOps(visitor, l, opL, op, right);
        }
      }

      const topLevelVisitor: TJ.Visitor<A.Program | A.Expr | A.TypeLetBind | A.Variant, void> = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>) => {
          const body = expr.dict.block;
          if (body.$name === 's-block') {
            wfBlockStmts(visitor, body.dict.l, body.dict.stmts, true);
          } else {
            visit(visitor, body);
          }
          wrapVisitAllowSMethod(visitor, expr.dict._provide, false);
          visit(visitor, expr.dict['provided-types']);
          listToArray(expr.dict.imports).forEach(i => visit(visitor, i));
        },
        's-type': (visitor, expr: TJ.Variant<A.Expr, 's-type'>) => {
          visit<A.Ann>(wellFormedVisitor, expr.dict.ann);
        },
        's-newtype': (visitor, expr: TJ.Variant<A.Expr, 's-newtype'>) => {
          return;
        },
        's-type-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-type-let-expr'>) => {
          if (!expr.dict.blocky && A['is-s-block'].app(expr.dict.body)) {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.binds).forEach(b => visit(visitor, b));
          wrapVisitAllowSMethod(wellFormedVisitor, expr.dict.body, false);
        },
        's-type-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-type-bind'>) => {
          visit<A.Ann>(wellFormedVisitor, expr.dict.ann);
        },
        's-newtype-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-newtype-bind'>) => {
          return;
        },
        's-variant': (visitor, expr: TJ.Variant<A.Variant, 's-variant'>) => {
          const members = listToArray(expr.dict.members);
          const withMembers = listToArray(expr.dict['with-members']);
          const memberBinds = members.map(m => m.dict.bind);
          memberBinds.forEach(bind => {
            if (bind.$name === 's-tuple-bind') {
              wfError(runtime.ffi.makeList([
                ED.text.app("Tuple binding not allowed as variant member ")
              ]),
                bind.dict.l);
            }
          });
          const ids = fieldsToBinds(withMembers).concat(memberBinds);
          ensureUniqueIdsOrBindings(ids, false);
          const underscores = members
            .filter(member => A['is-s-bind'].app(member.dict.bind) && A['is-s-underscore'].app(member.dict.bind.dict.id));
          if (underscores.length !== 0) {
            addError(C['underscore-as'].app(underscores[0].dict.l, "a data variant name"));
          }
          checkUnderscoreName(withMembers, "a field name");
          members.forEach(m => visit(wellFormedVisitor, m));
          withMembers.forEach(wm => wrapVisitAllowSMethod(wellFormedVisitor, wm, true));
        },
        's-singleton-variant': (visitor, expr: TJ.Variant<A.Variant, 's-singleton-variant'>) => {
          const withMembers = listToArray(expr.dict['with-members']);
          ensureUniqueIdsOrBindings(fieldsToBinds(withMembers), false);
          withMembers.forEach(wm => wrapVisitAllowSMethod(wellFormedVisitor, wm, true));
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>) => {
          visit(wellFormedVisitor, expr);
        }
      };
      visit<A.Program>(topLevelVisitor, ast);
      if (errors.length === 0) {
        return C.ok.app(ast);
      } else {
        return C.err.app(runtime.ffi.makeList(errors));
      }
    }
    return runtime.makeModuleReturn({
      "check-well-formed": runtime.makeFunction(checkWellFormed)
    }, {});
  }
})