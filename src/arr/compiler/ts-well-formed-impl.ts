import type * as A from './ts-ast';
import type * as C from './ts-compile-structs';
import type * as ED from './error-display';
import type * as TJ from './ts-codegen-helpers';
import type * as S from './ts-srcloc';
import type { List, PFunction, Option } from './ts-impl-types';
import { CompileOptions } from './ts-compiler-lib-impl';

export type Exports = {
  dict: {
    values: {
      dict: {
        'check-well-formed': PFunction<(ast: A.Program, options: CompileOptions) => C.CompileResult<A.Program>>,
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['error-display.arr'] },
    { 'import-type': 'builtin', name: 'srcloc' },
  ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "check-well-formed": "tany"
    }
  },
  theModule: function (runtime, _, __, tj: TJ.Exports, Ain: (A.Exports), Cin: (C.Exports), EDin: (ED.Exports), Sin: (S.Exports)) {
    const A = Ain.dict.values.dict;
    const C = Cin.dict.values.dict;
    const ED = EDin.dict.values.dict;
    const S = Sin.dict.values.dict;

    let logger: PFunction<(val: any, _ignored: Option<any>) => void>;
    function LOG(val: any): void {
      logger.app(val, runtime.ffi.makeNone());
    }

    function checkWellFormed(ast: A.Program, options: CompileOptions): C.CompileResult<A.Program> {
      const {
        visit,
        listToArray,
        nameToName,
        nameToSourceString,
        formatSrcloc,
        ExhaustiveSwitchError,
        InternalCompilerError,
      } = tj;
      let errors: C.CompileError[] = [];
      let inCheckBlock = false;
      let allowSMethod = false;
      let curShared = [];
      let paramCurrentWhereEverywhere = false;

      logger = options.dict.log;
      // LOG(`In ts-well-formed impl for ${formatSrcloc(ast.dict.l, true)}\n`);

      // TODO (manas) move this back to ts-srcloc
      function upTo(l: TJ.Variant<A.Srcloc, 'srcloc'>, l2: TJ.Variant<A.Srcloc, 'srcloc'>): TJ.Variant<A.Srcloc, 'srcloc'> {
        if (l.dict['start-char'] <= l2.dict['end-char']) {
          return S.srcloc.app(
            l.dict.source,
            l.dict['start-line'],
            l.dict['start-column'],
            l.dict['start-char'],
            l2.dict['start-line'],
            l2.dict['start-column'],
            l2.dict['start-char'],
          );
        } else {
          return l;
        }
      }

      function uptoEnd(l: TJ.Variant<A.Srcloc, 'srcloc'>, l2: TJ.Variant<A.Srcloc, 'srcloc'>): TJ.Variant<A.Srcloc, 'srcloc'> {
        if (l.dict['start-char'] <= l2.dict['end-char']) {
          return S.srcloc.app(
            l.dict.source,
            l.dict['start-line'],
            l.dict['start-column'],
            l.dict['start-char'],
            l2.dict['end-line'], l2.dict['end-column'], l2.dict['end-char']
          );
        } else {
          return l;
        }
      }

      function atStart(l: TJ.Variant<A.Srcloc, 'srcloc'>): TJ.Variant<A.Srcloc, 'srcloc'> {
        return S.srcloc.app(
          l.dict.source,
          l.dict['start-line'],
          l.dict['start-column'],
          l.dict['start-char'],
          l.dict['start-line'],
          l.dict['start-column'],
          l.dict['start-char'],
        );
      }

      function plus(l: TJ.Variant<A.Srcloc, 'srcloc'>, other: TJ.Variant<A.Srcloc, 'srcloc'>): A.Srcloc {
        if (l.dict['start-char'] <= other.dict['start-char']) {
          if (l.dict['end-char'] >= other.dict['end-char']) {
            return l;
          } else {
            return S.srcloc.app(
              l.dict.source,
              l.dict['start-line'],
              l.dict['start-column'],
              l.dict['start-char'],
              other.dict['end-line'],
              other.dict['end-column'],
              other.dict['end-char']
            );
          }
        } else {
          if (l.dict['end-char'] > other.dict['end-char']) {
            return S.srcloc.app(
              l.dict.source,
              other.dict['start-line'],
              other.dict['start-column'],
              other.dict['start-char'],
              l.dict['end-line'],
              l.dict['end-column'],
              l.dict['end-char']
            );
          } else {
            return other;
          }
        }
      }
      // end ts-srcloc

      // TODO (manas) move to ts-compile-structs
      const reactorFields = new Map<string, (l: A.Srcloc) => TJ.Variant<A.Ann, "a-name" | "a-any">>();
      reactorFields.set("last-image", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("on-tick", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("to-draw", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("on-key", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("on-mouse", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("stop-when", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
      reactorFields.set("seconds-per-tick", l => A['a-name'].app(l, A['s-type-global'].app("NumPositive")));
      reactorFields.set("title", l => A['a-name'].app(l, A['s-type-global'].app("String")));
      reactorFields.set("close-when-stop", l => A['a-name'].app(l, A['s-type-global'].app("Boolean")));
      reactorFields.set("init", l => A['a-any'].app(l));

      function getLocWithCheckBlock(l: A.Srcloc, checkLoc: Option<A.Srcloc>): A.Srcloc {
        switch (checkLoc.$name) {
          case 'none': {
            return l;
          }
          case 'some': {
            const lRef = l as TJ.Variant<A.Srcloc, 'srcloc'>;
            const l2 = checkLoc.dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
            return uptoEnd(lRef, l2);
          }
          default: {
            throw new ExhaustiveSwitchError(checkLoc);
          }
        }
      }

      function checkBlockLoc(l: A.Srcloc, checkLoc: Option<A.Srcloc>): S.Srcloc | null {
        switch (checkLoc.$name) {
          case 'none': {
            return null;
          }
          case 'some': {
            const lRef = l as TJ.Variant<A.Srcloc, 'srcloc'>;
            const cl = checkLoc.dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
            return uptoEnd(cl, lRef);
          }
          default: {
            throw new ExhaustiveSwitchError(checkLoc);
          }
        }
      }

      function sMethodHelper(visitor, expr: TJ.Variant<A.Expr, 's-method'> | TJ.Variant<A.Member, 's-method-field'>, parentBlockLoc: S.Srcloc) {
        const args = listToArray(expr.dict.args);
        if (args.length === 0) {
          addError(C['no-arguments'].app(expr));
        }
        ensureUniqueBindings(args, false);
        if (expr.dict._check.$name === 'some') {
          ensureEmptyBlock(expr.dict.l, "methods", expr.dict._check.dict.value as TJ.Variant<A.Expr, 's-block'>);
        }
        if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
          wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
        }
        args.forEach(arg => visit(visitor, arg, parentBlockLoc));
        visit(visitor, expr.dict.ann, parentBlockLoc);
        wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
        wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
        wrapVisitCheck(visitor, expr.dict._check, checkLoc);
      }

      function bindHelper(visitor, l: A.Srcloc, bind: A.Bind, val: A.Expr, varOrRec: 'var' | 'rec', parentBlockLoc: S.Srcloc) {
        let pointless;
        let str;
        switch (varOrRec) {
          case 'var': {
            pointless = 'pointless-var';
            str = "Variable";
            break;
          }
          case 'rec': {
            pointless = 'pointless-rec';
            str = "Recursive";
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(varOrRec);
          }
        }

        switch (bind.$name) {
          case 's-bind': {
            if (A['is-s-underscore'].app(bind.dict.id)) {
              const lRef = l as TJ.Variant<A.Srcloc, 'srcloc'>;
              const bindL = bind.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
              addError(C[pointless].app(plus(atStart(lRef), bindL)));
            }
            wrapVisitAllowSMethod(visitor, bind, false, parentBlockLoc);
            wrapVisitAllowSMethod(visitor, val, false, parentBlockLoc);
            break;
          }
          case 's-tuple-bind': {
            wfError(runtime.ffi.makeList([
              ED.text.app(str + " bindings must be names and cannot be tuple bindings ")
            ]), bind.dict.l);
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(bind);
          }
        }
      }

      const reservedNames = new Map<string, boolean>();
      reservedNames.set("function", true);
      reservedNames.set("break", true);
      reservedNames.set("return", true);
      reservedNames.set("do", true);
      reservedNames.set("yield", true);
      reservedNames.set("throw", true);
      reservedNames.set("continue", true);
      reservedNames.set("while", true);
      reservedNames.set("class", true);
      reservedNames.set("interface", true);
      reservedNames.set("type", true);
      reservedNames.set("generator", true);
      reservedNames.set("alias", true);
      reservedNames.set("extends", true);
      reservedNames.set("implements", true);
      reservedNames.set("module", true);
      reservedNames.set("package", true);
      reservedNames.set("namespace", true);
      reservedNames.set("use", true);
      reservedNames.set("public", true);
      reservedNames.set("private", true);
      reservedNames.set("protected", true);
      reservedNames.set("static", true);
      reservedNames.set("const", true);
      reservedNames.set("enum", true);
      reservedNames.set("super", true);
      reservedNames.set("export", true);
      reservedNames.set("new", true);
      reservedNames.set("try", true);
      reservedNames.set("finally", true);
      reservedNames.set("debug", true);
      reservedNames.set("spy", true);
      reservedNames.set("switch", true);
      reservedNames.set("this", true);
      reservedNames.set("match", true);
      reservedNames.set("case", true);
      reservedNames.set("with", true);
      reservedNames.set("__proto__", true);

      function addError(err: C.CompileError): void {
        errors.push(err);
      }

      function wfError(msg: List<ED.ErrorDisplay>, loc: A.Srcloc): void {
        addError(C['wf-err'].app(msg, loc));
      }

      function wfError2(msg: string, loc: A.Srcloc, loc2: A.Srcloc): void {
        addError(C['wf-err-split'].app(msg, runtime.ffi.makeList([loc, loc2])));
      }

      function reservedName(loc: A.Srcloc, id: string) {
        addError(C['reserved-name'].app(loc, id));
      }

      function wrapVisitCheck(visitor, target: Option<A.Expr>, parentBlockLoc: S.Srcloc) {
        let curInCheck = inCheckBlock;
        inCheckBlock = true;
        let curAllow = allowSMethod;
        allowSMethod = false;
        visit(visitor, target, parentBlockLoc);
        inCheckBlock = curInCheck;
        allowSMethod = curAllow;
      }

      function wrapVisitAllowSMethod(visitor, target: A.Expr | A.Provide | A.Member | A.Bind | A.Name | A.LetBind | A.LetrecBind | A.IfBranch | A.IfPipeBranch | A.CasesBranch | A.ForBind | A.LoadTableSpec, allow: boolean, parentBlockLoc: S.Srcloc): void {
        let curAllow = allowSMethod;
        allowSMethod = allow;
        visit(visitor, target, parentBlockLoc);
        allowSMethod = curAllow;
      }

      function ensureEmptyBlock(loc: A.Srcloc, typ: string, block: TJ.Variant<A.Expr, 's-block'>) {
        const stmts = listToArray(block.dict.stmts);
        if (!paramCurrentWhereEverywhere && stmts.length !== 0) {
          addError(C['unwelcome-where'].app(typ, loc, block.dict.l));
        }
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
          if (stmt.$name === 's-template') {
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

      function ensureUniqueCases(cases: A.CasesBranch[]) {
        for (let i = 0; i < cases.length; i++) {
          const f = cases[i];
          for (let j = i + 1; j < cases.length; j++) {
            const b = cases[j];
            if (f.dict.name === b.dict.name) {
              addError(C['duplicate-branch'].app(f.dict.name, b.dict['pat-loc'], f.dict['pat-loc']));
            }
          }
        }
      }

      // permitShadowedIds controls whether we should permit or prohibit even explicitly-marked shadowed names
      function ensureUniqueBindings(bindings: A.Bind[], permitShadowedIds: boolean): void {
        let ad = new Map<string, A.Srcloc>();
        function help(bind: A.Bind) {
          switch (bind.$name) {
            case 's-bind': {
              const { l, shadows, id } = bind.dict;
              if (id.$name === 's-underscore') {
                return;
              }
              if (shadows && permitShadowedIds) {
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

      function ensureUniqueFields(fields: A.Member[]) {
        for (let i = fields.length - 1; i >= 0; i--) {
          const checkName = fields[i].dict.name;
          for (let j = i - 1; j >= 0; j--) {
            if (checkName === fields[j].dict.name) {
              addError(C['duplicate-field'].app(checkName, fields[i].dict.l, fields[j].dict.l));
            }
          }
        }
      }

      function checkUnderscoreName(fields: {dict: {l: A.Srcloc, name: string}}[], kindOfThing: string): void {
        const underscores = fields.filter(f => f.dict.name === '_');
        if (underscores.length !== 0) {
          addError(C['underscore-as'].app(underscores[0].dict.l, kindOfThing));
        }
      }

      function ensureDistinctLines(loc: A.Srcloc, prevIsTemplate: boolean, stmts: A.Expr[]): void {
        for (let i = 0; i < stmts.length; i++) {
          const cur = stmts[i];
          switch (loc.$name) {
            case 'builtin': {
              loc = cur.dict.l;
              prevIsTemplate = cur.$name === 's-template';
              break;
            }
            case 'srcloc': {
              const endLine1 = loc.dict['end-line'];
              switch (cur.dict.l.$name) {
                case 'builtin': {
                  break;
                }
                case 'srcloc': {
                  const startLine2 = cur.dict.l.dict['start-line'];
                  if (endLine1 === startLine2) {
                    if (cur.$name === 's-template' && prevIsTemplate) {
                      addError(C['template-same-line'].app(loc, cur.dict.l));
                    } else if (cur.$name !== 's-template' && !prevIsTemplate) {
                      addError(C['same-line'].app(loc, cur.dict.l, A['is-s-paren'].app(cur)));
                    }
                  }
                  loc = cur.dict.l;
                  prevIsTemplate = cur.$name === 's-template';
                  break;
                }
                default: {
                  throw new ExhaustiveSwitchError(cur.dict.l);
                }
              }
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(loc);
            }
          }
        }
      }

      function ensureUniqueVariantIds(variants: A.Variant[], name: string, dataLoc: A.Srcloc) {
        for (let i = 0; i < variants.length; i++) {
          const v = variants[i];
          if (v.dict.name === name) {
            addError(C['data-variant-duplicate-name'].app(v.dict.name, v.dict.l, dataLoc));
          } else if (v.dict.name === "is-" + name) {
            addError(C['duplicate-is-data'].app(name, v.dict.l, dataLoc));
          } else if ("is-" + v.dict.name === name) {
            addError(C['duplicate-is-data-variant'].app(v.dict.name, dataLoc, v.dict.l));
          }
          for (let j = i + 1; j < variants.length; j++) {
            const b = variants[j];
            if (b.dict.name === v.dict.name) {
              addError(C['duplicate-variant'].app(v.dict.name, b.dict.l, v.dict.l));
            } else if (b.dict.name === "is-" + v.dict.name) {
              addError(C['duplicate-is-variant'].app(v.dict.name, b.dict.l, v.dict.l));
            } else if ("is-" + b.dict.name === v.dict.name) {
              addError(C['duplicate-is-variant'].app(b.dict.name, v.dict.l, b.dict.l));
            }
          }
        }
      }

      function wfLastStmt(blockLoc: A.Srcloc, stmt: A.Expr) {
        switch (stmt.$name) {
          case 's-let': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "let-binding"));
            break;
          }
          case 's-var': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "var-binding"));
            break;
          }
          case 's-rec': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "rec-binding"));
            break;
          }
          case 's-fun': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "fun-binding"));
            break;
          }
          case 's-data': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "data definition"));
            break;
          }
          case 's-contract': {
            addError(C['block-ending'].app(stmt.dict.l, blockLoc, "contract"));
            break;
          }
          default: {
            break;
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

      function reachableOps(visitor, l: A.Srcloc, opL: A.Srcloc, op: string, expr: A.Expr, parentBlockLoc: S.Srcloc) {
        switch (expr.$name) {
          case 's-op': {
            const { 'l': l2, 'op-l': opL2, 'op': op2, 'left': left2, 'right': right2 } = expr.dict;
            if (op === op2) {
              reachableOps(visitor, l, opL, op, left2, parentBlockLoc);
              reachableOps(visitor, l, opL, op, right2, parentBlockLoc);
            } else {
              addError(C['mixed-binops'].app(l, opname(op), opL, opname(op2), opL2));
            }
            break;
          }
          default: {
            wrapVisitAllowSMethod(visitor, expr, false, parentBlockLoc);
            break;
          }
        }
      }

      function rejectStandaloneExprs(stmts: A.Expr[], ignoreLast: boolean): void {
        function badStmt(l: A.Srcloc, stmt: A.Expr) {
          switch (stmt.$name) {
            case 's-op': {
              switch (stmt.dict.op) {
                case 'op==': {
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
                  break;
                }
                default: {
                  wfError(runtime.ffi.makeList([
                    ED.paragraph.app(runtime.ffi.makeList([
                      ED.text.app("A standalone "),
                      ED.highlight.app(ED.code.app(ED.text.app(stmt.dict.op.substr(2))), runtime.ffi.makeList([stmt.dict['op-l']]), 1),
                      ED.text.app(" operator expression probably isn't intentional.")
                    ]))
                  ]),
                    l);
                  break;
                }
              }
              break;
            }
            case 's-id': {
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone variable name probably isn't intentional.")
                ]))
              ]),
                l);
              break;
            }
            case 's-num':
            case 's-frac':
            case 's-rfrac':
            case 's-bool':
            case 's-str': {
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone value probably isn't intentional.")
                ]))
              ]),
                l);
              break;
            }
            case 's-dot': {
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone field-lookup expression probably isn't intentional.")
                ]))
              ]),
                l);
              break;
            }
            case 's-lam': {
              wfError(runtime.ffi.makeList([
                ED.paragraph.app(runtime.ffi.makeList([
                  ED.text.app("A standalone anonymous function expression probably isn't intentional.")
                ]))
              ]),
                l);
              break;
            }
            case 's-paren': {
              badStmt(l, stmt.dict.expr);
              break;
            }
          }
        }
        if (stmts.find(stmt => stmt.$name === 's-template') !== undefined) {
          return;
        }
        const toExamineLength = ignoreLast ? stmts.length - 1 : stmts.length;
        for (let i = 0; i < toExamineLength; i++) {
          badStmt(stmts[i].dict.l, stmts[i]);
        }
      }

      function wrapRejectStandalonesInCheck(target: A.Option<TJ.Variant<A.Expr, 's-block'>>) {
        let curInCheck = inCheckBlock;
        inCheckBlock = true;
        switch (target.$name) {
          case 'none': {
            break;
          }
          case 'some': {
            const stmts = listToArray(target.dict.value.dict.stmts);
            if (stmts.length !== 0) {
              rejectStandaloneExprs(stmts, true);
            }
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(target);
          }
        }
        inCheckBlock = curInCheck;
      }

      function wfBlockStmts(visitor, stmts: A.Expr[], topLevel: boolean, parentBlockLoc: S.Srcloc): void {
        function mapStmts(stmt: A.Expr): A.Bind | null {
          switch (stmt.$name) {
            case 's-var':
            case 's-let':
            case 's-rec': {
              return stmt.dict.name;
            }
            default: {
              return null;
            }
          }
        }
        const bindStmts: A.Bind[] = stmts.map(mapStmts).filter(stmt => stmt);
        ensureUniqueBindings(bindStmts, true);
        ensureDistinctLines(A['dummy-loc'], false, stmts);
        if (!inCheckBlock && !topLevel) {
          rejectStandaloneExprs(stmts, true);
        }
        stmts.forEach(stmt => wrapVisitAllowSMethod(visitor, stmt, false, parentBlockLoc));
      }

      function wfExamplesBody(body: TJ.Variant<A.Expr, 's-block'>) {
        const stmts = listToArray(body.dict.stmts);
        stmts.forEach(s => {
          if (s.$name !== 's-check-test' && s.$name !== 's-template') {
            addError(C['non-example'].app(s));
          }
        });
      }

      function wfTableHeaders(loc: A.Srcloc, headers: A.FieldName[]) {
        headers.forEach(h => {
          if (h.dict.name === '_') {
            addError(C['underscore-as'].app(h.dict.l, "as a table column's name in a table expression"));
          }
        });
        if (headers.length === 0) {
          addError(C['table-empty-header'].app(loc));
        }
        for (let i = 0; i < headers.length; i++) {
          const h = headers[i];
          if (reservedNames.has(h.dict.name)) {
            reservedName(h.dict.l, h.dict.name);
          }
          for (let j = i + 1; j < headers.length; j++) {
            if (h.dict.name === headers[j].dict.name) {
              addError(C['table-duplicate-column-name'].app(h, headers[j]));
            }
          }
        }
      }

      const wellFormedVisitor: TJ.Visitor<
        A.Program | A.ImportType | A.Ann | A.Expr | A.Member | A.LetBind | A.Bind | A.Import | A.Provide | A.ProvideTypes | A.LetrecBind | A.CasesBranch | A.IfBranch | A.IfPipeBranch | A.ForBind | A.VariantMember | A.AField, 
        void,
        S.Srcloc
      > = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>) => {
          throw new InternalCompilerError("Impossible");
        },
        's-special-import': (visitor, expr: TJ.Variant<A.ImportType, 's-special-import'>) => {
          const kind = expr.dict.kind;
          const args = listToArray(expr.dict.args);
          if (kind === 'my-gdrive' && args.length !== 1) {
            addError(C['import-arity-mismatch'].app(expr.dict.l, kind, expr.dict.args, 2, runtime.ffi.makeList(["the name of the file"])));
          } else if (kind === 'shared-gdrive' && args.length !== 2) {
            addError(C['import-arity-mismatch'].app(expr.dict.l, kind, expr.dict.args, 2, runtime.ffi.makeList(["the name of the file", "the file's id, which you can get from the share URL"])));
          } else if (kind === 'gdrive-js' && args.length !== 2) {
            addError(C['import-arity-mismatch'].app(expr.dict.l, kind, expr.dict.args, 2, runtime.ffi.makeList(["the name of the file", "the file's id"])));
          }
        },
        's-data': (visitor, expr: TJ.Variant<A.Expr, 's-data'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("data declaration", expr.dict.l, parentBlockLoc));
        },
        's-data-expr': (visitor, expr: TJ.Variant<A.Expr, 's-data-expr'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("data declaration", expr.dict.l, parentBlockLoc));
        },
        's-type': (visitor, expr: TJ.Variant<A.Expr, 's-type'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("type alias", expr.dict.l, parentBlockLoc));
        },
        's-newtype': (visitor, expr: TJ.Variant<A.Expr, 's-newtype'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("newtype", expr.dict.l, parentBlockLoc));
        },
        's-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-let-expr'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.binds).forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-contract': (visitor, expr: TJ.Variant<A.Expr, 's-contract'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("contract declaration", expr.dict.l, parentBlockLoc));
        },
        's-letrec-bind': (visitor, expr: TJ.Variant<A.LetrecBind, 's-letrec-bind'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          switch (expr.dict.b.$name) {
            case 's-bind': {
              break;
            }
            case 's-tuple-bind': {
              wfError(runtime.ffi.makeList([
                ED.text.app("Recursive bindings must be names and cannot be tuple bindings ")
              ]),
              expr.dict.b.dict.l);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict.b);
            }
          }
          visit(visitor, expr.dict.b, parentBlockLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.value, false, parentBlockLoc);
        },
        's-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-letrec'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.binds).forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-type-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-type-let-expr'>, parentBlockLoc) => {
          addError(C['non-toplevel'].app("type alias", expr.dict.l, parentBlockLoc));
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>, parentBlockLoc) => {
          const { l, 'op-l': opL, op, left, right } = expr.dict;
          reachableOps(visitor, l, opL, op, left, parentBlockLoc);
          reachableOps(visitor, l, opL, op, right, parentBlockLoc);
        },
        's-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-cases-branch'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (expr.dict.name === '_') {
            addError(C['underscore-as-pattern'].app(expr.dict['pat-loc']));
          }
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args.map(a => a.dict.bind), false);
          args.forEach(a => visit(visitor, a, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-singleton-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-singleton-cases-branch'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (expr.dict.name === '_') {
            addError(C['underscore-as-pattern'].app(expr.dict['pat-loc']));
          }
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-var': (visitor, expr: TJ.Variant<A.Expr, 's-var'>, parentBlockLoc) => {
          bindHelper(visitor, expr.dict.l, expr.dict.name, expr.dict.value, 'var', parentBlockLoc);
        },
        's-rec': (visitor, expr: TJ.Variant<A.Expr, 's-rec'>, parentBlockLoc) => {
          bindHelper(visitor, expr.dict.l, expr.dict.name, expr.dict.value, 'rec', parentBlockLoc);
        },
        's-var-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-var-bind'>, parentBlockLoc) => {
          bindHelper(visitor, expr.dict.l, expr.dict.b, expr.dict.value, 'var', parentBlockLoc);
        },
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>, parentBlockLoc) => {
          const stmts = listToArray(expr.dict.stmts);
          if (stmts.length === 0) {
            addError(C['wf-empty-block'].app(parentBlockLoc));
          } else {
            wfLastStmt(parentBlockLoc, stmts[stmts.length - 1]);
            wfBlockStmts(visitor, stmts, false, parentBlockLoc);
          }
        },
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-tuple-bind': (visitor, expr: TJ.Variant<A.Bind, 's-tuple-bind'>) => {
          return;
        },
        's-bind': (visitor, expr: TJ.Variant<A.Bind, 's-bind'>, parentBlockLoc) => {
          const nameStr = nameToSourceString(expr.dict.id);
          if (reservedNames.has(nameStr)) {
            reservedName(expr.dict.l, nameStr);
          }
          if (expr.dict.shadows && A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['pointless-shadow'].app(expr.dict.l));
          }
          wrapVisitAllowSMethod(visitor, expr.dict.id, false, parentBlockLoc);
          visit(visitor, expr.dict.ann, parentBlockLoc);
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>, parentBlockLoc) => {
          if (!inCheckBlock) {
            addError(C['unwelcome-test'].app(expr.dict.l));
          }
          if (expr.dict.refinement.$name === 'some') {
            switch (expr.dict.op.$name) {
              case 's-op-is':
              case 's-op-is-not': {
                break;
              }
              default: {
                addError(C['unwelcome-test-refinement'].app(
                  expr.dict.refinement.dict.value,
                  expr.dict.op));
                break;
              }
            }
          }
          wrapVisitAllowSMethod(visitor, expr.dict.left, false, parentBlockLoc)
          visit(visitor, expr.dict.right, parentBlockLoc);
          visit(visitor, expr.dict.cause, parentBlockLoc);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>, _parentBlockLoc) => {
          const parentBlockLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          sMethodHelper(visitor, expr, parentBlockLoc);
        },
        's-data-field': (visitor, expr: TJ.Variant<A.Member, 's-data-field'>, parentBlockLoc) => {
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          visit(visitor, expr.dict.value, parentBlockLoc);
        },
        's-mutable-field': (visitor, expr: TJ.Variant<A.Member, 's-mutable-field'>, parentBlockLoc) => {
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          visit(visitor, expr.dict.value, parentBlockLoc);
          visit(visitor, expr.dict.ann, parentBlockLoc);
        },
        's-method': (visitor, expr: TJ.Variant<A.Expr, 's-method'>, _parentBlockLoc) => {
          if (!allowSMethod) {
            addError(C['wf-bad-method-expression'].app(expr.dict.l));
          }
          const parentBlockLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          sMethodHelper(visitor, expr, parentBlockLoc);
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>, parentBlockLoc) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args, false);
          switch (expr.dict._check.$name) {
            case 'none': {
              break;
            }
            case 'some': {
              ensureEmptyBlock(expr.dict.l, "anonymous functions", expr.dict._check.dict.value as TJ.Variant<A.Expr, 's-block'>);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict._check);
            }
          }
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.params).forEach(param => visit(visitor, param, exprThroughCheckLoc));
          args.forEach(arg => visit(visitor, arg, exprThroughCheckLoc));
          visit(visitor, expr.dict.ann, exprThroughCheckLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, exprThroughCheckLoc);
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
          wrapVisitCheck(visitor, expr.dict._check, checkLoc);
        },
        's-fun': (visitor, expr: TJ.Variant<A.Expr, 's-fun'>, parentBlockLoc) => {
          const exprThroughCheckLoc =  getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args, false);
          listToArray(expr.dict.params).forEach(p => visit(visitor, p, exprThroughCheckLoc));
          args.forEach(a => visit(visitor, a, exprThroughCheckLoc));
          visit(visitor, expr.dict.ann, exprThroughCheckLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, exprThroughCheckLoc);
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          wrapVisitCheck(visitor, expr.dict._check, checkLoc);
        },
        's-obj': (visitor, expr: TJ.Variant<A.Expr, 's-obj'>, parentBlockLoc) => {
          const fields = listToArray(expr.dict.fields);
          ensureUniqueFields(fields);
          checkUnderscoreName(fields, "a field name");
          fields.forEach(f => wrapVisitAllowSMethod(visitor, f, true, parentBlockLoc));
        },
        's-extend': (visitor, expr: TJ.Variant<A.Expr, 's-extend'>, parentBlockLoc) => {
          const fields = listToArray(expr.dict.fields);
          ensureUniqueFields(fields);
          checkUnderscoreName(fields, "a field name");
          fields.forEach(f => wrapVisitAllowSMethod(visitor, f, true, parentBlockLoc));
        },
        's-dot': (visitor, expr: TJ.Variant<A.Expr, 's-dot'>, parentBlockLoc) => {
          if (expr.dict.field === '_') {
            addError(C['underscore-as'].app(expr.dict.l, "a field name"));
          }
          wrapVisitAllowSMethod(visitor, expr.dict.obj, false, parentBlockLoc);
        },
        's-tuple-get': (visitor, expr: TJ.Variant<A.Expr, 's-tuple-get'>) => {
          const {l, tup, index, "index-loc": indexLoc} = expr.dict;
          if (!Number.isInteger(index) || index < 0 || index > 1000) {
            addError(C['tuple-get-bad-index'].app(l, tup, index, indexLoc));
          }
        },
        's-check': (visitor, expr: TJ.Variant<A.Expr, 's-check'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const body = expr.dict.body as TJ.Variant<A.Expr, 's-block'>;
          const optionBody: A.Option<TJ.Variant<A.Expr, 's-block'>> = {$name: 'some', dict: { value: body }};
          if (!expr.dict['keyword-check']) {
            wrapVisitCheck(visitor, optionBody, parentBlockLoc);
            wfExamplesBody(body);
          } else {
            wrapVisitCheck(visitor, optionBody, parentBlockLoc);
            wrapRejectStandalonesInCheck(optionBody);
          }
        },
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.block.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.block]);
          }
          wrapVisitAllowSMethod(visitor, expr.dict.test, false, parentBlockLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.block, false, parentBlockLoc);
        },
        's-if': (visitor, expr: TJ.Variant<A.Expr, 's-if'>, parentBlockLoc) => {
          const branches = listToArray(expr.dict.branches);
          if (branches.length === 1) {
            addError(C['single-branch-if'].app(expr));
          }
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict._else, false, parentBlockLoc);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict._else, false, parentBlockLoc);
        },
        's-cases': (visitor, expr: TJ.Variant<A.Expr, 's-cases'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          ensureUniqueCases(branches);
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          visit(visitor, expr.dict.typ, parentBlockLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.val, false, parentBlockLoc);
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
        },
        's-cases-else': (visitor, expr: TJ.Variant<A.Expr, 's-cases-else'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          ensureUniqueCases(branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          visit(visitor, expr.dict.typ, parentBlockLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.val, false, parentBlockLoc);
          branches.forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          wrapVisitAllowSMethod(visitor, expr.dict._else, false, parentBlockLoc);
        },
        's-for': (visitor, expr: TJ.Variant<A.Expr, 's-for'>, _parentBlockLoc) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          wrapVisitAllowSMethod(visitor, expr.dict.iterator, false, parentBlockLoc);
          listToArray(expr.dict.bindings).forEach(b => wrapVisitAllowSMethod(visitor, b, false, parentBlockLoc));
          visit(visitor, expr.dict.ann, parentBlockLoc);
          wrapVisitAllowSMethod(visitor, expr.dict.body, false, parentBlockLoc);
        },
        's-frac': (visitor, expr: TJ.Variant<A.Expr, 's-frac'>) => {
          if (expr.dict.den === 0) {
            addError(C['zero-fraction'].app(expr.dict.l, expr.dict.num))
          }
        },
        's-rfrac': (visitor, expr: TJ.Variant<A.Expr, 's-rfrac'>) => {
          if (expr.dict.den === 0) {
            addError(C['zero-fraction'].app(expr.dict.l, expr.dict.num))
          }
        },
        's-id': (visitor, expr: TJ.Variant<A.Expr, 's-id'>) => {
          const id = nameToSourceString(expr.dict.id);
          if (reservedNames.has(id)) {
            reservedName(expr.dict.l, id);
          }
        },
        's-provide': (visitor, expr: TJ.Variant<A.Provide, 's-provide'>) => {
          if (expr.dict.block.$name !== 's-obj') {
            addError(C['non-object-provide'].app(expr.dict.l));
          }
        },
        's-reactor': (visitor, expr: TJ.Variant<A.Expr, 's-reactor'>, parentBlockLoc) => {
          const fields = listToArray(expr.dict.fields);
          const methodFields = fields.filter(f => f.$name === 's-method-field');
          if (methodFields.length !== 0) {
            wfError(runtime.ffi.makeList([
              ED.text.app("A reactor cannot contain method fields ")
            ]),
            methodFields[0].dict.l);
            return;
          }
          const hasInit = fields.find(f => f.dict.name === 'init') !== undefined;
          if (!hasInit) {
            wfError(runtime.ffi.makeList([
              ED.text.app("A reactor must have a field named "),
              ED.code.app(ED.text.app("init")),
              ED.text.app(" for the initial value ")
            ]),
            expr.dict.l);
          }
          const fieldsDict = new Map<string, A.Srcloc>();
          fields.forEach(f => {
            if (!reactorFields.has(f.dict.name)) {
              const seq = ED['h-sequence-sep'].app(runtime.ffi.makeList(Array.from(reactorFields.keys()).map(k =>
                ED.code.app(ED.text.app(k))
              )),
              ", ",
              ", or ");
              wfError(runtime.ffi.makeList([
                ED.text.app("Valid options for reactors are "),
                seq,
                ED.text.app(", but found one named "),
                ED.code.app(ED.text.app(f.dict.name)),
                ED.text.app(" ")
              ]),
              expr.dict.l)
            }
            const dupFieldLoc = fieldsDict.get(f.dict.name);
            if (dupFieldLoc !== undefined) {
              wfError2("Duplicate option in reactor: " + f.dict.name, f.dict.l, dupFieldLoc);
            } else {
              fieldsDict.set(f.dict.name, f.dict.l);
            }
            wrapVisitAllowSMethod(visitor, f, false, parentBlockLoc);
          })
        },
        's-table': (visitor, expr: TJ.Variant<A.Expr, 's-table'>, parentBlockLoc) => {
          const headers = listToArray(expr.dict.headers);
          wfTableHeaders(expr.dict.l, headers);
          if (headers.length === 0) {
            return;
          }
          const rows = listToArray(expr.dict.rows);
          rows.forEach(row => {
            const elems = listToArray(row.dict.elems);
            if (elems.length === 0) {
              addError(C['table-empty-row'].app(row.dict.l));
            }
            if (elems.length !== 0 && elems.length !== headers.length) {
              const headerLoc = plus(headers[0].dict.l as TJ.Variant<A.Srcloc, 'srcloc'>,
                headers[headers.length - 1].dict.l as TJ.Variant<A.Srcloc, 'srcloc'>);
              addError(C['table-row-wrong-size'].app(headerLoc, expr.dict.headers, row))
            }
            elems.forEach(e => wrapVisitAllowSMethod(visitor, e, false, parentBlockLoc));
          });
        },
        's-table-extend': (visitor, expr: TJ.Variant<A.Expr, 's-table-extend'>, parentBlockLoc) => {
          const boundNames = new Set<string>();
          listToArray(expr.dict['column-binds'].dict.binds).forEach(b => {
            let bBind = b as TJ.Variant<A.Bind, 's-bind'>;
            boundNames.add(nameToName(bBind.dict.id));
          });
          listToArray(expr.dict.extensions).forEach(e => {
            switch (e.$name) {
              case 's-table-extend-field': {
                wrapVisitAllowSMethod(visitor, e.dict.value, false, parentBlockLoc);
                visit(visitor, e.dict.ann, parentBlockLoc);
                break;
              }
              case 's-table-extend-reducer': {
                if (!boundNames.has(nameToName(e.dict.col))) {
                  addError(C['table-reducer-bad-column'].app(e, expr.dict['column-binds'].dict.l));
                }
                wrapVisitAllowSMethod(visitor, e.dict.reducer, false, parentBlockLoc);
                visit(visitor, e.dict.ann, parentBlockLoc);
                break;
              }
              default: {
                throw new ExhaustiveSwitchError(e);
              }
            }
          });
        },
        's-load-table': (visitor, expr: TJ.Variant<A.Expr, 's-load-table'>, parentBlockLoc) => {
          const headers = listToArray(expr.dict.headers);
          const spec = listToArray(expr.dict.spec);
          wfTableHeaders(expr.dict.l, headers);
          if (spec.length === 0) {
            addError(C['load-table-no-body'].app(expr));
            return;
          }
          const boundNames = new Set<string>();
          headers.forEach(h => boundNames.add(h.dict.name));
          const headerLoc = headers.length === 0 ?
            upTo(expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>,
              spec[0].dict.l as TJ.Variant<A.Srcloc, 'srcloc'>) :
            plus(headers[0].dict.l as TJ.Variant<A.Srcloc, 'srcloc'>,
              headers[headers.length - 1].dict.l as TJ.Variant<A.Srcloc, 'srcloc'>);
          let numSrcs = 0;
          let accDict = new Map<string, A.LoadTableSpec>();
          spec.forEach(s => {
            switch (s.$name) {
              case 's-sanitize': {
                const namestr = nameToName(s.dict.name);
                if (!boundNames.has(namestr)) {
                  addError(C['table-sanitizer-bad-column'].app(s, headerLoc));
                }
                const dup = accDict.get(namestr);
                if (dup !== undefined) {
                  addError(C['load-table-duplicate-sanitizer'].app(dup, namestr, s));
                } else {
                  accDict.set(namestr, s);
                }
                break;
              }
              case 's-table-src': {
                numSrcs += 1;
                break;
              }
              default: {
                throw new ExhaustiveSwitchError(s);
              }
            }
          })
          if (numSrcs !== 1) {
            addError(C['load-table-bad-number-srcs'].app(expr, numSrcs));
          }
          spec.forEach(s => wrapVisitAllowSMethod(visitor, s, false, parentBlockLoc));
        },
        'a-name': (visitor, expr: TJ.Variant<A.Ann, 'a-name'>) => {
          if (A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['underscore-as-ann'].app(expr.dict.l));
          }
        }
      }

      const topLevelVisitor: TJ.Visitor<
        A.Program | A.Expr | A.TypeLetBind | A.Variant | A.Member | A.Bind | A.LetBind | A.Import | A.Provide | A.ProvideTypes | A.LetrecBind | A.IfBranch | A.IfPipeBranch | A.CasesBranch | A.ForBind | A.VariantMember | A.Ann | A.AField, 
        void,
        S.Srcloc
      > = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>) => {
          const body = expr.dict.block;
          if (body.$name === 's-block') {
            wfBlockStmts(visitor, listToArray(body.dict.stmts), true, body.dict.l);
          } else {
            visit(visitor, body, body.dict.l);
          }
          wrapVisitAllowSMethod(visitor, expr.dict._provide, false, body.dict.l);
          visit(visitor, expr.dict['provided-types'], body.dict.l);
          listToArray(expr.dict.imports).forEach(i => visit(visitor, i, body.dict.l));
        },
        's-type': (visitor, expr: TJ.Variant<A.Expr, 's-type'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr.dict.ann, parentBlockLoc);
        },
        's-newtype': (visitor, expr: TJ.Variant<A.Expr, 's-newtype'>) => {
          return;
        },
        's-type-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-type-let-expr'>, parentBlockLoc) => {
          if (!expr.dict.blocky && A['is-s-block'].app(expr.dict.body)) {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.binds).forEach(b => visit(visitor, b, parentBlockLoc));
          wrapVisitAllowSMethod(wellFormedVisitor, expr.dict.body, false, parentBlockLoc);
        },
        's-type-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-type-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr.dict.ann, parentBlockLoc);
        },
        's-newtype-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-newtype-bind'>) => {
          return;
        },
        's-variant': (visitor, expr: TJ.Variant<A.Variant, 's-variant'>, parentBlockLoc) => {
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
          ensureUniqueBindings(ids, false);
          const underscores = members
            .filter(member => A['is-s-bind'].app(member.dict.bind) && A['is-s-underscore'].app(member.dict.bind.dict.id));
          if (underscores.length !== 0) {
            addError(C['underscore-as'].app(underscores[0].dict.l, "a data variant name"));
          }
          checkUnderscoreName(withMembers, "a field name");
          members.forEach(m => visit(wellFormedVisitor, m, parentBlockLoc));
          withMembers.forEach(wm => wrapVisitAllowSMethod(wellFormedVisitor, wm, true, parentBlockLoc));
        },
        's-singleton-variant': (visitor, expr: TJ.Variant<A.Variant, 's-singleton-variant'>, parentBlockLoc) => {
          const withMembers = listToArray(expr.dict['with-members']);
          ensureUniqueBindings(fieldsToBinds(withMembers), false);
          withMembers.forEach(wm => wrapVisitAllowSMethod(wellFormedVisitor, wm, true, parentBlockLoc));
        },
        's-data': (visitor, expr: TJ.Variant<A.Expr, 's-data'>, parentBlockLoc) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const variants = listToArray(expr.dict.variants);
          const shares = listToArray(expr.dict['shared-members']);
          ensureUniqueVariantIds(variants, expr.dict.name, expr.dict.l);
          checkUnderscoreName(variants, "a data variant name");
          checkUnderscoreName(shares, "a shared field name");
          checkUnderscoreName([{dict: {l: expr.dict.l, name: expr.dict.name}}], "a datatype name");
          let theCurShared = curShared;
          curShared = fieldsToBinds(shares);
          listToArray(expr.dict.params).forEach(p => visit(wellFormedVisitor, p, exprThroughCheckLoc));
          listToArray(expr.dict.mixins).forEach(m => wrapVisitAllowSMethod(wellFormedVisitor, m, false, exprThroughCheckLoc));
          variants.forEach(v => visit(visitor, v, exprThroughCheckLoc));
          shares.forEach(s => wrapVisitAllowSMethod(wellFormedVisitor, s, true, exprThroughCheckLoc));
          curShared = theCurShared;
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          wrapVisitCheck(wellFormedVisitor, expr.dict._check, checkLoc);
        },
        's-data-expr': (visitor, expr: TJ.Variant<A.Expr, 's-data-expr'>, parentBlockLoc) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const variants = listToArray(expr.dict.variants);
          const shares = listToArray(expr.dict['shared-members'])
          ensureUniqueVariantIds(variants, expr.dict.name, expr.dict.l);
          checkUnderscoreName(variants, "a data variant name");
          checkUnderscoreName(shares, "a shared field name");
          checkUnderscoreName([{dict: {l: expr.dict.l, name: expr.dict.name}}], "a datatype name");
          let theCurShared = curShared;
          curShared = fieldsToBinds(shares);
          listToArray(expr.dict.params).forEach(p => visit(wellFormedVisitor, p, exprThroughCheckLoc));
          listToArray(expr.dict.mixins).forEach(m => visit(wellFormedVisitor, m, exprThroughCheckLoc));
          variants.forEach(v => visit(wellFormedVisitor, v, exprThroughCheckLoc));
          shares.forEach(s => wrapVisitAllowSMethod(wellFormedVisitor, s, true, exprThroughCheckLoc));
          curShared = theCurShared;
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          wrapVisitCheck(wellFormedVisitor, expr.dict._check, checkLoc);
        },
        's-import': (visitor, expr: TJ.Variant<A.Import, 's-import'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-include': (visitor, expr: TJ.Variant<A.Import, 's-include'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-import-types': (visitor, expr: TJ.Variant<A.Import, 's-import-types'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-import-fields': (visitor, expr: TJ.Variant<A.Import, 's-import-fields'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-provide': (visitor, expr: TJ.Variant<A.Provide, 's-provide'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-provide-types': (visitor, expr: TJ.Variant<A.ProvideTypes, 's-provide-types'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-bind': (visitor, expr: TJ.Variant<A.Bind, 's-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-var-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-var-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-let-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-let-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-template': (visitor, expr: TJ.Variant<A.Expr, 's-template'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-let-expr'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-letrec-bind': (visitor, expr: TJ.Variant<A.LetrecBind, 's-letrec-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-letrec'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-hint-exp': (visitor, expr: TJ.Variant<A.Expr, 's-hint-exp'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-instantiate': (visitor, expr: TJ.Variant<A.Expr, 's-instantiate'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-fun': (visitor, expr: TJ.Variant<A.Expr, 's-fun'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-var': (visitor, expr: TJ.Variant<A.Expr, 's-var'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-rec': (visitor, expr: TJ.Variant<A.Expr, 's-rec'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-let': (visitor, expr: TJ.Variant<A.Expr, 's-let'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-ref': (visitor, expr: TJ.Variant<A.Expr, 's-ref'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-contract': (visitor, expr: TJ.Variant<A.Expr, 's-contract'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-assign': (visitor, expr: TJ.Variant<A.Expr, 's-assign'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if-branch': (visitor, expr: TJ.Variant<A.IfBranch, 's-if-branch'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if-pipe-branch': (visitor, expr: TJ.Variant<A.IfPipeBranch, 's-if-pipe-branch'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if': (visitor, expr: TJ.Variant<A.Expr, 's-if'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-cases-branch'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-singleton-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-singleton-cases-branch'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-cases': (visitor, expr: TJ.Variant<A.Expr, 's-cases'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-cases-else': (visitor, expr: TJ.Variant<A.Expr, 's-cases-else'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-paren': (visitor, expr: TJ.Variant<A.Expr, 's-paren'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-method': (visitor, expr: TJ.Variant<A.Expr, 's-method'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-extend': (visitor, expr: TJ.Variant<A.Expr, 's-extend'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-update': (visitor, expr: TJ.Variant<A.Expr, 's-update'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-tuple-get': (visitor, expr: TJ.Variant<A.Expr, 's-tuple-get'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-obj': (visitor, expr: TJ.Variant<A.Expr, 's-obj'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-array': (visitor, expr: TJ.Variant<A.Expr, 's-array'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-construct': (visitor, expr: TJ.Variant<A.Expr, 's-construct'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-app': (visitor, expr: TJ.Variant<A.Expr, 's-app'>, parentBlockLoc) => {
          if (expr.dict._fun.$name === 's-dot' && expr.dict._fun.dict.obj.$name === 's-id' && nameToName(expr.dict._fun.dict.obj.dict.id) === 'builtins' && expr.dict._fun.dict.field === 'trace-value') {
            wrapVisitAllowSMethod(visitor, expr.dict._fun, false, parentBlockLoc);
            listToArray(expr.dict.args).forEach(a => wrapVisitAllowSMethod(visitor, a, false, parentBlockLoc))
          } else {
            visit(wellFormedVisitor, expr, parentBlockLoc);
          }
        },
        's-prim-app': (visitor, expr: TJ.Variant<A.Expr, 's-prim-app'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-frac': (visitor, expr: TJ.Variant<A.Expr, 's-frac'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-reactor': (visitor, expr: TJ.Variant<A.Expr, 's-reactor'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-rfrac': (visitor, expr: TJ.Variant<A.Expr, 's-rfrac'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-id': (visitor, expr: TJ.Variant<A.Expr, 's-id'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-id-var': (visitor, expr: TJ.Variant<A.Expr, 's-id-var'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-id-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-id-letrec'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-dot': (visitor, expr: TJ.Variant<A.Expr, 's-dot'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-get-bang': (visitor, expr: TJ.Variant<A.Expr, 's-get-bang'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-bracket': (visitor, expr: TJ.Variant<A.Expr, 's-bracket'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-for': (visitor, expr: TJ.Variant<A.Expr, 's-for'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-check': (visitor, expr: TJ.Variant<A.Expr, 's-check'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-data-field': (visitor, expr: TJ.Variant<A.Member, 's-data-field'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-mutable-field': (visitor, expr: TJ.Variant<A.Member, 's-mutable-field'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-for-bind': (visitor, expr: TJ.Variant<A.ForBind, 's-for-bind'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-variant-member': (visitor, expr: TJ.Variant<A.VariantMember, 's-variant-member'>, parentBlockLoc) => {
          switch (expr.dict.bind.$name) {
            case 's-bind': {
              visit(wellFormedVisitor, expr, parentBlockLoc);
              break;
            }
            case 's-tuple-bind': {
              wfError(runtime.ffi.makeList([
                ED.text.app("Tuple binding not allowed as variant member")
              ]),
              expr.dict.bind.dict.l);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict.bind);
            }
          }
        },
        's-table': (visitor, expr: TJ.Variant<A.Expr, 's-table'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-load-table': (visitor, expr: TJ.Variant<A.Expr, 's-load-table'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        's-table-extend': (visitor, expr: TJ.Variant<A.Expr, 's-table-extend'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-arrow': (visitor, expr: TJ.Variant<A.Ann, 'a-arrow'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-arrow-argnames': (visitor, expr: TJ.Variant<A.Ann, 'a-arrow-argnames'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-method': (visitor, expr: TJ.Variant<A.Ann, 'a-method'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-record': (visitor, expr: TJ.Variant<A.Ann, 'a-record'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-app': (visitor, expr: TJ.Variant<A.Ann, 'a-app'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-pred': (visitor, expr: TJ.Variant<A.Ann, 'a-pred'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-dot': (visitor, expr: TJ.Variant<A.Ann, 'a-dot'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
        'a-field': (visitor, expr: TJ.Variant<A.AField, 'a-field'>, parentBlockLoc) => {
          visit(wellFormedVisitor, expr, parentBlockLoc);
        },
      };
      visit(topLevelVisitor, ast, ast.dict.l);
      if (errors.length === 0) {
        return C.ok.app(ast);
      } else {
        return C.err.app(runtime.ffi.makeList(errors));
      }
    }
    const exports: Exports['dict']['values']['dict'] = {
      "check-well-formed": runtime.makeFunction(checkWellFormed)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})