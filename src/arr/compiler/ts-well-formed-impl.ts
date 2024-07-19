import type * as A from './ts-ast';
import type * as C from './ts-compile-structs';
import type * as ED from './error-display';
import * as TJ from './ts-codegen-helpers';
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

type WFContext = {
  parentBlockLoc: A.Srcloc,
  inCheckBlock: boolean,
  allowSMethod: boolean,
  curShared: A.Bind[],
  paramCurrentEverywhere: boolean,
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

      function sMethodHelper(visitor, expr: TJ.Variant<A.Member, 's-method-field'>, wfContext: WFContext) {
        const args = listToArray(expr.dict.args);
        if (args.length === 0) {
          addError(C['no-arguments'].app(expr));
        }
        ensureUniqueBindings(args, false);
        if (expr.dict._check.$name === 'some') {
          ensureEmptyBlock(expr.dict.l, "methods", expr.dict._check.dict.value as TJ.Variant<A.Expr, 's-block'>, wfContext.paramCurrentEverywhere);
        }
        if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
          wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
        }
        args.forEach(arg => visit(visitor, arg, wfContext));
        visit(visitor, expr.dict.ann, wfContext);
        visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false});
        const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
        wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
        visit(visitor, expr.dict._check, {...wfContext, allowSMethod: false, inCheckBlock: true, parentBlockLoc: checkLoc});
      }

      function bindHelper(visitor, l: A.Srcloc, bind: A.Bind, val: A.Expr, varOrRec: 'var' | 'rec', wfContext: WFContext) {
        let pointless: 'pointless-var' | 'pointless-rec';
        let str: string;
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
            visit(visitor, bind, {...wfContext, allowSMethod: false});
            visit(visitor, val, {...wfContext, allowSMethod: false});
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

      function ensureEmptyBlock(loc: A.Srcloc, typ: string, block: TJ.Variant<A.Expr, 's-block'>, currentWhereEverywhere: boolean) {
        const stmts = listToArray(block.dict.stmts);
        if (!currentWhereEverywhere && stmts.length !== 0) {
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
                addError(C['duplicate-id'].app(nameToSourceString(id), l, ad.get(nameToName(id))!));
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

      function reachableOps(visitor, l: A.Srcloc, opL: A.Srcloc, op: string, expr: A.Expr, wfContext: WFContext) {
        switch (expr.$name) {
          case 's-op': {
            const { 'l': l2, 'op-l': opL2, 'op': op2, 'left': left2, 'right': right2 } = expr.dict;
            if (op === op2) {
              reachableOps(visitor, l, opL, op, left2, wfContext);
              reachableOps(visitor, l, opL, op, right2, wfContext);
            } else {
              addError(C['mixed-binops'].app(l, opname(op), opL, opname(op2), opL2));
            }
            break;
          }
          default: {
            visit(visitor, expr, {...wfContext, allowSMethod: false});
            break;
          }
        }
      }

      function rejectStandaloneExprs(stmts: A.Expr[], ignoreLast: boolean, inCheckBlock: boolean): void {
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
        switch (target.$name) {
          case 'none': {
            break;
          }
          case 'some': {
            const stmts = listToArray(target.dict.value.dict.stmts);
            if (stmts.length !== 0) {
              rejectStandaloneExprs(stmts, true, true);
            }
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(target);
          }
        }
      }

      function wfBlockStmts(visitor, stmts: A.Expr[], topLevel: boolean, wfContext: WFContext): void {
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
        const bindStmts: A.Bind[] = stmts.map(mapStmts).filter(stmt => stmt) as A.Bind[];
        ensureUniqueBindings(bindStmts, true);
        ensureDistinctLines(A['dummy-loc'], false, stmts);
        if (!wfContext.inCheckBlock && !topLevel) {
          rejectStandaloneExprs(stmts, true, wfContext.inCheckBlock);
        }
        const contextStmt = {...wfContext, allowSMethod: false};
        stmts.forEach(stmt => visit(visitor, stmt, contextStmt));
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
        A.Program | A.Use | A.ImportType | A.Ann | A.Expr | A.Member | A.LetBind | A.Bind | A.Import | A.Provide | A.ProvideTypes | A.LetrecBind | A.CasesBranch | A.IfBranch | A.IfPipeBranch | A.ForBind | A.VariantMember | A.AField, 
        void,
        WFContext
      > = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>) => {
          throw new InternalCompilerError("Impossible");
        },
        's-use': (visitor, expr: TJ.Variant<A.Use, 's-use'>) => {
          if (!(nameToName(expr.dict.n) === "context")) {
            const error = [
              ED.text.app("The only supported type of "),
              ED.code.app(ED.text.app("use")),
              ED.text.app(" is "),
              ED.code.app(ED.text.app("context")),
              ED.text.app(", but this program used "),
              ED.code.app(ED.text.app(nameToName(expr.dict.n)))
            ];
            addError(C['wf-error'].app(runtime.ffi.makeList(error), expr.dict.l))
          }
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
        's-data': (visitor, expr: TJ.Variant<A.Expr, 's-data'>, wfContext) => {
          addError(C['non-toplevel'].app("data declaration", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-data-expr': (visitor, expr: TJ.Variant<A.Expr, 's-data-expr'>, wfContext) => {
          addError(C['non-toplevel'].app("data declaration", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-type': (visitor, expr: TJ.Variant<A.Expr, 's-type'>, wfContext) => {
          addError(C['non-toplevel'].app("type alias", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-newtype': (visitor, expr: TJ.Variant<A.Expr, 's-newtype'>, wfContext) => {
          addError(C['non-toplevel'].app("newtype", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-let-expr'>, wfContext) => {
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          const contextBind = {...wfContext, allowSMethod: false, parentBlockLoc: expr.dict.l};
          listToArray(expr.dict.binds).forEach(b => visit(visitor, b, contextBind));
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc: expr.dict.l});
        },
        's-contract': (visitor, expr: TJ.Variant<A.Expr, 's-contract'>, wfContext) => {
          addError(C['non-toplevel'].app("contract declaration", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-letrec-bind': (visitor, expr: TJ.Variant<A.LetrecBind, 's-letrec-bind'>, wfContext) => {
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
          visit(visitor, expr.dict.b, {...wfContext, parentBlockLoc});
          visit(visitor, expr.dict.value, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-letrec'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          const contextBind = {...wfContext, allowSMethod: false, parentBlockLoc};
          listToArray(expr.dict.binds).forEach(b => visit(visitor, b, contextBind));
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-type-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-type-let-expr'>, wfContext) => {
          addError(C['non-toplevel'].app("type alias", expr.dict.l, wfContext.parentBlockLoc));
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>, wfContext) => {
          const { l, 'op-l': opL, op, left, right } = expr.dict;
          reachableOps(visitor, l, opL, op, left, wfContext);
          reachableOps(visitor, l, opL, op, right, wfContext);
        },
        's-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-cases-branch'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          if (expr.dict.name === '_') {
            addError(C['underscore-as-pattern'].app(expr.dict['pat-loc']));
          }
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args.map(a => a.dict.bind), false);
          const contextArg = {...wfContext, parentBlockLoc};
          args.forEach(a => visit(visitor, a, contextArg));
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-singleton-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-singleton-cases-branch'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          if (expr.dict.name === '_') {
            addError(C['underscore-as-pattern'].app(expr.dict['pat-loc']));
          }
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-var': (visitor, expr: TJ.Variant<A.Expr, 's-var'>, wfContext) => {
          bindHelper(visitor, expr.dict.l, expr.dict.name, expr.dict.value, 'var', wfContext);
        },
        's-rec': (visitor, expr: TJ.Variant<A.Expr, 's-rec'>, wfContext) => {
          bindHelper(visitor, expr.dict.l, expr.dict.name, expr.dict.value, 'rec', wfContext);
        },
        's-var-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-var-bind'>, wfContext) => {
          bindHelper(visitor, expr.dict.l, expr.dict.b, expr.dict.value, 'var', wfContext);
        },
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>, wfContext) => {
          const stmts = listToArray(expr.dict.stmts);
          if (stmts.length === 0) {
            addError(C['wf-empty-block'].app(wfContext.parentBlockLoc));
          } else {
            wfLastStmt(wfContext.parentBlockLoc, stmts[stmts.length - 1]);
            wfBlockStmts(visitor, stmts, false, wfContext);
          }
        },
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-tuple-bind': (visitor, expr: TJ.Variant<A.Bind, 's-tuple-bind'>) => {
          return;
        },
        's-bind': (visitor, expr: TJ.Variant<A.Bind, 's-bind'>, wfContext) => {
          const nameStr = nameToSourceString(expr.dict.id);
          if (reservedNames.has(nameStr)) {
            reservedName(expr.dict.l, nameStr);
          }
          if (expr.dict.shadows && A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['pointless-shadow'].app(expr.dict.l));
          }
          visit(visitor, expr.dict.id, {...wfContext, allowSMethod: false});
          visit(visitor, expr.dict.ann, wfContext);
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>, wfContext) => {
          if (!wfContext.inCheckBlock) {
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
          visit(visitor, expr.dict.left, {...wfContext, allowSMethod: false})
          visit(visitor, expr.dict.right, wfContext);
          visit(visitor, expr.dict.cause, wfContext);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>, wfContext) => {
          const parentBlockLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          sMethodHelper(visitor, expr, {...wfContext, parentBlockLoc});
        },
        's-data-field': (visitor, expr: TJ.Variant<A.Member, 's-data-field'>, wfContext) => {
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          visit(visitor, expr.dict.value, wfContext);
        },
        's-mutable-field': (visitor, expr: TJ.Variant<A.Member, 's-mutable-field'>, wfContext) => {
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          visit(visitor, expr.dict.value, wfContext);
          visit(visitor, expr.dict.ann, wfContext);
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>, wfContext) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args, false);
          switch (expr.dict._check.$name) {
            case 'none': {
              break;
            }
            case 'some': {
              ensureEmptyBlock(expr.dict.l, "anonymous functions", expr.dict._check.dict.value as TJ.Variant<A.Expr, 's-block'>, wfContext.paramCurrentEverywhere);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict._check);
            }
          }
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          const contextHeader = {...wfContext, parentBlockLoc: exprThroughCheckLoc};
          listToArray(expr.dict.params).forEach(param => visit(visitor, param, contextHeader));
          args.forEach(arg => visit(visitor, arg, contextHeader));
          visit(visitor, expr.dict.ann, contextHeader);
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc: exprThroughCheckLoc});
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
          visit(visitor, expr.dict._check, {...wfContext, inCheckBlock: true, allowSMethod: false, parentBlockLoc: checkLoc});
        },
        's-fun': (visitor, expr: TJ.Variant<A.Expr, 's-fun'>, wfContext) => {
          const exprThroughCheckLoc =  getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          const args = listToArray(expr.dict.args);
          ensureUniqueBindings(args, false);
          const contextHeader = {...wfContext, parentBlockLoc: exprThroughCheckLoc};
          listToArray(expr.dict.params).forEach(p => visit(visitor, p, contextHeader));
          args.forEach(a => visit(visitor, a, contextHeader));
          visit(visitor, expr.dict.ann, contextHeader);
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc: exprThroughCheckLoc});
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          visit(visitor, expr.dict._check, {...wfContext, inCheckBlock: true, allowSMethod: false, parentBlockLoc: checkLoc});
        },
        's-obj': (visitor, expr: TJ.Variant<A.Expr, 's-obj'>, wfContext) => {
          const fields = listToArray(expr.dict.fields);
          ensureUniqueFields(fields);
          checkUnderscoreName(fields, "a field name");
          const fieldContext = {...wfContext, allowSMethod: true};
          fields.forEach(f => visit(visitor, f, fieldContext));
        },
        's-extend': (visitor, expr: TJ.Variant<A.Expr, 's-extend'>, wfContext) => {
          const fields = listToArray(expr.dict.fields);
          ensureUniqueFields(fields);
          checkUnderscoreName(fields, "a field name");
          const fieldContext = {...wfContext, allowSMethod: true};
          fields.forEach(f => visit(visitor, f, fieldContext));
        },
        's-dot': (visitor, expr: TJ.Variant<A.Expr, 's-dot'>, wfContext) => {
          if (expr.dict.field === '_') {
            addError(C['underscore-as'].app(expr.dict.l, "a field name"));
          }
          visit(visitor, expr.dict.obj, {...wfContext, allowSMethod: false});
        },
        's-tuple-get': (visitor, expr: TJ.Variant<A.Expr, 's-tuple-get'>) => {
          const {l, tup, index, "index-loc": indexLoc} = expr.dict;
          if (!Number.isInteger(index) || index < 0 || index > 1000) {
            addError(C['tuple-get-bad-index'].app(l, tup, index, indexLoc));
          }
        },
        's-check': (visitor, expr: TJ.Variant<A.Expr, 's-check'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const body = expr.dict.body as TJ.Variant<A.Expr, 's-block'>;
          const optionBody: A.Option<TJ.Variant<A.Expr, 's-block'>> = {$name: 'some', dict: { value: body }};
          if (!expr.dict['keyword-check']) {
            visit(visitor, optionBody, {...wfContext, inCheckBlock: true, allowSMethod: false, parentBlockLoc});
            wfExamplesBody(body);
          } else {
            visit(visitor, optionBody, {...wfContext, inCheckBlock: true, allowSMethod: false, parentBlockLoc});
            wrapRejectStandalonesInCheck(optionBody);
          }
        },
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.block.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.block]);
          }
          visit(visitor, expr.dict.test, {...wfContext, allowSMethod: false, parentBlockLoc});
          visit(visitor, expr.dict.block, {...wfContext, allowSMethod: false, parentBlockLoc});
        },
        's-if': (visitor, expr: TJ.Variant<A.Expr, 's-if'>, wfContext) => {
          const branches = listToArray(expr.dict.branches);
          if (branches.length === 1) {
            addError(C['single-branch-if'].app(expr));
          }
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          const branchContext = {...wfContext, allowSMethod: false};
          branches.forEach(b => visit(visitor, b, branchContext));
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          const branchContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          branches.forEach(b => visit(visitor, b, branchContext));
          visit(visitor, expr.dict._else, branchContext);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          const branchContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          branches.forEach(b => visit(visitor, b, branchContext));
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          const branchContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          branches.forEach(b => visit(visitor, b, branchContext));
          visit(visitor, expr.dict._else, branchContext);
        },
        's-cases': (visitor, expr: TJ.Variant<A.Expr, 's-cases'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          ensureUniqueCases(branches);
          if (!expr.dict.blocky) {
            wfBlockyBlocks(expr.dict.l, branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>));
          }
          visit(visitor, expr.dict.typ, {...wfContext, parentBlockLoc});
          visit(visitor, expr.dict.val, {...wfContext, allowSMethod: false, parentBlockLoc});
          const branchContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          branches.forEach(b => visit(visitor, b, branchContext));
        },
        's-cases-else': (visitor, expr: TJ.Variant<A.Expr, 's-cases-else'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          const branches = listToArray(expr.dict.branches);
          ensureUniqueCases(branches);
          if (!expr.dict.blocky && expr.dict._else.$name === 's-block') {
            const branchesBody = branches.map(b => b.dict.body as TJ.Variant<A.Expr, 's-block'>);
            branchesBody.unshift(expr.dict._else);
            wfBlockyBlocks(expr.dict.l, branchesBody);
          }
          visit(visitor, expr.dict.typ, {...wfContext, parentBlockLoc});
          visit(visitor, expr.dict.val, {...wfContext, allowSMethod: false, parentBlockLoc});
          const branchContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          branches.forEach(b => visit(visitor, b, branchContext));
          visit(visitor, expr.dict._else, branchContext);
        },
        's-for': (visitor, expr: TJ.Variant<A.Expr, 's-for'>, wfContext) => {
          const parentBlockLoc = expr.dict.l;
          if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          visit(visitor, expr.dict.iterator, {...wfContext, allowSMethod: false, parentBlockLoc});
          const bindContext = {...wfContext, allowSMethod: false, parentBlockLoc};
          listToArray(expr.dict.bindings).forEach(b => visit(visitor, b, bindContext));
          visit(visitor, expr.dict.ann, {...wfContext, parentBlockLoc});
          visit(visitor, expr.dict.body, {...wfContext, allowSMethod: false, parentBlockLoc});
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
        's-reactor': (visitor, expr: TJ.Variant<A.Expr, 's-reactor'>, wfContext) => {
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
            visit(visitor, f, {...wfContext, allowSMethod: false});
          })
        },
        's-table': (visitor, expr: TJ.Variant<A.Expr, 's-table'>, wfContext) => {
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
            const elemContext = {...wfContext, allowSMethod: false};
            elems.forEach(e => visit(visitor, e, elemContext));
          });
        },
        's-table-extend': (visitor, expr: TJ.Variant<A.Expr, 's-table-extend'>, wfContext) => {
          const boundNames = new Set<string>();
          listToArray(expr.dict['column-binds'].dict.binds).forEach(b => {
            let bBind = b as TJ.Variant<A.Bind, 's-bind'>;
            boundNames.add(nameToName(bBind.dict.id));
          });
          const eContext = {...wfContext, allowSMethod: false};
          listToArray(expr.dict.extensions).forEach(e => {
            switch (e.$name) {
              case 's-table-extend-field': {
                visit(visitor, e.dict.value, eContext);
                visit(visitor, e.dict.ann, wfContext);
                break;
              }
              case 's-table-extend-reducer': {
                if (!boundNames.has(nameToName(e.dict.col))) {
                  addError(C['table-reducer-bad-column'].app(e, expr.dict['column-binds'].dict.l));
                }
                visit(visitor, e.dict.reducer, eContext);
                visit(visitor, e.dict.ann, wfContext);
                break;
              }
              default: {
                throw new ExhaustiveSwitchError(e);
              }
            }
          });
        },
        's-load-table': (visitor, expr: TJ.Variant<A.Expr, 's-load-table'>, wfContext) => {
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
          const specContext = {...wfContext, allowSMethod: false};
          spec.forEach(s => visit(visitor, s, specContext));
        },
        'a-name': (visitor, expr: TJ.Variant<A.Ann, 'a-name'>) => {
          if (A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['underscore-as-ann'].app(expr.dict.l));
          }
        }
      }

      const topLevelVisitor: TJ.Visitor<
        A.Program | A.Use | A.Expr | A.TypeLetBind | A.Variant | A.Member | A.Bind | A.LetBind | A.Import | A.Provide | A.ProvideTypes | A.LetrecBind | A.IfBranch | A.IfPipeBranch | A.CasesBranch | A.ForBind | A.VariantMember | A.Ann | A.AField, 
        void,
        WFContext
      > = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>, wfContext: WFContext) => {
          const body = expr.dict.block;
          if (body.$name === 's-block') {
            wfBlockStmts(visitor, listToArray(body.dict.stmts), true, {...wfContext, parentBlockLoc: body.dict.l});
          } else {
            visit(visitor, body, {...wfContext, parentBlockLoc: body.dict.l});
          }
          visit(visitor, expr.dict._provide, {...wfContext, allowSMethod: false, parentBlockLoc: body.dict.l});
          visit(visitor, expr.dict['provided-types'], {...wfContext, parentBlockLoc: body.dict.l});
          const importContext = {...wfContext, parentBlockLoc: body.dict.l};
          listToArray(expr.dict.imports).forEach(i => visit(visitor, i, importContext));
        },
        's-type': (visitor, expr: TJ.Variant<A.Expr, 's-type'>, wfContext: WFContext) => {
          visit(wellFormedVisitor, expr.dict.ann, wfContext);
        },
        's-newtype': (visitor, expr: TJ.Variant<A.Expr, 's-newtype'>) => {
          return;
        },
        's-type-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-type-let-expr'>, wfContext) => {
          if (!expr.dict.blocky && A['is-s-block'].app(expr.dict.body)) {
            wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
          }
          listToArray(expr.dict.binds).forEach(b => visit(visitor, b, wfContext));
          visit(wellFormedVisitor, expr.dict.body, {...wfContext, allowSMethod: false});
        },
        's-type-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-type-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr.dict.ann, wfContext);
        },
        's-newtype-bind': (visitor, expr: TJ.Variant<A.TypeLetBind, 's-newtype-bind'>) => {
          return;
        },
        's-variant': (visitor, expr: TJ.Variant<A.Variant, 's-variant'>, wfContext) => {
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
          members.forEach(m => visit(wellFormedVisitor, m, wfContext));
          const wmContext = {...wfContext, allowSMethod: true};
          withMembers.forEach(wm => visit(wellFormedVisitor, wm, wmContext));
        },
        's-singleton-variant': (visitor, expr: TJ.Variant<A.Variant, 's-singleton-variant'>, wfContext) => {
          const withMembers = listToArray(expr.dict['with-members']);
          ensureUniqueBindings(fieldsToBinds(withMembers), false);
          const wmContext = {...wfContext, allowSMethod: true};
          withMembers.forEach(wm => visit(wellFormedVisitor, wm, wmContext));
        },
        's-data': (visitor, expr: TJ.Variant<A.Expr, 's-data'>, wfContext) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const variants = listToArray(expr.dict.variants);
          const shares = listToArray(expr.dict['shared-members']);
          ensureUniqueVariantIds(variants, expr.dict.name, expr.dict.l);
          checkUnderscoreName(variants, "a data variant name");
          checkUnderscoreName(shares, "a shared field name");
          checkUnderscoreName([{dict: {l: expr.dict.l, name: expr.dict.name}}], "a datatype name");
          const binds = fieldsToBinds(shares);
          const paramContext = {...wfContext, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          listToArray(expr.dict.params).forEach(p => visit(wellFormedVisitor, p, paramContext));
          const mixinContext = {...wfContext, allowSMethod: false, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          listToArray(expr.dict.mixins).forEach(m => visit(wellFormedVisitor, m, mixinContext));
          const variantContext = {...wfContext, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          variants.forEach(v => visit(visitor, v, variantContext));
          const shareContext = {...wfContext, allowSMethod: true, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          shares.forEach(s => visit(wellFormedVisitor, s, shareContext));
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          visit(wellFormedVisitor, expr.dict._check, {...wfContext, allowSMethod: false, inCheckBlock: true, parentBlockLoc: checkLoc});
        },
        's-data-expr': (visitor, expr: TJ.Variant<A.Expr, 's-data-expr'>, wfContext) => {
          const exprThroughCheckLoc = getLocWithCheckBlock(expr.dict.l, expr.dict['_check-loc']);
          const variants = listToArray(expr.dict.variants);
          const shares = listToArray(expr.dict['shared-members'])
          ensureUniqueVariantIds(variants, expr.dict.name, expr.dict.l);
          checkUnderscoreName(variants, "a data variant name");
          checkUnderscoreName(shares, "a shared field name");
          checkUnderscoreName([{dict: {l: expr.dict.l, name: expr.dict.name}}], "a datatype name");
          const binds = fieldsToBinds(shares);
          const paramMixinContext = {...wfContext, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          listToArray(expr.dict.params).forEach(p => visit(wellFormedVisitor, p, paramMixinContext));
          listToArray(expr.dict.mixins).forEach(m => visit(wellFormedVisitor, m, paramMixinContext));
          const variantContext = {...wfContext, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          variants.forEach(v => visit(wellFormedVisitor, v, variantContext));
          const shareContext = {...wfContext, allowSMethod: true, parentBlockLoc: exprThroughCheckLoc, curShared: binds};
          shares.forEach(s => visit(wellFormedVisitor, s, shareContext));
          const checkLoc = checkBlockLoc(expr.dict.l, expr.dict['_check-loc']) ?? expr.dict.l;
          wrapRejectStandalonesInCheck(expr.dict._check as Option<TJ.Variant<A.Expr, 's-block'>>);
          visit(wellFormedVisitor, expr.dict._check, {...wfContext, allowSMethod: false, inCheckBlock: true, parentBlockLoc: checkLoc});
        },
        's-use': (visitor, expr: TJ.Variant<A.Use, 's-use'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-import': (visitor, expr: TJ.Variant<A.Import, 's-import'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-include': (visitor, expr: TJ.Variant<A.Import, 's-include'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-import-types': (visitor, expr: TJ.Variant<A.Import, 's-import-types'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-import-fields': (visitor, expr: TJ.Variant<A.Import, 's-import-fields'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-provide': (visitor, expr: TJ.Variant<A.Provide, 's-provide'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-provide-types': (visitor, expr: TJ.Variant<A.ProvideTypes, 's-provide-types'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-bind': (visitor, expr: TJ.Variant<A.Bind, 's-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-var-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-var-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-let-bind': (visitor, expr: TJ.Variant<A.LetBind, 's-let-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-template': (visitor, expr: TJ.Variant<A.Expr, 's-template'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-let-expr': (visitor, expr: TJ.Variant<A.Expr, 's-let-expr'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-letrec-bind': (visitor, expr: TJ.Variant<A.LetrecBind, 's-letrec-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-letrec'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-instantiate': (visitor, expr: TJ.Variant<A.Expr, 's-instantiate'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-fun': (visitor, expr: TJ.Variant<A.Expr, 's-fun'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-var': (visitor, expr: TJ.Variant<A.Expr, 's-var'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-rec': (visitor, expr: TJ.Variant<A.Expr, 's-rec'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-let': (visitor, expr: TJ.Variant<A.Expr, 's-let'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-contract': (visitor, expr: TJ.Variant<A.Expr, 's-contract'>, wfContext) => {
          return;
        },
        's-assign': (visitor, expr: TJ.Variant<A.Expr, 's-assign'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if-branch': (visitor, expr: TJ.Variant<A.IfBranch, 's-if-branch'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if-pipe-branch': (visitor, expr: TJ.Variant<A.IfPipeBranch, 's-if-pipe-branch'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if': (visitor, expr: TJ.Variant<A.Expr, 's-if'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-cases-branch'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-singleton-cases-branch': (visitor, expr: TJ.Variant<A.CasesBranch, 's-singleton-cases-branch'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-cases': (visitor, expr: TJ.Variant<A.Expr, 's-cases'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-cases-else': (visitor, expr: TJ.Variant<A.Expr, 's-cases-else'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-paren': (visitor, expr: TJ.Variant<A.Expr, 's-paren'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-extend': (visitor, expr: TJ.Variant<A.Expr, 's-extend'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-update': (visitor, expr: TJ.Variant<A.Expr, 's-update'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-tuple-get': (visitor, expr: TJ.Variant<A.Expr, 's-tuple-get'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-obj': (visitor, expr: TJ.Variant<A.Expr, 's-obj'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-array': (visitor, expr: TJ.Variant<A.Expr, 's-array'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-construct': (visitor, expr: TJ.Variant<A.Expr, 's-construct'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-app': (visitor, expr: TJ.Variant<A.Expr, 's-app'>, wfContext) => {
          if (expr.dict._fun.$name === 's-dot' && expr.dict._fun.dict.obj.$name === 's-id' && nameToName(expr.dict._fun.dict.obj.dict.id) === 'builtins' && expr.dict._fun.dict.field === '$traceValue') {
            visit(visitor, expr.dict._fun, {...wfContext, allowSMethod: false});
            const argContext = {...wfContext, allowSMethod: false};
            listToArray(expr.dict.args).forEach(a => visit(visitor, a, argContext))
          } else {
            visit(wellFormedVisitor, expr, wfContext);
          }
        },
        's-prim-app': (visitor, expr: TJ.Variant<A.Expr, 's-prim-app'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-frac': (visitor, expr: TJ.Variant<A.Expr, 's-frac'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-reactor': (visitor, expr: TJ.Variant<A.Expr, 's-reactor'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-rfrac': (visitor, expr: TJ.Variant<A.Expr, 's-rfrac'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-id': (visitor, expr: TJ.Variant<A.Expr, 's-id'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-id-var': (visitor, expr: TJ.Variant<A.Expr, 's-id-var'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-id-letrec': (visitor, expr: TJ.Variant<A.Expr, 's-id-letrec'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-dot': (visitor, expr: TJ.Variant<A.Expr, 's-dot'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-get-bang': (visitor, expr: TJ.Variant<A.Expr, 's-get-bang'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-bracket': (visitor, expr: TJ.Variant<A.Expr, 's-bracket'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-for': (visitor, expr: TJ.Variant<A.Expr, 's-for'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-check': (visitor, expr: TJ.Variant<A.Expr, 's-check'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-data-field': (visitor, expr: TJ.Variant<A.Member, 's-data-field'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-mutable-field': (visitor, expr: TJ.Variant<A.Member, 's-mutable-field'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-for-bind': (visitor, expr: TJ.Variant<A.ForBind, 's-for-bind'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-variant-member': (visitor, expr: TJ.Variant<A.VariantMember, 's-variant-member'>, wfContext) => {
          switch (expr.dict.bind.$name) {
            case 's-bind': {
              visit(wellFormedVisitor, expr, wfContext);
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
        's-table': (visitor, expr: TJ.Variant<A.Expr, 's-table'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-load-table': (visitor, expr: TJ.Variant<A.Expr, 's-load-table'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        's-table-extend': (visitor, expr: TJ.Variant<A.Expr, 's-table-extend'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-arrow': (visitor, expr: TJ.Variant<A.Ann, 'a-arrow'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-arrow-argnames': (visitor, expr: TJ.Variant<A.Ann, 'a-arrow-argnames'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-method': (visitor, expr: TJ.Variant<A.Ann, 'a-method'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-record': (visitor, expr: TJ.Variant<A.Ann, 'a-record'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-app': (visitor, expr: TJ.Variant<A.Ann, 'a-app'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-pred': (visitor, expr: TJ.Variant<A.Ann, 'a-pred'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-dot': (visitor, expr: TJ.Variant<A.Ann, 'a-dot'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
        'a-field': (visitor, expr: TJ.Variant<A.AField, 'a-field'>, wfContext) => {
          visit(wellFormedVisitor, expr, wfContext);
        },
      };
      const initialWFContext: WFContext = {
        parentBlockLoc: ast.dict.l,
        inCheckBlock: false,
        allowSMethod: false,
        curShared: [],
        paramCurrentEverywhere: false,
      };
      visit(topLevelVisitor, ast, initialWFContext);
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