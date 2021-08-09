import type * as A from './ts-ast';
import type * as C from './ts-compile-structs';
import type * as ED from './error-display';
import type * as TJ from './ts-codegen-helpers';
import type * as S from './ts-srcloc';
import type { List, PFunction, Option } from './ts-impl-types';

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

    function checkWellFormed(ast: A.Program, options): C.CompileResult<A.Program> {
      const {
        visit,
        listToArray,
        nameToName,
        nameToSourceString,
        formatSrcloc,
        ExhaustiveSwitchError,
      } = tj;
      let errors: C.CompileError[] = [];
      let inCheckBlock = false;
      let allowSMethod = false;
      let curShared = [];
      let paramCurrentWhereEverywhere = false;
      let parentBlockLoc: A.Srcloc | null = null;

      logger = options.dict.log;
      // LOG(`In ts-well-formed impl for ${formatSrcloc(ast.dict.l, true)}\n`);

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

      function sMethodHelper(visitor, expr: TJ.Variant<A.Expr, 's-method'> | TJ.Variant<A.Member, 's-method-field'>) {
        const args = listToArray(expr.dict.args);
        if (args.length === 0) {
          // TODO(manas): instead of A['s-method'] this should just be
          // expr but no-arguments currently only accept A.Expr
          addError(C['no-arguments'].app(A['s-method'].app(
            expr.dict.l,
            expr.dict.name,
            expr.dict.params,
            expr.dict.args,
            expr.dict.ann,
            expr.dict.doc,
            expr.dict.body,
            expr.dict['_check-loc'],
            expr.dict._check,
            expr.dict.blocky
          )));
        }
        ensureUniqueIdsOrBindings(args, false);
        switch (expr.dict._check.$name) {
          case 'none': {
            break;
          }
          case 'some': {
            ensureEmptyBlock(expr.dict.l, "methods", expr.dict._check.dict.value as TJ.Variant<A.Expr, 's-block'>);
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(expr.dict._check);
          }
        }
        if (!expr.dict.blocky && expr.dict.body.$name === 's-block') {
          wfBlockyBlocks(expr.dict.l, [expr.dict.body]);
        }
        args.forEach(arg => visit(visitor, arg));
        visit(visitor, expr.dict.ann);
        wrapVisitAllowSMethod(visitor, expr.dict.body, false);
        switch (expr.dict['_check-loc'].$name) {
          case 'none': {
            break;
          }
          case 'some': {
            const l = expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
            const l2 = expr.dict['_check-loc'].dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
            parentBlockLoc = uptoEnd(l2, l);
            break;
          }
          default: {
            throw new ExhaustiveSwitchError(expr.dict['_check-loc']);
          }
        }
        wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
        wrapVisitCheck(visitor, expr.dict._check);
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

      function reservedName(loc: A.Srcloc, id: string) {
        addError(C['reserved-name'].app(loc, id));
      }

      function wrapVisitCheck(visitor, target: Option<A.Expr>) {
        let curInCheck = inCheckBlock;
        inCheckBlock = true;
        let curAllow = allowSMethod;
        allowSMethod = false;
        visit(visitor, target);
        inCheckBlock = curInCheck;
        allowSMethod = curAllow;
      }

      function wrapVisitAllowSMethod(visitor, target: A.Expr | A.Provide | A.Member, allow: boolean): void {
        let curAllow = allowSMethod;
        allowSMethod = allow;
        visit(visitor, target);
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

      // checkShadows should be true for ensureUniqueBindings and false for Ids
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

      function reachableOps(visitor, l: A.Srcloc, opL: A.Srcloc, op: string, expr: A.Expr) {
        switch (expr.$name) {
          case 's-op': {
            const { 'l': l2, 'op-l': opL2, 'op': op2, 'left': left2, 'right': right2 } = expr.dict;
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

      function wfBlockStmts(visitor, l: A.Srcloc, stmts: A.Expr[], topLevel: boolean): void {
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
        const bindStmts: A.Bind[] = stmts.map(mapStmts).filter(stmt => stmt);
        ensureUniqueIdsOrBindings(bindStmts, true);
        ensureDistinctLines(A['dummy-loc'], false, runtime.ffi.makeList(stmts));
        if (!inCheckBlock && !topLevel) {
          rejectStandaloneExprs(stmts, true);
        }
        stmts.forEach(stmt => wrapVisitAllowSMethod(visitor, stmt, false));
      }

      const wellFormedVisitor: TJ.Visitor<A.Ann | A.Expr | A.Member, void> = {
        'a-name': (visitor, expr: TJ.Variant<A.Ann, 'a-name'>) => {
          if (A['is-s-underscore'].app(expr.dict.id)) {
            addError(C['underscore-as-ann'].app(expr.dict.l));
          }
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>) => {
          const { l, 'op-l': opL, op, left, right } = expr.dict;
          reachableOps(visitor, l, opL, op, left);
          reachableOps(visitor, l, opL, op, right);
        },
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>) => {
          const stmts = listToArray(expr.dict.stmts);
          if (stmts.length === 0) {
            addError(C['wf-empty-block'].app(parentBlockLoc));
          } else {
            wfLastStmt(parentBlockLoc, stmts[stmts.length - 1]);
            wfBlockStmts(visitor, parentBlockLoc, stmts, false);
          }
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>) => {
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
          wrapVisitAllowSMethod(visitor, expr.dict.left, false)
          visit(visitor, expr.dict.right);
          visit(visitor, expr.dict.cause);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>) => {
          let oldPbl = parentBlockLoc;
          switch (expr.dict['_check-loc'].$name) {
            case 'none': {
              parentBlockLoc = expr.dict.l;
              break;
            }
            case 'some': {
              const l = expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
              const l2 = expr.dict['_check-loc'].dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
              parentBlockLoc = uptoEnd(l, l2);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict['_check-loc']);
            }
          }
          if (reservedNames.has(expr.dict.name)) {
            reservedName(expr.dict.l, expr.dict.name);
          }
          sMethodHelper(visitor, expr);
          parentBlockLoc = oldPbl;
        },
        's-method': (visitor, expr: TJ.Variant<A.Expr, 's-method'>) => {
          if (!allowSMethod) {
            addError(C['wf-bad-method-expression'].app(expr.dict.l));
          }
          let oldPbl = parentBlockLoc;
          switch (expr.dict['_check-loc'].$name) {
            case 'none': {
              parentBlockLoc = expr.dict.l;
              break;
            }
            case 'some': {
              const l = expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
              const l2 = expr.dict['_check-loc'].dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
              parentBlockLoc = uptoEnd(l, l2);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict['_check-loc']);
            }
          }
          sMethodHelper(visitor, expr);
          parentBlockLoc = oldPbl;
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>) => {
          let oldPbl = parentBlockLoc;
          switch (expr.dict['_check-loc'].$name) {
            case 'none': {
              parentBlockLoc = expr.dict.l;
              break;
            }
            case 'some': {
              const l = expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
              const l2 = expr.dict['_check-loc'].dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
              parentBlockLoc = uptoEnd(l, l2);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict['_check-loc']);
            }
          }
          const args = listToArray(expr.dict.args);
          ensureUniqueIdsOrBindings(args, false);
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
          listToArray(expr.dict.params).forEach(param => visit(visitor, param));
          args.forEach(arg => visit(visitor, arg));
          visit(visitor, expr.dict.ann);
          wrapVisitAllowSMethod(visitor, expr.dict.body, false);
          switch (expr.dict['_check-loc'].$name) {
            case 'none': {
              break;
            }
            case 'some': {
              const l = expr.dict.l as TJ.Variant<A.Srcloc, 'srcloc'>;
              const l2 = expr.dict['_check-loc'].dict.value as TJ.Variant<A.Srcloc, 'srcloc'>;
              parentBlockLoc = uptoEnd(l2, l);
              break;
            }
            default: {
              throw new ExhaustiveSwitchError(expr.dict['_check-loc']);
            }
          }
          wrapRejectStandalonesInCheck(expr.dict._check as A.Option<TJ.Variant<A.Expr, 's-block'>>);
          wrapVisitCheck(visitor, expr.dict._check);
          parentBlockLoc = oldPbl;
        }
      }

      const topLevelVisitor: TJ.Visitor<A.Program | A.Expr | A.TypeLetBind | A.Variant | A.Member, void> = {
        's-program': (visitor, expr: TJ.Variant<A.Program, 's-program'>) => {
          const body = expr.dict.block;
          if (body.$name === 's-block') {
            wfBlockStmts(visitor, body.dict.l, listToArray(body.dict.stmts), true);
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
        's-block': (visitor, expr: TJ.Variant<A.Expr, 's-block'>) => {
          visit<A.Expr>(wellFormedVisitor, expr);
        },
        's-op': (visitor, expr: TJ.Variant<A.Expr, 's-op'>) => {
          visit<A.Expr>(wellFormedVisitor, expr);
        },
        's-check-test': (visitor, expr: TJ.Variant<A.Expr, 's-check-test'>) => {
          visit<A.Expr>(wellFormedVisitor, expr);
        },
        's-lam': (visitor, expr: TJ.Variant<A.Expr, 's-lam'>) => {
          visit<A.Expr>(wellFormedVisitor, expr);
        },
        's-method': (visitor, expr: TJ.Variant<A.Expr, 's-method'>) => {
          visit<A.Expr>(wellFormedVisitor, expr);
        },
        's-method-field': (visitor, expr: TJ.Variant<A.Member, 's-method-field'>) => {
          visit<A.Member>(wellFormedVisitor, expr);
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