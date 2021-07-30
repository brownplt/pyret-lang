import type * as A from './ts-ast';
import type * as C from './ts-compile-structs';
import type * as ED from './error-display';
import type * as TJ from './ts-codegen-helpers';
import type { List } from './ts-impl-types';

({
    requires: [
        { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
        { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
        { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
        { 'import-type': 'dependency', protocol: 'file', args: ['error-display.arr']},
    ],
    nativeRequires: ["escodegen", "path"],
    provides: {
        values : {
            "check-well-formed": "tany"
        }
    },
    theModule: function(runtime, _, __, tj : TJ.Exports, Ain : (A.Exports), Cin : (C.Exports), EDin : (ED.Exports)) {
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

            function addError(err: C.CompileError) {
                errors.push(err);
            }

            function wfError(msg: List<ED.ErrorDisplay>, loc: A.Srcloc) {
                addError(C['wf-err'].app(msg, loc));
            }

            function wrapVisitAllowSMethod(visitor, target: A.Expr | A.Provide, allow: boolean) {
                let curAllow = allowSMethod;
                allowSMethod = allow;
                visit(visitor, target);
                allowSMethod = curAllow;
            }

            function ensureUniqueBindings(bindings: A.Bind[]) {
                let ad = new Map<string, A.Srcloc>();
                function help(bind: A.Bind) {
                    switch (bind.$name) {
                        case 's-bind': {
                            const {l, shadows, id } = bind.dict;
                            if (id.$name === 's-underscore') {
                                return;
                            }
                            if (shadows) {
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

            function ensureDistinctLines(loc: A.Srcloc, prevIsTemplate: boolean, stmts: List<A.Expr>) {
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
                                            } else if (!A['is-s-template'].app(first) && !prevIsTemplate)  {
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

            function rejectStandaloneExprs(stmts: A.Expr[], ignoreLast: boolean) {
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
                for(let i = 0; i < toExamineLength; i++) {
                    badStmt(stmts[i].dict.l, stmts[i]);
                }
            }

            function wfBlockStmts(visitor, l: A.Srcloc, stmts: List<A.Expr>, topLevel: boolean) {
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
                ensureUniqueBindings(bindStmts);
                ensureDistinctLines(A['dummy-loc'], false, stmts);
                if (!inCheckBlock && !topLevel) {
                    rejectStandaloneExprs(listToArray(stmts), true);
                }
                listToArray(stmts).forEach(stmt => wrapVisitAllowSMethod(visitor, stmt, false));
            }

            const topLevelVisitor: TJ.Visitor<A.Program, void> = {
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