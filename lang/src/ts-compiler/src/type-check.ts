/*
  TS port of src/arr/compiler/type-check.arr.

  The bidirectional type checker: the checking/synthesis mutual recursion,
  data-type and variant checking, cases handling, examples/spies test
  inference, annotation-to-type conversion, and the toplevel
  `typeCheck(program, compileEnv, postCompileEnv, modules)` entry point.

  Conventions (see CONVENTIONS.md and the header of type-check-structs.ts):
  - Option<T> is T | undefined.
  - Pyret StringDict is Map<string, _> with copy-on-write via mapSet.
  - cases without else become switch with a `default: throw`.
  - Existential creation order (newExistential) matches the Pyret source.
*/

import * as A from './ast';
import * as SL from './srcloc';
import * as AU from './ast-util';
import * as TS from './type-structs';
import * as TCS from './type-check-structs';
import * as C from './compile-structs';
import { eliminatedScopeForm } from './ast-visitors';
import {
  InternalCompilerError,
  asVariant,
  field as getField,
  map2,
  mapGetValue,
  mapSet,
  nonNull,
  raise,
  toRepr,
} from './shared';

type Type = TS.Type;
type TypeMembers = TS.TypeMembers;
type DataType = TS.DataType;
type TypeVariant = TS.TypeVariant;
type Loc = A.Loc;
type Expr = A.Expr;
type Name = A.Name;
type Context = TCS.Context;
type TCInfo = TCS.TCInfo;
type AnyTypingResult = TCS.AnyTypingResult;
type AnyFoldResult<V> = TCS.AnyFoldResult<V>;

const newExistential = TS.newExistential;
const foldr2 = TS.foldr2;
const listToTypeSet = TCS.listToTypeSet;

// Port of the type-logger trove module: logs only when a global
// `window.logger` object has been installed (same as type-check-structs.ts).
function log(name: string, payload: string): void {
  const w: any = (globalThis as any).window;
  if (typeof w !== 'undefined' && typeof w.logger !== 'undefined') {
    w.logger.log(name, { value: payload });
  }
}


// Pyret `<option>.value`: raises when the option is none.
function optValue<X>(o: X | undefined): X {
  if (o === undefined) {
    return raise('Tried to get value from none');
  }
  return o;
}

// Pyret lists.split-at
function splitAt<X>(n: number, lst: X[]): { prefix: X[]; suffix: X[] } {
  if (n < 0) {
    return raise('Invalid index');
  }
  if (n > lst.length) {
    return raise('Index too large');
  }
  return { prefix: lst.slice(0, n), suffix: lst.slice(n) };
}

const primvalTypes: Map<string, Type> = new Map();
const primvalCopyKeys: string[] = ['builtins', 'nothing'];

// ################### Test Inference ####################

// an option containing the key of the function name,
// the arg-types (some of which are existentials),
// the return type (which may be an existential),
// and the existential that is the function's type
type TestInferenceData = {
  name: Name;
  argTypes: Type[];
  retType: Type;
  loc: Loc;
  existential: Type;
};

let testInferenceData: TestInferenceData | undefined = undefined;

let miscTestInferenceData: Name | undefined = undefined;

// #######################################################

export function optionBind<X, Y>(f: (x: X) => Y | undefined, maybeThing: X | undefined): Y | undefined {
  if (maybeThing === undefined) {
    return undefined;
  }
  return f(maybeThing);
}

export function split<X, Y>(ps: [X, Y][]): [X[], Y[]] {
  // foldr building both lists keeps the original order
  const xs: X[] = [];
  const ys: Y[] = [];
  for (const [x, y] of ps) {
    xs.push(x);
    ys.push(y);
  }
  return [xs, ys];
}

export function importToString(i: A.ImportType, c: C.CompileEnvironment): string {
  return c.uriByDepKey(AU.importToDep(i).key());
}

// if a t-name refers to a polymorphic data type convert it to a t-app with existentials
export function addExistentialsToDataName(typ: Type, context: Context): AnyFoldResult<Type> {
  switch (typ.$name) {
    case 't-name': {
      const dataType = context.getDataType(typ);
      if (dataType === undefined) {
        return new TCS.FoldErrors<Type>([new C.CantTypecheck('Expected a data type but got ' + typ.toString(), typ.l)]);
      } else {
        if (dataType.params.length === 0) {
          return new TCS.FoldResult<Type>(typ, context);
        } else {
          const newExistentials = dataType.params.map((aVar) => newExistential(aVar.l, false));
          const newType = new TS.TApp(typ, newExistentials, typ.l, typ.inferred);
          const newContext = context.addVariableSet(listToTypeSet(newExistentials));
          return new TCS.FoldResult<Type>(newType, newContext);
        }
      }
    }
    default:
      return new TCS.FoldResult<Type>(typ, context);
  }
}

export function valueExportSdToTypeSd(sd: Map<string, C.ValueExport>): Map<string, Type> {
  const tdict = new Map<string, Type>();
  for (const k of sd.keys()) {
    tdict.set(k, getField(mapGetValue(sd, k), 't'));
  }
  return tdict;
}

// I believe modules is always of type SD.MutableStringDict<Loadable> -Matt
export function typeCheck(program: A.Program, compileEnv: C.CompileEnvironment, postCompileEnv: C.ComputedEnvironment, modules: Map<string, C.Loadable>): C.CompileResult<TCS.Typed> {
  let context = TCS.emptyContext();
  for (const key of primvalCopyKeys) {
    primvalTypes.set(key, getField(mapGetValue(mapGetValue(modules, 'builtin://global').provides.values, key), 't'));
  }
  const globvs = compileEnv.globals.values;
  const globts = compileEnv.globals.types;
  for (const g of globvs.keys()) {
    if (context.globalTypes.has(new A.SGlobal(g).key())) {
      // context unchanged
    } else {
      // TODO(joe): type-check vars by making them refs

      if (g === '_') {
        // context unchanged
      } else {
        context = context.setGlobalTypes(mapSet(context.globalTypes, new A.SGlobal(g).key(), getField(compileEnv.globalValueValue(g), 't')));
      }
    }
  }
  for (const g of globts.keys()) {
    if (context.aliases.has(new A.STypeGlobal(g).key())) {
      // context unchanged
    } else {
      const origin = mapGetValue(globts, g);
      if (g === '_') {
        // context unchanged
      } else {
        const provs = compileEnv.providesByUri(origin.uriOfDefinition);
        if (provs !== undefined) {
          let t: Type;
          const aliased = provs.aliases.get(g);
          if (aliased === undefined) {
            const dataDef = provs.dataDefinitions.get(g);
            if (dataDef === undefined) {
              return raise('Key ' + g + ' not found in ' + toRepr(provs));
            } else {
              t = new TS.TName(TS.builtinUri, new A.STypeGlobal(g), new SL.Builtin('global'), false);
            }
          } else {
            t = aliased;
          }
          context = context.setAliases(mapSet(context.aliases, new A.STypeGlobal(g).key(), t));
        } else {
          return raise('Could not find module ' + toRepr(origin.uriOfDefinition) + ' in ' + toRepr(compileEnv.allModules) + ' in ' + toRepr(program.l));
        }
      }
    }
  }
  for (const k of modules.keys()) {
    if (context.modules.has(k)) {
      // context unchanged
    } else {
      const mod = mapGetValue(modules, k).provides;
      const key = mod.fromUri;
      const valsTypesDict = new Map<string, Type>();
      for (const k2 of mod.values.keys()) {
        const ve = mapGetValue(mod.values, k2);
        let typ: Type;
        switch (ve.$name) {
          case 'v-alias':
            typ = getField(compileEnv.valueByUriValue(ve.origin.uriOfDefinition, ve.origin.originalName.toname()), 't');
            break;
          default:
            typ = getField(ve, 't');
        }
        valsTypesDict.set(k2, typ);
      }
      const dtsDict = new Map<string, DataType>();
      for (const k2 of mod.dataDefinitions.keys()) {
        const de = mapGetValue(mod.dataDefinitions, k2);
        let typ: DataType;
        switch (de.$name) {
          case 'd-alias':
            typ = compileEnv.resolveDatatypeByUriValue(de.origin.uriOfDefinition, de.origin.originalName.toname());
            break;
          case 'd-type':
            typ = de.typ;
            break;
          default:
            throw new InternalCompilerError('Unknown DataExport in type-check');
        }
        dtsDict.set(k2, typ);
      }
      const valProvides = new TS.TRecord(valsTypesDict, program.l, false);
      const moduleType = new TS.TModule(key, valProvides, dtsDict, mod.aliases);
      context = context.setModules(mapSet(context.modules, key, moduleType));
      for (const d of mod.dataDefinitions.keys()) {
        context = context.setDataTypes(mapSet(context.dataTypes, d, nonNull(compileEnv.resolveDatatypeByUri(mod.fromUri, d))));
      }
    }
  }

  const { l, _use, _provide, providedTypes, provides, imports } = program;
  const body = program.block;

  // NOTE(joe) – we cannot use module-env/type-env/env here because they
  // represent the environment at the *end* of the module. So if the user
  // shadows an imported ID, we would pick up that name as the type of the
  // import. Instead, we filter through all the bindings looking for ones
  // that came from a module. This is slower, and having Yet Another
  // Datatype for "bindings after imports" would help here.

  const pce = asVariant(postCompileEnv, C.ComputedEnv);
  const mbinds = pce.moduleBindings;
  const vbinds = pce.bindings;
  const tbinds = pce.typeBindings;

  let newModuleNames = context.moduleNames;
  for (const key of mbinds.keys()) {
    newModuleNames = mapSet(newModuleNames, key, mapGetValue(mbinds, key).uri);
  }

  let newGlobalTypes = context.globalTypes;
  for (const key of vbinds.keys()) {
    const vbind = mapGetValue(vbinds, key);
    if (vbind.origin.newDefinition) {
      // unchanged
    } else {
      const thismod = mapGetValue(context.modules, vbind.origin.uriOfDefinition);
      const typ = getField(thismod.provides, 'fields').get(vbind.origin.originalName.toname());
      if (typ === undefined) {
        return raise('Cannot find value binding for ' + vbind.origin.originalName.toname());
      } else {
        newGlobalTypes = mapSet(newGlobalTypes, key, typ);
      }
    }
  }

  let newAliases = context.aliases;
  for (const key of tbinds.keys()) {
    const tbind = mapGetValue(tbinds, key);
    if (tbind.origin.newDefinition) {
      // unchanged
    } else {
      const typ = compileEnv.typeByUri(tbind.origin.uriOfDefinition, tbind.origin.originalName.toname());
      if (typ === undefined) {
        return raise('Cannot find type binding for ' + toRepr(tbind));
      } else {
        newAliases = mapSet(newAliases, key, typ);
      }
    }
  }

  context = new TCS.TypingContext(newGlobalTypes, newAliases, context.dataTypes, context.modules, newModuleNames, context.binds, context.constraints, context.info, context.misc);

  const tcResult = checking(body, new TS.TTop(l, false), true, context);
  switch (tcResult.$name) {
    case 'typing-result': {
      const newBody = tcResult.ast;
      context = tcResult.outContext;

      for (const key of context.misc.keys()) {
        const [funExamples, funName] = mapGetValue(context.misc, key);
        TCS.miscTestInference(funExamples, funName);
      }

      const foldedInfo = gatherProvides(provides[0], context);
      switch (foldedInfo.$name) {
        case 'fold-result':
          return C.ok(new TCS.Typed(new A.SProgram(l, _use, _provide, providedTypes, provides, imports, newBody), foldedInfo.v));
        case 'fold-errors':
          return C.err(foldedInfo.errors);
        default:
          throw new InternalCompilerError('Unknown FoldResult in type-check');
      }
    }
    case 'typing-error':
      return C.err(tcResult.errors);
    default:
      throw new InternalCompilerError('Unknown TypingResult in type-check');
  }
}

export function checking(e: Expr, expectTyp: Type, topLevel: boolean, context: Context): AnyTypingResult {
  return _checking(e, expectTyp, topLevel, context);
}

function _checking(e: Expr, expectType0: Type, topLevel: boolean, context0: Context): AnyTypingResult {
  const context1 = context0.addLevel();
  const expectType1 = TCS.resolveAlias(expectType0, context1);
  let foldExpect: AnyFoldResult<Type>;
  switch (expectType1.$name) {
    case 't-app': {
      if (TS.isTApp(expectType1.onto) || TS.isTForall(expectType1.onto)) {
        foldExpect = TCS.introduceOnto(expectType1, context1);
      } else {
        foldExpect = new TCS.FoldResult<Type>(expectType1, context1);
      }
      break;
    }
    default:
      foldExpect = new TCS.FoldResult<Type>(expectType1, context1);
  }
  return foldExpect.typingBind((expectType, context) => {
    const result = ((): AnyTypingResult => {
      if (TS.isTExistential(expectType) || TS.isTTop(expectType)) {
        return checkSynthesis(e, expectType, topLevel, context);
      } else {
        switch (e.$name) {
          case 's-module': {
            const { answer } = e;
            return checking(answer, expectType, false, context)
              .bind((_newAnswer, _typ, _context) => {
                // The Pyret source calls foldr-fold-result here with only its
                // function argument (the list/context/base arguments are
                // missing), which raises an arity error if this branch is
                // ever reached (it only runs when expect-type is neither top
                // nor an existential, and s-module is always checked against
                // t-top); mirror that behavior.
                throw new InternalCompilerError('checking: s-module calls foldr-fold-result with missing arguments in the Pyret source (arity error if reached)');
              });
          }
          case 's-template':
            return new TCS.TypingResult(e, expectType, context);
          case 's-type-let-expr':
          case 's-let-expr':
          case 's-letrec':
            return eliminatedScopeForm(e);
          case 's-hint-exp':
            return raise('checking for s-hint-exp not implemented');
          case 's-instantiate':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-block': {
            const { l, stmts } = e;
            // fun gen(curr, base): {link({curr; base.{1}}, base.{0}); t-top(l, false)} end
            let base: [[Expr, Type][], Type] = [[], expectType];
            for (let i = stmts.length - 1; i >= 0; i--) {
              base = [[[stmts[i], base[1]] as [Expr, Type], ...base[0]], new TS.TTop(l, false)];
            }
            const pairedStmts = base[0];
            return TCS.foldTyping((stmtTypePair: [Expr, Type], ctx: Context) =>
              checking(stmtTypePair[0], stmtTypePair[1], topLevel, ctx), pairedStmts, context)
              .typingBind((newStmts, ctx) =>
                new TCS.TypingResult(new A.SBlock(l, newStmts), expectType, ctx));
          }
          case 's-scope-block': {
            const { l, entries, tail } = e;
            const newEntries: A.ScopeEntry[] = [];
            const checkEntries = (i: number, topLevel2: boolean, ctx0: Context): AnyTypingResult => {
              if (i === entries.length) {
                return checking(tail, expectType, topLevel2, ctx0)
                  .mapExpr((newTail) => new A.SScopeBlock(l, newEntries, newTail));
              }
              const entry = entries[i];
              if (A.isSScopeLet(entry)) {
                const binds2 = entry.binds;
                // ignore-checker (the Pyret original's helper), inlined
                if (binds2.length === 1) {
                  const bindingId = getField(binds2[0].b, 'id');
                  if (bindingId.$name === 's-atom' && (bindingId as any).base.length >= 19
                      && (bindingId as any).base.substring(0, 19) === 'result-after-checks'
                      && A.isSModule(tail)) {
                    const ctx1 = ctx0.addBinding(bindingId.key(), new TS.TTop(l, false));
                    return checking(tail, new TS.TTop(l, false), true, ctx1)
                      .bind((_newModule, newType, ctx2) => {
                        const ctx3 = ctx2.removeBinding(bindingId.key());
                        for (let j = i; j < entries.length; j++) {
                          newEntries.push(entries[j]);
                        }
                        return new TCS.TypingResult(new A.SScopeBlock(l, newEntries, tail), newType, ctx3);
                      });
                  }
                }
                return TCS.foldTyping(synthesisLetBind, binds2, ctx0).typingBind((rhsResult, ctx2) => {
                  const newBinds = map2((binding: A.LetBind, rhs: Expr): A.LetBind => {
                    switch (binding.$name) {
                      case 's-let-bind':
                        return new A.SLetBind(binding.l, binding.b, rhs);
                      case 's-var-bind':
                        return new A.SVarBind(binding.l, binding.b, rhs);
                      default:
                        throw new InternalCompilerError('Unknown LetBind in checking');
                    }
                  }, binds2, rhsResult);
                  newEntries.push(new A.SScopeLet(entry.l, newBinds));
                  return checkEntries(i + 1, topLevel2, ctx2)
                    .bind((restExpr, restType, ctx3) => {
                      let ctx4 = ctx3;
                      for (let k = binds2.length - 1; k >= 0; k--) {
                        ctx4 = ctx4.removeBinding(getField(binds2[k].b, 'id').key());
                      }
                      return new TCS.TypingResult(restExpr, restType, ctx4);
                    });
                });
              } else if (A.isSScopeTypeLet(entry)) {
                return handleTypeLetBinds(entry.binds, ctx0).typingBind((_nothing, ctx2) => {
                  newEntries.push(entry);
                  return checkEntries(i + 1, true, ctx2);
                });
              } else if (A.isSScopeLetrec(entry)) {
                return handleLetrecBindings(entry.binds, topLevel2, ctx0, (newBinds, ctx2) => {
                  newEntries.push(new A.SScopeLetrec(entry.l, newBinds));
                  return checkEntries(i + 1, topLevel2, ctx2);
                });
              } else {
                return checking(entry.stmt, new TS.TTop(l, false), topLevel2, ctx0)
                  .bind((newStmt, _styp, ctx2) => {
                    newEntries.push(new A.SScopeStmt(entry.l, newStmt));
                    return checkEntries(i + 1, topLevel2, ctx2);
                  });
              }
            };
            return checkEntries(0, topLevel, context);
          }
          case 's-user-block':
            return raise('s-user-block should have already been desugared');
          case 's-fun':
            return raise('s-fun should have already been desugared');
          case 's-type':
            return raise('checking for s-type not implemented');
          case 's-newtype':
            return raise('checking for s-newtype not implemented');
          case 's-var':
            return raise('s-var should have already been desugared');
          case 's-rec':
            return raise('checking for s-rec not implemented');
          case 's-let':
            return raise('s-let should have already been desugared');
          case 's-ref':
            return raise('checking for s-ref not implemented');
          case 's-contract':
            return raise('checking for s-contract not implemented');
          case 's-when':
            return raise('s-when should have already been desugared');
          case 's-assign': {
            const { l, id, value } = e;
            return lookupId(l, id.key(), e, context).typingBind((idType, ctx) => {
              switch (idType.$name) {
                case 't-ref':
                  return checking(value, idType.typ, topLevel, ctx);
                default:
                  return new TCS.TypingError([new C.IncorrectTypeExpression(idType.toString(), l, new TS.TRef(idType, l, false).toString(), l, e)]);
              }
            });
          }
          case 's-if-pipe':
            return raise('s-if-pipe should have already been desugared');
          case 's-if-pipe-else':
            return raise('s-if-pipe-else should have already been desugared');
          case 's-if':
            return raise('s-if should have already been desugared');
          case 's-if-else': {
            const { l, branches, _else, blocky } = e;
            return TCS.mapFoldResult((branch: A.IfBranch, ctx: Context) => {
              const boolResult = checking(branch.test, TS.tBoolean(branch.l), false, ctx);
              return boolResult.foldBind((newTest, _t, ctx2) => {
                const bodyResult = checking(branch.body, expectType, false, ctx2);
                return bodyResult.foldBind((newBody, _t2, ctx3) =>
                  new TCS.FoldResult(new A.SIfBranch(branch.l, newTest, newBody), ctx3));
              });
            }, branches, context).typingBind((newBranches, ctx) =>
              checking(_else, expectType, false, ctx)
                .mapExpr((newElse) => new A.SIfElse(l, newBranches, newElse, blocky)));
          }
          case 's-cases': {
            const { l, typ, val, branches } = e;
            return checkingCases(l, typ, val, branches, undefined, expectType, context);
          }
          case 's-cases-else': {
            const { l, typ, val, branches, _else } = e;
            return checkingCases(l, typ, val, branches, _else, expectType, context);
          }
          case 's-op':
            return raise('checking for s-op not implemented');
          case 's-check-test': {
            if (testInferenceData !== undefined) {
              return collectExample(e, context).typingBind((_nothing, ctx) =>
                new TCS.TypingResult(e, expectType, ctx));
            } else {
              const ctx = miscCollectExample(e, context);
              return synthesisSCheckTest(e, e.l, e.op, e.refinement, e.left, e.right, e.cause, ctx);
            }
          }
          case 's-check-expr':
            return synthesis(e.expr, false, context); // XXX: this should probably use the annotation instead
          case 's-paren':
            return raise('s-paren should have already been desugared');
          case 's-lam': {
            const { l, name, params, args, ann, doc, body, _checkLoc, _check, blocky } = e;
            return checkFun(l, body, params, args, ann, expectType,
              (args2, ann2, body2) => new A.SLam(l, name, params, args2, ann2, doc, body2, _checkLoc, _check, blocky),
              context);
          }
          case 's-method':
            return raise('checking for s-method not implemented');
          case 's-extend':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-update':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-tuple': {
            const l = e.l;
            const elts = e.fields;
            switch (expectType.$name) {
              case 't-tuple': {
                const tElts = expectType.elts;
                if (!(elts.length === tElts.length)) {
                  // TODO(MATT): better error
                  return new TCS.TypingError([new C.IncorrectType('a tuple type with length ' + String(elts.length), l, expectType.toString(), expectType.l)]);
                } else {
                  const result = foldr2((acc: AnyFoldResult<Expr[]>, elt: Expr, eltType: Type) =>
                    acc.bind((exprs, ctx) =>
                      checking(elt, eltType, false, ctx)
                        .foldBind((newElt, _t, ctx2) =>
                          new TCS.FoldResult<Expr[]>([newElt, ...exprs], ctx2))),
                  new TCS.FoldResult<Expr[]>([], context), elts, tElts);
                  return result.typingBind((exprs, ctx) =>
                    new TCS.TypingResult(new A.STuple(l, exprs), expectType, ctx));
                }
              }
              default:
                return new TCS.TypingError([new C.IncorrectType(expectType.toString(), expectType.l, 'a tuple type', l)]);
            }
          }
          case 's-tuple-get':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-obj': {
            const { l, fields } = e;
            return TCS.instantiateObjectType(expectType, context).typingBind((expectType2, ctx) => {
              switch (expectType2.$name) {
                case 't-record': {
                  return collectMembers(fields, true, ctx).typingBind((fieldTypes, ctx2) => {
                    const tempObjectType = new TS.TRecord(fieldTypes, l, false);
                    const ctx3 = ctx2.addConstraint(tempObjectType, expectType2);
                    const foldNewFieldTypes = TCS.foldrFoldResult<A.Member, TypeMembers>((field, ctx4, memberTypes) =>
                      toTypeMember(field, mapGetValue(fieldTypes, field.name), tempObjectType, true, ctx4).bind((fieldType, ctx5) =>
                        new TCS.FoldResult(mapSet(memberTypes, field.name, fieldType), ctx5)),
                    fields, ctx3, new Map());
                    return foldNewFieldTypes.typingBind((_memberTypes, ctx4) =>
                      new TCS.TypingResult(new A.SObj(l, fields), expectType2, ctx4));
                  });
                }
                default:
                  return new TCS.TypingError([new C.IncorrectTypeExpression(expectType2.toString(), expectType2.l, 'an object type', l, e)]);
              }
            });
          }
          case 's-array': {
            const { l, values } = e;
            let wrapped: AnyFoldResult<Expr[]>;
            switch (expectType.$name) {
              case 't-app': {
                const rarray = expectType.onto;
                const args = expectType.args;
                if (TS.tArrayName.equals(rarray)) {
                  const paramType = args[0];
                  wrapped = TCS.foldTyping((value: Expr, ctx: Context) =>
                    checking(value, paramType, false, ctx), values, context);
                } else {
                  wrapped = new TCS.FoldErrors<Expr[]>([new C.IncorrectTypeExpression(TS.tArrayName.toString(), l, expectType.toString(), expectType.l, e)]);
                }
                break;
              }
              default:
                wrapped = new TCS.FoldErrors<Expr[]>([new C.IncorrectTypeExpression('a raw array', l, expectType.toString(), expectType.l, e)]);
            }
            return wrapped.typingBind((newValues, ctx) =>
              new TCS.TypingResult(new A.SArray(l, newValues), expectType, ctx));
          }
          case 's-construct':
            return raise('checking for s-construct not implemented');
          case 's-app':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-prim-app':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-prim-val':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-id':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-id-var-modref':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-id-modref':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-id-var':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-id-letrec':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-undefined':
            return raise('checking for s-undefined not implemented');
          case 's-srcloc':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-num':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-frac':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-rfrac':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-bool':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-str':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-dot':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-get-bang':
            return checkSynthesis(e, expectType, topLevel, context);
          case 's-bracket':
            return raise('checking for s-bracket not implemented');
          case 's-data':
            return raise('s-data should have already been desugared');
          case 's-data-expr':
            return raise('s-data-expr should have been handled by s-letrec');
          case 's-for':
            return raise('s-for should have already been desugared');
          case 's-check':
            return new TCS.TypingResult(e, expectType, context);
          default:
            throw new InternalCompilerError('No cases matched in checking for ' + e.$name);
        }
      }
    })();
    return result.solveBind();
  });
}

export function synthesis(e: Expr, topLevel: boolean, context: Context): AnyTypingResult {
  return _synthesis(e, topLevel, context);
}

function _synthesis(e: Expr, topLevel: boolean, context0: Context): AnyTypingResult {
  const context = context0.addLevel();
  const result = ((): AnyTypingResult => {
    switch (e.$name) {
      case 's-module': {
        const { l, answer, definedModules, definedValues, definedTypes, checks } = e;
        return synthesis(answer, false, context)
          .mapExpr((newAnswer) => new A.SModule(l, newAnswer, definedModules, definedValues, definedTypes, checks))
          .mapType((t) => t.setLoc(l));
      }
      case 's-template': {
        const l = e.l;
        const newExists = newExistential(l, false);
        const ctx = context.addVariable(newExists);
        return new TCS.TypingResult(e, newExists, ctx);
      }
      case 's-type-let-expr':
      case 's-let-expr':
      case 's-letrec':
        return eliminatedScopeForm(e);
      case 's-hint-exp':
        return raise('synthesis for s-hint-exp not implemented');
      case 's-instantiate': {
        const { l, expr, params } = e;
        return synthesisInstantiation(l, expr, params, topLevel, context);
      }
      case 's-block': {
        const { l, stmts } = e;
        let typ: Type = new TS.TTop(l, false);
        return TCS.foldTyping((stmt: Expr, ctx: Context) =>
          synthesis(stmt, topLevel, ctx).bind((stmtExpr, stmtTyp, ctx2) => {
            typ = stmtTyp;
            return new TCS.TypingResult(stmtExpr, stmtTyp, ctx2);
          }), stmts, context).typingBind((newStmts, ctx) =>
          new TCS.TypingResult(new A.SBlock(l, newStmts), typ.setLoc(l), ctx));
      }
      case 's-scope-block': {
        const { l, entries, tail } = e;
        const newEntries: A.ScopeEntry[] = [];
        const synthEntries = (i: number, topLevel2: boolean, ctx0: Context): AnyTypingResult => {
          if (i === entries.length) {
            return synthesis(tail, topLevel2, ctx0)
              .mapExpr((newTail) => new A.SScopeBlock(l, newEntries, newTail));
          }
          const entry = entries[i];
          if (A.isSScopeLet(entry)) {
            const binds2 = entry.binds;
            if (binds2.length === 1) {
              const bindingId = getField(binds2[0].b, 'id');
              if (bindingId.$name === 's-atom' && (bindingId as any).base.length >= 19
                  && (bindingId as any).base.substring(0, 19) === 'result-after-checks'
                  && A.isSModule(tail)) {
                const ctx1 = ctx0.addBinding(bindingId.key(), new TS.TTop(l, false));
                return checking(tail, new TS.TTop(l, false), true, ctx1)
                  .bind((_newModule, newType, ctx2) => {
                    const ctx3 = ctx2.removeBinding(bindingId.key());
                    for (let j = i; j < entries.length; j++) {
                      newEntries.push(entries[j]);
                    }
                    return new TCS.TypingResult(new A.SScopeBlock(l, newEntries, tail), newType, ctx3);
                  });
              }
            }
            return TCS.foldTyping(synthesisLetBind, binds2, ctx0).typingBind((newRhs, ctx2) => {
              const newBinds = map2((binding: A.LetBind, rhs: Expr): A.LetBind => {
                switch (binding.$name) {
                  case 's-let-bind':
                    return new A.SLetBind(binding.l, binding.b, rhs);
                  case 's-var-bind':
                    return new A.SVarBind(binding.l, binding.b, rhs);
                  default:
                    throw new InternalCompilerError('Unknown LetBind in synthesis');
                }
              }, binds2, newRhs);
              newEntries.push(new A.SScopeLet(entry.l, newBinds));
              return synthEntries(i + 1, false, ctx2)
                .mapType((t) => t.setLoc(entry.l))
                .bind((restExpr, restType, ctx3) => {
                  let ctx4 = ctx3;
                  for (let k = binds2.length - 1; k >= 0; k--) {
                    ctx4 = ctx4.removeBinding(getField(binds2[k].b, 'id').key());
                  }
                  return new TCS.TypingResult(restExpr, restType, ctx4);
                });
            });
          } else if (A.isSScopeTypeLet(entry)) {
            return handleTypeLetBinds(entry.binds, ctx0).typingBind((_nothing, ctx2) => {
              newEntries.push(entry);
              return synthEntries(i + 1, false, ctx2)
                .mapType((t) => t.setLoc(entry.l));
            });
          } else if (A.isSScopeLetrec(entry)) {
            return handleLetrecBindings(entry.binds, topLevel2, ctx0, (newBinds, ctx2) => {
              newEntries.push(new A.SScopeLetrec(entry.l, newBinds));
              return synthEntries(i + 1, topLevel2, ctx2)
                .mapType((t) => t.setLoc(entry.l));
            });
          } else {
            return synthesis(entry.stmt, topLevel2, ctx0)
              .bind((newStmt, _styp, ctx2) => {
                newEntries.push(new A.SScopeStmt(entry.l, newStmt));
                return synthEntries(i + 1, topLevel2, ctx2);
              });
          }
        };
        return synthEntries(0, topLevel, context)
          .mapType((t) => t.setLoc(l));
      }
      case 's-user-block':
        return raise('s-user-block should have already been desugared');
      case 's-fun':
        return raise('s-fun should have already been desugared');
      case 's-type':
        return raise('synthesis for s-type not implemented');
      case 's-newtype':
        return raise('synthesis for s-newtype not implemented');
      case 's-var':
        return raise('s-var should have already been desugared');
      case 's-rec':
        return raise('synthesis for s-rec not implemented');
      case 's-let':
        return raise('s-let should have already been desugared');
      case 's-ref':
        return raise('synthesis for s-ref not implemented');
      case 's-contract':
        return raise('synthesis for s-contract not implemented');
      case 's-when':
        return raise('s-when should have already been desugared');
      case 's-assign': {
        const { l, id, value } = e;
        return lookupId(l, id.key(), e, context).typingBind((idType, ctx) => {
          switch (idType.$name) {
            case 't-ref': {
              const argType = idType.typ;
              return checking(value, argType, topLevel, ctx).bind((newValue, _t, ctx2) =>
                new TCS.TypingResult(new A.SAssign(l, id, newValue), argType.setLoc(l), ctx2));
            }
            default:
              return new TCS.TypingError([new C.IncorrectTypeExpression(idType.toString(), l, new TS.TRef(idType, l, false).toString(), l, e)]);
          }
        });
      }
      case 's-if-pipe':
        return raise('s-if-pipe should have already been desugared');
      case 's-if-pipe-else':
        return raise('s-if-pipe-else should have already been desugared');
      case 's-if':
        return raise('s-if should have already been desugared');
      case 's-if-else': {
        const { l, branches, _else, blocky } = e;
        return TCS.mapFoldResult(handleIfBranch, branches, context).typingBind((result2, ctx) =>
          synthesis(_else, false, ctx).bind((newElse, elseType, ctx2) => {
            const splitResult = split(result2);
            const newBranches = splitResult[0];
            const newIfElse = new A.SIfElse(l, newBranches, newElse, blocky);
            return meetBranchTypes([elseType, ...splitResult[1]], l, ctx2).typingBind((ifElseType, ctx3) =>
              new TCS.TypingResult(newIfElse, ifElseType.setLoc(l), ctx3));
          }));
      }
      case 's-cases': {
        const { l, typ, val, branches } = e;
        return synthesisCases(l, typ, val, branches, undefined, context);
      }
      case 's-cases-else': {
        const { l, typ, val, branches, _else } = e;
        return synthesisCases(l, typ, val, branches, _else, context);
      }
      case 's-op':
        return raise('synthesis for s-op not implemented');
      case 's-check-test': {
        if (testInferenceData !== undefined) {
          return collectExample(e, context).typingBind((_nothing, ctx) => {
            const resultType = newExistential(e.l, false);
            const ctx2 = ctx.addVariable(resultType);
            return new TCS.TypingResult(e, resultType, ctx2);
          });
        } else {
          const ctx = miscCollectExample(e, context);
          return synthesisSCheckTest(e, e.l, e.op, e.refinement, e.left, e.right, e.cause, ctx);
        }
      }
      case 's-check-expr':
        return synthesis(e.expr, false, context); // XXX: this should probably use the annotation instead
      case 's-paren':
        return raise('s-paren should have already been desugared');
      case 's-lam': {
        const { l, name, params, args, ann, doc, body, _checkLoc, _check, blocky } = e;
        return synthesisFun(l, body, params, args, ann,
          (args2, ann2, body2) => new A.SLam(l, name, params, args2, ann2, doc, body2, _checkLoc, _check, blocky),
          topLevel, context);
      }
      case 's-method':
        return raise('synthesis for s-method not implemented');
      case 's-extend': {
        const { l, supe, fields } = e;
        return synthesis(supe, topLevel, context)
          .bind((newAst, newType, ctx) => synthesisExtend(l, newAst, newType, fields, ctx))
          .mapType((t) => t.setLoc(l));
      }
      case 's-update': {
        const { l, supe, fields } = e;
        return synthesis(supe, topLevel, context)
          .bind((newAst, newType, ctx) => synthesisUpdate(l, newAst, newType, fields, ctx))
          .mapType((t) => t.setLoc(l));
      }
      case 's-tuple': {
        const l = e.l;
        const elts = e.fields;
        const result2 = TCS.mapFoldResult((elt: Expr, ctx: Context) =>
          synthesis(elt, false, ctx)
            .foldBind((_a, eltType, ctx2) =>
              new TCS.FoldResult(eltType, ctx2)), elts, context);
        return result2.typingBind((typs, ctx) =>
          new TCS.TypingResult(new A.STuple(l, elts), new TS.TTuple(typs, l, false), ctx));
      }
      case 's-tuple-get': {
        const { l, tup, index, indexLoc } = e;
        return synthesis(tup, topLevel, context).bind((newAst, newType, ctx) =>
          synthesisTupleIndex(l, newAst, newType.l, newType, index,
            (l2, tup2, index2) => new A.STupleGet(l2, tup2, index2, indexLoc), ctx));
      }
      case 's-obj': {
        const { l, fields } = e;
        return collectMembers(fields, false, context).typingBind((fieldTypes, ctx) => {
          const initialObjType = new TS.TRecord(fieldTypes, l, false);
          const foldNewFieldTypes = TCS.foldrFoldResult<A.Member, TypeMembers>((field, ctx2, newFieldTypes) =>
            toTypeMember(field, mapGetValue(fieldTypes, field.name), initialObjType, false, ctx2).bind((newFieldType, ctx3) =>
              new TCS.FoldResult(mapSet(newFieldTypes, field.name, newFieldType), ctx3)),
          fields, ctx, new Map());
          return foldNewFieldTypes.typingBind((newFieldTypes, ctx2) =>
            new TCS.TypingResult(new A.SObj(l, fields), new TS.TRecord(newFieldTypes, l, false), ctx2));
        });
      }
      case 's-array': {
        const { l, values } = e;
        const process = (value: Expr, ctx: Context): AnyFoldResult<[Expr, Type]> =>
          synthesis(value, false, ctx).foldBind((expr, typ, ctx2) =>
            new TCS.FoldResult([expr, typ] as [Expr, Type], ctx2));

        return TCS.mapFoldResult(process, values, context).typingBind((result2, ctx) => {
          const [newValues, valueTypes] = split(result2);
          return meetBranchTypes(valueTypes, l, ctx).typingBind((arrayType, ctx2) => {
            const newArray = new A.SArray(l, newValues);
            return new TCS.TypingResult(newArray, TS.tArray(arrayType.setLoc(l), l), ctx2);
          });
        });
      }
      case 's-construct':
        return raise('synthesis for s-construct not implemented');
      case 's-app': {
        const { l, _fun, args } = e;
        return synthesisAppFun(l, _fun, args, context)
          .typingBind((funType, ctx) =>
            synthesisSpine(funType, (exprs) => new A.SApp(l, _fun, exprs), args, l, ctx));
      }
      case 's-prim-app': {
        const { l, _fun, args, appInfo } = e;
        return lookupId(l, _fun, e, context).typingBind((arrowType, ctx) =>
          synthesisSpine(arrowType, (exprs) => new A.SPrimApp(l, _fun, exprs, appInfo), args, l, ctx)
            .mapType((t) => t.setLoc(l)));
      }
      case 's-prim-val':
        return new TCS.TypingResult(e, mapGetValue(primvalTypes, e.name), context);
      case 's-id': {
        const { l, id } = e;
        return lookupId(l, id.key(), e, context).typingBind((idType, ctx) =>
          new TCS.TypingResult(e, idType, ctx));
      }
      case 's-id-var-modref': {
        const { uri, name } = e;
        const modTyps = mapGetValue(context.modules, uri);
        const provides = modTyps.provides;
        switch (provides.$name) {
          case 't-record': {
            const t = provides.fields.get(name);
            if (t === undefined) {
              return raise('Should be caught in unbound-ids: no such name on module ' + uri + ': ' + name);
            }
            return new TCS.TypingResult(e, t, context);
          }
          default:
            throw new InternalCompilerError('No cases matched on module provides in synthesis for s-id-var-modref');
        }
      }
      case 's-id-modref': {
        const { uri, name } = e;
        const modTyps = mapGetValue(context.modules, uri);
        const provides = modTyps.provides;
        switch (provides.$name) {
          case 't-record': {
            const t = provides.fields.get(name);
            if (t === undefined) {
              return raise('Should be caught in unbound-ids: no such name on module ' + uri + ': ' + name);
            }
            return new TCS.TypingResult(e, t, context);
          }
          default:
            throw new InternalCompilerError('No cases matched on module provides in synthesis for s-id-modref');
        }
      }
      case 's-id-var': {
        const { l, id } = e;
        return lookupId(l, id.key(), e, context).typingBind((idType, ctx) => {
          switch (idType.$name) {
            case 't-ref':
              return new TCS.TypingResult(e, idType.typ.setLoc(l), ctx);
            default:
              return new TCS.TypingError([new C.IncorrectTypeExpression(idType.toString(), idType.l, new TS.TRef(idType, l, false).toString(), l, e)]);
          }
        });
      }
      case 's-id-letrec': {
        const { l, id } = e;
        return lookupId(l, id.key(), e, context).typingBind((idType, ctx) =>
          new TCS.TypingResult(e, idType, ctx));
      }
      case 's-undefined':
        return raise('synthesis for s-undefined not implemented');
      case 's-srcloc':
        return new TCS.TypingResult(e, TS.tSrcloc(e.l), context);
      case 's-num':
        return new TCS.TypingResult(e, TS.tNumber(e.l), context);
      case 's-frac':
        return new TCS.TypingResult(e, TS.tNumber(e.l), context);
      case 's-rfrac':
        return new TCS.TypingResult(e, TS.tNumber(e.l), context);
      case 's-bool':
        return new TCS.TypingResult(e, TS.tBoolean(e.l), context);
      case 's-str':
        return new TCS.TypingResult(e, TS.tString(e.l), context);
      case 's-dot': {
        const { l, obj, field } = e;
        return synthesis(obj, topLevel, context).bind((newAst, newType, ctx) =>
          synthesisField(l, newAst, newType, field, (l2, obj2, field2) => new A.SDot(l2, obj2, field2), ctx));
      }
      case 's-get-bang': {
        const { l, obj, field } = e;
        return synthesis(obj, topLevel, context).bind((newAst, newType, ctx) =>
          synthesisField(l, newAst, newType, field, (l2, obj2, field2) => new A.SGetBang(l2, obj2, field2), ctx))
          .bind((newGetBang, fieldType, ctx) => {
            switch (fieldType.$name) {
              case 't-ref':
                return new TCS.TypingResult(newGetBang, fieldType.typ.setLoc(l), ctx);
              default:
                return new TCS.TypingError([new C.IncorrectTypeExpression(fieldType.toString(), fieldType.l, 'a ref type', l, e)]);
            }
          });
      }
      case 's-bracket':
        return raise('synthesis for s-bracket not implemented');
      case 's-data':
        return raise('s-data should have already been desugared');
      case 's-data-expr':
        return raise('s-data-expr should have been handled by s-letrec');
      case 's-for':
        return raise('s-for should have already been desugared');
      case 's-check': {
        const l = e.l;
        const resultType = newExistential(l, false);
        const ctx = context.addVariable(resultType);
        return new TCS.TypingResult(e, resultType, ctx);
      }
      default:
        throw new InternalCompilerError('No cases matched in synthesis for ' + e.$name);
    }
  })();
  return result.solveBind();
}

export function synthesisSpine(funType0: Type, recreate: (args: Expr[]) => Expr, args: Expr[], appLoc: Loc, context0: Context): AnyTypingResult {
  const context1 = context0.addLevel();
  return TCS.instantiateForall(funType0, context1).typingBind((funType, context) => {
    const result = ((): AnyTypingResult => {
      switch (funType.$name) {
        case 't-arrow': {
          const argTypes = funType.args;
          const retType = funType.ret;
          if (!(args.length === argTypes.length)) {
            return new TCS.TypingError([new C.IncorrectNumberOfArgs(recreate(args), funType)]);
          } else {
            return foldr2((acc: AnyFoldResult<Expr[]>, arg: Expr, argType: Type) =>
              acc.bind((exprs, ctx) =>
                checking(arg, argType, false, ctx)
                  .foldBind((newArg, _t, ctx2) =>
                    new TCS.FoldResult<Expr[]>([newArg, ...exprs], ctx2))),
            new TCS.FoldResult<Expr[]>([], context), args, argTypes).typingBind((exprs, ctx) =>
              new TCS.TypingResult(recreate(exprs), retType, ctx));
          }
        }
        case 't-existential': {
          const l = funType.l;
          const existentialArgs = args.map((_a) => newExistential(l, false));
          const existentialRet = newExistential(l, false);
          let ctx = context.addVariableSet(listToTypeSet([existentialRet, ...existentialArgs]));
          const newArrow = new TS.TArrow(existentialArgs, existentialRet, l, false);
          ctx = ctx.addConstraint(funType, newArrow);
          const result2 = foldr2((acc: AnyFoldResult<Expr[]>, arg: Expr, argType: Type) =>
            acc.bind((currentExprs, ctx2) =>
              checking(arg, argType, false, ctx2)
                .foldBind((newArg, _t, ctx3) =>
                  new TCS.FoldResult<Expr[]>([newArg, ...currentExprs], ctx3))),
          new TCS.FoldResult<Expr[]>([], ctx), args, existentialArgs);

          return result2.typingBind((newExprs, ctx2) =>
            new TCS.TypingResult(recreate(newExprs), existentialRet, ctx2));
        }
        case 't-app':
          return TCS.introduceOnto(funType, context).typingBind((onto, ctx) =>
            synthesisSpine(onto, recreate, args, appLoc, ctx));
        case 't-bot': {
          const l = funType.l;
          const inferred = funType.inferred;
          return TCS.foldTyping((arg: Expr, ctx: Context) =>
            checking(arg, new TS.TTop(l, false), false, ctx), args, context).typingBind((newArgs, ctx) =>
            new TCS.TypingResult(recreate(newArgs), new TS.TBot(l, inferred), ctx));
        }
        default:
          return new TCS.TypingError([new C.ApplyNonFunction(recreate(args), funType)]);
      }
    })();
    return result.solveBind().mapType((t) => t.setLoc(appLoc));
  });
}

export function checkSynthesis(e: Expr, expectType: Type, topLevel: boolean, context: Context): AnyTypingResult {
  return synthesis(e, topLevel, context).bind((newExpr, newType, ctx) => {
    // TODO(MATT): decide whether this should return new-type or expect-type
    return new TCS.TypingResult(newExpr, newType, ctx.addConstraint(newType, expectType));
  });
}

export function lookupId(blameLoc: A.Loc, idKey: string, idExpr: Expr, context: Context): AnyFoldResult<Type> {
  if (context.binds.has(idKey)) {
    return new TCS.FoldResult(mapGetValue(context.binds, idKey).setLoc(blameLoc), context);
  } else if (context.globalTypes.has(idKey)) {
    return new TCS.FoldResult(mapGetValue(context.globalTypes, idKey).setLoc(blameLoc), context);
  } else {
    return new TCS.FoldErrors<Type>([new C.UnboundId(idExpr)]);
  }
}

// TODO(MATT): this should require unifying of same-named methods
//             should it require unifying types of same-named members?
// Type checks data types
// Returns the list of all relevant letrec bindings
// Use the context returned from this function
export function handleDatatype(dataTypeBind: A.LetrecBind, bindings: A.LetrecBind[], context0: Context): AnyFoldResult<A.LetrecBind[]> {
  const dataExpr = dataTypeBind.value;
  switch (dataExpr.$name) {
    case 's-data-expr': {
      const { l, name, namet, params, variants } = dataExpr;
      const fields = dataExpr.sharedMembers;
      const context = context0.addLevel();
      const branderType: Type = new TS.TName(TS.local, namet, l, false);
      const tVars: Type[] = params.map((p) => new TS.TVar(p, l, false));
      const appliedBranderType: Type = tVars.length === 0 ? branderType : new TS.TApp(branderType, tVars, l, false);

      return TCS.mapFoldResult(collectVariantConstructor, variants, context).bind((initialVariantTypes, ctx1) => {
        const predicateType: Type = tVars.length === 0
          ? new TS.TArrow([branderType], TS.tBoolean(l), l, false)
          : new TS.TForall(tVars, new TS.TArrow([new TS.TApp(branderType, tVars, l, false)], TS.tBoolean(l), l, false), l, false);
        const initialDataFields = new Map<string, Type>();
        initialDataFields.set(name, predicateType);
        let dataFields = initialDataFields;
        for (const variantType of initialVariantTypes) {
          dataFields = mapSet(mapSet(dataFields, variantType.name, mkConstructorType(variantType, branderType, tVars)),
            'is-' + variantType.name, predicateType);
        }
        const ctx2 = ctx1.addBinding(getField(dataTypeBind.b, 'id').key(), new TS.TRecord(dataFields, l, false));
        return TCS.mapFoldResult((binding: A.LetrecBind, ctx: Context) =>
          synthesis(binding.value, false, ctx).foldBind((newValue, resultType, ctx3) =>
            new TCS.FoldResult(new A.SLetrecBind(binding.l, binding.b, newValue), ctx3.addBinding(getField(binding.b, 'id').key(), resultType))),
        bindings, ctx2).bind((newBindings, ctx3) =>
          TCS.mapFoldResult(collectVariant, variants, ctx3).bind((initialVariantTypes2, ctx4) =>
            collectMembers(fields, true, ctx4).bind((initialSharedFieldTypes, ctx5) => {
              const initialDataType = new TS.TData(name, tVars, initialVariantTypes2, initialSharedFieldTypes, l);
              let ctx6 = ctx5.setDataTypes(mapSet(ctx5.dataTypes, namet.key(), initialDataType));
              ctx6 = mergeCommonFields(initialVariantTypes2, l, ctx6);
              return TCS.mapFoldResult((variant: A.Variant, ctx: Context) =>
                checkVariant(variant, initialDataType.getVariantValue(variant.name), branderType, tVars, ctx),
              variants, ctx6).bind((newVariantTypes, ctx7) => {
                const variantTypeFields: TypeMembers[] = newVariantTypes.map((varType) => {
                  const allFields = new Map(varType.withFields);
                  for (let i = varType.fields.length - 1; i >= 0; i--) {
                    const [fieldName, fieldType] = varType.fields[i];
                    allFields.set(fieldName, fieldType);
                  }
                  return allFields;
                });
                let variantsMeet: TypeMembers;
                if (variantTypeFields.length === 0) {
                  variantsMeet = new Map();
                } else if (variantTypeFields.length === 1) {
                  variantsMeet = variantTypeFields[0];
                } else {
                  const first = variantTypeFields[0];
                  const rest = variantTypeFields.slice(1);
                  variantsMeet = rest.reduceRight((acc, mem) => meetFields(mem, acc, l, ctx7), first);
                }
                let extendedSharedFieldTypes = initialSharedFieldTypes;
                for (const key of variantsMeet.keys()) {
                  extendedSharedFieldTypes = mapSet(extendedSharedFieldTypes, key, mapGetValue(variantsMeet, key));
                }
                const sharedDataType = new TS.TData(name, tVars, newVariantTypes, extendedSharedFieldTypes, l);
                const ctx8 = ctx7.setDataTypes(mapSet(ctx7.dataTypes, namet.key(), sharedDataType));
                return TCS.foldrFoldResult<A.Member, TypeMembers>((field, ctx, newSharedFieldTypes) =>
                  checkSharedField(field, initialSharedFieldTypes, appliedBranderType, ctx).bind((fieldType, ctx9) =>
                    new TCS.FoldResult(mapSet(newSharedFieldTypes, field.name, fieldType), ctx9)),
                fields, ctx8, new Map()).bind((newSharedFieldTypes, ctx9) => {
                  let finalSharedFieldTypes = newSharedFieldTypes;
                  for (const key of variantsMeet.keys()) {
                    finalSharedFieldTypes = mapSet(finalSharedFieldTypes, key, mapGetValue(variantsMeet, key));
                  }
                  const finalDataType = new TS.TData(name, tVars, newVariantTypes, finalSharedFieldTypes, l);
                  return ctx9.solveLevel().bind((solution, ctx10) => {
                    const solvedDataType = solution.applyDataType(finalDataType);
                    const ctx11 = ctx10.setDataTypes(mapSet(ctx10.dataTypes, namet.key(), solvedDataType));
                    return new TCS.FoldResult<A.LetrecBind[]>([dataTypeBind, ...newBindings], ctx11);
                  });
                });
              });
            })));
      });
    }
    default:
      return raise('Expected an s-data-expr');
  }
}

// Checks with-members on a variant
export function checkVariant(variant: A.Variant, variantType: TypeVariant, dataType: Type, tVars: Type[], context: Context): AnyFoldResult<TypeVariant> {
  const refinedType = new TS.TDataRefinement(
    tVars.length === 0 ? dataType : new TS.TApp(dataType, tVars, dataType.l, false),
    variant.name, dataType.l, false);

  return TCS.foldrFoldResult<A.Member, TypeMembers>((member, ctx, memberTypes) => {
    const memberType = mapGetValue(variantType.withFields, member.name);
    return toTypeMember(member, memberType, refinedType, true, ctx).bind((checkedMemberType, ctx2) =>
      new TCS.FoldResult(mapSet(memberTypes, member.name, checkedMemberType), ctx2));
  }, variant.withMembers, context, new Map()).bind((memberTypes, ctx) => {
    let newVariantType: TypeVariant;
    switch (variantType.$name) {
      case 't-variant':
        newVariantType = new TS.TVariant(variantType.name, variantType.fields, memberTypes, variantType.l);
        break;
      case 't-singleton-variant':
        newVariantType = new TS.TSingletonVariant(variantType.name, memberTypes, variantType.l);
        break;
      default:
        throw new InternalCompilerError('Unknown TypeVariant in check-variant');
    }
    return new TCS.FoldResult(newVariantType, ctx);
  });
}

export function checkSharedField(field: A.Member, fieldTypes: TypeMembers, dataType: Type, context: Context): AnyFoldResult<Type> {
  const fieldType = mapGetValue(fieldTypes, field.name);
  return toTypeMember(field, fieldType, dataType, true, context);
}

// doesn't check data-fields that aren't methods or functions
// only checks data-fields that are functions when type-check-functions is true
export function toTypeMember(member: A.Member, typ: Type, selfType: Type, typeCheckFunctions: boolean, context: Context): AnyFoldResult<Type> {
  const addSelfType = (funType: Type): Type => {
    switch (funType.$name) {
      case 't-arrow':
        return new TS.TArrow([selfType, ...funType.args], funType.ret, funType.l, funType.inferred);
      case 't-forall': {
        const ontoArrow = funType.onto;
        switch (ontoArrow.$name) {
          case 't-arrow':
            return new TS.TForall(funType.introduces, new TS.TArrow([selfType, ...ontoArrow.args], ontoArrow.ret, ontoArrow.l, ontoArrow.inferred), funType.l, funType.inferred);
          default:
            return raise("method type is not a function (this shouldn't happen");
        }
      }
      default:
        return raise("method type is not a function (this shouldn't happen");
    }
  };

  const removeSelfType = (funType: Type): Type => {
    switch (funType.$name) {
      case 't-arrow':
        return new TS.TArrow(funType.args.slice(1), funType.ret, funType.l, funType.inferred);
      case 't-forall': {
        const ontoArrow = funType.onto;
        switch (ontoArrow.$name) {
          case 't-arrow':
            return new TS.TForall(funType.introduces, new TS.TArrow(ontoArrow.args.slice(1), ontoArrow.ret, ontoArrow.l, ontoArrow.inferred), funType.l, funType.inferred);
          default:
            return raise("method type is not a function (this shouldn't happen");
        }
      }
      default:
        return raise("method type is not a function (this shouldn't happen");
    }
  };

  switch (member.$name) {
    case 's-data-field': {
      const value = member.value;
      switch (value.$name) {
        case 's-method': {
          const { l: mL, name: mName, params, args, ann, doc, body, _checkLoc, _check, blocky } = value;
          const newType = addSelfType(typ);
          return checkFun(mL, body, params, args, ann, newType,
            (args2, ann2, body2) => new A.SMethod(mL, mName, params, args2, ann2, doc, body2, _checkLoc, _check, blocky),
            context)
            .foldBind((_ast, outType, ctx) =>
              new TCS.FoldResult(removeSelfType(outType), ctx));
        }
        case 's-lam': {
          if (typeCheckFunctions) {
            return checking(value, typ, false, context)
              .foldBind((_newAst, newType, ctx) =>
                new TCS.FoldResult(newType, ctx));
          } else {
            return new TCS.FoldResult(typ, context);
          }
        }
        default:
          return new TCS.FoldResult(typ, context);
      }
    }
    case 's-method-field': {
      const { l: mL, name, params, args, ann, doc, body, _checkLoc, _check, blocky } = member;
      const newType = addSelfType(typ);
      return checkFun(mL, body, params, args, ann, newType,
        (args2, ann2, body2) => new A.SMethod(mL, name, params, args2, ann2, doc, body2, _checkLoc, _check, blocky),
        context)
        .foldBind((_ast, outType, ctx) =>
          new TCS.FoldResult(removeSelfType(outType), ctx));
    }
    case 's-mutable-field':
      return raise('Mutable fields not handled yet');
    default:
      throw new InternalCompilerError('Unknown Member in to-type-member');
  }
}

export function collectVariantConstructor(variant: A.Variant, context: Context): AnyFoldResult<TypeVariant> {
  switch (variant.$name) {
    case 's-variant': {
      const { l, name, members } = variant;
      const processMember = (member: A.VariantMember, ctx: Context): AnyFoldResult<Type> => {
        let wrap: (x: Type) => Type;
        switch (member.memberType.$name) {
          case 's-normal':
            wrap = (x) => x.setLoc(member.l);
            break;
          case 's-mutable':
            wrap = (x) => new TS.TRef(x.setLoc(member.l), member.l, false);
            break;
          default:
            throw new InternalCompilerError('Unknown VariantMemberType in collect-variant-constructor');
        }
        return toType(getField(member.bind, 'ann'), ctx).bind((maybeType, ctx2) => {
          if (maybeType === undefined) {
            return new TCS.FoldErrors<Type>([new C.CantTypecheck('No type annotation provided on member', l)]);
          } else {
            return new TCS.FoldResult(wrap(maybeType), ctx2);
          }
        });
      };

      return TCS.foldrFoldResult<A.VariantMember, TS.VariantField[]>((member, ctx, typeMembers) =>
        processMember(member, ctx)
          .bind((memberType, ctx2) =>
            new TCS.FoldResult([[getField(member.bind, 'id').toname(), memberType] as TS.VariantField, ...typeMembers], ctx2)),
      members, context, []).bind((typeMembers, ctx) =>
        new TCS.FoldResult<TypeVariant>(new TS.TVariant(name, typeMembers, new Map(), l), ctx));
    }
    case 's-singleton-variant':
      return new TCS.FoldResult<TypeVariant>(new TS.TSingletonVariant(variant.name, new Map(), variant.l), context);
    default:
      throw new InternalCompilerError('Unknown Variant in collect-variant-constructor');
  }
}

export function collectVariant(variant: A.Variant, context: Context): AnyFoldResult<TypeVariant> {
  switch (variant.$name) {
    case 's-variant': {
      const { l, name, members, withMembers } = variant;
      const processMember = (member: A.VariantMember, ctx: Context): AnyFoldResult<Type> => {
        let wrap: (x: Type) => Type;
        switch (member.memberType.$name) {
          case 's-normal':
            wrap = (x) => x.setLoc(member.l);
            break;
          case 's-mutable':
            wrap = (x) => new TS.TRef(x.setLoc(member.l), member.l, false);
            break;
          default:
            throw new InternalCompilerError('Unknown VariantMemberType in collect-variant');
        }
        return toType(getField(member.bind, 'ann'), ctx).bind((maybeType, ctx2) => {
          if (maybeType === undefined) {
            return new TCS.FoldErrors<Type>([new C.CantTypecheck('No type annotation provided on member', l)]);
          } else {
            return new TCS.FoldResult(wrap(maybeType), ctx2);
          }
        });
      };

      return TCS.foldrFoldResult<A.VariantMember, TS.VariantField[]>((member, ctx, typeMembers) =>
        processMember(member, ctx)
          .bind((memberType, ctx2) =>
            new TCS.FoldResult([[getField(member.bind, 'id').toname(), memberType] as TS.VariantField, ...typeMembers], ctx2)),
      members, context, []).bind((typeMembers, ctx) =>
        collectMembers(withMembers, true, ctx).bind((typeWithMembers, ctx2) => {
          const typeVariant = new TS.TVariant(name, typeMembers, typeWithMembers, l);
          return new TCS.FoldResult<TypeVariant>(typeVariant, ctx2);
        }));
    }
    case 's-singleton-variant': {
      const { l, name, withMembers } = variant;
      return collectMembers(withMembers, true, context).bind((typeWithMembers, ctx) => {
        const typeVariant = new TS.TSingletonVariant(name, typeWithMembers, l);
        return new TCS.FoldResult<TypeVariant>(typeVariant, ctx);
      });
    }
    default:
      throw new InternalCompilerError('Unknown Variant in collect-variant');
  }
}

export function mkConstructorType(variantTyp: TypeVariant, branderTyp: Type, params: Type[]): Type {
  const innerType: Type = params.length === 0
    ? branderTyp
    : new TS.TApp(branderTyp, params, variantTyp.l, false);
  const refinedType = new TS.TDataRefinement(innerType, variantTyp.name, variantTyp.l, false).setLoc(variantTyp.l);
  switch (variantTyp.$name) {
    case 't-variant': {
      const { fields, l } = variantTyp;
      const fieldTypes = fields.map(([_fieldName, fieldType]): Type => {
        switch (fieldType.$name) {
          case 't-ref':
            return fieldType.typ;
          default:
            return fieldType;
        }
      });
      if (params.length === 0) {
        return new TS.TArrow(fieldTypes, refinedType, l, false);
      } else {
        return new TS.TForall(params, new TS.TArrow(fieldTypes, refinedType, l, false), l, false);
      }
    }
    case 't-singleton-variant': {
      if (params.length === 0) {
        return refinedType;
      } else {
        return new TS.TForall(params, refinedType, variantTyp.l, false);
      }
    }
    default:
      throw new InternalCompilerError('Unknown TypeVariant in mk-constructor-type');
  }
}

// collect-functions: if true gather annotations from lambda terms
//                    else synthesize lambda terms
export function collectMembers(members: A.Member[], collectFunctions: boolean, context: Context): AnyFoldResult<TypeMembers> {
  return TCS.foldrFoldResult<A.Member, TypeMembers>((member, ctx, typeMembers) =>
    collectMember(member, collectFunctions, ctx)
      .bind((memberType, ctx2) =>
        new TCS.FoldResult(mapSet(typeMembers, member.name, memberType), ctx2)),
  members, context, new Map());
}

export function collectMember(member: A.Member, collectFunctions: boolean, context: Context): AnyFoldResult<Type> {
  switch (member.$name) {
    case 's-data-field': {
      const value = member.value;
      switch (value.$name) {
        case 's-method': {
          const { l: mL, params, args, ann } = value;
          if (args.length === 0) {
            return new TCS.FoldErrors<Type>([new C.MethodMissingSelf(value)]);
          } else {
            const rest = args.slice(1);
            return collectBindings(rest, context).bind((bindings, ctx) =>
              lamToType(bindings, mL, params, args.slice(1), ann, !collectFunctions, ctx));
          }
        }
        case 's-lam': {
          const { l: lL, params, args, ann } = value;
          if (collectFunctions) {
            return collectBindings(args, context).bind((bindings, ctx) =>
              lamToType(bindings, lL, params, args, ann, false, ctx));
          } else {
            return synthesis(value, true, context)
              .foldBind((_a, valueType, ctx) =>
                new TCS.FoldResult(valueType, ctx));
          }
        }
        default:
          return synthesis(value, false, context)
            .foldBind((_a, valueType, ctx) =>
              new TCS.FoldResult(valueType, ctx));
      }
    }
    case 's-method-field': {
      const { l, params, args, ann } = member;
      if (args.length === 0) {
        return new TCS.FoldErrors<Type>([new C.MethodMissingSelf(member as unknown as Expr)]);
      } else {
        const rest = args.slice(1);
        return collectBindings(rest, context).bind((bindings, ctx) =>
          lamToType(bindings, l, params, args.slice(1), ann, !collectFunctions, ctx));
      }
    }
    case 's-mutable-field':
      return raise('Type checker does not handle mutable fields yet');
    default:
      throw new InternalCompilerError('Unknown Member in collect-member');
  }
}

export function checkingCases(l: Loc, ann: A.Ann, val: Expr, branches: A.CasesBranch[], maybeElse: Expr | undefined, expectType: Type, context: Context): AnyTypingResult {
  return handleCases(l, ann, val, branches, maybeElse, expectType, context, checkingCasesHasElse(expectType), checkingCasesNoElse(expectType));
}

export function checkingCasesHasElse(expectType: Type) {
  return (l: Loc, ann: A.Ann, newVal: Expr, splitResult: [A.CasesBranch[], Type[]], _else: Expr, context: Context): AnyTypingResult => {
    return checking(_else, expectType, false, context).bind((newElse, newType, ctx) => {
      const newCases = new A.SCasesElse(l, ann, newVal, splitResult[0], newElse, false);
      return new TCS.TypingResult(newCases, newType, ctx);
    });
  };
}

export function checkingCasesNoElse(expectType: Type) {
  return (l: Loc, ann: A.Ann, newVal: Expr, splitResult: [A.CasesBranch[], Type[]], context: Context): AnyTypingResult => {
    const newCases = new A.SCases(l, ann, newVal, splitResult[0], false);
    return new TCS.TypingResult(newCases, expectType, context);
  };
}

export function synthesisCases(l: Loc, ann: A.Ann, val: Expr, branches: A.CasesBranch[], maybeElse: A.Expr | undefined, context: Context): AnyTypingResult {
  return handleCases(l, ann, val, branches, maybeElse, undefined, context, synthesisCasesHasElse, synthesisCasesNoElse)
    .mapType((t) => t.setLoc(l));
}

export function synthesisCasesHasElse(l: Loc, ann: A.Ann, newVal: A.Expr, splitResult: [A.CasesBranch[], Type[]], _else: A.Expr, context: Context): AnyTypingResult {
  return synthesis(_else, false, context).bind((newElse, elseType, ctx) => {
    const newCases = new A.SCasesElse(l, ann, newVal, splitResult[0], newElse, false);
    return meetBranchTypes([elseType, ...splitResult[1]], l, ctx).typingBind((branchesType, ctx2) =>
      new TCS.TypingResult(newCases, branchesType.setLoc(l), ctx2));
  });
}

export function synthesisCasesNoElse(l: Loc, ann: A.Ann, newVal: Expr, splitResult: [A.CasesBranch[], Type[]], context: Context): AnyTypingResult {
  const newCases = new A.SCases(l, ann, newVal, splitResult[0], false);
  return meetBranchTypes(splitResult[1], l, context).typingBind((branchesType, ctx) =>
    new TCS.TypingResult(newCases, branchesType.setLoc(l), ctx));
}

export function handleCases(
  l: Loc,
  ann: A.Ann,
  val: Expr,
  branches: A.CasesBranch[],
  maybeElse: Expr | undefined,
  maybeExpect: Type | undefined,
  context: Context,
  hasElse: (l: Loc, ann: A.Ann, newVal: Expr, splitResult: [A.CasesBranch[], Type[]], _else: Expr, context: Context) => AnyTypingResult,
  noElse: (l: Loc, ann: A.Ann, newVal: Expr, splitResult: [A.CasesBranch[], Type[]], context: Context) => AnyTypingResult,
): AnyTypingResult {
  return toType(ann, context).typingBind((maybeType, ctx0) => {
    if (maybeType !== undefined) {
      const typ = maybeType;
      const ctx1 = ctx0.addLevel();
      return addExistentialsToDataName(typ, ctx1).typingBind((casesType, ctx2) =>
        synthesis(val, false, ctx2).bind((newVal0, valType0, ctx3) => {
          const ctx4 = ctx3.addConstraint(valType0, casesType);
          return new TCS.TypingResult(newVal0, valType0, ctx4);
        }).solveBind().bind((newVal, valType, ctx5) =>
          TCS.instantiateDataType(valType, ctx5).typingBind((dataType, ctx6) => {
            const branchTracker = trackBranches(dataType);
            const tempResult = TCS.mapFoldResult((branch: A.CasesBranch, ctx: Context) => {
              let maybeKeyToUpdate: string | undefined;
              switch (val.$name) {
                case 's-id':
                  maybeKeyToUpdate = val.id.key();
                  break;
                case 's-id-var':
                  maybeKeyToUpdate = val.id.key();
                  break;
                case 's-id-letrec':
                  maybeKeyToUpdate = val.id.key();
                  break;
                default:
                  maybeKeyToUpdate = undefined;
              }
              let ctx7 = ctx;
              if (maybeKeyToUpdate !== undefined) {
                ctx7 = ctx7.addBinding(maybeKeyToUpdate, new TS.TDataRefinement(valType, branch.name, l, true));
              }
              const branchResult = handleBranch(dataType, l, branch, maybeExpect, branchTracker.remove, ctx7);
              return branchResult.bind((branchTypePair, ctx8) => {
                let ctx9 = ctx8;
                if (maybeKeyToUpdate !== undefined) {
                  ctx9 = ctx9.addBinding(maybeKeyToUpdate, valType);
                }
                return new TCS.FoldResult(branchTypePair, ctx9);
              });
            }, branches, ctx6);

            return tempResult.typingBind((result, ctx7) => {
              const splitResult = split(result);
              const remainingBranches = branchTracker.get();
              if (maybeElse !== undefined) {
                if (remainingBranches.length === 0) {
                  return new TCS.TypingError([new C.UnnecessaryElseBranch(typ.toString(), l)]);
                } else {
                  return hasElse(l, ann, newVal, splitResult, maybeElse, ctx7);
                }
              } else {
                if (remainingBranches.length === 0) {
                  return noElse(l, ann, newVal, splitResult, ctx7);
                } else {
                  // TODO(MATT): more appropriate error here
                  return new TCS.TypingError([new C.NonExhaustivePattern(remainingBranches, typ.toString(), l)]);
                }
              }
            });
          })));
    } else {
      return new TCS.TypingError([new C.CantTypecheck('Could not resolve type on cases statement', l)]);
    }
  });
}

export function handleBranch(dataType: DataType, casesLoc: A.Loc, branch: A.CasesBranch, maybeCheck: Type | undefined, remove: (tv: TypeVariant) => void, context: Context): AnyFoldResult<[A.CasesBranch, Type]> {
  const handleBody = (variant: TypeVariant, body: A.Expr, process: (newBody: Expr, typ: Type, context: Context) => AnyFoldResult<[A.CasesBranch, Type]>, ctx: Context): AnyFoldResult<[A.CasesBranch, Type]> => {
    remove(variant);
    if (maybeCheck !== undefined) {
      return checking(body, maybeCheck, false, ctx)
        .foldBind(process);
    } else {
      return synthesis(body, false, ctx)
        .foldBind(process);
    }
  };

  const tv = dataType.getVariant(branch.name);
  if (tv !== undefined) {
    switch (tv.$name) {
      case 't-variant': {
        const fields = tv.fields;
        switch (branch.$name) {
          case 's-cases-branch': {
            const { l, patLoc, name, args, body } = branch;
            const process = (newBody: Expr, typ: Type, ctx: Context): AnyFoldResult<[A.CasesBranch, Type]> => {
              const newBranch = new A.SCasesBranch(l, patLoc, name, args, newBody);
              return new TCS.FoldResult([newBranch, typ] as [A.CasesBranch, Type], ctx);
            };

            if (!(args.length === fields.length)) {
              return new TCS.FoldErrors<[A.CasesBranch, Type]>([new C.IncorrectNumberOfBindings(branch, tv)]);
            } else {
              const ctx0 = context.addLevel();
              return foldr2((foldContext: AnyFoldResult<undefined>, arg: A.CasesBind, field: TS.VariantField) =>
                foldContext.bind((_nothing, ctx) => {
                  const argType = field[1];
                  return toType(getField(arg.bind, 'ann'), ctx).bind((maybeType, ctx2) => {
                    if (maybeType !== undefined) {
                      const ctx3 = ctx2.addConstraint(argType, maybeType);
                      return new TCS.FoldResult<undefined>(undefined, ctx3.addBinding(getField<A.Name>(arg.bind, 'id').key(), maybeType));
                    } else {
                      return new TCS.FoldResult<undefined>(undefined, ctx2.addBinding(getField<A.Name>(arg.bind, 'id').key(), argType));
                    }
                  });
                }), new TCS.FoldResult<undefined>(undefined, ctx0), args, fields).bind((_nothing, ctx) =>
                ctx.solveLevel().bind((solution, ctx2) => {
                  const ctx3 = ctx2.substituteInBinds(solution);
                  return handleBody(tv, body, process, ctx3)
                    .bind((result, ctx4) => {
                      let ctx5 = ctx4;
                      for (let i = args.length - 1; i >= 0; i--) {
                        ctx5 = ctx5.removeBinding(getField(args[i].bind, 'id').key());
                      }
                      return new TCS.FoldResult(result, ctx5);
                    });
                }));
            }
          }
          case 's-singleton-cases-branch': {
            const { l, name } = branch;
            return new TCS.FoldErrors<[A.CasesBranch, Type]>([new C.CasesSingletonMismatch(name, l, false)]);
          }
          default:
            throw new InternalCompilerError('Unknown CasesBranch in handle-branch');
        }
      }
      case 't-singleton-variant': {
        switch (branch.$name) {
          case 's-cases-branch': {
            const { l, name } = branch;
            return new TCS.FoldErrors<[A.CasesBranch, Type]>([new C.CasesSingletonMismatch(name, l, true)]);
          }
          case 's-singleton-cases-branch': {
            const { l, patLoc, name, body } = branch;
            const process = (newBody: Expr, typ: Type, ctx: Context): AnyFoldResult<[A.CasesBranch, Type]> => {
              const newBranch = new A.SSingletonCasesBranch(l, patLoc, name, newBody);
              return new TCS.FoldResult([newBranch, typ] as [A.CasesBranch, Type], ctx);
            };
            return handleBody(tv, body, process, context);
          }
          default:
            throw new InternalCompilerError('Unknown CasesBranch in handle-branch');
        }
      }
      default:
        throw new InternalCompilerError('Unknown TypeVariant in handle-branch');
    }
  } else {
    return new TCS.FoldErrors<[A.CasesBranch, Type]>([new C.UnnecessaryBranch(branch, dataType, casesLoc)]);
  }
}

export function trackBranches(dataType: DataType): { remove: (b: TypeVariant) => void; get: () => TypeVariant[] } {
  // The Pyret original folds the variants into a (list-)set; within a single
  // data type variant names are unique, so removal by name is equivalent to
  // structural-equality removal, and to-list preserves declaration order.
  let unhandledBranches: TypeVariant[] = [...dataType.variants];
  return {
    remove: (b: TypeVariant): void => {
      unhandledBranches = unhandledBranches.filter((v) => v.name !== b.name);
    },
    get: (): TypeVariant[] => unhandledBranches,
  };
}

export function synthesisField(accessLoc: Loc, obj: Expr, objType: Type, fieldName: string, recreate: (l: Loc, obj: Expr, field: string) => Expr, context: Context): AnyTypingResult {
  return TCS.instantiateObjectType(objType, context).typingBind((objType2, ctx) => {
    switch (objType2.$name) {
      case 't-record': {
        const fieldTyp = objType2.fields.get(fieldName);
        if (fieldTyp !== undefined) {
          return new TCS.TypingResult(recreate(accessLoc, obj, fieldName), fieldTyp, ctx);
        } else {
          const synthesizedType = newExistential(accessLoc, false);
          const ctx2 = ctx.addVariable(synthesizedType)
            .addFieldConstraint(objType2, fieldName, synthesizedType);
          return new TCS.TypingResult(recreate(accessLoc, obj, fieldName), synthesizedType, ctx2);
        }
      }
      case 't-existential': {
        const synthesizedType = newExistential(accessLoc, false);
        const ctx2 = ctx.addVariable(synthesizedType)
          .addFieldConstraint(objType2, fieldName, synthesizedType);
        return new TCS.TypingResult(recreate(accessLoc, obj, fieldName), synthesizedType, ctx2);
      }
      default:
        return TCS.instantiateDataType(objType2, ctx).typingBind((dataType, ctx2) => {
          const fieldTyp = dataType.fields.get(fieldName);
          if (fieldTyp !== undefined) {
            return new TCS.TypingResult(recreate(accessLoc, obj, fieldName), fieldTyp, ctx2);
          } else {
            return new TCS.TypingError([new C.ObjectMissingField(fieldName, objType2.toString(), objType2.l, accessLoc)]);
          }
        });
    }
  });
}

export function synthesisAppFun(appLoc: Loc, _fun: Expr, args: Expr[], context: Context): AnyFoldResult<Type> {
  const chooseType = (methodName: string): AnyFoldResult<Type> => {
    // there should be two args here because its a binop
    const objExists = newExistential(args[0].l, false);
    const otherType = newExistential(args[1].l, false);
    const retType = newExistential(appLoc, false);
    const arrowType = new TS.TArrow([objExists, otherType], retType, appLoc, false);
    const ctx = context.addVariable(objExists).addVariable(otherType).addVariable(retType)
      .addFieldConstraint(objExists, methodName, new TS.TArrow([otherType], retType, appLoc, false));
    return new TCS.FoldResult<Type>(arrowType, ctx);
  };
  switch (_fun.$name) {
    case 's-id': {
      const id = _fun.id;
      const idIsGlobal = (s: string): boolean => A.isSGlobal(id) && id.s === s;
      if (idIsGlobal('_plus')) {
        return chooseType('_plus');
      } else if (idIsGlobal('_times')) {
        return chooseType('_times');
      } else if (idIsGlobal('_divide')) {
        return chooseType('_divide');
      } else if (idIsGlobal('_minus')) {
        return chooseType('_minus');
      } else if (idIsGlobal('_lessthan')) {
        return chooseType('_lessthan');
      } else if (idIsGlobal('_lessequal')) {
        return chooseType('_lessequal');
      } else if (idIsGlobal('_greaterthan')) {
        return chooseType('_greaterthan');
      } else if (idIsGlobal('_greaterequal')) {
        return chooseType('_greaterequal');
      } else {
        return synthesis(_fun, false, context).foldBind((_a, newType, ctx) =>
          new TCS.FoldResult(newType, ctx));
      }
    }
    default:
      return synthesis(_fun, false, context).foldBind((_a, newType, ctx) =>
        new TCS.FoldResult(newType, ctx));
  }
}

export function handleTypeLetBinds(bindings: A.TypeLetBind[], context: Context): AnyFoldResult<undefined[]> {
  return TCS.mapFoldResult((binding: A.TypeLetBind, ctx: Context): AnyFoldResult<undefined> => {
    switch (binding.$name) {
      case 's-type-bind': {
        const { l, name, params, ann } = binding;
        return toType(ann, ctx).bind((maybeTyp, ctx2) => {
          if (maybeTyp === undefined) {
            // TODO(MATT): is this correct?
            return new TCS.FoldErrors<undefined>([new C.UnboundTypeId(ann)]);
          } else {
            const typ = maybeTyp;
            let aliasType: Type;
            if (params.length === 0) {
              aliasType = typ;
            } else {
              const forall = params.map((param): Type => new TS.TVar(param, l, false));
              aliasType = new TS.TForall(forall, typ, l, false);
            }
            const ctx3 = ctx2.setAliases(mapSet(ctx2.aliases, name.key(), aliasType));
            return new TCS.FoldResult<undefined>(undefined, ctx3);
          }
        });
      }
      case 's-newtype-bind': {
        const { l, name, namet } = binding;
        const typ: Type = new TS.TName(TS.local, namet, l, false);
        const nametKey = namet.key();
        let ctx2 = ctx.setAliases(mapSet(ctx.aliases, name.key(), typ));
        const recordFields = new Map<string, Type>();
        recordFields.set('test', new TS.TArrow([typ], TS.tBoolean(l), l, false));
        recordFields.set('brand', new TS.TArrow([new TS.TTop(l, false)], typ, l, false));
        ctx2 = ctx2.addBinding(nametKey, new TS.TRecord(recordFields, l, false));
        return new TCS.FoldResult<undefined>(undefined, ctx2);
      }
      default:
        throw new InternalCompilerError('Unknown TypeLetBind in handle-type-let-binds');
    }
  }, bindings, context);
}

// type checks letrec bindings
// types body with the provided function
export function handleLetrecBindings(binds: A.LetrecBind[], topLevel: boolean, context0: Context, handleBody: (binds: A.LetrecBind[], context: Context) => AnyTypingResult): AnyTypingResult {
  const context = context0.addLevel();
  return collectLetrecBindings(binds, topLevel, context).typingBind((collected, ctx1) => {
    const [bindingsToType, collectedTypes] = collected.bindings;
    const dataBindings = collected.dataBindings;
    const ctx2 = ctx1.addDictToBindings(collectedTypes);
    return TCS.foldrFoldResult<[A.LetrecBind, A.LetrecBind[]], A.LetrecBind[]>((dataBinding, ctx, typedBindings) =>
      handleDatatype(dataBinding[0], dataBinding[1], ctx).bind((newBindings, ctx3) =>
        new TCS.FoldResult([...newBindings, ...typedBindings], ctx3)),
    dataBindings, ctx2, []).typingBind((newDataBinds, ctx3) => {
      const foldRhs = TCS.foldTyping((binding: A.LetrecBind, ctx: Context): AnyTypingResult => {
        switch (binding.$name) {
          case 's-letrec-bind': {
            const { l: l2, b, value } = binding;
            const expectedType = mapGetValue(collectedTypes, getField(b, 'id').key());
            const exampleEntry = getField(ctx.constraints, 'exampleTypes').get(expectedType.key());
            if (exampleEntry !== undefined) {
              const partialType = exampleEntry[1];
              testInferenceData = {
                name: getField(b, 'id'),
                argTypes: partialType.argTypes,
                retType: partialType.retType,
                loc: partialType.loc,
                existential: expectedType,
              };
              if (A.isSLam(value)) {
                const checkBlock = optValue(value._check);
                const result = checking(checkBlock, new TS.TTop(l2, false), false, ctx);
                testInferenceData = undefined;
                return result.bind((_a, resultType, ctx4) =>
                  new TCS.TypingResult(value, resultType, ctx4));
              } else {
                return raise('the right hand side should be a lambda');
              }
            } else {
              const miscEntry = ctx.misc.get(getField(b, 'id').key());
              if (miscEntry !== undefined) {
                miscTestInferenceData = getField(b, 'id');
              }

              let ctx4 = ctx.addLevel();
              const freeVars = expectedType.freeVariables();
              ctx4 = ctx4.addVariableSet(freeVars);
              const result = checking(value, expectedType, false, ctx4).bind((newAst, newType, ctx5) =>
                ctx5.solveLevel().typingBind((solution, ctx6) => {
                  const ctx7 = ctx6.substituteInBinds(solution);
                  const newType2 = solution.generalize(solution.apply(newType));
                  const ctx8 = ctx7.addBinding(getField(b, 'id').key(), newType2);
                  if (A.isSLam(value)) {
                    if (value._check !== undefined) {
                      const checkBlock = value._check;
                      return checking(checkBlock, new TS.TTop(optValue(value._checkLoc), false), false, ctx8).bind((_a, _t, ctx9) =>
                        new TCS.TypingResult(newAst, newType2, ctx9));
                    } else {
                      return new TCS.TypingResult(newAst, newType2, ctx8);
                    }
                  } else {
                    return new TCS.TypingResult(newAst, newType2, ctx8);
                  }
                }));

              miscTestInferenceData = undefined;
              return result;
            }
          }
          default:
            throw new InternalCompilerError('Unknown LetrecBind in handle-letrec-bindings');
        }
      }, bindingsToType, ctx3);
      return foldRhs.typingBind((newRhs, ctx4) => {
        const newBinds = map2((binding: A.LetrecBind, rhs: Expr): A.LetrecBind =>
          new A.SLetrecBind(binding.l, binding.b, rhs), bindingsToType, newRhs);
        const allNewBinds = [...newDataBinds, ...newBinds];
        return ctx4.solveLevel().typingBind((solution, ctx5) => {
          const ctx6 = ctx5.substituteInBinds(solution);
          return handleBody(allNewBinds, ctx6)
            .bind((newAst, newType, ctx7) => {
              let ctx8 = ctx7;
              for (let i = binds.length - 1; i >= 0; i--) {
                ctx8 = ctx8.removeBinding(getField(binds[i].b, 'id').key());
              }
              return new TCS.TypingResult(newAst, newType, ctx8);
            });
        });
      });
    });
  });
}

type CollectedLetrecBindings = {
  dataBindings: [A.LetrecBind, A.LetrecBind[]][];
  bindings: [A.LetrecBind[], Map<string, Type>];
};

// Separates out s-data-expr related bindings (which will always come first)
// Collects the annotated types of all other bindings
export function collectLetrecBindings(binds: A.LetrecBind[], topLevel: boolean, context: Context): AnyFoldResult<CollectedLetrecBindings> {
  const helper = (binds2: A.LetrecBind[], topLevel2: boolean, ctx: Context, dataBindings: [A.LetrecBind, A.LetrecBind[]][], bindings: [A.LetrecBind[], Map<string, Type>]): AnyFoldResult<CollectedLetrecBindings> => {
    if (binds2.length > 0) {
      const firstBind = binds2[0];
      const restBinds = binds2.slice(1);
      const firstValue = firstBind.value;
      switch (firstValue.$name) {
        case 's-data-expr': {
          const numDataBinds = (2 * firstValue.variants.length) + 1;
          const splitList = splitAt(numDataBinds, restBinds);
          const dataBinds = splitList.prefix;
          const remainingBinds = splitList.suffix;
          return helper(remainingBinds, topLevel2, ctx, [[firstBind, dataBinds] as [A.LetrecBind, A.LetrecBind[]], ...dataBindings], bindings);
        }
        default: {
          return collectBindings([firstBind.b], ctx).bind((collected, ctx2) => {
            const ctx3 = ctx2.addDictToBindings(collected);
            const initialType = mapGetValue(collected, getField(firstBind.b, 'id').key());
            let folded: AnyFoldResult<Map<string, Type>>;
            if (TS.isTExistential(initialType)) {
              switch (firstBind.value.$name) {
                case 's-lam': {
                  const lamExpr = firstBind.value;
                  const { l: lamL, params: lamParams, args: lamArgs, ann: lamAnn, _check } = lamExpr;
                  folded = collectBindings(lamArgs, ctx3).bind((argColl, ctx4) => {
                    let lamTypeFold: AnyFoldResult<Type>;
                    if (_check !== undefined) {
                      const checkBlock = _check;
                      lamTypeFold = lamToType(argColl, lamL, lamParams, lamArgs, lamAnn, false, ctx4).bind((lamType, ctx5) => {
                        const logPayload = '{'
                          + "'function-name': " + "'" + getField(firstBind.b, 'id').toname() + "'" + ','
                          + "'annotated-type': " + "'" + lamType.toString() + "'" + ','
                          + "'check-block': " + "'" + checkBlock.tosource().pretty(72).join('\n') + "'" + ','
                          + '}';
                        log('initial-test-inference-data', logPayload);

                        switch (lamType.$name) {
                          case 't-arrow': {
                            const { args: tempArgs, ret: tempRet, l: tempL } = lamType;
                            if (lamType.freeVariables().size > 0) {
                              const newExists = newExistential(tempL, true);
                              let ctx6 = ctx5.addVariable(newExists);
                              ctx6 = ctx6.addExampleVariable(newExists, tempArgs, tempRet, tempL,
                                (typ: Type, ctxx: Context) => checking(firstBind.value, typ, topLevel2, ctxx),
                                getField(firstBind.b, 'id').toname());
                              return new TCS.FoldResult<Type>(newExists, ctx6);
                            } else {
                              const ctx6 = ctx5.addMiscExampleVariable(getField(firstBind.b, 'id').key(), getField(firstBind.b, 'id').toname());
                              return new TCS.FoldResult<Type>(lamType, ctx6);
                            }
                          }
                          default: {
                            const ctx6 = ctx5.addMiscExampleVariable(getField(firstBind.b, 'id').key(), getField(firstBind.b, 'id').toname());
                            return lamToType(argColl, lamL, lamParams, lamArgs, lamAnn, topLevel2, ctx6);
                          }
                        }
                      });
                    } else {
                      lamTypeFold = lamToType(argColl, lamL, lamParams, lamArgs, lamAnn, topLevel2, ctx4);
                    }
                    return lamTypeFold.bind((lamType, ctx5) =>
                      new TCS.FoldResult(mapSet(collected, getField(firstBind.b, 'id').key(), lamType), ctx5));
                  });
                  break;
                }
                default:
                  folded = new TCS.FoldResult(collected, ctx3);
              }
            } else {
              folded = new TCS.FoldResult(collected, ctx3);
            }
            return folded.bind((collectedBindings, ctx4) => {
              const key = getField(firstBind.b, 'id').key();
              return helper(restBinds, topLevel2, ctx4, dataBindings,
                [[...bindings[0], firstBind], mapSet(bindings[1], key, mapGetValue(collectedBindings, key))]);
            });
          });
        }
      }
    } else {
      return new TCS.FoldResult<CollectedLetrecBindings>({ dataBindings: dataBindings, bindings: bindings }, ctx);
    }
  };
  return helper(binds, topLevel, context, [], [[], new Map()]);
}

// Collects the annotated bindings and produces an existential for bindings that aren't annotated
// The existential is added to the current level's variables
export function collectBindings(binds: A.Bind[], context: Context): AnyFoldResult<Map<string, Type>> {
  return TCS.foldrFoldResult<A.Bind, Map<string, Type>>((binding, ctx, dict) =>
    toType(getField(binding, 'ann'), ctx).bind((maybeType, ctx2) => {
      const newType = maybeType !== undefined
        ? maybeType.setLoc(binding.l)
        : newExistential(binding.l, true);
      const ctx3 = ctx2.addVariable(newType);
      return new TCS.FoldResult(mapSet(dict, getField(binding, 'id').key(), newType), ctx3);
    }), binds, context, new Map());
}

// adds any existentials generated to the current level's variables
export function lamToType(coll: Map<string, Type>, l: Loc, params: A.Name[], args: A.Bind[], retAnn: A.Ann, topLevel: boolean, context: Context): AnyFoldResult<Type> {
  return toType(retAnn, context).bind((maybeType, ctx) => {
    const retType = maybeType !== undefined ? maybeType : newExistential(l, true);
    const ctx2 = ctx.addVariable(retType);
    const foldArgTypes = TCS.mapFoldResult((arg: A.Bind, ctx3: Context): AnyFoldResult<Type> => {
      const argId = getField(arg, 'id');
      const argIsUnderscore = A.isSAtom(argId) ? argId.base === '$underscore' : false;
      const argType = mapGetValue(coll, argId.key());
      if (topLevel && TS.isTExistential(argType) && !argIsUnderscore) {
        return new TCS.FoldErrors<Type>([new C.ToplevelUnann(arg)]);
      } else {
        const ctx4 = ctx3.addVariable(argType);
        return new TCS.FoldResult(argType, ctx4);
      }
    }, args, ctx2);

    return foldArgTypes.bind((argTypes, ctx3) => {
      const arrowType = new TS.TArrow(argTypes, retType, l, false);
      if (params.length === 0) {
        return new TCS.FoldResult<Type>(arrowType, ctx3);
      } else {
        if (TS.isTExistential(retType)) {
          return new TCS.FoldErrors<Type>([new C.PolymorphicReturnTypeUnann(l)]);
        } else {
          const forall = params.map((param): Type => new TS.TVar(param, l, false));
          return new TCS.FoldResult<Type>(new TS.TForall(forall, arrowType, l, false), ctx3);
        }
      }
    });
  });
}

// TODO(MATT): this should not generalize the arguments
export function synthesisFun(l: Loc, body: Expr, params: A.Name[], args: A.Bind[], retAnn: A.Ann, recreate: (args: A.Bind[], ann: A.Ann, body: A.Expr) => A.Expr, topLevel: boolean, context0: Context): AnyTypingResult {
  const setRetType = (lamType: Type, retType: Type): Type => {
    switch (lamType.$name) {
      case 't-arrow':
        return new TS.TArrow(lamType.args, retType, lamType.l, lamType.inferred);
      case 't-forall': {
        const onto = lamType.onto;
        switch (onto.$name) {
          case 't-arrow':
            return new TS.TForall(lamType.introduces, new TS.TArrow(onto.args, retType, onto.l, onto.inferred), lamType.l, lamType.inferred);
          default:
            return raise("This shouldn't happen (non-function type lambda)");
        }
      }
      default:
        return raise("This shouldn't happen (non-function type lambda)");
    }
  };

  const context = context0.addLevel();
  const collected = collectBindings(args, context);
  return collected.typingBind((coll, ctx) => {
    const foldLamType = lamToType(coll, l, params, args, retAnn, topLevel, ctx);
    return foldLamType.typingBind((lamType, ctx2) => {
      let foldRetType: AnyFoldResult<Type>;
      switch (lamType.$name) {
        case 't-arrow':
          foldRetType = new TCS.FoldResult(lamType.ret, ctx2);
          break;
        case 't-forall': {
          const onto = lamType.onto;
          switch (onto.$name) {
            case 't-arrow':
              foldRetType = new TCS.FoldResult(onto.ret, ctx2);
              break;
            default:
              return raise("This shouldn't happen (non-function type lambda)");
          }
          break;
        }
        default:
          return raise("This shouldn't happen (non-function type lambda)");
      }
      return foldRetType.typingBind((retType, ctx3) =>
        checking(body, retType, false, ctx3.addDictToBindings(coll))
          .bind((newBody, newRetType, ctx4) =>
            new TCS.TypingResult(recreate(args, retAnn, newBody),
              setRetType(lamType, newRetType),
              ctx4)));
    });
  }).solveBind();
}

export function synthesisLetBind(binding: A.LetBind, context0: Context): AnyTypingResult {
  const context = context0.addLevel();
  const result = ((): AnyTypingResult => {
    switch (binding.$name) {
      case 's-let-bind': {
        const { l, b, value } = binding;
        return toType(getField(b, 'ann'), context).typingBind((maybeType, ctx) => {
          const annType = maybeType !== undefined ? maybeType : newExistential(l, true);
          const ctx2 = ctx.addVariable(annType);
          return checking(value, annType, false, ctx2)
            .bind((newValue, newType, ctx3) =>
              new TCS.TypingResult(newValue, newType, ctx3.addBinding(getField(b, 'id').key(), newType)));
        });
      }
      case 's-var-bind': {
        const { l, b, value } = binding;
        return toType(getField(b, 'ann'), context).typingBind((maybeType, ctx) => {
          const annType = maybeType !== undefined ? maybeType : newExistential(l, true);
          const ctx2 = ctx.addVariable(annType);
          return checking(value, annType, false, ctx2)
            .bind((newValue, newType, ctx3) =>
              new TCS.TypingResult(newValue, new TS.TRef(newType, l, false), ctx3.addBinding(getField(b, 'id').key(), new TS.TRef(newType, l, false))));
        });
      }
      default:
        throw new InternalCompilerError('Unknown LetBind in synthesis-let-bind');
    }
  })();
  return result.solveBind();
}

export function synthesisExtend(updateLoc: Loc, obj: Expr, objType: Type, fields: A.Member[], context: Context): AnyTypingResult {
  return collectMembers(fields, false, context).typingBind((newMembers, ctx) =>
    TCS.instantiateObjectType(objType, ctx).typingBind((objType2, ctx2) => {
      switch (objType2.$name) {
        case 't-record': {
          let finalFields = objType2.fields;
          for (const key of newMembers.keys()) {
            finalFields = mapSet(finalFields, key, mapGetValue(newMembers, key));
          }
          return new TCS.TypingResult(new A.SExtend(updateLoc, obj, fields), new TS.TRecord(finalFields, updateLoc, objType2.inferred), ctx2);
        }
        case 't-existential':
          return new TCS.TypingError([new C.UnableToInfer(objType2.l)]);
        default:
          return new TCS.TypingError([new C.IncorrectTypeExpression(objType2.toString(), objType2.l, 'an object type', updateLoc, obj)]);
      }
    }));
}

export function synthesisUpdate(updateLoc: Loc, obj: Expr, objType: Type, fields: A.Member[], context: Context): AnyTypingResult {
  return TCS.instantiateObjectType(objType, context).typingBind((objType2, ctx) => {
    switch (objType2.$name) {
      case 't-record': {
        const tFields = objType2.fields;
        return TCS.foldrFoldResult<A.Member, A.Member[]>((field, ctx2, _newFields) => {
          const oldType = tFields.get(field.name);
          if (oldType === undefined) {
            return new TCS.FoldErrors<A.Member[]>([new C.ObjectMissingField(field.name, objType2.toString(), objType2.l, updateLoc)]);
          } else {
            switch (oldType.$name) {
              case 't-ref':
                return checking(getField(field, 'value'), oldType.typ, false, ctx2).foldBind((newValue, _t, ctx3) =>
                  new TCS.FoldResult([new A.SDataField(field.l, field.name, newValue), ...fields], ctx3));
              default:
                return new TCS.FoldErrors<A.Member[]>([new C.IncorrectType(oldType.toString(), oldType.l, new TS.TRef(oldType, updateLoc, false).toString(), updateLoc)]);
            }
          }
        }, fields, ctx, []).typingBind((finalFields, ctx2) =>
          new TCS.TypingResult(new A.SUpdate(updateLoc, obj, finalFields), objType2, ctx2));
      }
      case 't-existential':
        return new TCS.TypingError([new C.UnableToInfer(objType2.l)]);
      default: {
        return TCS.instantiateDataType(objType2, ctx).typingBind((dataType, ctx2) =>
          TCS.foldrFoldResult<A.Member, A.Member[]>((field, ctx3, _newFields) => {
            const oldType = dataType.fields.get(field.name);
            if (oldType === undefined) {
              return new TCS.FoldErrors<A.Member[]>([new C.ObjectMissingField(field.name, objType2.toString(), objType2.l, updateLoc)]);
            } else {
              switch (oldType.$name) {
                case 't-ref':
                  return checking(getField(field, 'value'), oldType.typ, false, ctx3).foldBind((newValue, _t, ctx4) =>
                    new TCS.FoldResult([new A.SDataField(field.l, field.name, newValue), ...fields], ctx4));
                default:
                  return new TCS.FoldErrors<A.Member[]>([new C.IncorrectType(oldType.toString(), oldType.l, new TS.TRef(oldType, updateLoc, false).toString(), updateLoc)]);
              }
            }
          }, fields, ctx2, []).typingBind((finalFields, ctx3) =>
            new TCS.TypingResult(new A.SUpdate(updateLoc, obj, finalFields), objType2, ctx3)));
        // typing-error([list: C.incorrect-type-expression(tostring(obj-type), obj-type.l, "an object type", update-loc, obj)])
      }
    }
  });
}

// TODO(MATT): this might be totally broken
export function checkFun(funLoc: Loc, body: Expr, params: A.Name[], args: A.Bind[], retAnn: A.Ann, expectType: Type, recreate: (args: A.Bind[], ann: A.Ann, body: Expr) => Expr, context0: Context): AnyTypingResult {
  const context = context0.addLevel();
  const lamBindings = collectBindings(args, context);
  // TODO(MATT): checking when polymorphic lambda but non-polymorphic type

  const result = ((): AnyTypingResult => {
    switch (expectType.$name) {
      case 't-arrow': {
        const expectArgs = expectType.args;
        const retType = expectType.ret;
        return lamBindings.typingBind((tempLamBinds0, ctx) => {
          if (!(tempLamBinds0.size === expectArgs.length)) {
            const expected = 'a function with ' + String(expectArgs.length) + ' arguments';
            const found = 'a function with ' + String(args.length) + ' arguments';
            return new TCS.TypingError([new C.IncorrectType(expected, funLoc, found, expectType.l)]);
          } else {
            const tempLamBinds = foldr2((lamBinds0: Map<string, Type>, arg: A.Bind, expectArgType: Type) => {
              const key = getField(arg, 'id').key();
              const boundType = mapGetValue(lamBinds0, key);
              if (TS.isTExistential(boundType)) {
                return mapSet(lamBinds0, key, expectArgType);
              } else {
                return lamBinds0;
              }
            }, tempLamBinds0, args, expectArgs);
            // params.foldr over {lam-binds; context}
            let lamBinds = tempLamBinds;
            let ctx2 = ctx;
            for (let i = params.length - 1; i >= 0; i--) {
              const param = params[i];
              const newExists = newExistential(funLoc, false);
              const oldBinds = lamBinds;
              const newBinds = new Map(oldBinds);
              for (const key of oldBinds.keys()) {
                newBinds.set(key, mapGetValue(oldBinds, key).substitute(newExists, new TS.TVar(param, funLoc, false)));
              }
              lamBinds = newBinds;
              ctx2 = ctx2.addVariable(newExists);
            }
            const lamArgTypes = args.map((arg) => mapGetValue(lamBinds, getField(arg, 'id').key()));
            const ctx3 = foldr2((ctxAcc: Context, lamArgType: Type, expectArgType: Type) =>
              ctxAcc.addConstraint(lamArgType, expectArgType), ctx2.addDictToBindings(lamBinds), lamArgTypes, expectArgs);
            const bodyResult = checking(body, retType, false, ctx3);
            return bodyResult.bind((newBody, _newRetType, ctx4) =>
              new TCS.TypingResult(recreate(args, retAnn, newBody), expectType, ctx4));
          }
        });
      }
      case 't-forall': {
        const { introduces, onto, l, inferred } = expectType;
        return checkFun(funLoc, body, params, args, retAnn, onto, recreate, context)
          .mapType((t) => new TS.TForall(introduces, t, l, inferred));
      }
      case 't-existential':
        return checkSynthesis(recreate(args, retAnn, body), expectType, false, context);
      case 't-app': {
        const foldOnto = TCS.introduceOnto(expectType, context);
        return foldOnto.typingBind((onto, ctx) =>
          checkFun(funLoc, body, params, args, retAnn, onto, recreate, ctx));
      }
      case 't-top':
        return lamBindings.typingBind((newBinds, ctx) => {
          const bodyResult = checking(body, expectType, false, ctx.addDictToBindings(newBinds));
          return bodyResult.bind((newBody, _newType, ctx2) =>
            new TCS.TypingResult(recreate(args, retAnn, newBody), expectType, ctx2));
        });
      default:
        return new TCS.TypingError([new C.IncorrectType(expectType.toString(), expectType.l, 'a function', funLoc)]);
    }
  })();
  return result.solveBind();
}

// TODO(MATT): this might be totally broken
// generalization can flip the order of variables
export function synthesisInstantiation(l: Loc, expr: Expr, params: A.Ann[], topLevel: boolean, context: Context): AnyTypingResult {
  return synthesis(expr, topLevel, context).bind((newExpr, tmpType0, ctx) => {
    let tmpType = tmpType0;
    if (TCS.isConstraintSystem(ctx.constraints)) {
      const tmpSolution = new TCS.ConstraintSolution(ctx.constraints.variables, new Map());
      tmpType = tmpSolution.generalize(tmpType);
    }
    switch (tmpType.$name) {
      case 't-forall': {
        const introduces = tmpType.introduces;
        const onto = tmpType.onto;
        return TCS.mapFoldResult(toType, params, ctx).typingBind((newMaybeTypes, ctx2) => {
          let maybeNewTypes: Type[] | undefined = [];
          for (let i = newMaybeTypes.length - 1; i >= 0; i--) {
            const maybeType = newMaybeTypes[i];
            const newTypesAcc = maybeNewTypes;
            maybeNewTypes = optionBind((typ: Type) =>
              optionBind((listTypes: Type[]): Type[] => [typ, ...listTypes], newTypesAcc), maybeType);
          }
          if (maybeNewTypes === undefined) {
            return new TCS.TypingError([new C.CantTypecheck('Failure to determine types of forall', l)]);
          } else {
            const newTypes = maybeNewTypes;
            if (!(newTypes.length === introduces.length)) {
              return new TCS.TypingError([new C.CantTypecheck('Expected ' + String(introduces.length) + ' type arguments, but got ' + String(newTypes.length) + ' arguments.', l)]);
            } else {
              const newType = foldr2((curr: Type, variable: Type, replacement: Type) =>
                curr.substitute(replacement, variable), onto, introduces, newTypes);
              const newInst = new A.SInstantiate(l, newExpr, params);
              return new TCS.TypingResult(newInst, newType.setLoc(l), ctx2);
            }
          }
        });
      }
      case 't-existential':
        return new TCS.TypingError([new C.UnableToInfer(tmpType.l)]);
      default:
        return new TCS.TypingError([new C.IncorrectType(tmpType.toString(), tmpType.l, 'a polymorphic type', l)]);
    }
  });
}

export function handleIfBranch(branch: A.IfBranch, context: Context): AnyFoldResult<[A.IfBranch, Type]> {
  return checking(branch.test, TS.tBoolean(branch.l), false, context).foldBind(
    (newTest, _t, ctx) =>
      synthesis(branch.body, false, ctx).foldBind(
        (newBody, bodyType, ctx2) => {
          const newBranch = new A.SIfBranch(branch.l, newTest, newBody);
          return new TCS.FoldResult([newBranch, bodyType] as [A.IfBranch, Type], ctx2);
        }));
}

export function synthesisTupleIndex(accessLoc: Loc, tup: Expr, tupTypeLoc: Loc, tupType: Type, index: number, recreate: (l: Loc, tup: Expr, index: number) => Expr, context: Context): AnyTypingResult {
  const nonTupErr = new TCS.TypingError([new C.IncorrectType(tupType.toString(), tupTypeLoc, 'a tuple type', accessLoc)]);
  return tupleView(accessLoc, tupTypeLoc, tupType,
    (l, maybeTupMembers) => {
      if (maybeTupMembers !== undefined) {
        const tupMembers = maybeTupMembers;
        if (index >= tupMembers.length) {
          return new TCS.TypingError([new C.TupleTooSmall(index, tupMembers.length, '{' + tupMembers.map((t) => t.toString()).join('; ') + '}', l, accessLoc)]);
        } else {
          return new TCS.TypingResult(recreate(l, tup, index), tupMembers[index], context);
        }
      } else {
        return nonTupErr;
        // TODO(MATT): decide about this
      }
    }, context);
}

export function tupleView(accessLoc: Loc, tupTypeLoc: Loc, tupType: Type,
  handle: (l: Loc, maybeTupMembers: Type[] | undefined) => AnyTypingResult,
  context: Context): AnyTypingResult {
  const nonTupErr = new TCS.TypingError([new C.IncorrectType(tupType.toString(), tupTypeLoc, 'a tuple type', accessLoc)]);
  switch (tupType.$name) {
    case 't-tuple':
      return handle(tupTypeLoc, tupType.elts);
    case 't-forall': {
      const { introduces, onto } = tupType;
      const newExistentials = introduces.map((aVar) => newExistential(aVar.l, false));
      const newTupType = foldr2((newOnto: Type, aVar: Type, aExists: Type) =>
        newOnto.substitute(aExists, aVar), onto, introduces, newExistentials);
      const ctx = context.addVariableSet(listToTypeSet(newExistentials));
      return tupleView(accessLoc, tupTypeLoc, newTupType, handle, ctx);
    }
    case 't-existential':
      return new TCS.TypingError([new C.UnableToInfer(tupType.l)]);
    default:
      return nonTupErr;
  }
}

export function meetBranchTypes(branchTypes: Type[], loc: Loc, context: Context): AnyFoldResult<Type> {
  const newExists = newExistential(loc, false);
  let ctx = context.addLevel().addVariable(newExists);
  for (let i = branchTypes.length - 1; i >= 0; i--) {
    ctx = ctx.addConstraint(branchTypes[i], newExists);
  }
  return ctx.solveLevel().bind((solution, ctx2) => {
    const meetType = solution.generalize(solution.apply(newExists));
    return new TCS.FoldResult(meetType, ctx2);
  });
}

// Adds constraints between methods with the same name across all variants
export function mergeCommonFields(variants: TypeVariant[], dataLoc: Loc, context: Context): Context {
  const getInAll = (fieldName: string, members: TypeMembers[]): { fieldName: string; types: Type[] } | undefined => {
    let acc: { fieldName: string; types: Type[] } | undefined = { fieldName: fieldName, types: [] };
    for (const member of members) {
      acc = optionBind((fieldTypes: { fieldName: string; types: Type[] }) =>
        optionBind((memberFieldType: Type) =>
          ({ fieldName: fieldName, types: [memberFieldType, ...fieldTypes.types] }), member.get(fieldName)), acc);
    }
    return acc;
  };

  let fieldsToMerge: { fieldName: string; types: Type[] }[];
  if (variants.length === 0) {
    fieldsToMerge = [];
  } else {
    const withFields = variants.map((variant) => variant.withFields);
    const first = variants[0];
    fieldsToMerge = [...first.withFields.keys()]
      .map((fieldName) => getInAll(fieldName, withFields))
      .filter((x): x is { fieldName: string; types: Type[] } => x !== undefined);
  }
  let ctx = context;
  for (let i = fieldsToMerge.length - 1; i >= 0; i--) {
    const fieldAndTypes = fieldsToMerge[i];
    const mergeExistential = newExistential(dataLoc, false);
    let ctx2 = ctx.addVariable(mergeExistential);
    for (let j = fieldAndTypes.types.length - 1; j >= 0; j--) {
      ctx2 = ctx2.addConstraint(mergeExistential, fieldAndTypes.types[j]);
    }
    ctx = ctx2;
  }
  return ctx;
}

export function meetFields(aFields: TypeMembers, bFields: TypeMembers, loc: Loc, context: Context): TypeMembers {
  const introduce = (typ: Type, tempContext: Context): [Type, Context] => {
    switch (typ.$name) {
      case 't-forall': {
        const { introduces, onto } = typ;
        const newExistentials = introduces.map((aVar) => newExistential(aVar.l, false));
        const newOnto = foldr2((acc: Type, aVar: Type, aExists: Type) =>
          acc.substitute(aExists, aVar), onto, introduces, newExistentials);
        return [newOnto, tempContext.addVariableSet(listToTypeSet(newExistentials))];
      }
      default:
        return [typ, tempContext];
    }
  };

  const meetMembers: TypeMembers = new Map();
  for (const aFieldName of aFields.keys()) {
    const bType0 = bFields.get(aFieldName);
    if (bType0 === undefined) {
      continue;
    }
    const aType0 = mapGetValue(aFields, aFieldName);
    const tempExistential = newExistential(loc, false);
    const tempContext0 = context.addLevel().addVariable(tempExistential);
    const [aType, tempContext1] = introduce(aType0, tempContext0);
    const [bType, tempContext2] = introduce(bType0, tempContext1);
    const tempContext3 = tempContext2.addConstraint(tempExistential, aType).addConstraint(tempExistential, bType);
    const foldSolution = tempContext3.solveLevel();
    switch (foldSolution.$name) {
      case 'fold-errors':
        break;
      case 'fold-result': {
        const solution = foldSolution.v;
        const meetType = solution.generalize(solution.apply(tempExistential));
        meetMembers.set(aFieldName, meetType);
        break;
      }
      default:
        throw new InternalCompilerError('Unknown FoldResult in meet-fields');
    }
  }
  return meetMembers;
}

export function gatherProvides(_provide: A.ProvideBlock, context: Context): AnyFoldResult<TCInfo> {
  switch (_provide.$name) {
    case 's-provide-block': {
      const provideSpecs = _provide.specs;
      const initialInfo = new TCS.TCInfo(new Map(), context.info.aliases, context.info.dataTypes);
      return TCS.foldrFoldResult<A.ProvideSpec, TCInfo>((spec, ctx, info) => {
        switch (spec.$name) {
          case 's-provide-name': {
            const nameSpec = spec.nameSpec;
            switch (nameSpec.$name) {
              case 's-local-ref': {
                const valueKey = nameSpec.name.key();
                if (info.types.has(valueKey)) {
                  return new TCS.FoldResult(info, ctx);
                } else {
                  // MARK(joe): test as-name here; it appears unused
                  const typ0 = ctx.info.types.get(valueKey);
                  if (typ0 !== undefined) {
                    const typ = typ0.setInferred(false);
                    return new TCS.FoldResult(new TCS.TCInfo(mapSet(info.types, valueKey, typ), info.aliases, info.dataTypes), ctx);
                  } else {
                    const typ = mapGetValue(ctx.globalTypes, valueKey).setInferred(false);
                    return new TCS.FoldResult(new TCS.TCInfo(mapSet(info.types, valueKey, typ), info.aliases, info.dataTypes), ctx);
                  }
                }
              }
              case 's-remote-ref':
                return new TCS.FoldResult(info, ctx);
              default:
                throw new InternalCompilerError('No cases matched on NameSpec in gather-provides');
            }
          }
          case 's-provide-type': {
            const nameSpec = spec.nameSpec;
            switch (nameSpec.$name) {
              case 's-local-ref': {
                const aliasKey = nameSpec.name.key();
                if (info.aliases.has(aliasKey)) {
                  return new TCS.FoldResult(info, ctx);
                } else {
                  const typ = mapGetValue(ctx.aliases, aliasKey);
                  return new TCS.FoldResult(new TCS.TCInfo(info.types, mapSet(info.aliases, aliasKey, typ), info.dataTypes), ctx);
                }
              }
              case 's-remote-ref':
                return new TCS.FoldResult(info, ctx);
              default:
                throw new InternalCompilerError('No cases matched on NameSpec in gather-provides');
            }
          }
          case 's-provide-module':
            return new TCS.FoldResult(info, ctx);
          case 's-provide-data': {
            const nameSpec = spec.nameSpec;
            switch (nameSpec.$name) {
              case 's-local-ref': {
                const dataKey = nameSpec.name.key();
                if (info.dataTypes.has(dataKey)) {
                  return new TCS.FoldResult(info, ctx);
                } else {
                  const typ = mapGetValue(ctx.dataTypes, dataKey);
                  return new TCS.FoldResult(new TCS.TCInfo(info.types, info.aliases, mapSet(info.dataTypes, dataKey, typ as unknown as Type)), ctx);
                }
              }
              case 's-remote-ref':
                return new TCS.FoldResult(info, ctx);
              default:
                throw new InternalCompilerError('No cases matched on NameSpec in gather-provides');
            }
          }
          default:
            throw new InternalCompilerError('Unknown ProvideSpec in gather-provides');
        }
      }, provideSpecs, context, initialInfo);
    }
    default:
      return raise('By type-check time, all provides should be resolved to a provide-block');
  }
}

export function toType(inAnn: A.Ann, context: Context): AnyFoldResult<Type | undefined> {
  switch (inAnn.$name) {
    case 'a-blank':
      return new TCS.FoldResult<Type | undefined>(undefined, context);
    case 'a-any':
      return new TCS.FoldResult<Type | undefined>(new TS.TTop(inAnn.l, false), context);
    case 'a-name': {
      const { l, id } = inAnn;
      const typ = context.aliases.get(id.key());
      if (typ !== undefined) {
        const resultType = TCS.resolveAlias(typ, context).setLoc(l);
        return new TCS.FoldResult<Type | undefined>(resultType, context);
      } else {
        return new TCS.FoldErrors<Type | undefined>([new C.UnboundTypeId(inAnn)]);
      }
    }
    case 'a-type-var': {
      const { l, id } = inAnn;
      return new TCS.FoldResult<Type | undefined>(new TS.TVar(id, l, false), context);
    }
    case 'a-arrow-argnames': {
      const { l, args, ret, useParens } = inAnn;
      return toType(new A.AArrow(l, args.map((a) => a.ann), ret, useParens), context);
    }
    case 'a-arrow': {
      const { l, args, ret } = inAnn;
      const foldArgTyps = TCS.mapFoldResult((arg: A.Ann, ctx: Context): AnyFoldResult<Type> =>
        toType(arg, ctx).bind((maybeNewTyp, ctx2) => {
          if (maybeNewTyp === undefined) {
            return new TCS.FoldErrors<Type>([new C.CantTypecheck('no annotation provided on ' + toRepr(arg), l)]);
          } else {
            return new TCS.FoldResult(maybeNewTyp, ctx2);
          }
        }), args, context);

      return foldArgTyps.bind((argTyps, ctx) =>
        toType(ret, ctx).bind((maybeRetTyp, ctx2) => {
          if (maybeRetTyp === undefined) {
            return new TCS.FoldErrors<Type | undefined>([new C.CantTypecheck('no annotation provided on ' + toRepr(ret), l)]);
          } else {
            return new TCS.FoldResult<Type | undefined>(new TS.TArrow(argTyps, maybeRetTyp, l, false), ctx2);
          }
        }));
    }
    case 'a-method':
      return new TCS.FoldErrors<Type | undefined>([new C.CantTypecheck('a-method not yet implemented', inAnn.l)]);
    case 'a-record': {
      const { l, fields } = inAnn;
      const fieldsResult = TCS.foldrFoldResult<A.AField, TypeMembers>((field, ctx, fieldsDict) =>
        toType(field.ann, ctx).bind((maybeTyp, ctx2) => {
          if (maybeTyp === undefined) {
            return new TCS.FoldErrors<TypeMembers>([new C.CantTypecheck('no annotation provided on ' + toRepr(field), l)]);
          } else {
            return new TCS.FoldResult(mapSet(fieldsDict, field.name, maybeTyp), ctx2);
          }
        }), fields, context, new Map());

      return fieldsResult.bind((members, ctx) =>
        new TCS.FoldResult<Type | undefined>(new TS.TRecord(members, l, false), ctx));
    }
    case 'a-tuple': {
      const { l, fields: elts } = inAnn;
      const foldEltTyps = TCS.mapFoldResult((elt: A.Ann, ctx: Context): AnyFoldResult<Type> =>
        toType(elt, ctx).bind((maybeNewTyp, ctx2) => {
          if (maybeNewTyp === undefined) {
            const newExists = newExistential(l, true);
            const ctx3 = ctx2.addVariable(newExists);
            return new TCS.FoldResult<Type>(newExists, ctx3);
          } else {
            return new TCS.FoldResult<Type>(maybeNewTyp, ctx2);
          }
        }), elts, context);
      return foldEltTyps.bind((newElts, ctx) =>
        new TCS.FoldResult<Type | undefined>(new TS.TTuple(newElts, l, false), ctx));
    }
    case 'a-app': {
      const { l, ann, args } = inAnn;
      return toType(ann, context).bind((maybeTyp, ctx) => {
        if (maybeTyp === undefined) {
          return new TCS.FoldErrors<Type | undefined>([new C.CantTypecheck('no annotation provided on ' + toRepr(ann), l)]);
        } else {
          const typ = maybeTyp;
          const argsResult = TCS.mapFoldResult((arg: A.Ann, ctx2: Context) => toType(arg, ctx2), args, ctx);
          return argsResult.bind((maybeArgTypes, ctx2) => {
            const foldArgTyps = TCS.mapFoldResult((maybeArgTyp: Type | undefined, ctx3: Context): AnyFoldResult<Type> => {
              if (maybeArgTyp === undefined) {
                return new TCS.FoldErrors<Type>([new C.CantTypecheck('no annotation provided on app argument', l)]);
              } else {
                return new TCS.FoldResult(maybeArgTyp, ctx3);
              }
            }, maybeArgTypes, ctx2);
            return foldArgTyps.bind((argTyps, ctx3) => {
              const appType = new TS.TApp(typ, argTyps, l, false);
              const resolvedType = TCS.resolveAlias(appType, ctx3);
              return new TCS.FoldResult<Type | undefined>(resolvedType, ctx3);
            });
          });
        }
      });
    }
    case 'a-pred': {
      const { l, ann, exp } = inAnn;
      return toType(ann, context).bind((maybeTyp, ctx) => {
        if (maybeTyp !== undefined) {
          const typ = maybeTyp;
          const expectType = new TS.TArrow([typ], TS.tBoolean(l), l, false);
          return checking(exp, expectType, false, ctx).foldBind((_a, _t, ctx2) =>
            new TCS.FoldResult<Type | undefined>(typ, ctx2));
        } else {
          return new TCS.FoldErrors<Type | undefined>([new C.CantTypecheck('missing annotation on ' + toRepr(ann), l)]);
        }
      });
    }
    case 'a-dot': {
      const { l, obj, field } = inAnn;
      const key = obj.key();
      const origin = context.moduleNames.get(key);
      if (origin === undefined) {
        return new TCS.FoldErrors<Type | undefined>([new C.NoModule(l, obj.toname())]);
      } else {
        const tMod = mapGetValue(context.modules, origin);
        if (tMod.aliases.has(field)) {
          const typ = TCS.resolveAlias(mapGetValue(tMod.aliases, field), context);
          return new TCS.FoldResult<Type | undefined>(typ, context);
        } else {
          return new TCS.FoldErrors<Type | undefined>([new C.UnboundTypeId(inAnn)]);
        }
      }
    }
    case 'a-checked':
      return new TCS.FoldErrors<Type | undefined>([new C.CantTypecheck('a-checked should not be appearing before type checking', A.dummyLoc)]);
    default:
      throw new InternalCompilerError('Unknown Ann in to-type');
  }
}

// NOTE: ignore-checker is inlined in the s-scope-block cases above.

export function synthesisSCheckTest(e: Expr, loc: Loc, op: A.CheckOp, refinement: Expr | undefined, left: Expr, right: Expr | undefined, cause: Expr | undefined, context: Context): AnyTypingResult {
  void cause;
  const createResult = (ctx: Context): AnyTypingResult => {
    const resultType = newExistential(loc, false);
    const ctx2 = ctx.addVariable(resultType);
    return new TCS.TypingResult(e, resultType, ctx2);
  };

  const synthesisEquivalent = (_l: Loc): AnyTypingResult => {
    if (right !== undefined) {
      const rhs = right;
      return synthesis(left, false, context).bind((_a, leftType, ctx) =>
        synthesis(rhs, false, ctx).bind((_b, rightType, ctx2) => {
          const ctx3 = ctx2.addConstraint(leftType, rightType);
          return createResult(ctx3);
        }));
    } else {
      return raise('Expected test to have a right hand side');
    }
  };

  const synthesisRefinement = (l: Loc): AnyTypingResult => {
    if (refinement !== undefined) {
      const refine = refinement;
      if (right !== undefined) {
        const rhs = right;
        return synthesis(left, false, context).bind((_a, leftType, ctx) =>
          synthesis(rhs, false, ctx).bind((_b, rightType, ctx2) =>
            synthesis(refine, false, ctx2).bind((_c, refinementType, ctx3) => {
              const ctx4 = ctx3.addConstraint(refinementType, new TS.TArrow([leftType, rightType], TS.tBoolean(loc), l, false));
              return createResult(ctx4);
            })));
      } else {
        return raise('Expected test to have a right hand side');
      }
    } else {
      return synthesisEquivalent(l);
    }
  };

  const synthesisPredicate = (l: Loc): AnyTypingResult => {
    if (right !== undefined) {
      const rhs = right;
      return synthesis(left, false, context).bind((_a, leftType, ctx) =>
        synthesis(rhs, false, ctx).bind((_b, predType, ctx2) => {
          const ctx3 = ctx2.addConstraint(predType, new TS.TArrow([leftType], TS.tBoolean(loc), l, false));
          return createResult(ctx3);
        }));
    } else {
      return raise('Expected test to have a right hand side');
    }
  };

  const synthesisString = (_l: Loc): AnyTypingResult => {
    if (right !== undefined) {
      const rhs = right;
      return synthesis(left, false, context).bind((_a, _leftType, ctx) =>
        checking(rhs, TS.tString(loc), false, ctx).bind((_b, _t, ctx2) =>
          createResult(ctx2)));
    } else {
      return raise('Expected test to have a right hand side');
    }
  };

  const synthesisException = (l: Loc): AnyTypingResult => {
    if (right !== undefined) {
      const rhs = right;
      return synthesis(left, false, context).bind((_a, _leftType, ctx) =>
        synthesis(rhs, false, ctx).bind((_b, predType, ctx2) => {
          const ctx3 = ctx2.addConstraint(predType, new TS.TArrow([new TS.TTop(l, false)], TS.tBoolean(loc), l, false));
          return createResult(ctx3);
        }));
    } else {
      return raise('Expected test to have a right hand side');
    }
  };

  switch (op.$name) {
    case 's-op-is':
      return synthesisRefinement(op.l);
    case 's-op-is-roughly':
      return synthesisEquivalent(op.l);
    case 's-op-is-not-roughly':
      return synthesisEquivalent(op.l);
    case 's-op-is-op':
      return synthesisEquivalent(op.l);
    case 's-op-is-not':
      return synthesisRefinement(op.l);
    case 's-op-is-not-op':
      return synthesisEquivalent(op.l);
    case 's-op-satisfies':
      return synthesisPredicate(op.l);
    case 's-op-satisfies-not':
      return synthesisPredicate(op.l);
    case 's-op-raises':
      return synthesisString(op.l);
    case 's-op-raises-other':
      return synthesisString(op.l);
    case 's-op-raises-not':
      return synthesis(left, false, context).bind((_a, _leftType, ctx) =>
        createResult(ctx));
    case 's-op-raises-satisfies':
      return synthesisException(op.l);
    case 's-op-raises-violates':
      return synthesisException(op.l);
    default:
      throw new InternalCompilerError('Unknown CheckOp in synthesis-s-check-test');
  }
}

// ################### Test Inference ####################

export function collectExample(e: Expr, context: Context): AnyFoldResult<undefined> {
  if (testInferenceData === undefined) {
    return new TCS.FoldResult<undefined>(undefined, context);
  }
  const inferenceData = testInferenceData;
  switch (e.$name) {
    case 's-check-test': {
      const { op, refinement, left: lhs, right: rhs } = e;
      switch (op.$name) {
        case 's-op-is': {
          if (refinement !== undefined) {
            return new TCS.FoldResult<undefined>(undefined, context);
          }
          switch (lhs.$name) {
            case 's-app': {
              const { _fun, args } = lhs;
              let maybeId: Name | undefined;
              switch (_fun.$name) {
                case 's-id':
                  maybeId = _fun.id;
                  break;
                case 's-id-var':
                  maybeId = _fun.id;
                  break;
                case 's-id-letrec':
                  maybeId = _fun.id;
                  break;
                default:
                  maybeId = undefined;
              }
              if (maybeId === undefined) {
                return new TCS.FoldResult<undefined>(undefined, context);
              }
              const id = maybeId;
              if (inferenceData.name.key() === id.key()) {
                const foldArgTypes = foldr2((foldResultArgs: AnyFoldResult<Type[]>, arg: Expr, expectArg: Type) =>
                  foldResultArgs.bind((resultArgs, ctx) => {
                    if (TS.isTExistential(expectArg)) {
                      return synthesis(arg, false, ctx).foldBind((_a, resultType, ctx2) =>
                        new TCS.FoldResult([resultType, ...resultArgs], ctx2));
                    } else {
                      return checking(arg, expectArg, false, ctx).foldBind((_a, _t, ctx2) =>
                        new TCS.FoldResult([expectArg, ...resultArgs], ctx2));
                    }
                  }), new TCS.FoldResult<Type[]>([], context), args, inferenceData.argTypes);
                return foldArgTypes.bind((argTypes, ctx) => {
                  const expectRetType = inferenceData.retType;
                  let retFold: AnyFoldResult<Type>;
                  if (TS.isTExistential(expectRetType)) {
                    retFold = synthesis(optValue(rhs), false, ctx).foldBind((_a, resultType, ctx2) =>
                      new TCS.FoldResult(resultType, ctx2));
                  } else {
                    retFold = checking(optValue(rhs), expectRetType, false, ctx).foldBind((_a, _t, ctx2) =>
                      new TCS.FoldResult(expectRetType, ctx2));
                  }
                  return retFold.bind((retType, ctx2) => {
                    const ctx3 = ctx2.addExampleType(inferenceData.existential, new TS.TArrow(argTypes, retType, inferenceData.loc, true));
                    return new TCS.FoldResult<undefined>(undefined, ctx3);
                  });
                });
              } else {
                return new TCS.FoldResult<undefined>(undefined, context);
              }
            }
            default:
              return new TCS.FoldResult<undefined>(undefined, context);
          }
        }
        default:
          return new TCS.FoldResult<undefined>(undefined, context);
      }
    }
    default:
      return new TCS.FoldResult<undefined>(undefined, context);
  }
}

export function miscCollectExample(e: Expr, context: Context): Context {
  if (miscTestInferenceData === undefined) {
    return context;
  }
  const funName = miscTestInferenceData;
  switch (e.$name) {
    case 's-check-test': {
      const { op, refinement, left: lhs, right: rhs } = e;
      switch (op.$name) {
        case 's-op-is': {
          if (refinement !== undefined) {
            return context;
          }
          switch (lhs.$name) {
            case 's-app': {
              const { _fun, args } = lhs;
              let maybeId: Name | undefined;
              switch (_fun.$name) {
                case 's-id':
                  maybeId = _fun.id;
                  break;
                case 's-id-var':
                  maybeId = _fun.id;
                  break;
                case 's-id-letrec':
                  maybeId = _fun.id;
                  break;
                default:
                  maybeId = undefined;
              }
              if (maybeId === undefined) {
                return context;
              }
              const id = maybeId;
              if (funName.key() === id.key()) {
                let foldArgTypes: AnyFoldResult<Type[]> = new TCS.FoldResult<Type[]>([], context);
                for (let i = args.length - 1; i >= 0; i--) {
                  const arg = args[i];
                  const acc = foldArgTypes;
                  foldArgTypes = acc.bind((resultArgs, ctx) =>
                    synthesis(arg, false, ctx).foldBind((_a, resultType, ctx2) =>
                      new TCS.FoldResult([resultType, ...resultArgs], ctx2)));
                }
                const foldRetType = foldArgTypes.bind((argTypes, ctx) =>
                  synthesis(optValue(rhs), false, ctx).foldBind((_a, resultType, ctx2) =>
                    new TCS.FoldResult([argTypes, resultType] as [Type[], Type], ctx2)));
                switch (foldRetType.$name) {
                  case 'fold-result': {
                    const [argTypes, resultType] = foldRetType.v;
                    const ctx = foldRetType.context;
                    return ctx.addMiscExampleType(funName.key(), new TS.TArrow(argTypes, resultType, A.dummyLoc, false));
                  }
                  case 'fold-errors':
                    return context;
                  default:
                    throw new InternalCompilerError('Unknown FoldResult in misc-collect-example');
                }
              } else {
                return context;
              }
            }
            default:
              return context;
          }
        }
        default:
          return context;
      }
    }
    default:
      return context;
  }
}

// #######################################################
