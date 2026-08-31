/*
  TS port of src/arr/compiler/anf.arr (the A-normalization pass).
  See CONVENTIONS.md.

  Continuations (the ANFCont type and the `k` parameters threaded through
  `anf`/`anf-name`/`anf-name-rec`/...) are ported directly as TS closures:
  Pyret's `(N.ALettable -> N.AExpr)` becomes
  `(lettable: N.ALettable) => N.AExpr`. Call order is preserved exactly so
  gensym-generated names (`anf_...`) match the Pyret compiler's output.
*/

import * as A from './ast';
import * as SL from './srcloc';
import * as N from './ast-anf';
import { InternalCompilerError, raise, map2, toRepr as torepr, field, asVariant } from './shared';
import { jsnums, PyretNumber, throwingErrbacks } from './interop/js-numbers';
import { eliminatedScopeForm } from './ast-visitors';

export type Loc = SL.Srcloc;

export type ANFCont = (lettable: N.ALettable) => N.AExpr;

export function getValue(o: any): A.Expr { return o.value; }

export const names = A.globalNames;
export const flatPrimApp = new A.PrimAppInfoC(false);

export function mkId(loc: N.Loc, base: string): { id: A.Name; idB: N.ABind; idE: N.AId } {
  const t = names.makeAtom(base);
  return { id: t, idB: bind(loc, t), idE: new N.AId(loc, t) };
}

/*
  This code is careful to maintain the translation order (hence gensym order) as
  exactly the Pyret compiler's. There are more direct ways to do this that
  wouldn't result in byte-identical output; this somewhat tortured process lets
  us do the fold exactly as the natural recursive solution did.
*/
const HOLE: N.ALettable = new N.AVal(N.dummyLoc, new N.AUndefined(N.dummyLoc));

function holeExpr(): N.AExpr {
  return new N.AExpr([], HOLE);
}

function isHole(ae: N.AExpr): boolean {
  return ae.e === HOLE;
}

// Prepend `heads` (destructively) onto `tail` and return it.
function prependHeads(heads: N.AExprHead[], tail: N.AExpr): N.AExpr {
  if (heads.length === 0) {
    return tail;
  }
  for (const h of tail.heads) {
    heads.push(h);
  }
  tail.heads = heads;
  return tail;
}

export function anfTerm(e: A.Expr): N.AExpr {
  return anf(e, (x) => new N.AExpr([], x));
}

export function bind(l: N.Loc, id: A.Name): N.ABind {
  return new N.ABind(l, id, A.aBlank);
}

export function anfBind(b: A.Bind): N.ABind {
  switch (b.$name) {
    case 's-bind':
      return new N.ABind(b.l, b.id, b.ann);
    default:
      throw new InternalCompilerError('No case matched in anfBind: ' + b.$name);
  }
}

export function anfCasesBind(cb: A.CasesBind): N.ACasesBind {
  switch (cb.$name) {
    case 's-cases-bind':
      return new N.ACasesBind(cb.l, cb.fieldType, anfBind(cb.bind));
    default:
      throw new InternalCompilerError('No case matched in anfCasesBind: ' + (cb as any).$name);
  }
}

export function anfCasesBranch(branch: A.CasesBranch): N.ACasesBranch {
  switch (branch.$name) {
    case 's-cases-branch':
      return new N.ACasesBranch(branch.l, branch.patLoc, branch.name,
        branch.args.map(anfCasesBind), anfTerm(branch.body));
    case 's-singleton-cases-branch':
      return new N.ASingletonCasesBranch(branch.l, branch.patLoc, branch.name, anfTerm(branch.body));
    default:
      throw new InternalCompilerError('No case matched in anfCasesBranch: ' + (branch as any).$name);
  }
}

export function anfName(expr: A.Expr, nameHint: string, k: (v: N.AVal) => N.AExpr): N.AExpr {
  return anf(expr, (lettable) => {
    if (N.isAVal(lettable)) {
      return k(lettable.v);
    } else {
      const t = mkId(expr.l, nameHint);
      const rest = k(t.idE);
      rest.heads.unshift(new N.ALet(expr.l, t.idB, lettable));
      return rest;
    }
  });
}

export function anfNameRec(
  exprs: A.Expr[],
  nameHint: string,
  k: (vs: N.AVal[]) => N.AExpr
): N.AExpr {
  const vs: N.AVal[] = [];
  const heads: N.AExprHead[] = [];
  for (const f of exprs) {
    const translated = anf(f, (lettable) => {
      if (N.isAVal(lettable)) {
        vs.push(lettable.v);
        return holeExpr();
      } else {
        const t = mkId(f.l, nameHint);
        vs.push(t.idE);
        return new N.AExpr([new N.ALet(f.l, t.idB, lettable)], HOLE);
      }
    });
    if (!isHole(translated)) {
      throw new InternalCompilerError('anfNameRec: element continuation was not invoked or not in tail position: ' + f.$name);
    }
    for (const h of translated.heads) {
      heads.push(h);
    }
  }
  return prependHeads(heads, k(vs));
}

export function anfNameArr(expr: A.Expr, name: A.Name, idx: number, k: () => N.AExpr): N.AExpr {
  return anf(expr, (lettable) => {
    const rest = k();
    rest.heads.unshift(new N.AArrLet(expr.l, new N.ABind(expr.l, name, A.aBlank), idx, lettable));
    return rest;
  });
}

export function anfNameArrRec(
  exprs: A.Expr[],
  name: A.Name,
  ind: number,
  k: () => N.AExpr
): N.AExpr {
  // Like anfNameRec, but every element gets an AArrLet head.
  const heads: N.AExprHead[] = [];
  for (let i = 0; i < exprs.length; i++) {
    const f = exprs[i];
    const translated = anf(f, (lettable) =>
      new N.AExpr([new N.AArrLet(f.l, new N.ABind(f.l, name, A.aBlank), ind + i, lettable)], HOLE));
    if (!isHole(translated)) {
      throw new InternalCompilerError('anfNameArrRec: element continuation was not invoked or not in tail position');
    }
    for (const h of translated.heads) {
      heads.push(h);
    }
  }
  return prependHeads(heads, k());
}

export function anfProgram(e: A.Program): N.AProg {
  switch (e.$name) {
    case 's-program':
      // Note: provides have been desugared to a structure with no expressions, just
      // names and Ann information
      // MARK(joe/ben): provides
      return new N.AProgram(e.l, e.provides[0], e.imports, anfTerm(e.block));
    default:
      throw new InternalCompilerError('No case matched in anfProgram: ' + (e as any).$name);
  }
}

export function anfBlock(esInit: A.Expr[], k: ANFCont): N.AExpr {
  if (esInit.length === 0) {
    return raise('Empty block');
  }
  return anfLinear(new A.SBlock(esInit[0].l, esInit), k);
}

/*
  Iterative translation of the program's "linear spine".

  After scope resolution a program is a deeply right-nested alternation of
  s-block / s-let-expr / s-type-let-expr / s-letrec: each binding's body
  contains the entire rest of the program. The natural CPS formulation
  nests one whole anf() activation per statement/binding, which overflows
  fixed-size stacks (e.g. browsers, where the CLI's --stack-size escape
  hatch does not exist) on long programs.

*/
function anfLinear(eInit: A.Expr, k: ANFCont): N.AExpr {
  const spine: N.AExprHead[] = [];

  function emit(translated: N.AExpr): void {
    if (!isHole(translated)) {
      throw new InternalCompilerError('anfLinear: statement continuation was not invoked or not in tail position');
    }
    for (const h of translated.heads) {
      spine.push(h);
    }
  }

  let e: A.Expr = eInit;
  for (;;) {
    switch (e.$name) {
      case 's-block': {
        const stmts = e.stmts;
        if (stmts.length === 0) {
          return raise('Empty block');
        }
        // Note: assuming blocks don't end in let/var here
        for (let i = 0; i < stmts.length - 1; i++) {
          const f = stmts[i];
          emit(anf(f, (lettable) => new N.AExpr([new N.ASeq(f.l, lettable)], HOLE)));
        }
        e = stmts[stmts.length - 1];
        continue;
      }
      case 's-scope-block': {
        for (const entry of e.entries) {
          if (A.isSScopeLet(entry)) {
            for (const b of entry.binds) {
              emitLetBind(b);
            }
          } else if (A.isSScopeTypeLet(entry)) {
            for (const f of entry.binds) {
              let newBind: N.ATypeBind;
              switch (f.$name) {
                case 's-type-bind':
                  newBind = new N.ATypeBind(f.l, f.name, f.ann); // TODO(MATT): is this going to have to change?
                  break;
                case 's-newtype-bind':
                  newBind = new N.ANewtypeBind(f.l, f.name, f.namet);
                  break;
                default:
                  throw new InternalCompilerError('No case matched in anf s-scope-type-let: ' + (f as any).$name);
              }
              spine.push(new N.ATypeLet(entry.l, newBind));
            }
          } else if (A.isSScopeLetrec(entry)) {
            for (const b of entry.binds) {
              emitLetBind(new A.SVarBind(b.l, b.b, new A.SUndefined(entry.l)));
            }
            for (const b of entry.binds) {
              const stmt = new A.SAssign(b.l, field(b.b, 'id'), b.value);
              emit(anf(stmt, (lettable) => new N.AExpr([new N.ASeq(stmt.l, lettable)], HOLE)));
            }
          } else {
            const f = entry.stmt;
            emit(anf(f, (lettable) => new N.AExpr([new N.ASeq(f.l, lettable)], HOLE)));
          }
        }
        e = e.tail;
        continue;
      }
      default:
        return prependHeads(spine, anf(e, k));
    }
  }

  function emitLetBind(f: A.LetBind): void {
    switch (f.$name) {
      case 's-var-bind': {
        const l2 = f.l;
        const b = asVariant(f.b, A.SBind);
        const val = f.value;
        if (A.isABlank(b.ann) || A.isAAny(b.ann)) {
          emit(anfName(val, 'var', (newVal) =>
            new N.AExpr([new N.AVar(l2, new N.ABind(l2, b.id, b.ann), new N.AVal(newVal.l, newVal))], HOLE)));
        } else {
          const varName = mkId(l2, 'var');
          emit(anf(val, (lettable) =>
            new N.AExpr([
              new N.ALet(l2, varName.idB, lettable),
              new N.AVar(l2, new N.ABind(l2, b.id, b.ann), new N.AVal(l2, varName.idE)),
            ], HOLE)));
        }
        break;
      }
      case 's-let-bind': {
        const l2 = f.l;
        const b = asVariant(f.b, A.SBind);
        const val = f.value;
        emit(anf(val, (lettable) =>
          new N.AExpr([new N.ALet(l2, new N.ABind(l2, b.id, b.ann), lettable)], HOLE)));
        break;
      }
      default:
        throw new InternalCompilerError('No case matched in anf let bind: ' + (f as any).$name);
    }
  }
}

export function anf(e: A.Expr, k: ANFCont): N.AExpr {
  switch (e.$name) {
    case 's-module': {
      const l = e.l;
      const adms = e.definedModules.map((dm) =>
        new N.ADefinedModule(dm.name, dm.value, dm.uri));
      const adts = e.definedTypes.map((dt) =>
        new N.ADefinedType(dt.name, dt.typ));
      const needsValue = e.definedValues.filter(A.isSDefinedValue);
      return anfNameRec(needsValue.map(getValue), 'defined_value', (advs0) => {
        const advs = map2((name: string, adv: N.AVal): N.ADefinedValue =>
          new N.ADefinedValue(name, adv), needsValue.map((dv) => dv.name), advs0);
        const avars = e.definedValues.filter(A.isSDefinedVar).map((dvar) =>
          new N.ADefinedVar(dvar.name, dvar.id));

        return anfName(e.answer, 'answer', (ans) =>
          anfName(e.checks, 'checks', (chks) =>
            k(new N.AModule(l, ans, adms, [...advs, ...avars], adts, chks))));
      });
    }
    case 's-num':
      return k(new N.AVal(e.l, new N.ANum(e.l, e.n)));
    // num, den are exact ints, and s-frac desugars to the exact rational num/den
    case 's-frac': // Possibly unneeded if removed by desugar?
      return k(new N.AVal(e.l, new N.ANum(e.l, jsnums.divide(e.num, e.den, throwingErrbacks))));
    // num, den are exact ints, and s-rfrac desugars to the roughnum fraction corresponding to num/den
    case 's-rfrac': // Possibly unneeded if removed by desugar?
      return k(new N.AVal(e.l, new N.ANum(e.l,
        jsnums.toRoughnum(jsnums.divide(e.num, e.den, throwingErrbacks), throwingErrbacks))));
    case 's-str':
      return k(new N.AVal(e.l, new N.AStr(e.l, e.s)));
    case 's-undefined':
      return k(new N.AVal(e.l, new N.AUndefined(e.l)));
    case 's-bool':
      return k(new N.AVal(e.l, new N.ABool(e.l, e.b)));
    case 's-prim-val':
      return k(new N.AVal(e.l, new N.APrimVal(e.l, e.name)));
    case 's-id':
      return k(new N.AVal(e.l, new N.AId(e.l, e.id)));
    case 's-id-modref':
      return k(new N.AVal(e.l, new N.AIdModref(e.l, e.id, e.uri, e.name)));
    case 's-id-var-modref':
      return k(new N.AIdVarModref(e.l, e.id, e.uri, e.name));
    case 's-srcloc':
      return k(new N.AVal(e.l, new N.ASrcloc(e.l, e.loc)));
    case 's-scope-block':
      return anfLinear(e, k);
    case 's-type-let-expr':
    case 's-let-expr':
    case 's-letrec':
      return eliminatedScopeForm(e);

    case 's-data-expr': {
      const l = e.l;
      const dataName = e.name;
      const dataNameT = e.namet;
      const variants = e.variants;
      const shared = e.sharedMembers;
      function anfMember(member: A.VariantMember): N.AVariantMember {
        switch (member.$name) {
          case 's-variant-member': {
            const l2 = member.l;
            const typ = member.memberType;
            const b = member.bind;
            let aType: N.AMemberType;
            switch (typ.$name) {
              case 's-normal':
                aType = new N.ANormal();
                break;
              case 's-mutable':
                aType = new N.AMutable();
                break;
              default:
                throw new InternalCompilerError('No case matched in anfMember member type: ' + (typ as any).$name);
            }
            let newBind: N.ABind;
            switch (b.$name) {
              case 's-bind':
                newBind = new N.ABind(b.l, b.id, b.ann);
                break;
              default:
                throw new InternalCompilerError('No case matched in anfMember bind: ' + (b as any).$name);
            }
            return new N.AVariantMember(l2, aType, newBind);
          }
          default:
            throw new InternalCompilerError('No case matched in anfMember: ' + (member as any).$name);
        }
      }
      function anfVariant(v: A.Variant, kv: (av: N.AVariant) => N.AExpr): N.AExpr {
        switch (v.$name) {
          case 's-variant': {
            const withExprs = v.withMembers.map(getValue);
            return anfNameRec(withExprs, 'anf_variant_member', (ts) => {
              const newFields = map2((f: A.Member, t: N.AVal): N.AField =>
                new N.AField(f.l, field(f, 'name'), t), v.withMembers, ts);
              return kv(new N.AVariant(v.l, v.constrLoc, v.name, v.members.map(anfMember), newFields));
            });
          }
          case 's-singleton-variant': {
            const withExprs = v.withMembers.map(getValue);
            return anfNameRec(withExprs, 'anf_singleton_variant_member', (ts) => {
              const newFields = map2((f: A.Member, t: N.AVal): N.AField =>
                new N.AField(f.l, field(f, 'name'), t), v.withMembers, ts);
              return kv(new N.ASingletonVariant(v.l, v.name, newFields));
            });
          }
          default:
            throw new InternalCompilerError('No case matched in anfVariant: ' + (v as any).$name);
        }
      }
      function anfVariants(vs: A.Variant[], ks: (avs: N.AVariant[]) => N.AExpr): N.AExpr {
        const avs: N.AVariant[] = [];
        const heads: N.AExprHead[] = [];
        for (const f of vs) {
          const translated = anfVariant(f, (av) => {
            avs.push(av);
            return holeExpr();
          });
          if (!isHole(translated)) {
            throw new InternalCompilerError('anf s-data-expr: variant continuation was not invoked or not in tail position');
          }
          for (const h of translated.heads) {
            heads.push(h);
          }
        }
        return prependHeads(heads, ks(avs));
      }
      const exprs = shared.map(getValue);

      return anfNameRec(exprs, 'anf_shared', (ts) => {
        const newShared = map2((f: A.Member, t: N.AVal): N.AField =>
          new N.AField(f.l, field(f, 'name'), t), shared, ts);
        return anfVariants(variants, (newVariants) =>
          k(new N.ADataExpr(l, dataName, dataNameT, newVariants, newShared)));
      });
    }

    case 's-if-else': {
      /*
        The arms of a chain are NOT one branch list by the time they
        reach this pass: desugarIf rewrites an N-arm if into N
        right-nested single-branch s-if-else nodes, so this walks both a
        node's branch list and the chain of s-if-else nodes in else
        position, collecting every arm into ONE flat AIf. Each arm's
        test-computation heads become the arm's branch heads (they run
        only once the earlier arms have failed); the first arm's heads
        go outside the AIf, into the enclosing chain, exactly where the
        nested representation put them. Arm tests and bodies are
        translated in the original order and k still runs after every
        arm, so gensym order is unchanged.
      */
      let node = e;
      const branches: N.AIfBranch[] = [];
      let elseBody: N.AExpr | undefined = undefined;
      for (;;) {
        const { l, branches: bs, _else } = node;
        if (bs.length === 0) {
          return raise('Empty branches');
        }
        const elseIsIf = _else.$name === 's-if-else';
        for (let i = 0; i < bs.length; i++) {
          const f = bs[i];
          const isLastArm = (i === bs.length - 1) && !elseIsIf;
          let test: N.AVal | undefined = undefined;
          let body: N.AExpr | undefined = undefined;
          const translated = anfName(f.test, 'anf_if', (t) => {
            test = t;
            body = anfTerm(f.body);
            if (isLastArm) {
              elseBody = anfTerm(_else);
            }
            return holeExpr();
          });
          if (test === undefined || body === undefined) {
            throw new InternalCompilerError('anf s-if-else: branch continuation was not invoked');
          }
          if (!isHole(translated)) {
            throw new InternalCompilerError('anf s-if-else: branch continuation result not in tail position');
          }
          branches.push(new N.AIfBranch(l, translated.heads, test, body));
        }
        if (!elseIsIf) {
          break;
        }
        node = _else as typeof e;
      }
      const first = branches[0];
      const outerHeads = first.heads;
      first.heads = [];
      return prependHeads(outerHeads, k(new N.AIf(first.l, branches, elseBody!)));
    }
    case 's-cases-else': {
      const { l, typ, val, branches, _else } = e;
      return anfName(val, 'cases_val',
        (v) => k(new N.ACases(l, typ, v, branches.map(anfCasesBranch), anfTerm(_else))));
    }
    case 's-block':
      return anfBlock(e.stmts, k);

    case 's-check-expr': {
      const { l, expr, ann } = e;
      const name = mkId(l, 'ann_check_temp');
      const bindings = [new A.SLetBind(l, new A.SBind(l, false, name.id, ann), expr)];
      return anf(A.sScopeLetBlock(l, bindings, new A.SId(l, name.id)), k);
    }

    case 's-lam': {
      const { l, name, args, ann: ret, body } = e;
      if (A.isABlank(ret) || A.isAAny(ret)) {
        return k(new N.ALam(l, name, args.map((a) => new N.ABind(a.l, field(a, 'id'), field(a, 'ann'))), ret, anfTerm(body)));
      } else {
        const temp = mkId(l, 'ann_check_temp');
        return k(new N.ALam(l, name, args.map((a) => new N.ABind(a.l, field(a, 'id'), field(a, 'ann'))), ret,
          anfTerm(A.sScopeLetBlock(l,
            [new A.SLetBind(l, new A.SBind(l, false, temp.id, ret), body)],
            new A.SId(l, temp.id)))));
      }
    }
    case 's-method': {
      const { l, name, args, ann: ret, body } = e;
      if (A.isABlank(ret) || A.isAAny(ret)) {
        return k(new N.AMethod(l, name, args.map((a) => new N.ABind(a.l, field(a, 'id'), field(a, 'ann'))), ret, anfTerm(body)));
      } else {
        const temp = mkId(l, 'ann_check_temp');
        return k(new N.AMethod(l, name, args.map((a) => new N.ABind(a.l, field(a, 'id'), field(a, 'ann'))), ret,
          anfTerm(A.sScopeLetBlock(l,
            [new A.SLetBind(l, new A.SBind(l, false, temp.id, ret), body)],
            new A.SId(l, temp.id)))));
      }
    }
    case 's-tuple': {
      const l = e.l;
      return anfNameRec(e.fields, 'anf_tuple_fields', (vs) =>
        k(new N.ATuple(l, vs)));
    }
    case 's-tuple-get': {
      const { l, tup, index } = e;
      return anfName(tup, 'anf_tuple_get', (v) => k(new N.ATupleGet(l, v, index)));
    }
    case 's-array': {
      const { l, values } = e;
      const arrayId = names.makeAtom('anf_array');
      const head = new N.ALet(
        l,
        bind(l, arrayId),
        new N.APrimApp(l, 'makeArrayN', [new N.ANum(l, jsnums.fromFixnum(values.length) as PyretNumber)], flatPrimApp));
      const rest = anfNameArrRec(values, arrayId, 0, () =>
        k(new N.AVal(l, new N.AId(l, arrayId))));
      rest.heads.unshift(head);
      return rest;
    }

    case 's-app-enriched': {
      const { l, _fun: f, args, appInfo } = e;
      switch (f.$name) {
        case 's-dot': {
          const m = f.field;
          return anfName(f.obj, 'anf_method_obj', (v) =>
            anfNameRec(args, 'anf_arg', (vs) =>
              k(new N.AMethodApp(l, v, m, vs))));
        }
        case 's-lam': {
          /// NOTE: This case implements the inline-lams visitor transformation
          /// It can be safely eliminated without affecting the semantics of
          /// the transformation, but does help eliminate some unneeded lambdas
          const params = f.args;
          const ann = f.ann;
          const body = f.body;
          if (params.length === args.length) {
            const letBinds = map2((p: A.Bind, a: A.Expr): A.LetBind =>
              new A.SLetBind(p.l, p, a), params, args);
            let inlined: A.Expr;
            switch (ann.$name) {
              case 'a-blank':
                inlined = A.sScopeLetBlock(l, letBinds, body);
                break;
              case 'a-any':
                inlined = A.sScopeLetBlock(l, letBinds, body);
                break;
              default: {
                const a = A.globalNames.makeAtom('inline_body');
                inlined = A.sScopeLetBlock(l,
                  [...letBinds, new A.SLetBind(body.l, new A.SBind(l, false, a, ann), body)],
                  new A.SId(l, a));
                break;
              }
            }
            return anf(inlined, k);
          } else {
            return anfName(f, 'anf_fun', (v) =>
              anfNameRec(args, 'anf_arg', (vs) =>
                k(new N.AApp(l, v, vs, appInfo))));
          }
        }
        default:
          return anfName(f, 'anf_fun', (v) =>
            anfNameRec(args, 'anf_arg', (vs) =>
              k(new N.AApp(l, v, vs, appInfo))));
      }
    }

    case 's-prim-app': {
      const { l, _fun: f, args, appInfo } = e;
      return anfNameRec(args, 'anf_arg', (vs) =>
        k(new N.APrimApp(l, f, vs, appInfo)));
    }

    case 's-instantiate':
      return anf(e.expr, k);

    case 's-dot': {
      const { l, obj, field } = e;
      return anfName(obj, 'anf_bracket', (tObj) => k(new N.ADot(l, tObj, field)));
    }

    case 's-bracket':
      return raise('Impossible');

    case 's-ref':
      return k(new N.ARef(e.l, e.ann));

    case 's-id-var':
      return k(new N.AIdVar(e.l, e.id));

    case 's-id-letrec': {
      const { l, id, safe } = e;
      if (safe) {
        return k(new N.AVal(l, new N.AIdSafeLetrec(l, id)));
      } else {
        return k(new N.AIdLetrec(l, id, safe));
      }
    }

    case 's-get-bang': {
      const { l, obj, field } = e;
      return anfName(obj, 'anf_get_bang', (t) => k(new N.AGetBang(l, t, field)));
    }

    case 's-assign': {
      const { l, id, value } = e;
      return anfName(value, 'anf_assign', (v) => k(new N.AAssign(l, id, v)));
    }

    case 's-obj': {
      const { l, fields } = e;
      const exprs = fields.map(getValue);

      return anfNameRec(exprs, 'anf_obj', (ts) => {
        const newFields = map2((f: A.Member, t: N.AVal): N.AField =>
          new N.AField(f.l, field(f, 'name'), t), fields, ts);
        return k(new N.AObj(l, newFields));
      });
    }

    case 's-update': {
      const { l, supe: obj, fields } = e;
      const exprs = fields.map(getValue);

      return anfName(obj, 'anf_update', (o) =>
        anfNameRec(exprs, 'anf_update', (ts) => {
          const newFields = map2((f: A.Member, t: N.AVal): N.AField =>
            new N.AField(f.l, field(f, 'name'), t), fields, ts);
          return k(new N.AUpdate(l, o, newFields));
        }));
    }

    case 's-extend': {
      const { l, supe: obj, fields } = e;
      const exprs = fields.map(getValue);

      return anfName(obj, 'anf_extend', (o) =>
        anfNameRec(exprs, 'anf_extend', (ts) => {
          const newFields = map2((f: A.Member, t: N.AVal): N.AField =>
            new N.AField(f.l, field(f, 'name'), t), fields, ts);
          return k(new N.AExtend(l, o, newFields));
        }));
    }

    case 's-let':
      return raise('s-let should have been desugared already: ' + torepr(e));
    case 's-var':
      return raise('s-var should have been desugared already: ' + torepr(e));
    case 's-spy-block':
      return raise('s-spy-block should have been desugared already: ' + torepr(e));
    case 's-user-block':
      return raise('s-user-block should have been desugared already: ' + torepr(e));
    default:
      return raise('Missed case in anf: ' + torepr(e));
  }
}
