/*
  Ported from: src/arr/compiler/flatness.arr
  Flatness analysis: is a function "flat" (guaranteed not to need the
  Pyret stack)? See CONVENTIONS.md.

  A flatness environment maps from ANF id names (by .key()) to

  - undefined (Pyret none), if the name is for a function with an
    infinitely deep body
  - n (Pyret some(n)), where n is the number of nested calls that the
    function contains

  If a name isn't present, it is equivalent to containing a mapping for
  undefined.

  This notion is naturally extended to named annotations, which are similar
  to functions in that they delay computation until later.

  CAREFUL: because some(0) ports to the falsy value 0, Flatness values are
  always compared with explicit `=== undefined` / `!== undefined` checks,
  never truthiness. Where Pyret's MutableStringDict get-now produced
  Option<Flatness> (a double Option), `.has()` is used to distinguish
  "absent" from "present with value none".
*/

import * as A from './ast';
import * as AA from './ast-anf';
import * as C from './compile-structs';
import { InternalCompilerError, mapGetValue, raise, toRepr as torepr, field, asVariant } from './shared';

export type Flatness = number | undefined;
export type FEnv = Map<string, Flatness>;
// The { sd; ad } tuple returned by make-prog-flatness-env
export type FlatnessEnv = [FEnv, FEnv];

export function flatnessMax(a: Flatness, b: Flatness): Flatness {
  // read the docs, maybe there's a quicker way to write this
  if (a !== undefined) {
    if (b !== undefined) {
      return Math.max(a, b);
    } else {
      return undefined;
    }
  } else {
    return undefined;
  }
}

// Calculate the flatness of an annotation. Does not change val-env and ann-env
export function annFlatness(
  ann: A.Ann,
  valEnv: FEnv,
  annEnv: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): Flatness {
  switch (ann.$name) {
    case 'a-blank': return 0;
    case 'a-any': return 0;
    case 'a-name':
      // get-now(...).or-else(none): absent and stored-none coincide here
      return annEnv.get(ann.id.key());
    case 'a-type-var': return 0;
    case 'a-arrow':
      // NOTE(joe): This is a flat check because it's not higher-order; we don't check args and ret
      return 0;
    case 'a-arrow-argnames':
      // NOTE(joe): This is a flat check because it's not higher-order; we don't check args and ret
      return 0;
    case 'a-method': return 0;
    case 'a-record': {
      let flatness: Flatness = 0;
      for (const f of ann.fields) {
        flatness = flatnessMax(flatness, annFlatness(f.ann, valEnv, annEnv, mb, env));
      }
      return flatness;
    }
    case 'a-tuple': {
      let flatness: Flatness = 0;
      for (const f of ann.fields) {
        flatness = flatnessMax(flatness, annFlatness(f, valEnv, annEnv, mb, env));
      }
      return flatness;
    }
    case 'a-app':
      // NOTE(joe): the args are ignored because we don't dynamically check
      // the Number in List<Number>
      return annFlatness(ann.ann, valEnv, annEnv, mb, env);
    case 'a-pred': {
      const valFlatness = valEnv.get(field(ann.exp, 'id').key());
      return flatnessMax(
        annFlatness(ann.ann, valEnv, annEnv, mb, env),
        valFlatness
      );
    }
    case 'a-dot': {
      const moduleInfo = mapGetValue(env.allModules, mapGetValue(mb, ann.obj.key()).uri);
      const provides = moduleInfo.provides;
      if (provides.dataDefinitions.has(ann.field)) {
        return 0;
      } else if (provides.aliases.has(ann.field)) {
        // NOTE(joe): We'd love to do something like the below; however,
        // the things in aliases are TYPES, which don't match the type of
        // ann-flatness, so we can't tell what the flatness of an ann is
        // from its provides, limiting the effectiveness of checking for
        // refinements cross-module

        // ann-flatness(provides.aliases.get-value(field), val-env, ann-env, mb, env)
        // So we return none instead
        return undefined;
      } else {
        return undefined;
      }
    }
    case 'a-checked': return undefined;
    default:
      throw new InternalCompilerError('annFlatness: unknown ann ' + (ann as any).$name);
  }
}

// (Mutably) fills in the sd (value environment) with flatnesses for predicates
// and constructors, and ad (type environment) with flatnesses for datatype annotations
// and type aliases. Return value should be ignored.
export function makeExprDataEnv(
  aexpr: AA.AExpr,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment,
  typeNameToVariants: Map<string, AA.AVariant[]>,
  aliasToTypeName: Map<string, string>
): void {
  for (const head of aexpr.heads) {
    makeHeadDataEnv(head, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
  }
  makeLettableDataEnv(aexpr.e, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
}

export function makeHeadDataEnv(
  head: AA.AExprHead,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment,
  typeNameToVariants: Map<string, AA.AVariant[]>,
  aliasToTypeName: Map<string, string>
): void {
  switch (head.$name) {
    case 'a-type-let': {
      const bind = head.bind;
      switch (bind.$name) {
        case 'a-newtype-bind':
          // We know that the annotation for a newtype bind is just a flat
          // brand check, so make it some(0)
          ad.set(bind.name.key(), 0);
          break;
        case 'a-type-bind':
          ad.set(bind.name.key(), annFlatness(bind.ann, sd, ad, mb, env));
          break;
        default:
          throw new InternalCompilerError('makeExprDataEnv: unknown type bind ' + (bind as any).$name);
      }
      return;
    }
    case 'a-let': {
      const bind = head.bind;
      const val = head.e;
      if (AA.isADataExpr(val)) {
        typeNameToVariants.set(bind.id.key(), val.variants);
        // Make self-mapping entry so we know it's a "type" name
        aliasToTypeName.set(bind.id.key(), bind.id.key());
      } else if (AA.isAIdSafeLetrec(val)) {
        // If we say
        // x = Type
        // y = x
        // z = y
        // We say z and y are aliases of x
        // (NOTE: as in the Pyret original, this test can never succeed for
        // an ALettable; a-id-safe-letrec is an AVal variant)
        const typeNameOpt = aliasToTypeName.get((val as unknown as AA.AIdSafeLetrec).id.key());
        if (typeNameOpt !== undefined) {
          aliasToTypeName.set(bind.id.key(), typeNameOpt);
        }
      } else if (AA.isADot(val) && AA.isAIdSafeLetrec(val.obj)) {
        // Check for: xyz = Type.is-variant or xyz = Type.flat-constructor
        const typeNameOpt = aliasToTypeName.get(val.obj.id.key());
        if (typeNameOpt !== undefined) {
          const typeName = typeNameOpt;
          const variants = mapGetValue(typeNameToVariants, typeName);

          const isIsFunction = variants.some((v) => ('is-' + v.name) === val.field);
          if (isIsFunction) {
            sd.set(bind.id.key(), 0);
          }

          const theVariant = variants.find((v) => (v.name === val.field) && AA.isAVariant(v)) as AA.AVariant$ | undefined;
          if (theVariant !== undefined) {
            let variantFlatness: Flatness = 0;
            for (const m of theVariant.members) {
              variantFlatness = flatnessMax(variantFlatness, annFlatness(m.bind.ann, sd, ad, mb, env));
            }
            sd.set(bind.id.key(), variantFlatness);
          }
        }
      } else {
        // nothing
      }
      makeLettableDataEnv(val, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      return;
    }
    case 'a-arr-let':
      makeLettableDataEnv(head.e, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      return;
    case 'a-var':
      // (as in the Pyret original, a-var's value is not examined here)
      return;
    case 'a-seq':
      makeLettableDataEnv(head.e1, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      return;
    default:
      throw new InternalCompilerError('makeHeadDataEnv: unknown head ' + (head as any).$name);
  }
}

export function makeLettableDataEnv(
  lettable: AA.ALettable,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment,
  typeNameToVariants: Map<string, AA.AVariant[]>,
  aliasToTypeName: Map<string, string>
): void {
  // default-ret = none (return value is ignored by all callers)
  switch (lettable.$name) {
    case 'a-if': {
      // Arm order matches the nested chain: each arm's body, then the
      // next arm's test-computation heads, ..., then the else.
      for (const b of lettable.branches) {
        for (const h of b.heads) {
          makeHeadDataEnv(h, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
        }
        makeExprDataEnv(b.body, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      }
      makeExprDataEnv(lettable.elseBody, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      break;
    }
    case 'a-assign': {
      const value = lettable.value;
      if (AA.isAId(value)) {
        if (sd.has(value.id.key())) {
          sd.set(lettable.id.key(), sd.get(value.id.key()));
        }

        if (aliasToTypeName.has(value.id.key())) {
          const valType = mapGetValue(aliasToTypeName, value.id.key());
          aliasToTypeName.set(lettable.id.key(), valType);
        }
      }

      if (AA.isAIdSafeLetrec(value)) {
        const typeNameOpt = aliasToTypeName.get(value.id.key());
        if (typeNameOpt !== undefined) {
          aliasToTypeName.set(lettable.id.key(), typeNameOpt);
        }
      }
      break;
    }
    case 'a-cases': {
      const visitBranch = (caseBranch: AA.ACasesBranch): void => {
        makeExprDataEnv(caseBranch.body, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      };
      lettable.branches.forEach(visitBranch);
      makeExprDataEnv(lettable._else, sd, ad, mb, env, typeNameToVariants, aliasToTypeName);
      break;
    }
    // (The Pyret original also lists an a-id-safe-letrec branch, but
    // a-id-safe-letrec is not an ALettable variant, so it is unreachable.)
    case 'a-module':
    case 'a-app':
    case 'a-method-app':
    case 'a-prim-app':
    case 'a-ref':
    case 'a-tuple':
    case 'a-tuple-get':
    case 'a-obj':
    case 'a-update':
    case 'a-extend':
    case 'a-dot':
    case 'a-colon':
    case 'a-get-bang':
    case 'a-lam':
    case 'a-method':
    case 'a-id-var':
    case 'a-id-var-modref':
    case 'a-id-letrec':
    case 'a-val':
    case 'a-data-expr':
      break;
    default:
      throw new InternalCompilerError('makeLettableDataEnv: unknown lettable ' + (lettable as any).$name);
  }
}

/*
  Per-head flatness: the forward part (run in statement order; includes
  all sd/ad mutations and the value-side flatness) returns the backward
  part, a frame combining the flatness of everything after the head —
  work the recursive formulation on the nested representation did after
  its body call (a-let's annFlatness and the flatnessMax combinations),
  applied in the original unwind order (deepest first).
*/
type FlatnessFrame = (bodyFlatness: Flatness) => Flatness;

export function headFlatnessForward(
  head: AA.AExprHead,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): FlatnessFrame {
  switch (head.$name) {
    case 'a-type-let':
      return (bodyFlatness) => bodyFlatness;
    case 'a-let': {
      const bind = head.bind;
      const val = head.e;

      let valFlatness: Flatness;
      if (AA.isALam(val)) {
        const retFlatness = annFlatness(val.ret, sd, ad, mb, env);
        let argsFlatness = retFlatness;
        for (const elt of val.args) {
          argsFlatness = flatnessMax(argsFlatness, annFlatness(elt.ann, sd, ad, mb, env));
        }

        const bodyFlatness = makeExprFlatnessEnv(val.body, sd, ad, mb, env);
        const lamFlatness = flatnessMax(bodyFlatness, argsFlatness);

        sd.set(bind.id.key(), lamFlatness);
        // flatness of defining this lambda is 0, since we're not actually
        // doing anything with it
        valFlatness = 0;
      } else if (AA.isAIdSafeLetrec(val)) {
        // If we're binding this name to something that's already been defined
        // just copy over the definition
        // (NOTE: as in the Pyret original, this test can never succeed for
        // an ALettable; a-id-safe-letrec is an AVal variant)
        const valISL = val as unknown as AA.AIdSafeLetrec;
        if (sd.has(valISL.id.key())) {
          sd.set(bind.id.key(), sd.get(valISL.id.key()));
        }
        // flatness of the binding part of the let is 0 since we don't
        // call anything
        valFlatness = 0;
      } else if (AA.isAVal(val) && AA.isAIdModref(val.v)) {
        const funFlatness = getFlatnessForModuleFun(val.v.id, val.v.name, mb, env);
        sd.set(bind.id.key(), funFlatness);
        valFlatness = 0;
      } else {
        valFlatness = makeLettableFlatnessEnv(val, sd, ad, mb, env);
      }

      return (bodyFlatness) => {
        const annF = annFlatness(bind.ann, sd, ad, mb, env);
        return flatnessMax(flatnessMax(valFlatness, bodyFlatness), annF);
      };
    }
    case 'a-arr-let': {
      // Could maybe try to add some string like "bind.name + idx" to the
      // sd to let us keep track of the flatness if e is an a-lam, but for
      // now we don't since I'm not sure it'd work right.
      const annF = annFlatness(head.bind.ann, sd, ad, mb, env);
      const lettF = makeLettableFlatnessEnv(head.e, sd, ad, mb, env);
      return (bodyFlatness) => flatnessMax(annF, flatnessMax(lettF, bodyFlatness));
    }
    case 'a-var': {
      // Do same thing with a-var as with a-let for now
      const annF = annFlatness(head.bind.ann, sd, ad, mb, env);
      return (bodyFlatness) => flatnessMax(annF, bodyFlatness);
    }
    case 'a-seq': {
      const aFlatness = makeLettableFlatnessEnv(head.e1, sd, ad, mb, env);
      return (bodyFlatness) => flatnessMax(aFlatness, bodyFlatness);
    }
    default:
      throw new InternalCompilerError('headFlatnessForward: unknown head ' + (head as any).$name);
  }
}

// Calculate the flatness of aexpr, and along the way mutably update sd to
// contain mappings for all defined names of functions
export function makeExprFlatnessEnv(
  aexpr: AA.AExpr,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): Flatness {
  const frames: FlatnessFrame[] = [];
  for (const head of aexpr.heads) {
    frames.push(headFlatnessForward(head, sd, ad, mb, env));
  }
  let result = makeLettableFlatnessEnv(aexpr.e, sd, ad, mb, env);
  for (let i = frames.length - 1; i >= 0; i--) {
    result = frames[i](result);
  }
  return result;
}

export function incrementFlatness(f: Flatness): Flatness {
  if (f === undefined) {
    return undefined;
  } else {
    return f + 1;
  }
}

export function getFlatnessForCall(funName: string, sd: FEnv): Flatness {
  // If it's not in our lookup dict OR the flatness is none treat it the same
  if (sd.has(funName)) {
    return incrementFlatness(sd.get(funName));
  } else {
    return undefined;
  }
}

export function getFlatnessForModuleFun(
  id: A.Name,
  field: string,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): Flatness {
  const moduleInfo = mapGetValue(env.allModules, mapGetValue(mb, id.key()).uri);
  const provides = moduleInfo.provides;
  const valueExport = provides.values.get(field);
  if (valueExport === undefined) {
    return undefined;
  } else if (C.isVFun(valueExport)) {
    return valueExport.flatness;
  } else {
    return undefined;
  }
}

export function getFlatnessForModuleCall(
  id: A.Name,
  field: string,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): Flatness {
  return incrementFlatness(getFlatnessForModuleFun(id, field, mb, env));
}

export function makeLettableFlatnessEnv(
  lettable: AA.ALettable,
  sd: FEnv,
  ad: FEnv,
  mb: Map<string, C.ModuleBind>,
  env: C.CompileEnvironment
): Flatness {
  const defaultRet: Flatness = 0;
  switch (lettable.$name) {
    case 'a-module':
      return defaultRet;
    case 'a-if': {
      // The nested chain computed
      //   max(t1, wrap2(max(t2, wrap3(... max(tn, else) ...))))
      // where wrap_i applies arm i's test-computation head frames.
      // Forward pass (arm bodies and head pre-work, in arm order), then
      // a backward fold — the same value and the same sd/ad effect order
      // as the recursion, without stack growth in the arm count.
      const branches = lettable.branches;
      const branchFrames: FlatnessFrame[][] = [];
      const branchFlats: Flatness[] = [];
      for (const b of branches) {
        const frames: FlatnessFrame[] = [];
        for (const h of b.heads) {
          frames.push(headFlatnessForward(h, sd, ad, mb, env));
        }
        branchFrames.push(frames);
        branchFlats.push(makeExprFlatnessEnv(b.body, sd, ad, mb, env));
      }
      let result = makeExprFlatnessEnv(lettable.elseBody, sd, ad, mb, env);
      for (let i = branches.length - 1; i >= 0; i--) {
        result = flatnessMax(branchFlats[i], result);
        const frames = branchFrames[i];
        for (let j = frames.length - 1; j >= 0; j--) {
          result = frames[j](result);
        }
      }
      return result;
    }

    // NOTE -- a-assign might not be flat b/c it checks annotations
    case 'a-assign': {
      if (AA.isAId(lettable.value) && sd.has(lettable.value.id.key())) {
        // get-now(...).or-else(some(0)): absent means some(0); a stored
        // none stays none
        const current: Flatness = sd.has(lettable.id.key()) ? sd.get(lettable.id.key()) : 0;
        sd.set(lettable.id.key(),
          flatnessMax(current, mapGetValue(sd, lettable.value.id.key())));
      }
      return defaultRet;
    }

    case 'a-app': {
      const f = lettable._fun;
      // Look up flatness in the dictionary
      if (AA.isAId(f) || AA.isAIdSafeLetrec(f)) {
        return getFlatnessForCall(f.id.key(), sd);
      } else if (AA.isAIdModref(f)) {
        return getFlatnessForModuleCall(f.id, f.name, mb, env);
      } else {
        // This should never happen in a "correct" program, but it's not our job
        // to do this kind of checking here, so don't raise an error.
        return undefined;
      }
    }

    case 'a-method-app':
      // For now method calls are infinite flatness
      return undefined;

    // TODO: Treat prim-app as flat always? Track depths of prim-anns?
    case 'a-prim-app':
      return getFlatnessForCall(lettable.f, sd);

    // May check unknown annotations, so is nonflat
    case 'a-update':
      return undefined;

    // These are flat value constructors, and due to ANF, they only contain
    // values as sub-fields
    case 'a-ref': return defaultRet;
    case 'a-tuple': return defaultRet;
    case 'a-tuple-get': return defaultRet;
    case 'a-obj': return defaultRet;

    case 'a-extend': return defaultRet;
    case 'a-dot': return defaultRet;
    case 'a-colon': return defaultRet;
    case 'a-get-bang': return defaultRet;
    case 'a-lam': return defaultRet;
    case 'a-method': return defaultRet;
    case 'a-id-var': return defaultRet;
    case 'a-id-var-modref': return defaultRet;
    case 'a-id-letrec': return defaultRet;
    // (The Pyret original also lists an a-id-safe-letrec branch, but
    // a-id-safe-letrec is not an ALettable variant, so it is unreachable.)
    case 'a-val': return defaultRet;
    case 'a-data-expr': return defaultRet;
    // NOTE -- cases might not be flat b/c it checks annotations
    case 'a-cases': {
      // Flatness is the max of the flatness all the cases branches
      const combine = (caseBranch: AA.ACasesBranch, maxFlatAcc: Flatness): Flatness => {
        const branchFlatness = makeExprFlatnessEnv(caseBranch.body, sd, ad, mb, env);
        return flatnessMax(maxFlatAcc, branchFlatness);
      };
      let maxFlat: Flatness = 0;
      for (const b of lettable.branches) {
        maxFlat = combine(b, maxFlat);
      }

      const elseFlat = makeExprFlatnessEnv(lettable._else, sd, ad, mb, env);
      const typFlat = annFlatness(lettable.typ, sd, ad, mb, env);
      return flatnessMax(typFlat, flatnessMax(maxFlat, elseFlat));
    }
    default:
      throw new InternalCompilerError('makeLettableFlatnessEnv: unknown lettable ' + (lettable as any).$name);
  }
}

export function makeProgFlatnessEnv(
  anfed: AA.AProg,
  postEnv: C.ComputedEnvironment,
  env: C.CompileEnvironment
): FlatnessEnv {
  const pe = asVariant(postEnv, C.ComputedEnv);
  const bindings = pe.bindings;
  const moduleBindings = pe.moduleBindings;
  const mb = moduleBindings;
  const typeBindings = pe.typeBindings;

  const sd: FEnv = new Map();
  for (const k of bindings.keys()) {
    const vb = mapGetValue(bindings, k);
    if (!vb.origin.newDefinition) {
      if (A.isSGlobal(vb.atom)) {
        const name = vb.atom.toname();
        const ve = env.globalValue(name);
        if (ve !== undefined) {
          if (C.isVFun(ve)) {
            sd.set(vb.atom.key(), ve.flatness);
          }
        }
      } else {
        const valueExport = env.valueByUri(vb.origin.uriOfDefinition, vb.origin.originalName.toname());
        if (valueExport === undefined) {
          raise('The name: ' + vb.atom.toname() + ' could not be found on the module ' + vb.origin.uriOfDefinition);
        } else {
          if (C.isVFun(valueExport)) {
            sd.set(k, valueExport.flatness);
          }
        }
      }
    }
  }

  const ad: FEnv = new Map();
  function initTypeProvides(provides: C.Provides, tb: C.TypeBind): void {
    const name = tb.origin.originalName.toname();

    if (provides.dataDefinitions.has(name)) {
      // NOTE(joe): Datatypes _must_ just be flat brand checks
      ad.set(tb.atom.key(), 0);
    } else if (provides.aliases.has(name)) {
      // NOTE(joe): Right now we don't trust any cross-module aliases. We need to
      // get either a representation of flatness for annotations in provides, or
      // make sure that all provided annotations have a path back to the
      // underlying annotation in terms of datatypes and simple constructors so we
      // can use ann-flatness on them
      ad.set(tb.atom.key(), undefined);
    } else {
      raise("Unknown type key (shouldn't happen): " + name);
    }
  }
  for (const k of typeBindings.keys()) {
    const tb = mapGetValue(typeBindings, k);
    if (!tb.origin.newDefinition) {
      if (A.isSTypeGlobal(tb.atom)) {
        const name = tb.atom.toname();
        const providesOpt = env.providesByTypeName(name);
        if (providesOpt !== undefined) {
          initTypeProvides(providesOpt, tb);
        }
      } else {
        const modProvides = env.providesByUri(tb.origin.uriOfDefinition);
        if (modProvides === undefined) {
          raise('There is a type binding whose module is not in the compile env: ' + torepr(k) + ' ' + tb.origin.uriOfDefinition);
        } else {
          initTypeProvides(modProvides, tb);
        }
      }
    }
  }

  // cases(AA.AProg) anfed: | a-program(_, prov, imports, body)
  const body = anfed.body;
  makeExprDataEnv(body, sd, ad, mb, env, new Map<string, AA.AVariant[]>(), new Map<string, string>());
  makeExprFlatnessEnv(body, sd, ad, mb, env);
  return [sd, ad];
}

export function getDefinedValues(ast: AA.AProg): Map<string, string> {
  // The program's tail lettable must be the a-module.
  const tail = ast.body.e;
  if (!AA.isAModule(tail)) {
    raise('Ill-formed ANF ast: ' + torepr(tail));
  }
  const theModule = tail as AA.AModule;
  const theDvs = theModule.definedValues;

  const dvsDict = new Map<string, string>();
  for (const d of theDvs) {
    switch (d.$name) {
      case 'a-defined-value':
        dvsDict.set(d.name, field(d.value, 'id').key());
        break;
      case 'a-defined-var':
        dvsDict.set(d.name, d.id.key());
        break;
      default:
        throw new InternalCompilerError('getDefinedValues: unknown defined value ' + (d as any).$name);
    }
  }

  return dvsDict;
}

export function getFlatProvides(
  provides: C.Provides,
  env: C.CompileEnvironment,
  postEnv: C.ComputedEnvironment,
  flatnessEnvAndTypes: FlatnessEnv,
  ast: AA.AProg
): C.Provides {
  // dvs-dict is computed (and may raise on ill-formed ASTs) but unused,
  // exactly as in the Pyret original
  getDefinedValues(ast);
  const flatnessEnv = flatnessEnvAndTypes[0];
  const pe = asVariant(postEnv, C.ComputedEnv);
  // cases(C.Provides) provides: | provides(uri, modules, values, aliases, datatypes)
  const uri = provides.fromUri;
  const modules = provides.modules;
  const values = provides.values;
  const aliases = provides.aliases;
  const datatypes = provides.dataDefinitions;
  const newValues = new Map<string, C.ValueExport>();
  for (const k of values.keys()) {
    let newVal: C.ValueExport;
    const bind = pe.env.get(k);
    if (bind === undefined) {
      newVal = mapGetValue(values, k);
    } else {
      // MutableStringDict<Flatness>.get-now is a double Option: use has()
      // to distinguish absent from present-but-none
      const hasFlatness = flatnessEnv.has(bind.atom.key());
      const maybeFlatness = flatnessEnv.get(bind.atom.key());
      const ve = mapGetValue(values, k);
      let existingVal: C.ValueExport;
      if (C.isVAlias(ve)) {
        existingVal = env.valueByUriValue(ve.origin.uriOfDefinition, ve.origin.originalName.toname());
      } else {
        existingVal = ve;
      }
      if (!hasFlatness) {
        newVal = ve;
      } else {
        // existing-val.t errors in Pyret if existing-val is a v-alias;
        // mirrored here with a checked field access
        newVal = new C.VFun(ve.origin, field(existingVal, 't'), k, maybeFlatness);
      }
    }
    newValues.set(k, newVal);
  }
  return new C.Provides(uri, modules, newValues, aliases, datatypes);
}
