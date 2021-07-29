import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as SL from './ts-srcloc';
import type * as CS from './ts-compile-structs';
import type * as TD from './ts-type-defaults';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as TCSH from './ts-compile-structs-helpers';
import type { List, MutableStringDict, PFunction, StringDict, Option, PTuple } from './ts-impl-types';
import { typeofTypeAnnotation } from '@babel/types';

({
  requires: [
    { 'import-type': 'builtin', name: 'srcloc'},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-check-structs.arr']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-type-defaults']},
 ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "type-check": "tany",
      "empty-context": "tany",
    }
  },
  theModule: function(runtime, _, __, SL : SL.Exports, tj : TJ.Exports, TCSH : (TCSH.Exports), TSin : TS.Exports, A : A.Exports, CSin : CS.Exports, TCS : TCS.Exports, TD : TD.Exports) {
    const {
      ExhaustiveSwitchError,
      InternalCompilerError,
      MakeName,
      listToArray,
      nameToKey,
      nameToName,
      sameName,
      formatSrcloc,
      map,
      mapFromStringDict,
      mapFromMutableStringDict,
      stringDictFromMap,
    } = tj;
    const { builtin } = SL.dict.values.dict;
    const TS = TSin.dict.values.dict;
    const builtinUri = TS['module-uri'].app("builtin://global");
    const {
      globalValueValue,
      callMethod,
      unwrap,
      providesByUri,
      valueByUriValue,
      resolveDatatypeByUriValue,
      typeByUri,
    } = TCSH;
    const CS = CSin.dict.values.dict;
    const { 's-global': sGlobal, 's-type-global': sTypeGlobal } = A.dict.values.dict;
    const { 
      typed,
      'tc-info': tcInfo,
      'empty-info': emptyInfo,
      'fold-result': foldResult,
      'fold-errors': foldErrors,
      "typing-context": typingContext
    } = TCS.dict.values.dict;
    
    class TypeCheckFailure extends Error {
      errs : CS.CompileError[];
      constructor(...errs : CS.CompileError[]) {
        super("type error " + require('util').inspect(errs, {depth:null}));
        this.errs = errs;
      }
    }

    function prettyIsh(v : any) : string {
      return require('util').inspect(v, {depth:null});
    }

    function sameOrigin(o1 : TS.NameOrigin, o2 : TS.NameOrigin) : boolean {
      switch(o1.$name) {
        case "local": {
          if(o1.$name !== o2.$name) { return false; }
          return true;
        }
        case "module-uri": { 
          if(o1.$name !== o2.$name) { return false; }
          return o1.dict.uri === o2.dict.uri;
        }
        case "dependency": {
          if(o1.$name !== o2.$name) { return false; }
          return o1.dict.dep === o2.dict.dep;
        }
      }

    }

    function foldrFoldResult<X, Y>(f : (x: X, context: TCS.Context, acc: Y) => TCS.FoldResult<Y>, xs: X[], context: TCS.Context, base: Y): TCS.FoldResult<Y> {
      return xs.reduceRight((prev: TCS.FoldResult<Y>, cur: X): TCS.FoldResult<Y> => {
        switch(prev.$name) {
          case 'fold-errors': return prev;
          case 'fold-result': {
            return f(cur, prev.dict.context, prev.dict.v);
          }
          default: throw new ExhaustiveSwitchError(prev);
        }
      }, foldResult.app(base, context));
    }

    function toType(inAnn : A.Ann, context : Context) : TS.Type | false {
      function toArrowType(l : SL.Srcloc, argAnns : A.Ann[], ret : A.Ann) : TS.Type {
        const argTyps: TS.Type[] = [];
        for (let arg of argAnns) {
          const argTyp = toType(arg, context);
          if (argTyp) {
            argTyps.push(argTyp);
          } else {
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`no argument annotation provided on ${arg}`, l));
          }
        }
        const retTyp = toType(ret, context);
        if (!retTyp) {
          throw new TypeCheckFailure(CS['cant-typecheck'].app(`no return annotation provided on ${ret}`, l));
        }
        return TS['t-arrow'].app(runtime.ffi.makeList(argTyps), retTyp, l, false);
      }
      switch(inAnn.$name) {
        case 'a-blank': return false;
        case 'a-any': return TS['t-top'].app(inAnn.dict.l, false);
        case 'a-name': {
          const idKey = nameToKey(inAnn.dict.id);
          if (context.aliases.has(idKey)) {
            return context.aliases.get(idKey);
          } else {
            throw new TypeCheckFailure(CS['unbound-type-id'].app(inAnn));
          }
        }
        case 'a-type-var': {
          return TS['t-var'].app(inAnn.dict.id, inAnn.dict.l, false);
        }
        case 'a-arrow-argnames': {
          const args = listToArray(inAnn.dict.args);
          return toArrowType(inAnn.dict.l, args.map(a => a.dict.ann), inAnn.dict.ret);
        }
        case 'a-arrow': {
          return toArrowType(inAnn.dict.l, listToArray(inAnn.dict.args), inAnn.dict.ret);
        }
        case 'a-method': {
          throw new TypeCheckFailure(CS['cant-typecheck'].app("a-method toType not yet implemented", inAnn.dict.l));
        }
        case 'a-record': {
          const fields = listToArray(inAnn.dict.fields);
          const fieldTyps = new Map<string, TS.Type>();
          for (const field of fields) {
            const fieldTyp = toType(field.dict.ann, context);
            if (fieldTyp) {
              fieldTyps.set(field.dict.name, fieldTyp);
            } else {
              throw new TypeCheckFailure(CS['cant-typecheck'].app(`no annotation provided on field ${field}`, inAnn.dict.l));
            }
          }
          return TS['t-record'].app(stringDictFromMap(fieldTyps), inAnn.dict.l, false);
        }
        case 'a-tuple': {
          const elts = listToArray(inAnn.dict.fields);
          const eltTyps : TS.Type[] = [];
          for (const elt of elts) {
            const eltTyp = toType(elt, context);
            if (eltTyp) {
              eltTyps.push(eltTyp);
            } else {
              const newExists = newExistential(inAnn.dict.l, true);
              context.addVariable(newExists);
              eltTyps.push(newExists);
            }
          }
          return TS['t-tuple'].app(runtime.ffi.makeList(eltTyps), inAnn.dict.l, false);
        }
        case 'a-app': {
          const annTyp = toType(inAnn.dict.ann, context);
          if (!annTyp) {
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`no annotation provided on ${inAnn.dict.ann}`, inAnn.dict.l));
          }
          const args = listToArray(inAnn.dict.args);
          const argTyps : TS.Type[] = [];
          for (let arg of args) {
            const argTyp = toType(arg, context);
            if (!argTyp) {
              throw new TypeCheckFailure(CS['cant-typecheck'].app(`no annotation on app argument ${arg}`, inAnn.dict.l));
            }
            argTyps.push(argTyp);
          }
          return TS['t-app'].app(annTyp, runtime.ffi.makeList(argTyps), inAnn.dict.l, false);
        }
        case 'a-pred': {
          const annTyp = toType(inAnn.dict.ann, context);
          if (!annTyp) {
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`no annotation provided on ${inAnn.dict.ann}`, inAnn.dict.l));
          }
          const expectTyp = TS['t-arrow'].app(runtime.ffi.makeList([annTyp]), tBoolean(inAnn.dict.l), inAnn.dict.l, false);
          checking(inAnn.dict.exp, expectTyp, false, context);
          return annTyp;
        }
        case 'a-dot': {
          const objKey = nameToKey(inAnn.dict.obj)
          if (!context.moduleNames.has(objKey)) {
            throw new TypeCheckFailure(CS['no-module'].app(inAnn.dict.l, nameToName(inAnn.dict.obj)));
          }
          const origin = context.moduleNames.get(objKey);
          const tMod = context.modules.get(origin);
          const aliases = mapFromStringDict(tMod.dict.aliases);
          if (aliases.has(inAnn.dict.field)) {
            return resolveAlias(aliases.get(inAnn.dict.field), context);
          } else {
            throw new TypeCheckFailure(CS['unbound-type-id'].app(inAnn));
          }
        }
        case 'a-checked': {
          throw new TypeCheckFailure(CS['cant-typecheck'].app(`a-checked should not be appearing before type-checking: ${prettyIsh(inAnn)}`, A.dict.values.dict['dummy-loc']));
        }
        default:
          throw new ExhaustiveSwitchError(inAnn);
      }
    }

    const primitiveTypesUri = TS['module-uri'].app("builtin://primitive-types");

    function tNumber(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Number"), l, false);
    }
    function tBoolean(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Boolean"), l, false);
    }

    // Note: if typeKey(t1) === typeKey(t2), then t1 == t2 (as Pyret values)
    function typeKey(type : TS.Type & { $key?: string }) : string {
      if(!("$key" in type)) {
        const key = _typeKey(type);
        type.$key = key;
      }
      return type.$key;
    }
    function _typeKey(type: TS.Type): string {
      switch(type.$name) {
        case 't-name': {
          const { "module-name": modName, id } = type.dict;
          switch(modName.$name) {
            case 'local': return nameToKey(id);
            case 'module-uri': return `${modName.dict.uri}.${nameToKey(id)}`;
            case 'dependency': 
              throw new InternalCompilerError(`Should not get dependency in type-checker: ${modName.dict.dep}`);
            default: throw new ExhaustiveSwitchError(modName);
          }
        }
        case 't-arrow': {
          const { args, ret } = type.dict;
          return `(${listToArray(args).map(typeKey).join(", ")} -> ${typeKey(ret)})`;
        }
        case 't-app': {
          const { onto, args } = type.dict;
          return `${typeKey(onto)}<${listToArray(args).map(typeKey).join(", ")}>`;
        }
        case 't-top': return 'Any';
        case 't-bot': return 'Bot';
        case 't-record': {
          const fields = mapFromStringDict(type.dict.fields);
          const fieldStrs = [];
          for (const [field, typ] of fields) {
            fieldStrs.push(`${field} :: ${typeKey(typ)}`);
          }
          return `{${fieldStrs.join(", ")}}`;
        }
        case 't-tuple': {
          const elts = listToArray(type.dict.elts);
          return `{${elts.map(typeKey).join("; ")}}`;
        }
        case 't-forall': {
          const { introduces, onto } = type.dict;
          return `<${listToArray(introduces).map(typeKey).join(", ")}>${typeKey(onto)}`;
        }
        case 't-ref': return `ref ${typeKey(type.dict.typ)}`;
        case 't-data-refinement': {
          const { "data-type": dataType, "variant-name": variantName } = type.dict;
          return `(${typeKey(dataType)}%is-${variantName})`;
        }
        case 't-var': return nameToKey(type.dict.id);
        case 't-existential': return nameToKey(type.dict.id);
        default: throw new ExhaustiveSwitchError(type);
      }
    }

    function gatherProvides(provide: A.ProvideBlock, context: Context): TCS.TCInfo {
      switch(provide.$name) {
        case 's-provide-block': {
          const curTypes = new Map<string, TS.Type>();
          const curAliases = context.aliases;
          const curData = context.dataTypes;
          // Note(Ben): I'm doing two things differently than the original Pyret code:
          // 1. I'm traversing the list of specs from first to last.  If this ultimately matters,
          //    we could reverse the array on the next line before traversing it.
          // 2. I'm mutably updatng the three dictionaries object above, rather than functionally 
          //    folding over a `TCInfo` object.  Since the original code never produced `fold-errors`
          //    objects, it's not necessary to mimic all of foldr-fold-result here.
          for (const spec of listToArray(provide.dict.specs)) {
            switch(spec.$name) {
              case 's-provide-name': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const valueKey = nameToKey(nameSpec.dict.name);
                    if (curTypes.has(valueKey)) {
                      break; // nothing more to do
                    } else {
                      // MARK(joe): test as-name here; it appears unused
                      const getValueFromContext = context.info.types.get(valueKey);
                      if (getValueFromContext.$name) {
                        const typ = setInferred(getValueFromContext, false);
                        curTypes.set(valueKey, typ);
                      }
                      else {
                        const typ = setInferred(context.globalTypes.get(valueKey), false);
                        curTypes.set(valueKey, typ);
                      }
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              case 's-provide-type': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const aliasKey = nameToKey(nameSpec.dict.name);
                    if (curAliases.has(aliasKey)) {
                      break; // nothing to do
                    } else {
                      const typ = context.aliases.get(aliasKey);
                      curAliases.set(aliasKey, typ);
                      break;
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              case 's-provide-module': {
                break; // nothing to do here
              }
              case 's-provide-data': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const dataKey = nameToKey(nameSpec.dict.name);
                    if (curData.has(dataKey)) {
                      break; // nothing to do
                    } else {
                      const typ = context.dataTypes.get(dataKey);
                      curData.set(dataKey, typ);
                      break;
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              default: throw new ExhaustiveSwitchError(spec);
            }
          }
          return new TCInfo(curTypes, curAliases, curData).toPyretTCInfo();
        }
        default: throw new ExhaustiveSwitchError(provide.$name);
      }
    }

    // NOTE(Ben): In places where I use arrays instead of Pyret lists,
    // I use Array.push rather than Array.unshift, so the arrays
    // aren't necessarily in the same order as they were in the Pyret
    // implementation.  I don't know whether this will be important.
    type ExampleTypeInfo = {
      existential: TS.Type,
      annTypes: {argTypes: TS.Type[], retType : TS.Type, loc : SL.Srcloc},
      exampleTypes: TS.Type[],
      checkFunction: (type: TS.Type, context: Context) => TS.Type,
      functionName: string
    };

    type ExampleTypes = Map<string, ExampleTypeInfo>;
    
    type FieldConstraint = Map<string, TS.Type[]>;

    function mapMapValues<K, V1, V2>(m : Map<K, V1>, f : ((val : V1) => V2)) : Map<K, V2> {
      const ret = new Map<K, V2>();
      for (const [k, v1] of m.entries()) {
        ret.set(k, f(v1));
      }
      return ret;
    }

    type Constraint = {subtype: TS.Type, supertype: TS.Type};
    type Refinement = {existential: TS.Type, dataRefinement: TJ.Variant<TS.Type, 't-data-refinement'>};

    class ConstraintLevel {
      name?: string;
      // the constrained existentials
      variables : Map<string, TS.Type>;
      // list of {subtype; supertype}
      constraints : Array<Constraint>;
      // list of {existential; t-data-refinement} 
      refinementConstraints : Array<Refinement>;
      // type -> {type, field labels -> field types (with the location of their use)}
      fieldConstraints : Map<string, [TS.Type, FieldConstraint]>; 
      // types for examples?
      exampleTypes : ExampleTypes;

      constructor(name? : string) {
        this.variables = new Map();
        this.constraints = [];
        this.refinementConstraints = [];
        this.fieldConstraints = new Map();
        this.exampleTypes = new Map();
        this.name = name;
      }

      addFieldConstraint(type : TS.Type, fieldName : string, fieldType : TS.Type) : void{
        const typKey = typeKey(type);
        if (this.fieldConstraints.has(typKey)) {
          const [_typ, labelMapping] = this.fieldConstraints.get(typKey);
          if (labelMapping.has(fieldName)) {
            labelMapping.get(fieldName).push(fieldType);
          } else {
            labelMapping.set(fieldName, [fieldType]);
          }
        } else {
          const newMap: FieldConstraint = new Map();
          newMap.set(fieldName, [fieldType]);
          this.fieldConstraints.set(typKey, [type, newMap]);
        }
      }
    }
    class ConstraintSystem {
      levels: ConstraintLevel[];
      constructor() {
        this.levels = [];
      }
      toString() {
        const result = { levels: [] }
        for(let level of this.levels) {
          const l = {name : level.name, variables: {}, constraints: [], fieldConstraints: {}};
          result.levels.push(l);
          for(let [key, val] of level.variables) {
            l.variables[key] = typeKey(val);
          }
          for(let {supertype, subtype} of level.constraints) {
            l.constraints.push(`${typeKey(subtype)} < ${typeKey(supertype)}`);
          }
          for(let [key, [typ, constraint]] of level.fieldConstraints) {
            l.constraints[key] = {};
            for(let [fieldName, typs] of constraint) {
              l.constraints[key][fieldName] = `[ ${typs.map(typeKey).join(", ")} ]`;
            }
          }
        }
        return prettyIsh(result);
      }
      toPyretConstraintSystem(): TCS.ConstraintSystem {
        let ret : TCS.ConstraintSystem = TCS.dict.values.dict['no-constraints'];
        for (const level of this.levels) {
          let fieldConstraints = mapMapValues(level.fieldConstraints, ([type, fc]) : PTuple<[TS.Type, StringDict<List<TS.Type>>]> => {
            return runtime.makeTuple([
              type, 
              stringDictFromMap(mapMapValues(fc, runtime.ffi.makeList))
            ]);
          });
          ret = TCS.dict.values.dict['constraint-system'].app(
            runtime.ffi.makeTreeSet(level.variables.values()),
            runtime.ffi.makeList(level.constraints.map((c) => runtime.makeTuple([c.subtype, c.supertype]))),
            runtime.ffi.makeList(level.refinementConstraints.map((r) => runtime.makeTuple([r.existential, r.dataRefinement]))),
            stringDictFromMap(fieldConstraints),
            // {Type; {arg-types :: List<Type>, ret-type :: Type, loc :: Loc}; List<Type>; (Type, Context -> TypingResult); String}
            stringDictFromMap(mapMapValues(level.exampleTypes, (exTyInfo) => {
              return runtime.makeTuple([
                exTyInfo.existential,
                runtime.makeObject({
                  'arg-types': runtime.ffi.makeList(exTyInfo.annTypes.argTypes),
                  'ret-type': exTyInfo.annTypes.retType,
                  'loc': exTyInfo.annTypes.loc,
                }),
                runtime.ffi.makeList(exTyInfo.exampleTypes),
                runtime.makeFunction(exTyInfo.checkFunction),
                exTyInfo.functionName
              ]);
            })),
            ret
          )
        }
        return ret;
      }

      ensureLevel(msg : string): void {
        if (this.levels.length === 0) {
          throw new InternalCompilerError(msg);
        }
      }
      curLevel(): ConstraintLevel { return this.levels[this.levels.length - 1]; }
      addVariable(variable : TS.Type): void {
        this.ensureLevel("Can't add variable to an uninitialized system");
        if (variable.$name === 't-existential') {
          this.curLevel().variables.set(typeKey(variable), variable);
        }
      }
      addVariableSet(variables: Map<string, TS.Type> | TS.Type[]): void {
        this.ensureLevel("Can't add variables to an uninitialized system");
        const curLevel = this.curLevel();
        if(variables instanceof Map) {
          for (const [key, typ] of variables.entries()) {
            curLevel.variables.set(key, typ);
          }
        }
        else {
          for (const variable of variables) {
            curLevel.variables.set(typeKey(variable), variable);
          }
        }
      }
      addConstraint(subtype: TS.Type, supertype: TS.Type): void {
        // inlining the call to ensureLevel so we don't needlessly construct the message
        if (this.levels.length === 0) { 
          const msg = `Can't add constraint to an uninitialized system: ${JSON.stringify(subtype)} = ${JSON.stringify(supertype)}\n${formatSrcloc(subtype.dict.l, true)}\n${formatSrcloc(supertype.dict.l, true)}`;
          throw new InternalCompilerError(msg);
        }
        if (subtype.$name === 't-existential' && supertype.$name === 't-data-refinement') {
          this.curLevel().refinementConstraints.push({
            existential: subtype,
            dataRefinement: supertype,
          });
        } else if (supertype.$name === 't-existential' && subtype.$name === 't-data-refinement') {
          this.curLevel().refinementConstraints.push({
            existential: supertype,
            dataRefinement: subtype,
          });
        } else {
          this.curLevel().constraints.push({ subtype, supertype });
        }
      }
      addFieldConstraint(type: TS.Type, fieldName: string, fieldType: TS.Type): void {
        this.ensureLevel("Can't add field constraints to an uninitialized system");
        const curLevel = this.curLevel();
        curLevel.addFieldConstraint(type, fieldName, fieldType);
      }
      addExampleVariable(
        existential: TS.Type,
        argTypes: TS.Type[],
        retType: TS.Type,
        loc: SL.Srcloc, 
        checkFunction: ExampleTypeInfo['checkFunction'],
        functionName: string
      ): void {
        this.ensureLevel("Can't add example variable to an uninitialized system");
        this.curLevel().exampleTypes.set(typeKey(existential), {
          existential,
          annTypes: { argTypes, retType, loc },
          exampleTypes: [],
          checkFunction,
          functionName
        })
      }
      addExampleType(existential: TS.Type, type: TS.Type): void {
        const existentialKey = typeKey(existential);
        let level = this.levels.length - 1;
        while (level >= 0) {
          const curLevel = this.levels[level];
          if (!curLevel.exampleTypes.has(existentialKey)) {
            level -= 1;
          } else {
            const curInfo = curLevel.exampleTypes.get(existentialKey);
            curInfo.exampleTypes.push(type);
            return;
          }
        }
        throw new InternalCompilerError("Can't add example type to an uninitialized system");
      }
      addLevel(name?: string) : void{
        const level = new ConstraintLevel(name);
        this.levels.push(level);
      }
      // ASSUMES(joe/ben): this === context.constraints
      // Therefore in solveHelperFields & friends we don't need to do any merging of variables
      // that happened in the existing type-checker, context.constraints aliases `this` and
      // sees the same updates. Joe and Ben assert/assume/hope/etc. that this is Good Mutation.
      solveLevelHelper(solution : ConstraintSolution, context : Context) : ConstraintSolution {
        const afterConstraints = solveHelperConstraints(this, solution, context);
        const afterRefinements = solveHelperRefinements(this, afterConstraints, context);
        const afterExamples = solveHelperExamples(this, afterRefinements, context);
        const afterFields = solveHelperFields(this, afterExamples, context);
        return afterFields;
      }
      // After solve-level, the system will have one less level (unless it's already
      // empty) It breaks the current level in two based on the set of variables
      // that came from examples, then solves them first by solving the part
      // unrelated to examples, then solving the part related to examples.  It then
      // propagates variables to the context at the next level (if there is a next
      // level) and returns the resulting system and solution
      solveLevel(context : Context) : ConstraintSolution {
        if (this.levels.length === 0) { return new ConstraintSolution(); }

        const levelToSplit = this.levels.pop();
        // NOTE(old impl): introduce a half level so any constraints depending
        // on test inference can be solved after test inference
        this.addLevel("solveLevel half level");
        for(let { existential } of levelToSplit.exampleTypes.values()) {
          this.addVariable(existential); // Adds to the newly added empty level from .addLevel()
          levelToSplit.variables.delete(typeKey(existential));
        }
        this.levels.push(levelToSplit);
        const solutionWithoutExamples = this.solveLevelHelper(new ConstraintSolution(), context);
        // this.curLevel() is logically at the same depth as levelToSplit, but re-fetch curLevel()
        // to allow solveLevelHelper implementation flexibility
        const variablesFromCurLevel = this.curLevel().variables;
        // Removes levelToSplit and exposes the empty added level from .addLevel()
        this.levels.pop();
        this.addVariableSet(variablesFromCurLevel);
        const solutionWithExamples = this.solveLevelHelper(solutionWithoutExamples, context);
        this.ensureLevel("Done solving the split level, there should be the empty level left");
        // curLevel() is logically at the same depth as the empty level from addLevel() above
        const variablesToPreserve = this.curLevel().variables;
        // Removes the level that just had the examples in it
        this.levels.pop();
        if(this.levels.length > 0) { this.addVariableSet(variablesToPreserve); }
        return new ConstraintSolution(variablesToPreserve, solutionWithExamples.substitutions);
      }
    }

    function substituteInConstraints(newType : TS.Type, typeVar : TS.Type, constraints : Constraint[]) {
      return constraints.map(({subtype, supertype}) => {
        return {
          subtype: substitute(subtype, newType, typeVar),
          supertype: substitute(supertype, newType, typeVar)
        };
      });
    }
    
    function substituteInRefinements(newType : TS.Type, typeVar : TS.Type, refinements : Refinement[]) : Refinement[] {
      return refinements.map(({existential, dataRefinement}) => {
        return {
          existential: substitute(existential, newType, typeVar),
          // NOTE(joe): the cast below assumes that substitute cannot change
          // the shape of a dataRefinement
          dataRefinement: substitute(dataRefinement, newType, typeVar) as Refinement["dataRefinement"]
        };
      });
    }

    function substituteInFields(newType : TS.Type, typeVar : TS.Type, fieldConstraints : Map<string, [TS.Type, FieldConstraint]>) {
      for(let [key, [constraintType, fieldMappings]] of fieldConstraints) {
        const newConstraintType = substitute(constraintType, newType, typeVar);
        for(let [fieldName, types] of fieldMappings) {
          fieldMappings.set(fieldName, types.map(t => substitute(t, newType, typeVar)));
        }
        // NOTE(joe/ben): why is this key not updated?
        fieldConstraints.set(key, [newConstraintType, fieldMappings]);
      }
    }

    function substituteInExamples(newType : TS.Type, typeVar : TS.Type, examples : ExampleTypes) {
      for(let [key, exampleTypeInfo] of examples) {
        exampleTypeInfo.exampleTypes = exampleTypeInfo.exampleTypes.map(t => substitute(t, newType, typeVar));
      }
    }

    function addSubstitution(newType : TS.Type, typeVar : TS.Type, system : ConstraintSystem, solution : ConstraintSolution) {
      solution.substitutions.set(typeKey(typeVar), [newType, typeVar]);
      const curLevel = system.curLevel();
      curLevel.constraints = substituteInConstraints(newType, typeVar, curLevel.constraints);
      curLevel.refinementConstraints = substituteInRefinements(newType, typeVar, curLevel.refinementConstraints);
      substituteInFields(newType, typeVar, curLevel.fieldConstraints);
      substituteInExamples(newType, typeVar, curLevel.exampleTypes);
    }

    function solveHelperFields(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      const { fieldConstraints, variables } = system.curLevel();
      const entries = [...(fieldConstraints.entries())];
      while(entries.length !== 0) {
        const [key, [typ, fieldMappings]] = entries.pop();
        const instantiated = instantiateObjectType(typ, context);
        switch(typ.$name) {
          case "t-record": {
            const fields = mapFromStringDict(typ.dict.fields);
            const requiredFieldSet = new Set(fieldMappings.keys());
            const intersection = new Set<string>();
            const remainingFields = new Set<string>();
            for(let s of requiredFieldSet) {
              if(fields.has(s)) { intersection.add(s); }
              else { remainingFields.add(s); }
            }
            if(remainingFields.size > 0) {
              const missingFieldErrors = [...remainingFields].map(rfn =>
                CS['object-missing-field'].app(rfn, String(typ), typ.dict.l, fieldMappings.get(rfn)[0].dict.l)
              );
              throw new TypeCheckFailure(...missingFieldErrors);
            }
            else {
              for(let fieldName of intersection) {
                for(let fieldType of fieldMappings.get(fieldName)) {
                  const objectFieldType = fields.get(fieldName);
                  system.addConstraint(objectFieldType, fieldType);
                }
              }
              // NOTE(joe/ben): this early return from the loop is kinda weird. Note that
              // solveLevelHelper will get back here! But the existing algorithm does not
              // process the rest of the fieldConstraints now, in the case of t-record
              // it goes onto other constraints first.
              return system.solveLevelHelper(solution, context);
            }
          }
          case "t-existential": {
            if(variables.has(typeKey(typ))) {
              throw new TypeCheckFailure(CS['unable-to-infer'].app(typ.dict.l));
            }
            for(let [fieldName, fieldTypes] of fieldMappings) {
              for(let fieldType of fieldTypes) {
                system.levels[system.levels.length - 2].addFieldConstraint(typ, fieldName, fieldType);
              }
            }
            // NOTE(ben/joe): this is a recursive call in the original Pyret code
            continue;
          }
          default: {
            const dataType = instantiateDataType(typ, context);
            const dataFields = mapFromStringDict(dataType.dict.fields);
            for(let [fieldName, fieldTypes] of fieldMappings) {
              if(dataFields.has(fieldName)) {
                const dataFieldType = dataFields.get(fieldName);
                for(let fieldType of fieldTypes) {
                  system.addConstraint(dataFieldType, fieldType);
                }
              }
              else {
                throw new TypeCheckFailure(CS['object-missing-field'].app(fieldName, String(typ), typ.dict.l, fieldMappings.get(fieldName)[0].dict.l))
              }
            }
            return system.solveLevelHelper(solution, context);
          }
        }
      }
      return solution;
    }

    function solveHelperConstraints(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      const { constraints, variables } = system.curLevel();
      // NOTE(joe): Compared to type-check-structs.arr, here continue; is a recursive call
      while(constraints.length !== 0) {
        const { subtype, supertype } = constraints.pop();
        if(supertype.$name === "t-top" || subtype.$name === "t-bot") {
          continue;
        }

        switch(supertype.$name) {
          case "t-existential": {
            const { id: aId, l: aLoc } = supertype.dict;
            switch(subtype.$name) {
              case "t-existential": {
                const { id: bId, l: bLoc } = supertype.dict;
                if (nameToKey(aId) === nameToKey(bId)) {
                  continue;
                }
                else if(variables.has(typeKey(subtype))) {
                  addSubstitution(supertype, subtype, system, solution);
                  continue;
                }
                else if(variables.has(typeKey(supertype))) {
                  addSubstitution(subtype, supertype, system, solution);
                  continue;
                }
                else {
                  // NOTE(joe/ben): 1. Is this repeatable as a method? 2. Why
                  // push out the constraint to the next level?
                  system.levels[system.levels.length - 2].constraints.push({ subtype, supertype });
                  continue;
                }
              }
              default: {
                if(variables.has(typeKey(supertype))) {
                  if(varNotFoundInType(subtype, supertype)) {
                    addSubstitution(subtype, supertype, system, solution);
                    continue;
                  }
                  else {
                    throw new TypeCheckFailure(CS['cant-typecheck'].app(
                      `The types ${String(supertype)} and ${String(subtype)} are mutually recursive and their constraints cannot be solved.`, supertype.dict.l));
                  }
                }
                else {
                  system.levels[system.levels.length - 2].constraints.push({ subtype, supertype });
                  continue;
                }
              }
            }
          }
          case "t-data-refinement": {
            constraints.push({ subtype, supertype: supertype.dict['data-type']});
            continue;
          }
          case "t-forall": {
            const newOnto = instantiateForallWithFreshVars(supertype, system);
            constraints.push({ subtype, supertype: newOnto });
            continue;
          }
          default: {
            switch(subtype.$name) {
              case "t-name": {
                if(supertype.$name !== "t-name") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const sameModule = sameOrigin(subtype.dict['module-name'], supertype.dict['module-name']);
                const sameId = sameName(subtype.dict.id, supertype.dict.id);
                if (sameModule && sameId) { continue; }
                else { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
              }
              case "t-arrow": {
                if(supertype.$name !== "t-arrow") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const subtypeArgs = listToArray(subtype.dict.args);
                const supertypeArgs = listToArray(supertype.dict.args);
                if (subtypeArgs.length !== supertypeArgs.length) { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                for (let i = 0; i < supertypeArgs.length; i++) {
                  // contravariant argument types
                  constraints.push({ subtype: supertypeArgs[i], supertype: subtypeArgs[i] });
                }
                // covariant return type
                constraints.push({ subtype: subtype.dict.ret, supertype: supertype.dict.ret });
                continue;
              }
              case "t-app": {
                if(supertype.$name !== "t-app") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const subtypeArgs = listToArray(subtype.dict.args);
                const supertypeArgs = listToArray(supertype.dict.args);
                if (subtypeArgs.length !== supertypeArgs.length) { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                for (let i = 0; i < supertypeArgs.length; i++) {
                  // covariant argument types
                  constraints.push({ subtype: subtypeArgs[i], supertype: supertypeArgs[i] });
                }
                // covariant return type
                constraints.push({ subtype: subtype.dict.onto, supertype: supertype.dict.onto });
                continue;
              }
              case "t-top": {
                // We've already checked that supertype is not t-top, above
                throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype));
              }
              /*
              This case can't happen, because of the short-circuiting on line 578
              case "t-bot": {
                if(supertype.$name !== "t-top") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                continue;
              }
              */
              case "t-record": {
                if(supertype.$name !== "t-record") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const subtypeFields = mapFromStringDict(subtype.dict.fields);
                const supertypeFields = mapFromStringDict(supertype.dict.fields);
                for (let [superKey, superFieldType] of supertypeFields) {
                  if (!subtypeFields.has(superKey)) { 
                    // TODO(Matt): field-missing error
                    throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); 
                  }
                  constraints.push({ subtype: subtypeFields.get(superKey), supertype: superFieldType });
                }
                continue;
              }
              case "t-tuple": {
                if(supertype.$name !== "t-tuple") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const subtypeElts = listToArray(subtype.dict.elts);
                const supertypeElts = listToArray(supertype.dict.elts);
                if (subtypeElts.length !== supertypeElts.length) { 
                  // TODO(Matt): more specific error
                  throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); 
                }
                for (let i = 0; i < supertypeElts.length; i++) {
                  // covariant argument types
                  constraints.push({ subtype: subtypeElts[i], supertype: supertypeElts[i] });
                }
                continue;                
              }
              case "t-forall": {
                const newOnto = instantiateForallWithFreshVars(subtype, system);
                constraints.push({ subtype: newOnto, supertype });
                continue;
              }
              case "t-ref": {
                if(supertype.$name !== "t-ref") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                // NOTE(Ben): should this be *invariant* (and add two constraints instead of just one)?
                constraints.push({ subtype: subtype.dict.typ, supertype: supertype.dict.typ });
                continue;
              }
              case "t-data-refinement": {
                constraints.push({ subtype: subtype.dict['data-type'], supertype });
                continue;
              }
              case "t-var": {
                if(supertype.$name === "t-var" && sameName(subtype.dict.id, supertype.dict.id)) { continue; }
                throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype));
              }
              case "t-existential": {
                constraints.push({ subtype: supertype, supertype: subtype });
                continue;
              }
              default: throw new ExhaustiveSwitchError(subtype);
            }
          }

        }
      }

      return solution;
    }
    function solveHelperRefinements(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      return solution;
    }
    function solveHelperExamples(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      return solution;
    }

    class ConstraintSolution {
      variables : Map<string, TS.Type>
      substitutions: Map<string, [TS.Type, TS.Type]>

      constructor(variables? : Map<string, TS.Type>, substitutions? : Map<string, [TS.Type, TS.Type]>) {
        this.variables = variables ?? new Map();
        this.substitutions = substitutions ?? new Map();
      }

      toString() {
        let result = { variables: {}, substitutions: {} };
        for(let [key, variable] of this.variables) {
          result.variables[key] = typeKey(variable);
        }
        for(let [key, [t1, t2]] of this.substitutions) {
          result.substitutions[key] = [typeKey(t1), typeKey(t2)];
        }
        return prettyIsh(result);
      }

      apply(typ : TS.Type) : TS.Type {
        const thisCS = this;
        return map<TS.Type>({
          "t-record": (self, t) => {
            const fields = mapFromStringDict(t.dict.fields);
            for(let [name, val] of fields.entries()) {
              fields.set(name, thisCS.apply(val));
            }
            return TS['t-record'].app(stringDictFromMap(fields), t.dict.l, t.dict.inferred);
          },
          "t-existential": (self, t) => {
            const key = typeKey(t);
            if(thisCS.substitutions.has(key)) {
              const [assignedType] = thisCS.substitutions.get(key);
              const inferred = t.dict.inferred || assignedType.dict.inferred;
              return thisCS.apply(setLocAndInferred(assignedType, t.dict.l, inferred));
            }
            else {
              return t;
            }
          }
        }, typ);
      }
      generalize(typ : TS.Type) : TS.Type {
        const thisCS = this;
        function collectVars(typ : TS.Type, varMapping : Map<string, TS.Type>) : TS.Type {
          return map<TS.Type>({
            "t-record": (self, t) => {
              const fields = mapFromStringDict(t.dict.fields);
              for(let [name, val] of fields.entries()) {
                fields.set(name, collectVars(val, varMapping));
              }
              return TS['t-record'].app(stringDictFromMap(fields), t.dict.l, t.dict.inferred);
            },
            "t-existential": (self, t) => {
              const key = typeKey(t);
              if(thisCS.variables.has(key)) {
                if(varMapping.has(key)) {
                  return varMapping.get(key);
                }
                else {
                  const newVar = newTypeVar(t.dict.l);
                  varMapping.set(key, newVar);
                  return newVar;
                }
              }
              else {
                return t;
              }

            }
          }, typ);
        }
        const varMapping = new Map<string, TS.Type>();
        const newTyp = collectVars(typ, varMapping);
        const vars = [...varMapping.values()];
        if(vars.length === 0) { return typ; }
        else { return TS['t-forall'].app(runtime.ffi.makeList(vars), newTyp, typ.dict.l, false); }
      }
    }

    class TCInfo {
      types: Map<string, TS.Type>;
      aliases: Map<string, TS.Type>;
      dataTypes: Map<string, TS.DataType>;

      constructor(
        types?: Map<string, TS.Type>,
        aliases?: Map<string, TS.Type>,
        dataTypes?: Map<string, TS.DataType>,
      ) {
        this.types = types ?? new Map();
        this.aliases = aliases ?? new Map();
        this.dataTypes = dataTypes ?? new Map();
      }

      static fromPyretTCInfo(info: TCS.TCInfo): TCInfo {
        return new TCInfo(
          mapFromStringDict(info.dict.types),
          mapFromStringDict(info.dict.aliases),
          mapFromStringDict(info.dict['data-types'])
        );
      }

      toPyretTCInfo() : TCS.TCInfo {
        return TCS.dict.values.dict['tc-info'].app(
          stringDictFromMap(this.types),
          stringDictFromMap(this.aliases),
          stringDictFromMap(this.dataTypes)
        )
      }

    }
    class Context {
      globalTypes : Map<string, TS.Type>;     // global name -> type
      aliases : Map<string, TS.Type>;         // t-name -> aliased type
      dataTypes : Map<string, TS.DataType>;   // t-name -> data type
      modules : Map<string, TS.ModuleType>;   // module name -> module type
      moduleNames : Map<string, string>;      // imported name -> module name
      binds : Map<string, TS.Type>;           // local name -> type
      constraints : ConstraintSystem;         // constraints should only be added with methods to ensure that they have the proper forms
      info : TCInfo;
      misc : Map<string, [TS.Type[], string]> // miscellaneous info that is used for logging. Keyed by the function name

      constructor(
        globalTypes: Map<string, TS.Type>, 
        aliases: Map<string, TS.Type>,
        dataTypes: Map<string, TS.DataType>,
        modules: Map<string, TS.ModuleType>,
        moduleNames: Map<string, string>,
      ) {
        this.globalTypes = globalTypes;
        this.aliases = aliases;
        this.dataTypes = dataTypes;
        this.modules = modules;
        this.moduleNames = moduleNames;
        this.binds = new Map();
        this.constraints = new ConstraintSystem();
        this.info = new TCInfo();
        this.misc = new Map();
      }

      toPyretContext(): TCS.Context {
        return TCS.dict.values.dict['typing-context'].app(
          stringDictFromMap(this.globalTypes),
          stringDictFromMap(this.aliases),
          stringDictFromMap(this.dataTypes),
          stringDictFromMap(this.modules),
          stringDictFromMap(this.moduleNames),
          stringDictFromMap(this.binds),
          this.constraints.toPyretConstraintSystem(),
          this.info.toPyretTCInfo(),
          stringDictFromMap(mapMapValues(this.misc, ([typs, name]) => runtime.makeTuple([runtime.ffi.makeList(typs), name]))),
        );
      }

      addLevel(name?: string) : void {
        this.constraints.addLevel(name);
      }

      addAlias(aliasKey : string, aliasTyp : TS.Type) {
        this.aliases.set(aliasKey, aliasTyp);
      }

      addBinding(termKey : string, assignedType : TS.Type) {
        this.binds.set(termKey, assignedType);
      }

      addDictToBindings(bindings : Map<string, TS.Type>) {
        for(let [key, typ] of bindings) {
          this.binds.set(key, typ);
        }
      }

      removeBinding(termKey : string) {
        this.binds.delete(termKey);
      }

      addVariable(v : TS.Type) {
        this.constraints.addVariable(v);
      }

      addVariableSet(vars: Map<string, TS.Type> | TS.Type[]): void {
        this.constraints.addVariableSet(vars)
      }

      addConstraint(subtype : TS.Type, supertype : TS.Type) {
        this.constraints.addConstraint(subtype, supertype);
      }

      substituteInBinds(solution : ConstraintSolution) {
        // TODO(joe/ben): two loops, one for apply() and one for generalize(), with
        // apply guarded by substitutions === 0 and generalize guarded by variables === 0
        if(solution.variables.size === 0 && solution.substitutions.size === 0) {
          return;
        }
        for(const [key, boundType] of this.binds) {
          this.binds.set(key, solution.generalize(solution.apply(boundType)));
          //LOG(`Substituting ${key} for ${typeKey(boundType)}`);
          //LOG(`~~~> ${typeKey(this.binds.get(key))}\n\n`);
        }
      }

      substituteInMisc(solution : ConstraintSolution) {
        return; // TODO(joe): fill
      }

      solveLevel() : ConstraintSolution {
        try {
          LOG(`Solving ${String(this.constraints)}\n\n`);
          const result = this.constraints.solveLevel(this);
          LOG(`With solution: ${String(result)}\n\n`);
          return result;
        }
        catch(e) {
          LOG(`Got an exception while solving: ${String(this.constraints)}\n\n`);
          throw e;
        }
      }

      solveAndResolveType(t : TS.Type) : TS.Type {
        const solution = this.solveLevel()
        this.substituteInBinds(solution);
        this.substituteInMisc(solution);
        const result = solution.apply(t);
        //LOG(`Solved and resolved: ${typeKey(t)} ===> ${typeKey(result)}\n\n`);
        return result;
      }
    }   

    function resolveAlias(type : TS.Type, context : Context) : TS.Type {
      switch(type.$name) {
        case 't-name': {
          const { "module-name": aName, id, l, inferred } = type.dict;
          switch(aName.$name) {
            case 'dependency': {
              throw new InternalCompilerError(`Should not get dependency in typechecker: ${aName.dict.dep}`);
            }
            case 'local': {
              const aliased = context.aliases.get(nameToKey(id));
              if (aliased === undefined) { return type; }
              const resolved = resolveAlias(aliased, context)
              return setLocAndInferred(resolved, l, inferred);
            }
            case 'module-uri': {
              const mod = aName.dict.uri;
              if (mod === 'builtin') {
                const aliased = context.aliases.get(nameToKey(id));
                if (aliased === undefined) { return type; }
                return setLocAndInferred(aliased, l, inferred);
              } else {
                const modtyp = context.modules.get(mod);
                if (modtyp === undefined) {
                  throw new InternalCompilerError(`Module not found with name ${mod}`)
                }
                const dataType = callMethod(modtyp.dict.types, 'get', nameToName(id));
                switch(dataType.$name) {
                  case 'some': return type;
                  case 'none': {
                    const aliased = callMethod(modtyp.dict.aliases, 'get', nameToName(id));
                    switch(aliased.$name) {
                      case 'none': return type;
                      case 'some': {
                        const resolved = resolveAlias(aliased.dict.value, context);
                        return setLocAndInferred(resolved, l, inferred);
                      }
                      default: throw new ExhaustiveSwitchError(aliased);
                    }
                  }
                  default: throw new ExhaustiveSwitchError(dataType);
                }
              }
            }
            default: throw new ExhaustiveSwitchError(aName);
          }
        }
        default: return type;
      }
    }

    function setTypeLoc(type: TS.Type, loc: SL.Srcloc): TS.Type {
      const newType = Object.create(Object.getPrototypeOf(type));
      Object.assign(newType, type);
      newType.dict.l = loc;
      return newType;
    }

    function setInferred(type: TS.Type, inferred: boolean): TS.Type {
      const newType = Object.create(Object.getPrototypeOf(type));
      Object.assign(newType, type);
      newType.dict.inferred = inferred;
      return newType;
    }

    function setLocAndInferred(type: TS.Type, loc: SL.Srcloc, inferred: boolean): TS.Type {
      const newType = Object.create(Object.getPrototypeOf(type));
      Object.assign(newType, type);
      newType.dict.l = loc;
      newType.dict.inferred = inferred;
      return newType;
    }
 
    function substitute(type: TS.Type, newType: TS.Type, typeVar: TS.Type): TS.Type {
      switch(type.$name) {
        case 't-name': return type;
        case 't-arrow': {
          const { args, ret, l, inferred } = type.dict;
          const newArgs = listToArray(args).map((t) => substitute(t, newType, typeVar));
          const newRet = substitute(ret, newType, typeVar);
          return TS['t-arrow'].app(runtime.ffi.makeList(newArgs), newRet, l, inferred);
        }
        case 't-app': {
          const { args, onto, l, inferred } = type.dict;
          const newArgs = listToArray(args).map((t) => substitute(t, newType, typeVar));
          const newOnto = substitute(onto, newType, typeVar);
          return TS['t-app'].app(newOnto, runtime.ffi.makeList(newArgs), l, inferred);
        }
        case 't-top': return type;
        case 't-bot': return type;
        case 't-record': {
          const { fields, l, inferred } = type.dict;
          const newFields = mapFromStringDict(fields);
          for (const key of newFields.keys()) {
            newFields.set(key, substitute(newFields.get(key), newType, typeVar));
          }
          return TS['t-record'].app(stringDictFromMap(newFields), l, inferred);
        }
        case 't-tuple': {
          const { elts, l, inferred } = type.dict;
          const newElts = listToArray(elts).map((t) => substitute(t, newType, typeVar));
          return TS['t-tuple'].app(runtime.ffi.makeList(newElts), l, inferred);
        }
        case 't-forall': {
          // Note: doesn't need to be capture-avoiding thanks to resolve-names
          const { introduces, onto, l, inferred } = type.dict;
          const newOnto = substitute(onto, newType, typeVar);
          return TS['t-forall'].app(introduces, newOnto, l, inferred);
        }
        case 't-ref': {
          const { typ, l, inferred } = type.dict;
          const newTyp = substitute(typ, newType, typeVar);
          return TS['t-ref'].app(newTyp, l, inferred);
        }
        case 't-data-refinement': {
          const { "data-type": dataType, "variant-name": variantName, l, inferred } = type.dict;
          const newDataType = substitute(dataType, newType, typeVar);
          return TS['t-data-refinement'].app(newDataType, variantName, l, inferred);
        }
        case 't-var': {
          switch(typeVar.$name) {
            case 't-var': {
              if (sameName(type.dict.id, typeVar.dict.id)) {
                return setTypeLoc(newType, type.dict.l);
              } else {
                return type;
              }
            }
            default: return type;
          }
        }
        case 't-existential': {
          switch(typeVar.$name) {
            case 't-existential': {
              // inferred existentials keep their locations
              // this is along the lines of inferred argument types etc
              // uninferred existentials are used to equate different pieces of code
              // they should not keep their location
              if (sameName(type.dict.id, typeVar.dict.id)) {
                if (type.dict.inferred) {
                  return setTypeLoc(newType, type.dict.l);
                } else {
                  return newType;
                }
              } else {
                return type;
              }
            }
            default: return type;
          }
        }
        default: throw new ExhaustiveSwitchError(type);
      }
    }

    function varNotFoundInType(type: TS.Type, varType: TS.Type): boolean {
      switch(type.$name) {
        case 't-name': return true;
        case 't-arrow': {
          const { args, ret } = type.dict;
          return listToArray(args).every(t => varNotFoundInType(t, varType)) &&
            varNotFoundInType(ret, varType);
        }
        case 't-app': {
          const { args, onto } = type.dict;
          return listToArray(args).every(t => varNotFoundInType(t, varType)) &&
            varNotFoundInType(onto, varType);
        }
        case 't-top': return true;
        case 't-bot': return true;
        case 't-record': {
          const { fields } = type.dict;
          for(let t of mapFromStringDict(fields).values()) {
            if(!varNotFoundInType(t, varType)) { return false; }
          }
          return true;
        }
        case 't-tuple': {
          const { elts } = type.dict;
          return listToArray(elts).every(t => varNotFoundInType(t, varType));
        }
        case 't-forall': {
          // TODO(MATT): can we really ignore the introduces?
          // NOTE(joe): e.g. could it matter that the mutually recursive usage
          // hides behind a forall so it could be resolved. Not sure but this is
          // safe, just might report errors that it doesn't need to
          const { onto } = type.dict;
          return varNotFoundInType(onto, varType);
        }
        case 't-ref': {
          const { typ } = type.dict;
          return varNotFoundInType(typ, varType);
        }
        case 't-data-refinement': {
          const { "data-type": dataType } = type.dict;
          return varNotFoundInType(dataType, varType);
        }
        case 't-var': {
          return varType.$name !== 't-var' || !sameName(type.dict.id, varType.dict.id);
        }
        case 't-existential': {
          return varType.$name !== 't-existential' || !sameName(type.dict.id, varType.dict.id);
        }
        default: throw new ExhaustiveSwitchError(type);
      }
    }

    const TCNames = MakeName(0);

    function newExistential(l : SL.Srcloc, inferred : boolean): TJ.Variant<TS.Type, 't-existential'> {
      return TS['t-existential'].app(TCNames.makeAtom("%exists"), l, inferred);
    }

    function newTypeVar(l : SL.Srcloc): TJ.Variant<TS.Type, 't-var'> {
      return TS['t-var'].app(TCNames.makeAtom("%tyvar"), l, false);
    }

    function lookupId(blameLoc : SL.Srcloc, idKey : string, idExpr : A.Expr, context : Context) : TS.Type {
      if(context.binds.has(idKey)) {
        return setTypeLoc(context.binds.get(idKey), blameLoc);
      }
      else if(context.globalTypes.has(idKey)) {
        return setTypeLoc(context.globalTypes.get(idKey), blameLoc);
      }
      else {
        throw new TypeCheckFailure(CS['unbound-id'].app(idExpr));
      }
    }

    function removeRefinementAndForalls(typ : TS.Type): TS.Type {
      return map<TS.Type>({
        't-forall': (visitor: TJ.Visitor<TS.Type>, forall : TJ.Variant<TS.Type, 't-forall'>) => {
          const { introduces, onto } = forall.dict;
          let ret = onto;
          for (const aVar of listToArray(introduces)) {
            ret = substitute(ret, newExistential(aVar.dict.l, false), aVar);
          }
          return map(visitor, ret);
        },
        't-data-refinement': (visitor: TJ.Visitor<TS.Type>, dataRefinement: TJ.Variant<TS.Type, 't-data-refinement'>) => {
          return map(visitor, dataRefinement.dict['data-type']);
        },
        't-existential': (_, existential) => existential
      }, typ);
    }

    function generalizeType(curType : TS.Type, nextType : TS.Type) : TS.Type {
      const newVar = () => newExistential(curType.dict.l, false);
      switch(curType.$name) {
        case 't-name': {
          if (curType.$name === nextType.$name) {
            const sameModule = sameOrigin(curType.dict['module-name'], nextType.dict['module-name']);
            const sameId = sameName(curType.dict.id, nextType.dict.id);
            if (sameModule && sameId) {
              return curType;
            }
          }
          return newVar();
        }
        case 't-arrow': {
          if (nextType.$name === curType.$name) {
            const curArgs = listToArray(curType.dict.args);
            const nextArgs = listToArray(nextType.dict.args);
            if (curArgs.length === nextArgs.length) { 
              const genArgs: TS.Type[] = [];
              for (let i = 0; i < curArgs.length; i++) {
                genArgs[i] = generalizeType(curArgs[i], nextArgs[i]);
              }
              const genRet = generalizeType(curType.dict.ret, nextType.dict.ret);
              return TS['t-arrow'].app(runtime.ffi.makeList(genArgs), genRet, curType.dict.l, curType.dict.inferred);
            }
          }
          return newVar();
        }
        case 't-app': {
          if (nextType.$name === curType.$name) {
            const curArgs = listToArray(curType.dict.args);
            const nextArgs = listToArray(nextType.dict.args);
            if (curArgs.length === nextArgs.length) { 
              const genArgs: TS.Type[] = [];
              for (let i = 0; i < curArgs.length; i++) {
                genArgs[i] = generalizeType(curArgs[i], nextArgs[i]);
              }
              const genOnto = generalizeType(curType.dict.onto, nextType.dict.onto);
              return TS['t-app'].app(genOnto, runtime.ffi.makeList(genArgs), curType.dict.l, curType.dict.inferred);
            }
          }
          return newVar();
        }
        case 't-top': {
          if (nextType.$name === curType.$name) { return curType; }
          return newVar();
        }
        case 't-bot': {
          if (nextType.$name === curType.$name) { return curType; }
          return newVar();
        }
        case 't-record': {
          if (curType.$name === nextType.$name) {
            const curFields = mapFromStringDict(curType.dict.fields);
            const nextFields = mapFromStringDict(nextType.dict.fields);
            const genFields = new Map<string, TS.Type>();
            for (let [curField, curType] of curFields) {
              if (nextFields.has(curField)) {
                genFields.set(curField, generalizeType(curType, nextFields.get(curField)));
              }
            }
            return TS['t-record'].app(stringDictFromMap(genFields), curType.dict.l, curType.dict.inferred);
          }
          return newVar();
        }
        case 't-tuple': {
          if (nextType.$name === curType.$name) {
            const curElts = listToArray(curType.dict.elts);
            const nextElts = listToArray(nextType.dict.elts);
            if (curElts.length === nextElts.length) { 
              const genElts: TS.Type[] = [];
              for (let i = 0; i < curElts.length; i++) {
                genElts[i] = generalizeType(curElts[i], nextElts[i]);
              }
              return TS['t-tuple'].app(runtime.ffi.makeList(genElts), curType.dict.l, curType.dict.inferred);
            }
          }
          return newVar();
        }
        case 't-forall': {
          throw new InternalCompilerError("foralls should have been removed already");
        }
        case 't-ref': {
          if (nextType.$name === curType.$name) {
            return TS['t-ref'].app(
              generalizeType(curType.dict.typ, nextType.dict.typ), 
              curType.dict.l, 
              curType.dict.inferred);
          }
          return newVar();
        }
        case 't-data-refinement': {
          throw new InternalCompilerError("refinements should have been removed already");
        }
        case 't-var': {
          if (nextType.$name === curType.$name && sameName(curType.dict.id, nextType.dict.id)) {
            return curType;
          }
          return newVar();
        }
        case 't-existential': return curType;
        default: throw new ExhaustiveSwitchError(curType);
      }
    }

    // Examines a type and, if it is a t-forall, instantiates it with fresh variables
    // This process modifies system to record the newly generated variables.
    // All other types are unmodified.
    function instantiateForallWithFreshVars(type : TS.Type, system : ConstraintSystem): TS.Type {
      switch (type.$name) {
        case 't-forall': {
          const { introduces, onto } = type.dict;
          const introducesArr = listToArray(introduces);
          const newExistentials = introducesArr.map((i) => newExistential(i.dict.l, false));
          let newOnto = onto;
          for (let i = 0; i < newExistentials.length; i++) {
            newOnto = substitute(newOnto, introducesArr[i], newExistentials[i]);
          }
          system.addVariableSet(newExistentials);
          return newOnto;
        }
        default: return type;
      }
    }

    // FOR DEBUGGNG AID; this will be set within typeCheck using its `options` parameter
    let logger: PFunction<(val: any, _ignored: Option<any>) => void>;
    function LOG(val: any): void {
      logger.app(val, runtime.ffi.makeNone());
    }

    // NOTE(joe/ben): This was called introduce-onto in the original Pyret implementation
    function simplifyTApp(appType : TJ.Variant<TS.Type, "t-app">, context : Context) : TS.Type {
      const args = listToArray(appType.dict.args);
      const onto = resolveAlias(appType.dict.onto, context);
      switch(onto.$name) {
        case 't-forall': {
          const introduces = listToArray(onto.dict.introduces);
          if (args.length !== introduces.length) {
            throw new TypeCheckFailure(CS['bad-type-instantiation'].app(appType, introduces.length));
          }
          let newOnto: TS.Type = onto;
          for (let i = 0; i < args.length; i++) {
            newOnto = substitute(newOnto, args[i], introduces[i]);
          }
          return newOnto;
        }
        case 't-app': {
          const newOnto = simplifyTApp(onto, context);
          return simplifyTApp(
            TS['t-app'].app(newOnto, appType.dict.args, appType.dict.l, appType.dict.inferred), 
            context);
        }
        default: throw new TypeCheckFailure(CS['bad-type-instantiation'].app(appType, 0));
      }
    }

    function substituteVariant(variant: TS.TypeVariant, newType: TS.Type, typeVar: TS.Type): TS.TypeVariant {
      switch(variant.$name) {
        case 't-variant': {
          const fields = listToArray(variant.dict.fields);
          const withFields = mapFromStringDict(variant.dict['with-fields']);
          for (let i = 0; i < fields.length; i++) {
            fields[i].vals[1] = substitute(fields[i].vals[1], newType, typeVar);
          }
          substituteFields(withFields, newType, typeVar);
          return TS['t-variant'].app(
            variant.dict.name,
            runtime.ffi.makeList(fields),
            stringDictFromMap(withFields),
            variant.dict.l);
        }
        case 't-singleton-variant': {
          const withFields = mapFromStringDict(variant.dict['with-fields']);
          substituteFields(withFields, newType, typeVar);
          return TS['t-singleton-variant'].app(variant.dict.name, stringDictFromMap(withFields), variant.dict.l);
        }
        default: throw new ExhaustiveSwitchError(variant);
      }
    }
    function substituteFields(fields: Map<string, TS.Type>, newType: TS.Type, typeVar: TS.Type): void {
      for (let [f, fType] of fields) {
        fields[f] = substitute(fType, newType, typeVar);
      }
    }

    function instantiateObjectType(typ : TS.Type, context : Context) : TS.Type {
      typ = resolveAlias(typ, context);
      switch(typ.$name) {
        case "t-name": { return typ; }
        case "t-app": {
          const { onto, args, l, inferred } = typ.dict;
          const aOnto = resolveAlias(onto, context);
          const argsArray = listToArray(args);
          switch(aOnto.$name) {
            case "t-name": return TS['t-app'].app(aOnto, args, l, inferred);
            case "t-forall": {
              const introduces = listToArray(aOnto.dict.introduces);
              const bOnto = aOnto.dict.onto;
              if (argsArray.length !== introduces.length) {
                throw new TypeCheckFailure(CS['bad-type-instantiation'].app(typ, introduces.length));
              }
              else {
                let newOnto = bOnto;
                for(let i = 0; i < argsArray.length; i += 1) {
                  newOnto = substitute(newOnto, argsArray[i], introduces[i]);
                }
                return newOnto;
              }
            }
            case "t-app": {
              const newOnto = instantiateObjectType(aOnto.dict.onto, context);
              return instantiateObjectType(TS['t-app'].app(newOnto, args, l, inferred), context);
            }
            case "t-existential": {
              throw new TypeCheckFailure(CS['unable-to-infer'].app(aOnto.dict.l));
            }
            default: {
              throw new TypeCheckFailure(CS['incorrect-type'].app(String(aOnto), aOnto.dict.l, "a polymorphic type", l));
            }
          }
        }
        case "t-record": return typ;
        case "t-data-refinement": {
          const newDataType = instantiateObjectType(typ.dict['data-type'], context);
          return TS['t-data-refinement'].app(newDataType, typ.dict['variant-name'], typ.dict.l, typ.dict.inferred);
        }
        case "t-existential": return typ;
        case "t-forall": {
          const instantiated = instantiateForallWithFreshVars(typ, context.constraints);
          return instantiateObjectType(instantiated, context);
        }
        default: {
          throw new TypeCheckFailure(CS['incorrect-type'].app(String(typ), typ.dict.l, "an object type", typ.dict.l));
        }
      }
    }

    function instantiateDataType(typ: TS.Type, context: Context): TS.DataType {
      function helper(typ : TS.Type, context: Context): TS.DataType {
        switch (typ.$name) {
          case 't-name': {
            const nameKey = nameToKey(typ.dict.id);
            if (context.dataTypes.has(nameKey)) { return context.dataTypes.get(nameKey); }
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`Expected a data type but got ${String(typ)}`, typ.dict.l));
          }
          case 't-app': {
            const args = listToArray(typ.dict.args);
            const onto = resolveAlias(typ.dict.onto, context);
            switch (onto.$name) {
              case 't-name': {
                const dataType = helper(onto, context);
                const params = listToArray(dataType.dict.params);
                const variants = listToArray(dataType.dict.variants);
                const fields = mapFromStringDict(dataType.dict.fields);
                if (args.length === params.length) {
                  for (let i = 0; i < args.length; i++) {
                    for (let v = 0; v < variants.length; v++) {
                      variants[v] = substituteVariant(variants[v], args[i], params[i]);
                    }
                    substituteFields(fields, args[i], params[i]);
                  }
                  return TS['t-data'].app(
                    dataType.dict.name, 
                    runtime.ffi.makeList([]), 
                    runtime.ffi.makeList(variants), 
                    stringDictFromMap(fields), 
                    dataType.dict.l);
                } else {
                  throw new TypeCheckFailure(CS['bad-type-instantiation'].app(typ, params.length));
                }
              }
              default: {
                const newOnto = simplifyTApp(typ, context);
                return instantiateDataType(newOnto, context);
              }
            }
          }
          case 't-data-refinement': {
            const dataType = instantiateDataType(typ.dict['data-type'], context);
            const variant = listToArray(dataType.dict.variants).find((v) => v.dict.name === typ.dict['variant-name']);
            if (variant === undefined) {
              throw new InternalCompilerError(`data type ${dataType.dict.name} did not have a variant named ${typ.dict['variant-name']}`);
            }
            const newFields = mapFromStringDict(dataType.dict.fields);
            for (const [f, fType] of mapFromStringDict(variant.dict['with-fields'])) {
              newFields.set(f, fType);
            }
            if (variant.$name === 't-variant') {
              for (const f of listToArray(variant.dict.fields)) {
                const [name, type] = f.vals;
                newFields.set(name, type);
              }
            }
            const { name, params, variants, l } = dataType.dict;
            return TS['t-data'].app(name, params, variants, stringDictFromMap(newFields), l);
          }
          case 't-forall': {
            const newTyp = instantiateForallWithFreshVars(typ, context.constraints);
            return instantiateDataType(newTyp, context);
          }
          case 't-existential': {
            throw new TypeCheckFailure(CS['unable-to-infer'].app(typ.dict.l))
          }
          default: throw new TypeCheckFailure(CS['cant-typecheck'].app(`Expected a data type but got ${String(typ)}`, typ.dict.l));
        }
      }
      const ret = helper(typ, context);
      const params = listToArray(ret.dict.params);
      if (params.length > 0) {
        throw new TypeCheckFailure(CS['cant-typecheck'].app(`${String(typ)} expected ${params.length} type arguments, but received none`, ret.dict.l));
      }
      return ret;
    }

    function checking(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : void {
      return _checking(e, expectTyp, topLevel, context);
    }

    function _checking(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : void {
      context.addLevel(`_checking(${e.$name})`);
      function solveAndReturn() {
        context.solveLevel();
        return;
      }
      expectTyp = resolveAlias(expectTyp, context);
      if(expectTyp.$name === 't-app') {
        expectTyp = simplifyTApp(expectTyp, context);
      }
      if(expectTyp.$name === 't-existential' || expectTyp.$name === 't-top') {
        checkSynthesis(e, expectTyp, topLevel, context);
        return solveAndReturn();
      }
      switch(e.$name) {
        case 's-template': return solveAndReturn();
        case 's-type-let-expr': {
          handleTypeLetBinds(listToArray(e.dict.binds), context);
          // Note(Ben): why is this always toplevel=true?
          checking(e.dict.body, expectTyp, true, context);
          return solveAndReturn();
        }
        case 's-lam': {
          checkFun(e.dict.l, e.dict.body, e.dict.params, e.dict.args, e.dict.ann, expectTyp, e, context);
          return solveAndReturn();
        }
        case 's-tuple': {
          if(expectTyp.$name !== 't-tuple') {
            throw new TypeCheckFailure(CS['incorrect-type'].app(`${typeKey(expectTyp)}`, expectTyp.dict.l, "a tuple type", e.dict.l));
          }
          const elts = listToArray(e.dict.fields);
          const expectElts = listToArray(expectTyp.dict.elts);
          if(elts.length !== expectElts.length) {
            throw new TypeCheckFailure(CS['incorrect-type'].app(`a tuple type with length ${elts.length}`, e.dict.l, typeKey(expectTyp), expectTyp.dict.l));
          }
          for(let i = 0; i < elts.length; i += 1) {
            checking(elts[i], expectElts[i], false, context);
          }
          return solveAndReturn();
        }
        case 's-tuple-get':
        case 's-id': {
          checkSynthesis(e, expectTyp, topLevel, context);
          return solveAndReturn();
        }
        default:
          throw new InternalCompilerError("_checking switch " + e.$name);
      }
    }

    // TODO(MATT): this should not generalize the arguments
    function checkFun(funLoc : SL.Srcloc, body : A.Expr, params : List<A.Name>, args : List<A.Bind>, retAnn : A.Ann, expectTyp : TS.Type, original : A.Expr, context : Context) {
      context.addLevel();
      // NOTE(joe/ben): the original implementation called collectBindings here.
      // However, it's only actually used in the cases that traverse into `body`, so
      // save calling it for those cases.
      const paramsArray = listToArray(params);
      const argsArray = listToArray(args)  as TJ.Variant<A.Bind, "s-bind">[];

      // TODO(MATT): checking when polymorphic lambda but non-polymorphic type
      switch(expectTyp.$name) {
        case 't-arrow': {
          const lamBindings = collectBindings(argsArray, context);
          const expectArgs = listToArray(expectTyp.dict.args);
          if(lamBindings.size !== expectArgs.length) {
            const expected = `a function with ${expectArgs.length} arguments`;
            const found = `a function with ${lamBindings.size} arguments`;
            throw new TypeCheckFailure(CS['incorrect-type'].app(expected, funLoc, found, expectTyp.dict.l));
          }
          for(let i = 0; i < expectArgs.length; i += 1) {
            const key = nameToKey(argsArray[i].dict.id);
            const typ = lamBindings.get(key);
            if(typ.$name === 't-existential') {
              lamBindings.set(key, expectArgs[i]);
            }
          }
          for(let param of paramsArray) {
            const newExists = newExistential(funLoc, false);
            const paramType = TS['t-var'].app(param, funLoc, false);
            for(let [key, typ] of lamBindings) {
              lamBindings.set(key, substitute(typ, newExists, paramType));
            }
            context.addVariable(newExists);
          }

          context.addDictToBindings(lamBindings);

          for(let i = 0; i < argsArray.length; i += 1) {
            const typ = lamBindings.get(nameToKey(argsArray[i].dict.id));
            context.addConstraint(typ, expectArgs[i]);
          }

          checking(body, expectTyp.dict.ret, false, context);

          return context.solveAndResolveType(expectTyp);
        }
        case 't-forall': {
          checkFun(funLoc, body, params, args, retAnn, expectTyp.dict.onto, original, context);
          return context.solveAndResolveType(expectTyp);
        }
        case 't-existential': {
          return context.solveAndResolveType(checkSynthesis(original, expectTyp, false, context));
        }
        case 't-app': {
          const foldOnto = simplifyTApp(expectTyp, context);
          checkFun(funLoc, body, params, args, retAnn, foldOnto, original, context);
          return context.solveAndResolveType(foldOnto);
        }
        case 't-top': {
          const lamBindings = collectBindings(argsArray, context);
          context.addDictToBindings(lamBindings);
          checking(body, expectTyp, false, context);
          return context.solveAndResolveType(expectTyp);
        }
      }
    }

    function collectBindings(binds : TJ.Variant<A.Bind, "s-bind">[], context : Context) : Map<string, TS.Type> {
      const bindings = new Map<string, TS.Type>();
      for(let b of binds) {
        const typ = toType(b.dict.ann, context);
        let newTyp : TS.Type;
        if(typ) {
          newTyp = setTypeLoc(typ, b.dict.l);
        }
        else {
          newTyp = newExistential(b.dict.l, true);
          context.addVariable(newTyp);
        }
        bindings.set(nameToKey(b.dict.id), newTyp);
        return bindings;
      }
    }

    function synthesisAppFun(appLoc : SL.Srcloc, fun : A.Expr, args : A.Expr[], context: Context) : TS.Type{
      const newType = synthesis(fun, false, context);
      return newType;
    }

    function synthesisSpine(funType : TS.Type, original: A.Expr, args : A.Expr[], appLoc : SL.Srcloc, context : Context) : TS.Type {
      context.addLevel(`synthesisSpine(${original.$name})`);
      function wrapReturn(t : TS.Type) {
        context.solveLevel();
        return setTypeLoc(t, appLoc);
      }
      funType = instantiateForallWithFreshVars(funType, context.constraints);
      switch(funType.$name) {
        case "t-arrow": {
          const argTypes = listToArray(funType.dict.args);
          if(args.length !== argTypes.length) {
            throw new TypeCheckFailure(CS['incorrect-number-of-args'].app(runtime.ffi.makeList(args), funType));
          }
          for(let i = 0; i < args.length; i += 1) {
            checking(args[i], argTypes[i], false, context);
          }
          return wrapReturn(funType.dict.ret);
        }
        case "t-existential": {
          const existentialArgs = args.map(a => newExistential(funType.dict.l, false));
          const existentialRet = newExistential(funType.dict.l, false);
          context.addVariableSet([existentialRet, ...existentialArgs]);
          const newArrow = TS['t-arrow'].app(runtime.ffi.makeList(existentialArgs), existentialRet, funType.dict.l, false);
          context.addConstraint(funType, newArrow);
          for(let i = 0; i < args.length; i += 1) {
            checking(args[i], existentialArgs[i], false, context);
          }
          return wrapReturn(existentialRet);
        }
        case "t-app": {
          const onto = simplifyTApp(funType, context);
          return wrapReturn(synthesisSpine(onto, original, args, appLoc, context));
        }
        case "t-bot": {
          for(let a of args) {
            checking(a, TS['t-top'].app(funType.dict.l, false), false, context);
          }
          return wrapReturn(funType);
        }
        default:
          throw new TypeCheckFailure(CS['apply-non-function'].app(original, funType));
      }
    }

    function checkSynthesis(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : TS.Type {
      const newType = synthesis(e, topLevel, context);
      context.addConstraint(newType, expectTyp);
      // TODO(MATT, 2017): decide whether this should return new-type or expect-type
      return newType;
    }

    function synthesis(e : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      context.addLevel(`synthesis(${e.$name})`);
      return context.solveAndResolveType(_synthesis(e, topLevel, context));
    }

    function _synthesis(e : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      switch(e.$name) {
        case 's-module': {
          const resultTyp = synthesis(e.dict.answer, false, context);
          return setTypeLoc(resultTyp, e.dict.l);
        }
        case 's-template': {
          const newExists = newExistential(e.dict.l, false);
          context.addVariable(newExists);
          return newExists;
        }
        case 's-type-let-expr': {
          handleTypeLetBinds(listToArray(e.dict.binds), context);
          const typ = synthesis(e.dict.body, false, context);
          return setTypeLoc(typ, e.dict.l);
        }
        case 's-block': {
          let typ : TS.Type = TS['t-top'].app(e.dict.l, false);
          for(const stmt of listToArray(e.dict.stmts)) {
            const stmtTyp = synthesis(stmt, topLevel, context);
            typ = stmtTyp;
          }
          return setTypeLoc(typ, e.dict.l);
        }
        case 's-let-expr': {
          const rhsResult : TS.Type[] = [];
          const binds = listToArray(e.dict.binds);
          for(const lb of binds) {
            rhsResult.push(synthesisLetBind(lb, context));
          }
          const newType = synthesis(e.dict.body, false, context);
          for(const b of binds) {
            context.removeBinding(nameToKey((b.dict.b as TJ.Variant<A.Bind, "s-bind">).dict.id));
          }
          return newType;
        }
        case 's-id': {
          const idTyp = lookupId(e.dict.l, nameToKey(e.dict.id), e, context);
          return idTyp;
        }
        case 's-num': return tNumber(e.dict.l);
        case 's-bool': return tBoolean(e.dict.l);
        case 's-app':
          const args = listToArray(e.dict.args);
          const funType = synthesisAppFun(e.dict.l, e.dict._fun, args, context);
          return synthesisSpine(funType, e, args, e.dict.l, context);
        case 's-srcloc':
          return TS['t-top'].app(e.dict.l, false);
        case 's-prim-app': {
          const arrowType = lookupId(e.dict.l, e.dict._fun, e, context);
          const result = synthesisSpine(arrowType, e, listToArray(e.dict.args), e.dict.l, context);
          return setTypeLoc(result, e.dict.l);
        }
        case 's-tuple': {
          const eltTyps = [];
          for(let elt of listToArray(e.dict.fields)) {
            eltTyps.push(synthesis(elt, false, context));
          }
          return TS['t-tuple'].app(runtime.ffi.makeList(eltTyps), e.dict.l, false);
        }
        case 's-tuple-get': {
          const newType = synthesis(e.dict.tup, topLevel, context); // TODO(joe): toplevel should be false?
          return synthesisTupleIndex(e.dict.l, newType.dict.l, newType, e.dict.index, context);
        }
        case 's-lam': {
          return synthesisFun(e.dict.l, e.dict.body, e.dict.params, e.dict.args, e.dict.ann, e, topLevel, context);
        }
        default:
          throw new InternalCompilerError("_synthesis switch " + e.$name);
      }
    }

    function synthesisFun(l : SL.Srcloc, body : A.Expr, params : List<A.Name>, args : List<A.Bind>, ann : A.Ann, original : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      function setRetType(lamType : TS.Type, retType : TS.Type) {
        switch(lamType.$name) {
          case 't-arrow': {
            return TS['t-arrow'].app(lamType.dict.args, retType, lamType.dict.l, lamType.dict.inferred);
          }
          case 't-forall': {
            const { introduces, onto, l, inferred } = lamType.dict;
            if(onto.$name !== 't-arrow') {
              throw new InternalCompilerError("This shouldn't happen (non-function type lambda)");
            }
            return TS['t-forall'].app(introduces, setRetType(onto, retType), l, inferred);
          }
          default:
            throw new InternalCompilerError("This shouldn't happen (non-function type lambda)");
        }
      }

      context.addLevel();
      const argsArray = listToArray(args) as TJ.Variant<A.Bind, "s-bind">[];
      const collected = collectBindings(argsArray, context);
      const { arrow, ret } = lamToType(collected, l, params, argsArray, ann, topLevel, context);
      context.addDictToBindings(collected);
      checking(body, ret, false, context);
      return context.solveAndResolveType(setRetType(arrow, ret));
    }

    function handleTypeLetBinds(bindings : A.TypeLetBind[], context : Context) : void {
      for (let bind of bindings) {
        const l = bind.dict.l;
        switch(bind.$name) {
          case 's-type-bind': {
            const typ = toType(bind.dict.ann, context);
            if (!typ) {
              // TODO(Matt): is this correct?
              throw new TypeCheckFailure(CS['unbound-type-id'].app(bind.dict.ann));
            }
            let aliasTyp : TS.Type;
            const params = listToArray(bind.dict.params);
            if (params.length === 0) {
              aliasTyp = typ;
            } else {
              const forall = params.map(p => TS['t-var'].app(p, l, false));
              aliasTyp = TS['t-forall'].app(runtime.ffi.makeList(forall), typ, l, false);
            }
            context.addAlias(nameToKey(bind.dict.name), aliasTyp);
            return;
          }
          case 's-newtype-bind': {
            const typ = TS['t-name'].app(TS.local, bind.dict.namet, l, false);
            const nametKey = nameToKey(bind.dict.namet);
            context.addAlias(nameToKey(bind.dict.name), typ);
            const brander = new Map<string, TS.Type>();
            brander.set("test", TS['t-arrow'].app(runtime.ffi.makeList([typ]), tBoolean(l), l, false));
            brander.set("brand", TS['t-arrow'].app(runtime.ffi.makeList([TS['t-top'].app(l, false)]), typ, l, false));
            context.addBinding(nametKey, TS['t-record'].app(stringDictFromMap(brander), l, false));
            return;
          }
          default:
            throw new ExhaustiveSwitchError(bind);
        }
      }
    }

    function lamToType(collected : Map<string, TS.Type>, l : SL.Srcloc, params : List<A.Name>, args : TJ.Variant<A.Bind, "s-bind">[], retAnn : A.Ann, topLevel : boolean, context : Context) : { arrow: TS.Type, ret: TS.Type } {
      const maybeType = toType(retAnn, context);
      const retTyp = maybeType || newExistential(l, true);
      if(!maybeType) { context.addVariable(retTyp) };
      const argTypes = [];
      for(let arg of args) {
        const argType = collected.get(nameToKey(arg.dict.id));
        const argIsUnderscore = arg.dict.id.$name === "s-atom" && arg.dict.id.dict.base === "$underscore";
        if(topLevel && argType.$name === 't-existential' && !(argIsUnderscore)) {
          throw new TypeCheckFailure(CS['toplevel-unann'].app(arg));
        }
        // NOTE(joe/ben): Skipping adding the variable for argType because
        // collectBindings should have done it already
        argTypes.push(argType);
      }
      const arrowType = TS['t-arrow'].app(runtime.ffi.makeList(argTypes), retTyp, l, false);
      if(params.$name === 'empty') {
        return { arrow: arrowType, ret: retTyp };
      }
      else {
        const forall = listToArray(params).map(p => TS['t-var'].app(p, l, false));
        return { arrow: TS['t-forall'].app(runtime.ffi.makeList(forall), arrowType, l, false), ret: retTyp }
      }
    }

    function synthesisTupleIndex(accessLoc : SL.Srcloc, tupTypeLoc : SL.Srcloc, tupType : TS.Type, index : number, context : Context) {
      const tupMembers = tupleView(accessLoc, tupTypeLoc, tupType, context);
      if(index >= tupMembers.length) {
        throw new TypeCheckFailure(CS['tuple-too-small'].app(index, tupMembers.length, "{" + tupMembers.map(typeKey).join("; ") + " }", tupTypeLoc, accessLoc));
      }
      else {
        return tupMembers[index];
      }
    }

    function tupleView(accessLoc : SL.Srcloc, tupTypeLoc : SL.Srcloc, tupType : TS.Type, context : Context) : TS.Type[] {
      switch(tupType.$name) {
        case 't-tuple': return listToArray(tupType.dict.elts);
        case 't-forall':
          const newType = instantiateForallWithFreshVars(tupType, context.constraints);
          return tupleView(accessLoc, tupTypeLoc, newType, context);
        case 't-existential':
          throw new TypeCheckFailure(CS['unable-to-infer'].app(tupType.dict.l));
        default:
          throw new TypeCheckFailure(CS['incorrect-type'].app(typeKey(tupType), tupTypeLoc, "a tuple type", accessLoc));
      }

    }

    function synthesisLetBind(binding : A.LetBind, context : Context) : TS.Type {
      context.addLevel(`synthesisLetBind(${binding.$name}))`);
      switch(binding.$name) {
        case 's-let-bind':
          const b = (binding.dict.b as TJ.Variant<A.Bind, "s-bind">);
          const maybeType = toType(b.dict.ann, context);
          let annTyp : TS.Type;
          if(maybeType === false) {
            annTyp = newExistential(binding.dict.l, true);
          }
          else {
            annTyp = maybeType;
          }
          context.addVariable(annTyp);
          checking(binding.dict.value, annTyp, false, context);
          context.addBinding(nameToKey(b.dict.id), annTyp);
          return context.solveAndResolveType(annTyp);
        case 's-var-bind':
          throw new InternalCompilerError("Not yet implemented in synthesisLetBind: s-var-bind");
        default:
          throw new ExhaustiveSwitchError(binding);
      }
    }

    const emptyContext = new Context(
      new Map(TD['default-types']),
      new Map(TD['default-aliases']),
      new Map(TD['default-data-exprs']),
      new Map(TD['default-modules']),
      new Map()
    );

    function typeCheck(program: A.Program, compileEnv : CS.CompileEnvironment, postCompileEnv : CS.ComputedEnvironment, modules : MutableStringDict<CS.Loadable>, options) {
      logger = options.dict.log;

      const provides = listToArray(program.dict.provides);

      const globVs = mapFromStringDict(compileEnv.dict.globals.dict.values);
      const globTs = mapFromStringDict(compileEnv.dict.globals.dict.types);

      const contextGlobTs = new Map(TD['default-aliases']);
      const contextGlobVs = new Map(TD['default-types']);
      const contextGlobMods = new Map(TD['default-modules']);
      const contextGlobModnames = new Map<string, string>();
      const contextGlobDTs = new Map<string, TS.DataType>();

      for (const g of globVs.keys()) {
        const key = nameToKey(sGlobal.app(g));
        if (contextGlobVs.has(key)) {
          continue;
        }
        else {
          if(g === "_") {
            continue;
          }
          else {
            const ve =  globalValueValue(compileEnv, g);
            contextGlobVs.set(key, ve.dict.t);
          }
        }
      }

      for (const g of globTs.keys()) {
        const key = nameToKey(sTypeGlobal.app(g));
        if (contextGlobTs.has(key)) {
          continue;
        }
        else {
          const origin = globTs.get(g);
          if (g === "_") { continue; }
          else {
            const provs = unwrap(providesByUri(compileEnv, origin.dict['uri-of-definition']),
                `Could not find module ${origin.dict['uri-of-definition']} in ${listToArray(callMethod(compileEnv.dict['all-modules'], 'keys-list-now'))} at ${formatSrcloc(program.dict.l, true)}}`);
            let t: TS.Type;
            const alias = callMethod(provs.dict.aliases, 'get', g);
            switch(alias.$name) {
              case 'some': { t = alias.dict.value; break; }
              case 'none': {
                const dd = callMethod(provs.dict['data-definitions'], 'get', g);
                switch(dd.$name) {
                  case 'none':
                    // Note(Ben): could use `unwrap(callMethod(...))` above, but since this
                    // error message is expensive to compute, I didn't.
                    const keys = [
                      ...listToArray(callMethod(provs.dict.aliases, 'keys-list')),
                      ...listToArray(callMethod(provs.dict['data-definitions'], 'keys-list'))
                    ];
                    throw new InternalCompilerError(`Key ${g} not found in ${keys}`);
                  case 'some':
                    t = TS['t-name'].app(builtinUri, sTypeGlobal.app(g), builtin.app("global"), false);
                    break;
                  default: throw new ExhaustiveSwitchError(dd, "computing aliases from data defs");
                }
                break;
              }
              default: throw new ExhaustiveSwitchError(alias, "computing aliases");
            }
            contextGlobTs.set(key, t);
          }
        }
      }

      for (let k of listToArray(callMethod(modules, 'keys-list-now'))) {
        if (contextGlobMods.has(k)) {
          continue;
        }
        else {
          // NOTE/TODO/REVISIT(joe/ben/luna): Can we just resolve these with valueByUriValue/resolveDatatypeByUriValue
          const mod = callMethod(modules, 'get-value-now', k).dict.provides;
          const key = mod.dict['from-uri'];
          let valsTypesDict = new Map<string, TS.Type>();
          for (let valKey of listToArray(callMethod(mod.dict.values, 'keys-list'))) {
            let typ : TS.Type;
            const ve = callMethod(mod.dict.values, 'get-value', valKey);
            switch(ve.$name) {
              case 'v-alias':
                const { origin } = ve.dict;
                typ = valueByUriValue(compileEnv, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name'])).dict.t;
                break;
              default:
                typ = ve.dict.t;
            }
            valsTypesDict.set(valKey, typ);
          }
          let dataDict = new Map<string, TS.DataType>();
          for (let dataKey of listToArray(callMethod(mod.dict['data-definitions'], 'keys-list'))) {
            const de = callMethod(mod.dict['data-definitions'], 'get-value', dataKey);
            let typ : TS.DataType;
            switch(de.$name) {
              case 'd-alias':
                const { origin } = de.dict;
                typ = resolveDatatypeByUriValue(compileEnv, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
                break;
              default:
                typ = de.dict.typ;
            }
            dataDict.set(dataKey, typ);
          }
          const valProvides = TS['t-record'].app(stringDictFromMap(valsTypesDict), program.dict.l, false);
          const moduleType = TS['t-module'].app(key, valProvides, stringDictFromMap(dataDict), mod.dict.aliases);
          contextGlobMods.set(key, moduleType);
          for(let dataKey of listToArray(callMethod(mod.dict['data-definitions'], 'keys-list'))) {
            // NOTE(joe): changed this to byUri***Value*** to not return an
            // Option, which conflicted with the type of the data-types field of
            // context (but evidently never triggered a dynamic error in our tests)
            const resolved = resolveDatatypeByUriValue(compileEnv, key, dataKey);
            contextGlobDTs.set(dataKey, resolved);
          }
        }
      }

      if(postCompileEnv.$name === "computed-none") {
        throw new InternalCompilerError(`type-check got computed-none postCompileEnv in ${formatSrcloc(program.dict.l, true)}`);
      }

      // NOTE(joe) – we cannot use module-env/type-env/env here because they
      // represent the environment at the *end* of the module. So if the user
      // shadows an imported ID, we would pick up that name as the type of the
      // import. Instead, we filter through all the bindings looking for ones
      // that came from a module. This is slower, and having Yet Another
      // Datatype for "bindings after imports" would help here.
    
      const mbinds = mapFromMutableStringDict(postCompileEnv.dict['module-bindings']);
      const vbinds = mapFromMutableStringDict(postCompileEnv.dict.bindings);
      const tbinds = mapFromMutableStringDict(postCompileEnv.dict['type-bindings']);

      for (const [key, mbind] of mbinds.entries()) {
        contextGlobModnames.set(key, mbind.dict.uri);
      }

      for (const [key, vbind] of vbinds.entries()) {
        if (vbind.dict.origin.dict['new-definition']) { continue; }
        else {
          const thismod = contextGlobMods.get(vbind.dict.origin.dict['uri-of-definition']);
          const originalName = nameToName(vbind.dict.origin.dict['original-name']);
          const field = unwrap(callMethod(thismod.dict.provides.dict.fields, 'get', originalName), `Cannot find value bind for ${originalName} in ${formatSrcloc(program.dict.l, true)}`);
          contextGlobVs.set(key, field);
        }
      }

      for (const [key, tbind] of tbinds.entries()) {
        const origin = tbind.dict.origin;
        if (origin.dict['new-definition']) { continue; }
        else {
          const originalName = nameToName(origin.dict['original-name']);
          const originalType = unwrap(
            typeByUri(compileEnv, origin.dict['uri-of-definition'], originalName),
            `Cannot find type bind for ${originalName} in ${formatSrcloc(program.dict.l, true)}`);
          contextGlobTs.set(key, originalType)
        }
      }


      const contextFromModules = new Context(
        contextGlobVs,
        contextGlobTs,
        contextGlobDTs,
        contextGlobMods,
        contextGlobModnames);

      try {
        checking(program.dict.block, TS['t-top'].app(program.dict.l, false), true, contextFromModules);
      }
      catch(e) {
        console.error("Got a type-checking error", e);
        LOG("Got a type-checking error " + require('util').inspect(e, {depth:null}) + "\n");
      }

      const info = gatherProvides(provides[0], contextFromModules);
      return CS.ok.app(typed.app(program, info));
    }
    return runtime.makeModuleReturn({
      'type-check': runtime.makeFunction(typeCheck),
      'empty-context': emptyContext.toPyretContext(),
    }, {});
  }
})