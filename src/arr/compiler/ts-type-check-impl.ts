import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as SL from './ts-srcloc';
import type * as CS from './ts-compile-structs';
import type * as TD from './ts-type-defaults';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as TCSH from './ts-compile-structs-helpers';
import type { List, MutableStringDict, PFunction, StringDict, Option, PTuple } from './ts-impl-types';

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
      constructor(...errs : CS.CompileError[]) {
        super("type error " + require('util').inspect(errs));
      }
    }

    /*
    function substitute(substIn : TS.Type, newType : TS.Type, typeVar : TS.Type) : TS.Type {
      return map({
        "t-var": (self, substFor) => {
          if(typeVar.$name === "t-var" && sameName(substFor.dict.id, typeVar.dict.id)) {
            return setTypeLoc(newType, substFor.dict.l);
          }
          return substFor;
        },
        "t-existential": (self, substFor) => {
          if(typeVar.$name === "t-var" && sameName(substFor.dict.id, typeVar.dict.id)) {
            if(substFor.dict.inferred) {
              return setTypeLoc(newType, substFor.dict.l);
            }
            else {
              return newType;
            }
            
          }
          return substFor;
        }
      }, substIn);
    }
    */

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
      switch(inAnn.$name) {
        case 'a-blank': return false;
        default:
          throw new InternalCompilerError("toType switch " + inAnn.$name);
      }
    }

    const primitiveTypesUri = TS['module-uri'].app("builtin://primitive-types");

    function tNumber(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Number"), l, false);
    }

    function typeKey(type : TS.Type & { $key?: string }) : string {
      if(!("$key" in type)) {
        const key = _typeKey(type);
        type.$key = key;
      }
      return type.$key;
    }
    function _typeKey(type: TS.Type): string {
      // TODO(joe): memoize the result on the type object?
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

      constructor() {
        this.variables = new Map();
        this.constraints = [];
        this.refinementConstraints = [];
        this.fieldConstraints = new Map();
        this.exampleTypes = new Map();
      }
    }
    class ConstraintSystem {
      levels: ConstraintLevel[];
      constructor() {
        this.levels = [];
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
        const typKey = typeKey(type);
        if (curLevel.fieldConstraints.has(typKey)) {
          const [_typ, labelMapping] = curLevel.fieldConstraints.get(typKey);
          if (labelMapping.has(fieldName)) {
            labelMapping.get(fieldName).push(fieldType);
          } else {
            labelMapping.set(fieldName, [fieldType]);
          }
        } else {
          const newMap: FieldConstraint = new Map();
          newMap.set(fieldName, [fieldType]);
          curLevel.fieldConstraints.set(typKey, [type, newMap]);
        }
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
      addLevel() : void{
        const level = new ConstraintLevel();
        this.levels.push(level);
      }
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
        this.addLevel();
        for(let typkey of levelToSplit.variables.keys()) {
          const { existential } = levelToSplit.exampleTypes.get(typkey);
          this.addVariable(existential); // Adds to the newly added empty level from .addLevel()
          levelToSplit.variables.delete(typkey);
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

    function solveHelperFields(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      return solution;
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

    function solveHelperConstraints(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      const { constraints, variables } = system.curLevel();
      if(constraints.length === 0) { return new ConstraintSolution(); }

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
            const { introduces, onto, l } = supertype.dict;
            const newExistentials = listToArray(introduces).map(v => newExistential(v.dict.l, false));
            let newOnto = onto;
            newExistentials.forEach((exists, index) => {
              newOnto = substitute(newOnto, exists, introduces[index]);
            });
            system.addVariableSet(newExistentials);
            constraints.push({ subtype, supertype: newOnto });
            continue;
          }
          default: {
            switch(subtype.$name) {
              case "t-name": {
                if(!(supertype.$name === "t-name")) { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const sameModule = sameOrigin(subtype.dict['module-name'], supertype.dict['module-name']);
                const sameId = sameName(subtype.dict.id, supertype.dict.id);
                if (sameModule && sameId) { continue; }
                else { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
              }
              // TODO(joe): add cases starting at type-check-structs:607
              // These cases appear to "just" destructure matching type shapes
              // to add more constraints and then continue to unify

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

      apply(typ : TS.Type) : TS.Type {
        return typ;
      }
      generalize(typ : TS.Type) : TS.Type {
        return typ;
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

      addLevel() : void {
        this.constraints.addLevel();
      }

      addBinding(termKey : string, assignedType : TS.Type) {
        this.binds.set(termKey, assignedType);
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
        for(const [key, boundType] of this.binds) {
          this.binds.set(key, solution.generalize(solution.apply(boundType)));
        }
      }

      substituteInMisc(solution : ConstraintSolution) {
        return; // TODO(joe): fill
      }

      solveLevel() : ConstraintSolution {
        return this.constraints.solveLevel(this);
      }

      solveAndResolveType(t : TS.Type) : TS.Type {
        const solution = this.solveLevel()
        this.substituteInBinds(solution);
        this.substituteInMisc(solution);
        return solution.apply(t);
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
      const newType = map({}, type);
      newType.dict.l = loc;
      return newType;
    }

    function setInferred(type: TS.Type, inferred: boolean): TS.Type {
      const newType = map({}, type);
      newType.dict.inferred = inferred;
      return newType;
    }

    function setLocAndInferred(type: TS.Type, loc: SL.Srcloc, inferred: boolean): TS.Type {
      const newType = map({}, type);
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

    // Examines a type and, if it is a t-forall, instantiates it with fresh variables
    // This process modifies context to record the newly generated variables.
    // All other types are unmodified.
    function instantiateForallWithFreshVars(type : TS.Type, context: Context): TS.Type {
      switch(type.$name) {
        case 't-forall': {
          const { introduces, onto, l, inferred } = type.dict;
          const introducesArr = listToArray(introduces);
          const newExistentials = introducesArr.map((i) => newExistential(l, false));
          let newOnto = onto;
          for (let i = 0; i < newExistentials.length; i++) {
            newOnto = substitute(newOnto, introducesArr[i], newExistentials[i]);
          }
          context.addVariableSet(newExistentials);
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

    function checking(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : void {
      return _checking(e, expectTyp, topLevel, context);
    }

    function _checking(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : void {
      context.addLevel();
      expectTyp = resolveAlias(expectTyp, context);
      if(expectTyp.$name === 't-app') {
        expectTyp = simplifyTApp(expectTyp, context);
      }
      if(expectTyp.$name === 't-existential' || expectTyp.$name === 't-top') {
        checkSynthesis(e, expectTyp, topLevel, context);
        return;
      }
      else {
        switch(e.$name) {
          default:
            throw new InternalCompilerError("_checking switch " + e.$name);
        }
      }
      return null;
    }

    function checkSynthesis(e : A.Expr, expectTyp : TS.Type, topLevel : boolean, context : Context) : TS.Type {
      const newType = synthesis(e, topLevel, context);
      context.addConstraint(newType, expectTyp);
      // TODO(MATT, 2017): decide whether this should return new-type or expect-type
      return newType;
    }

    function synthesis(e : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      context.addLevel();
      return context.solveAndResolveType(_synthesis(e, topLevel, context));
    }

    function _synthesis(e : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      switch(e.$name) {
        case 's-module':
          break;
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
        case 's-srcloc':
        case 's-app':
        case 's-prim-app':
          return TS['t-top'].app(e.dict.l, false);
        default:
          throw new InternalCompilerError("_synthesis switch " + e.$name);
      }
    }

    function synthesisLetBind(binding : A.LetBind, context : Context) : TS.Type {
      context.addLevel();
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