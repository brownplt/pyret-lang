import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as SL from './ts-srcloc';
import type * as CS from './ts-compile-structs';
import type * as TD from './ts-type-defaults';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as TRED from './ts-render-error-display';
import type { Runtime, List, MutableStringDict, PFunction, StringDict, Option, PTuple } from './ts-impl-types';
import { CompileOptions } from './ts-compiler-lib-impl';

export type Exports = {
  dict: {
    values: {
      dict: {
        'type-check': PFunction<(program: A.Program, compileEnv : CS.CompileEnvironment, postCompileEnv : CS.ComputedEnvironment, modules : MutableStringDict<CS.Loadable>, options: CompileOptions) => CS.CompileResult<TCS.Typed>>,
        'empty-context': TCS.Context,
      }
    }
  }
}

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
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-render-error-display']},
 ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "type-check": "tany",
      "empty-context": "tany",
    }
  },
  theModule: function(runtime: Runtime, _, __, SL : SL.Exports, tj : TJ.Exports, TCSH : (TCSH.Exports), TSin : TS.Exports, Ain : A.Exports, CSin : CS.Exports, TCS : TCS.Exports, TD : TD.Exports, TRED : TRED.Exports) {
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
    const A = Ain.dict.values.dict;
    const { 's-global': sGlobal, 's-type-global': sTypeGlobal } = A;
    const {
      typed,
      'tc-info': tcInfo,
      'empty-info': emptyInfo,
      'fold-result': foldResult,
      'fold-errors': foldErrors,
      "typing-context": typingContext
    } = TCS.dict.values.dict;
    const {
      'display-to-string': displayToString
    } = TRED.dict.values.dict;

    class TypeCheckFailure extends Error {
      errs : CS.CompileError[];
      constructor(...errs : CS.CompileError[]) {
        runtime['GAS'] = 5000000;
        runtime['RUNGAS'] = 5000000;
        var rendered = "";
        errs.forEach(e => {
          const reason = callMethod(e as any, "render-reason");
          const stringReason = displayToString.app(reason, runtime.makeFunction(String), runtime.ffi.makeList([]));
          rendered += stringReason;
          rendered += ` [${e.$name}]`
        })

        super("type error " + rendered);
        this.errs = errs;
      }
    }

    function prettyIsh(v : any) : string {
      return JSON.stringify(v);
      // return require('util').inspect(v, {depth:null});
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
            return context.aliases.get(idKey)!;
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
          const origin = context.moduleNames.get(objKey)!;
          const tMod = context.modules.get(origin)!;
          const aliases = mapFromStringDict(tMod.dict.aliases);
          if (aliases.has(inAnn.dict.field)) {
            return resolveAlias(aliases.get(inAnn.dict.field)!, context);
          } else {
            throw new TypeCheckFailure(CS['unbound-type-id'].app(inAnn));
          }
        }
        case 'a-checked': {
          throw new TypeCheckFailure(CS['cant-typecheck'].app(`a-checked should not be appearing before type-checking: ${prettyIsh(inAnn)}`, A['dummy-loc']));
        }
        default:
          throw new ExhaustiveSwitchError(inAnn);
      }
    }

    function freeVariables(typ : TS.Type): TypeSet {
      switch(typ.$name) {
        case 't-existential': {
          const ret = new TypeSet();
          ret.add(typ);
          return ret;
        }
        case 't-name':
        case 't-var':
        case 't-top':
        case 't-bot': return new TypeSet();
        case 't-arrow': {
          const ret = new TypeSet();
          for (let arg of listToArray(typ.dict.args)) {
            ret.combine(freeVariables(arg));
          }
          ret.combine(freeVariables(typ.dict.ret));
          return ret;
        }
        case 't-app': {
          const ret = new TypeSet();
          for (let arg of listToArray(typ.dict.args)) {
            ret.combine(freeVariables(arg));
          }
          ret.combine(freeVariables(typ.dict.onto));
          return ret;
        }
        case 't-record': {
          const ret = new TypeSet();
          for (let [_key, t] of mapFromStringDict(typ.dict.fields)) {
            ret.combine(freeVariables(t));
          }
          return ret;
        }
        case 't-tuple': {
          const ret = new TypeSet();
          for (let t of listToArray(typ.dict.elts)) {
            ret.combine(freeVariables(t));
          }
          return ret;
        }
        case 't-forall': {
          const ret = freeVariables(typ.dict.onto);
          // NOTE(Ben): this may not be needed, given name resolution
          ret.removeAll(...listToArray(typ.dict.introduces)); 
          return ret;
        }
        case 't-ref': return freeVariables(typ.dict.typ);
        case 't-data-refinement': return freeVariables(typ.dict['data-type']);
        default: throw new ExhaustiveSwitchError(typ);
      }
    }

    function meetBranchTypes(branchTypes : TS.Type[], loc : SL.Srcloc, context: Context): TS.Type {
      const newExists = newExistential(loc, false);
      context.addLevel("meet branch types");
      context.addVariable(newExists);
      for (let branchType of branchTypes) {
        context.addConstraint(branchType, newExists);
      }
      const solution = context.solveLevel();
      return solution.generalize(solution.apply(newExists));
    }

    const primitiveTypesUri = TS['module-uri'].app("builtin://primitive-types");

    const tArrayName = TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("RawArray"), A['dummy-loc'], false);

    function tNumber(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Number"), l, false);
    }
    function tBoolean(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Boolean"), l, false);
    }
    function tNothing(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("Nothing"), l, false);
    }
    function tString(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(primitiveTypesUri, sTypeGlobal.app("String"), l, false);
    }

    function tArray(t : TS.Type, l : SL.Srcloc): TS.Type {
      return TS['t-app'].app(setTypeLoc(tArrayName, l), runtime.ffi.makeList([t]), l, false);
    }

    function tSrcloc(l : SL.Srcloc) : TS.Type {
      return TS['t-name'].app(builtinUri, sTypeGlobal.app("Loc"), l, false);
    }

    // Note: if typeKey(t1) === typeKey(t2), then t1 == t2 (as Pyret values)
    const typeKeys = new WeakMap<TS.Type, string>();
    function typeKey(type : TS.Type & { $key?: string }) : string {
      if(!typeKeys.has(type)) {
        const key = _typeKey(type);
        typeKeys.set(type, key);
      }
      return typeKeys.get(type)!;
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
          const fieldStrs: string[] = [];
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
          const curAliases = context.info.aliases;
          const curData = context.info.dataTypes;
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
                      if (getValueFromContext) {
                        const typ = setInferred(getValueFromContext, false);
                        curTypes.set(valueKey, typ);
                      }
                      else {
                        const globalType = context.globalTypes.get(valueKey);
                        if (!globalType) {
                          const typeKeys = [...context.info.types.keys()].join(',');
                          const globalKeys = [...context.globalTypes.keys()].join(',');
                          throw new InternalCompilerError(`Could not find global type for ${valueKey} in types [${typeKeys}] or globals [${globalKeys}]; got ${String(globalType)}`);
                        }
                        const typ = setInferred(globalType, false);
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
                      const typ = context.aliases.get(aliasKey)!;
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
                      const typ = context.dataTypes.get(dataKey)!;
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
    function objMapValues<K, V1, V2>(m : Map<K, V1>, f : ((val : V1) => V2)) : Record<string, V2> {
      const ret: Record<string, V2> = {};
      for (const [k, v1] of m.entries()) {
        ret[String(k)] = f(v1);
      }
      return ret;
    }

    type Constraint = {subtype: TS.Type, supertype: TS.Type};
    type Refinement = {existential: TJ.Variant<TS.Type, 't-existential'>, dataRefinements: TJ.Variant<TS.Type, 't-data-refinement'>[]};

    class TypeSet<T extends TS.Type = TS.Type> {
      contents: Map<string, T>;
      constructor(...typs: readonly T[]) { 
        this.contents = new Map();
        this.addAll(...typs);
      }
      values(): IterableIterator<T> { return this.contents.values(); }
      size(): number { return this.contents.size }
      add(typ : T): void {
        this.contents.set(typeKey(typ), typ);
      }
      addAll(...typs: readonly T[]): void {
        for (let typ of typs) {
          this.contents.set(typeKey(typ), typ);
        }
      }
      combine(typs: TypeSet<T>): void {
        this.addAll(...typs.values());
      }
      union<U extends TS.Type = T>(typs: TypeSet<U>) : TypeSet<T | U> {
        const ret = new TypeSet<T | U>();
        ret.combine(this);
        ret.combine(typs);
        return ret;
      }
      intersect<U extends TS.Type = T>(typs: TypeSet<U>): void {
        const keys = this.contents.keys();
        for (let k of keys) {
          if (!typs.contents.has(k)) {
            this.contents.delete(k);
          }
        }
      }
      intersection<U extends TS.Type = T>(typs: TypeSet<U>): TypeSet<T & U> {
        const ret = new TypeSet<T & U>();
        for (let [k, t] of this.contents) {
          if (this.contents.has(k) && typs.contents.has(k)) {
            ret.add(t as (T & U));
          }
        }
        return ret;
      }
      remove(typ: TS.Type): void {
        this.contents.delete(typeKey(typ));
      }
      removeAll(...typs: readonly TS.Type[]) {
        for (let typ of typs) {
          this.contents.delete(typeKey(typ));
        }
      }
      subtract<U extends TS.Type>(typs: TypeSet<U>): void {
        this.removeAll(...typs.values());
      }
      difference<U extends TS.Type>(typs: TypeSet<U>): TypeSet<T> {
        const ret = new TypeSet<T>();
        ret.combine(this);
        ret.subtract(typs);
        return ret;
      }

    }
    class ConstraintLevel {
      name?: string;
      // the constrained existentials
      variables : Map<string, TS.Type>;
      // list of {subtype; supertype}
      constraints : Array<Constraint>;
      // list of {existential; t-data-refinement}
      refinementConstraints : Map<string, Refinement>;
      // type -> {type, field labels -> field types (with the location of their use)}
      fieldConstraints : Map<string, [TS.Type, FieldConstraint]>;
      // types for examples?
      exampleTypes : ExampleTypes;

      constructor(name? : string) {
        this.variables = new Map();
        this.constraints = [];
        this.refinementConstraints = new Map();
        this.fieldConstraints = new Map();
        this.exampleTypes = new Map();
        this.name = name;
      }

      toInertData() {
        return {
          name: this.name,
          variables: objMapValues(this.variables, typeKey),
          constraints: this.constraints.map((c) => {
            const {subtype, supertype} = c;
            return `${typeKey(subtype)} < ${typeKey(supertype)}`;
          }),
          fieldConstraints: objMapValues(this.fieldConstraints, ([_typ, constraint]) => {
            return objMapValues(constraint, (typs) => `[ ${typs.map(typeKey).join(", ")} ]`);
          }),
          refinementConstraints: objMapValues(this.refinementConstraints, (r) => {
            const { existential, dataRefinements } = r;
            return `${typeKey(existential)} < (${dataRefinements.map(typeKey).join(', ')})`;
          }),
          exampleTypes: objMapValues(this.exampleTypes, (e) => {
            const { annTypes, exampleTypes, existential, functionName } = e;
            return {
              functionName,
              existential: typeKey(existential),
              annTypes: {
                argTypes: annTypes.argTypes.map(typeKey),
                retType: typeKey(annTypes.retType),
                loc: formatSrcloc(annTypes.loc, true),
              },
              exampleTypes: exampleTypes.map(typeKey),
            }
          })
        }
      }
      toString() {
        return prettyIsh(this.toInertData());
      }

      addConstraint(subtype: TS.Type, supertype: TS.Type) {
        if (subtype.$name === 't-existential' && supertype.$name === 't-data-refinement') {
          this.addRefinementConstraint(subtype, supertype);
        } else if (supertype.$name === 't-existential' && subtype.$name === 't-data-refinement') {
          this.addRefinementConstraint(supertype, subtype);
        } else {
          this.constraints.push({ subtype, supertype });
        }
      }
      addRefinementConstraint(existential: TJ.Variant<TS.Type, 't-existential'>, dataRefinement: TJ.Variant<TS.Type, 't-data-refinement'>) {
        const key = typeKey(existential);
        if (this.refinementConstraints.has(key)) {
          this.refinementConstraints.get(key)!.dataRefinements.push(dataRefinement);
        } else {
          this.refinementConstraints.set(key, { existential, dataRefinements: [dataRefinement] });
        }
      }

      addFieldConstraint(type : TS.Type, fieldName : string, fieldType : TS.Type) : void{
        const typKey = typeKey(type);
        if (this.fieldConstraints.has(typKey)) {
          const [typ, labelMapping] = this.fieldConstraints.get(typKey)!;
          if (labelMapping.has(fieldName)) {
            labelMapping.get(fieldName)!.push(fieldType);
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
      toInertData() {
        return {
          levels: this.levels.map((l) => l.toInertData())
        };
      }
      toString() {
        return prettyIsh(this.toInertData());
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
            runtime.ffi.makeTreeSet([...level.variables.values()]),
            runtime.ffi.makeList(level.constraints.map((c) => runtime.makeTuple([c.subtype, c.supertype]))),
            runtime.ffi.makeList([...mapMapValues(level.refinementConstraints, (r) => {
              return r.dataRefinements.map((dr) => runtime.makeTuple([r.existential, dr]));
            }).values()].flat()),
            stringDictFromMap(fieldConstraints),
            // {Type; {arg-types :: List<Type>, ret-type :: Type, loc :: Loc}; List<Type>; (Type, Context -> TypingResult); String}
            stringDictFromMap(mapMapValues(level.exampleTypes, (exTyInfo: ExampleTypeInfo) => {
              return runtime.makeTuple([
                exTyInfo.existential,
                runtime.makeObject({
                  'arg-types': runtime.ffi.makeList(exTyInfo.annTypes.argTypes),
                  'ret-type': exTyInfo.annTypes.retType,
                  'loc': exTyInfo.annTypes.loc,
                }),
                runtime.ffi.makeList(exTyInfo.exampleTypes),
                runtime.makeFunction((t: TS.Type, c: TCS.Context): TCS.TypingResult => {
                  throw new Error("Don't actually do this!");
                }),
                exTyInfo.functionName
              ]);
            })),
            ret
          )
        }
        return ret;
      }

      ensureLevel(msg : string, offset?: number): void {
        if (this.levels.length < (offset ?? 0)) {
          throw new InternalCompilerError(msg);
        }
      }
      curLevel(): ConstraintLevel { return this.levels[this.levels.length - 1]; }
      nextLevel(): ConstraintLevel { 
        this.ensureLevel("Can't get next level with only one level available", 1);
        return this.levels[this.levels.length - 2]; 
      }
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
        LOG(`Adding constraint (at level ${this.levels.length}) ${typeKey(subtype)}  <:  ${typeKey(supertype)}\n`);
        this.curLevel().addConstraint(subtype, supertype);
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
            const curInfo = curLevel.exampleTypes.get(existentialKey)!;
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
      /**
       * After solve-level, the system will have one less level (unless it's already
       * empty) It breaks the current level in two based on the set of variables
       * that came from examples, then solves them first by solving the part
       * unrelated to examples, then solving the part related to examples.  It then
       * propagates variables to the context at the next level (if there is a next
       * level) and returns the resulting system and solution

       * NOTE: We'd like solveLevel to be "like a transaction", such that either
       * it has no effect on the Context and produces an error, or it produces
       * a ConstraintSolution.  It appears to be the case that solveLevel can affect
       * the top *two* levels, so it's not quite a simple matter of cloning the top level
       * and restoring it upon failure...
       */
      solveLevel(context : Context) : ConstraintSolution {
        if (this.levels.length === 0) { return new ConstraintSolution(); }

        const levelToSplit = this.levels.pop()!;
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

    function substituteInConstraints(newType : TS.Type, typeVar : TJ.Variant<TS.Type, 't-var' | 't-existential'>, constraints : Constraint[]) {
      return constraints.map(({subtype, supertype}) => {
        return {
          subtype: substitute(subtype, newType, typeVar),
          supertype: substitute(supertype, newType, typeVar)
        };
      });
    }

    function substituteInRefinements(newType : TS.Type, typeVar : TJ.Variant<TS.Type, 't-var' | 't-existential'>, refinements : Map<string, Refinement>) : { newRefinements: Map<string, Refinement>, newConstraints: Constraint[] } {
      const newRefinements = new Map<string, Refinement>();
      const newConstraints: Constraint[] = []
      for (let [key, r] of refinements) {
        const { existential, dataRefinements } = r;
        const substExist = substitute(existential, newType, typeVar);
        for (let dataRefinement of dataRefinements) {
          const substData = substitute(dataRefinement, newType, typeVar);
          if (substExist.$name === 't-existential') {
            const key = typeKey(substExist);
            if (newRefinements.has(key)) {
              newRefinements.get(key)!.dataRefinements.push(substData);
            } else {
              newRefinements.set(key, { existential: substExist, dataRefinements: [substData] });
            }
          } else {
            newConstraints.push({ subtype: substExist, supertype: substData });
          }
        }
      }
      return { newRefinements, newConstraints };
    }

    function substituteInFields(newType : TS.Type, typeVar : TJ.Variant<TS.Type, 't-var' | 't-existential'>, fieldConstraints : Map<string, [TS.Type, FieldConstraint]>) {
      for(let [key, [constraintType, fieldMappings]] of fieldConstraints) {
        const newConstraintType = substitute(constraintType, newType, typeVar);
        for(let [fieldName, types] of fieldMappings) {
          fieldMappings.set(fieldName, types.map(t => substitute(t, newType, typeVar)));
        }
        // NOTE(joe/ben): why is this key not updated?
        fieldConstraints.set(key, [newConstraintType, fieldMappings]);
      }
    }

    function substituteInExamples(newType : TS.Type, typeVar : TJ.Variant<TS.Type, 't-var' | 't-existential'>, examples : ExampleTypes) {
      for(let [key, exampleTypeInfo] of examples) {
        exampleTypeInfo.exampleTypes = exampleTypeInfo.exampleTypes.map(t => substitute(t, newType, typeVar));
      }
    }

    function addSubstitution(newType : TS.Type, typeVar : TJ.Variant<TS.Type, 't-existential'>, system : ConstraintSystem, solution : ConstraintSolution) {
      solution.substitutions.set(typeKey(typeVar), {newType, typeVar});
      const curLevel = system.curLevel();
      curLevel.constraints = substituteInConstraints(newType, typeVar, curLevel.constraints);
      const { newRefinements, newConstraints } = substituteInRefinements(newType, typeVar, curLevel.refinementConstraints);
      curLevel.constraints.push(...newConstraints);
      curLevel.refinementConstraints = newRefinements;
      substituteInFields(newType, typeVar, curLevel.fieldConstraints);
      substituteInExamples(newType, typeVar, curLevel.exampleTypes);
    }

    function solveHelperFields(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      const { fieldConstraints, variables } = system.curLevel();
      const entries = [...(fieldConstraints.entries())];
      while(entries.length !== 0) {
        const [key, [typ, fieldMappings]] = entries.pop()!;
        // NOTE: because entries is an array copy of the keys of the map,
        // we have to delete the key from the original map in order to avoid infinite recursion
        fieldConstraints.delete(key);
        const instantiatedTyp = instantiateObjectType(typ, context);
        switch(instantiatedTyp.$name) {
          case "t-record": {
            const fields = mapFromStringDict(instantiatedTyp.dict.fields);
            const requiredFieldSet = new Set(fieldMappings.keys());
            const intersection = new Set<string>();
            const remainingFields = new Set<string>();
            for(let s of requiredFieldSet) {
              if(fields.has(s)) { intersection.add(s); }
              else { remainingFields.add(s); }
            }
            if(remainingFields.size > 0) {
              const missingFieldErrors = [...remainingFields].map(rfn =>
                CS['object-missing-field'].app(rfn, String(instantiatedTyp), instantiatedTyp.dict.l, fieldMappings.get(rfn)![0].dict.l)
              );
              throw new TypeCheckFailure(...missingFieldErrors);
            }
            else {
              for(let fieldName of intersection) {
                for(let fieldType of fieldMappings.get(fieldName)!) {
                  const objectFieldType = fields.get(fieldName)!;
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
            if(variables.has(typeKey(instantiatedTyp))) {
              LOG(`About to fail in solveHelperFields, typ = ${typeKey(typ)} => ${typeKey(instantiatedTyp)}\n`);
              LOG(`Current constraint: ${key} => [${typeKey(typ)} => ${prettyIsh(objMapValues(fieldMappings, (typs) => typs.map(typeKey).join(',')))}]\n`);
              throw new TypeCheckFailure(CS['unable-to-infer'].app(instantiatedTyp.dict.l));
            }
            for(let [fieldName, fieldTypes] of fieldMappings) {
              for(let fieldType of fieldTypes) {
                system.nextLevel().addFieldConstraint(instantiatedTyp, fieldName, fieldType);
              }
            }
            // NOTE(ben/joe): this is a recursive call in the original Pyret code
            continue;
          }
          default: {
            const dataType = instantiateDataType(instantiatedTyp, context);
            const dataFields = mapFromStringDict(dataType.dict.fields);
            for(let [fieldName, fieldTypes] of fieldMappings) {
              if(dataFields.has(fieldName)) {
                const dataFieldType = dataFields.get(fieldName)!;
                for(let fieldType of fieldTypes) {
                  system.addConstraint(dataFieldType, fieldType);
                }
              }
              else {
                throw new TypeCheckFailure(CS['object-missing-field'].app(fieldName, String(instantiatedTyp), instantiatedTyp.dict.l, fieldMappings.get(fieldName)![0].dict.l))
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
      while(constraints.length > 0) {
        const { subtype, supertype } = constraints.pop() as Constraint;
        if(supertype.$name === "t-top" || subtype.$name === "t-bot") {
          continue;
        }
        LOG(`In solveHelperConstraints, ${typeKey(subtype)}  <:  ${typeKey(supertype)}\n`);

        switch(supertype.$name) {
          case "t-existential": {
            switch(subtype.$name) {
              case "t-existential": {
                if (nameToKey(supertype.dict.id) === nameToKey(subtype.dict.id)) {
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
                  system.nextLevel().addConstraint(subtype, supertype);
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
                  system.nextLevel().addConstraint(subtype, supertype);
                  continue;
                }
              }
            }
          }
          case "t-data-refinement": {
            system.addConstraint(subtype,supertype.dict['data-type']);
            continue;
          }
          case "t-forall": {
            const newOnto = instantiateForallWithFreshVars(supertype, system, true);
            system.addConstraint(subtype, newOnto);
            continue;
          }
          case "t-record": {
            switch(subtype.$name) {
              case "t-app":
              case "t-name": {
                const instantiated = instantiateDataType(subtype, context);
                const availableFields = mapFromStringDict(instantiated.dict.fields);
                for (let [fieldName, fieldType] of mapFromStringDict(supertype.dict.fields)) {
                  const fieldInDataType = availableFields.get(fieldName);
                  if(fieldInDataType === undefined) {
                    throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); 
                  }
                  system.addConstraint(fieldInDataType, fieldType);
                }
                continue;
              }
              default: {
                // This deliberately falls through to the default: case for
                // handling all other subtypes. continue here would be incorrect
                // because we haven't made progress on this constraint
              }
            }
          }
          default: {
            switch(subtype.$name) {
              case "t-name": {
                if(supertype.$name !== "t-name") { 
                  LOG(`About to fail in solveHelperConstraints/super-default/sub-name, subtype = ${typeKey(subtype)} < ${typeKey(supertype)} = supertype\n`);
                  throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); 
                }
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
                  system.addConstraint(supertypeArgs[i], subtypeArgs[i]);
                }
                // covariant return type
                system.addConstraint(subtype.dict.ret, supertype.dict.ret);
                continue;
              }
              case "t-app": {
                if(supertype.$name !== "t-app") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                const subtypeArgs = listToArray(subtype.dict.args);
                const supertypeArgs = listToArray(supertype.dict.args);
                if (subtypeArgs.length !== supertypeArgs.length) { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                for (let i = 0; i < supertypeArgs.length; i++) {
                  // covariant argument types
                  system.addConstraint(subtypeArgs[i], supertypeArgs[i]);
                }
                // covariant return type
                system.addConstraint(subtype.dict.onto,supertype.dict.onto);
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
                  system.addConstraint(subtypeFields.get(superKey)!, superFieldType);
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
                  system.addConstraint(subtypeElts[i], supertypeElts[i]);
                }
                continue;
              }
              case "t-forall": {
                const newOnto = instantiateForallWithFreshVars(subtype, system, true);
                system.addConstraint(newOnto, supertype);
                continue;
              }
              case "t-ref": {
                if(supertype.$name !== "t-ref") { throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype)); }
                // NOTE(Ben): should this be *invariant* (and add two constraints instead of just one)?
                system.addConstraint(subtype.dict.typ, supertype.dict.typ);
                continue;
              }
              case "t-data-refinement": {
                system.addConstraint(subtype.dict['data-type'], supertype);
                continue;
              }
              case "t-var": {
                if(supertype.$name === "t-var" && sameName(subtype.dict.id, supertype.dict.id)) { continue; }
                throw new TypeCheckFailure(CS['type-mismatch'].app(subtype, supertype));
              }
              case "t-existential": {
                // NOTE(Ben): contravariant -- why?
                system.addConstraint(supertype, subtype);
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
      const { refinementConstraints, variables } = system.curLevel();
      // If we have any refinement constraints whose variables aren't in our level,
      // make them be the next level's problem
      const nextLevelConstraints = new Map<string, Refinement>();
      for (let [key, r] of refinementConstraints) {
        if (!variables.has(key)) {
          nextLevelConstraints.set(key, r);
        }
      }
      if (nextLevelConstraints.size > 0) {
        const nextLevel = system.nextLevel();
        for (let [key, r] of nextLevelConstraints) {
          refinementConstraints.delete(key);
          for (let dr of r.dataRefinements) {
            nextLevel.addRefinementConstraint(r.existential, dr);
          }
        }
      }
      // At this point,
      // * All normal subtyping constraints should be solved, and 
      // * All refinements should be in our variables
      // By construction, refinementConstraints has already merged all 
      // data refinements of the same existential variables.

      if(refinementConstraints.size === 0) { return solution; }
      const tempVariables = new TypeSet<TJ.Variant<TS.Type, 't-existential'>>();
      for (let refinement of refinementConstraints.values()) {
        const { existential, dataRefinements } = refinement;
        const tempVar = newExistential(existential.dict.l, false);
        LOG(`Creating tempVar ${typeKey(tempVar)} for ${typeKey(existential)}\n`);
        system.addVariable(tempVar);
        tempVariables.add(tempVar);
        for (let dr of dataRefinements) {
          LOG(`Constraining tempVar ${typeKey(tempVar)} < ${typeKey(dr.dict['data-type'])}\n`);
          system.addConstraint(tempVar, dr.dict['data-type']);
        }
      }
      const tempSolution = solveHelperConstraints(system, new ConstraintSolution(), context);
      const tempSubstitutions = new TypeSet(...[...tempSolution.substitutions.values()].map(({typeVar}) => typeVar));
      const newKeys = tempSubstitutions.difference(tempVariables);
      // TODO(Matt): make this more robust
      if (newKeys.size() > 0) { // || !tempSustem.refinementConstraints.size() > 0
        LOG(`newKeys: ${[...newKeys.values()].map(typeKey).join(',')}\n`);
        for (let tempVar of newKeys.values()) {
          solution.substitutions.set(typeKey(tempVar), tempSolution.substitutions.get(typeKey(tempVar))!)
        }
        LOG("About to recur...?\n");
        return solveHelperRefinements(system, new ConstraintSolution(new Map(), solution.substitutions), context);
      } else {
        // merge all constraints for each existential variable
        // same data-refinements get merged otherwise goes to the inner data type
        for (let refinement of refinementConstraints.values()) {
          const { existential: exists, dataRefinements: refinements } = refinement;
          const mergedType = (refinements as TS.Type[]).reduce((refinement, merged) => {
            if (merged.$name === 't-data-refinement') {
              if (refinement.$name === 't-data-refinement') {
                if (refinement.dict['variant-name'] === merged.dict['variant-name']) {
                  return merged;
                } else {
                  return refinement.dict['data-type'];
                }
              } else {
                return refinement;
              }
            } else {
              return merged;
            }
          });
          addSubstitution(mergedType, exists, system, solution);
        }
        return solution;
      }
    }
    function solveHelperExamples(system : ConstraintSystem, solution : ConstraintSolution, context : Context) : ConstraintSolution {
      return solution;
    }

    type Substitution = {newType: TS.Type, typeVar: TJ.Variant<TS.Type, 't-existential'>}
    class ConstraintSolution {
      variables : Map<string, TS.Type>
      substitutions: Map<string, Substitution>

      constructor(variables? : Map<string, TS.Type>, substitutions? : Map<string, Substitution>) {
        this.variables = variables ?? new Map();
        this.substitutions = substitutions ?? new Map();
      }

      toInertData() {
        return {
          variables: objMapValues(this.variables, typeKey),
          substitutions: objMapValues(this.substitutions, ({newType, typeVar}) => ({newType: typeKey(newType), typeVar: typeKey(typeVar)})),
        };
      }
      toString() {
        return prettyIsh(this.toInertData());
      }

      apply(typ : TS.Type) : TS.Type {
        const thisCS = this;
        return map<TS.Type>({
          "t-record": (self, t: TJ.Variant<TS.Type, 't-record'>) => {
            const fields = mapFromStringDict(t.dict.fields);
            for(let [name, val] of fields.entries()) {
              fields.set(name, thisCS.apply(val));
            }
            return TS['t-record'].app(stringDictFromMap(fields), t.dict.l, t.dict.inferred);
          },
          "t-existential": (self, t: TJ.Variant<TS.Type, 't-existential'>) => {
            const key = typeKey(t);
            if(thisCS.substitutions.has(key)) {
              const {newType: assignedType} = thisCS.substitutions.get(key)!;
              const inferred = t.dict.inferred || assignedType.dict.inferred;
              return thisCS.apply(setLocAndInferred(assignedType, t.dict.l, inferred));
            }
            else {
              return t;
            }
          }
        }, typ);
      }
      applyDataType(dataType: TS.DataType): TS.DataType {
        const { name, params, l } = dataType.dict;
        const variants = listToArray(dataType.dict.variants).map((v) => this.applyVariant(v));
        let fields = mapFromStringDict(dataType.dict.fields);
        fields = mapMapValues(fields, (typ) => this.generalize(this.apply(typ)));
        return TS['t-data'].app(name, params, runtime.ffi.makeList(variants), stringDictFromMap(fields), l);
      }
      applyVariant(variantType: TS.TypeVariant): TS.TypeVariant {
        switch(variantType.$name) {
          case 't-variant':{
            let withFields = mapFromStringDict(variantType.dict['with-fields']);
            withFields = mapMapValues(withFields, (typ) => this.generalize(this.apply(typ)));
            const { name, fields, l } = variantType.dict;
            return TS['t-variant'].app(name, fields, stringDictFromMap(withFields), l);
          }
          case 't-singleton-variant': {
            let withFields = mapFromStringDict(variantType.dict['with-fields']);
            withFields = mapMapValues(withFields, (typ) => this.generalize(this.apply(typ)));
            const { name, l } = variantType.dict;
            return TS['t-singleton-variant'].app(name, stringDictFromMap(withFields), l);
          }
          default: throw new ExhaustiveSwitchError(variantType);
        }
      }
      generalize(typ : TS.Type) : TS.Type {
        const thisCS = this;
        function collectVars(typ : TS.Type, varMapping : Map<string, TJ.Variant<TS.Type, 't-var'>>) : TS.Type {
          return map<TS.Type>({
            "t-record": (self, t: TJ.Variant<TS.Type, 't-record'>) => {
              const fields = mapFromStringDict(t.dict.fields);
              for(let [name, val] of fields.entries()) {
                fields.set(name, collectVars(val, varMapping));
              }
              return TS['t-record'].app(stringDictFromMap(fields), t.dict.l, t.dict.inferred);
            },
            "t-existential": (self, t: TJ.Variant<TS.Type, 't-existential'>) => {
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
        const varMapping = new Map<string, TJ.Variant<TS.Type, 't-var'>>();
        const newTyp = collectVars(typ, varMapping);
        const vars = [...varMapping.values()];
        const ret = (vars.length === 0) ? typ : TS['t-forall'].app(runtime.ffi.makeList(vars), newTyp, typ.dict.l, false);
        LOG(`Generalized ${typeKey(typ)} into ${typeKey(ret)}\n`);
        return ret;
      }
    }

    function renderData(data : TS.DataType) {
      return {
        name: data.dict.name,
        params: listToArray(data.dict.params).map(typeKey),
        variants: listToArray(data.dict.variants).map((v) => {
          return {
            l: formatSrcloc(v.dict.l, true),
            name: v.dict.name,
            withFields: objMapValues(mapFromStringDict(v.dict['with-fields']), typeKey),
          };
        }),
        fields: objMapValues(mapFromStringDict(data.dict.fields), typeKey),
      };
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

      toInertData() {
        return {
          types: objMapValues(this.types, typeKey),
          aliases: objMapValues(this.aliases, typeKey),
          dataTypes: objMapValues(this.dataTypes, renderData),
        }
      }

      toString() {
        return prettyIsh(this.toInertData());
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
      globalTypes : Map<string, TS.Type>;      // global name -> type
      aliases : Map<string, TS.Type>;          // t-name -> aliased type
      dataTypes : Map<string, TS.DataType>;    // t-name -> data type
      modules : Map<string, TS.ModuleType>;    // module name -> module type
      moduleNames : Map<string, string>;       // imported name -> module name
      binds : Map<string, TS.Type>;            // local name -> type
      constraints : ConstraintSystem;          // constraints should only be added with methods to ensure that they have the proper forms
      info : TCInfo;
      misc : Map<string, [TS.Type[], string]>; // miscellaneous info that is used for logging. Keyed by the function name

      /** an option containing the key of the function name,
       the arg-types (some of which are existentials),
       the return type (which may be an existential),
       and the existential that is the function's type
       */
      testInferenceData?: {
        name: A.Name,
        argTypes: TS.Type[],
        retType: TS.Type,
        loc: SL.Srcloc,
        existential: TS.Type,
      };

      miscTestInferenceData?: A.Name;

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

        this.testInferenceData = undefined;
        this.miscTestInferenceData = undefined;
      }

      toInertData() {
        return {
          globalTypes: objMapValues(this.globalTypes, typeKey),
          aliases: objMapValues(this.aliases, typeKey),
          dataTypes: objMapValues(this.dataTypes, renderData),
          modules: objMapValues(this.modules, (m) => {
            const { name, provides, types, aliases } = m.dict;
            return {
              name,
              provides: typeKey(provides),
              types: objMapValues(mapFromStringDict(types), renderData),
              aliases: objMapValues(mapFromStringDict(aliases), typeKey)
            }
          }),
          moduleNames: this.moduleNames,
          binds: objMapValues(this.binds, typeKey),
          constraints: this.constraints.toInertData(),
          info: this.info.toInertData(),
          misc: objMapValues(this.misc, (val) => {
            const [types, str] = val;
            return [types.map(typeKey), str];
          })
        };
      }
      toString() {
        return prettyIsh(this.toInertData());
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
        LOG(`Binding ${termKey} to ${typeKey(assignedType)}\n`);
        this.binds.set(termKey, assignedType);
      }

      addDictToBindings(bindings : Map<string, TS.Type>) {
        for(let [key, typ] of bindings) {
          LOG(`Binding ${key} to ${typeKey(typ)}\n`);
          this.binds.set(key, typ);
        }
      }

      /**
       * Removes a binding from the list of bindings and ADDS its finalized type
       * to the type info to be used by provides and other contexts (e.g. maybe
       * by show-type-on-hover)
       * @param termKey 
       */
      removeBinding(termKey : string) {
        LOG(`Deleting binding ${termKey} (which ${this.binds.has(termKey) ? 'was' : 'was NOT'} found)\n`);
        this.info.types.set(termKey, this.binds.get(termKey)!);
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

      addFieldConstraint(typ : TS.Type, fieldName: string, fieldType: TS.Type) {
        this.constraints.addFieldConstraint(typ, fieldName, fieldType);
      }

      addMiscExampleVariable(funKey: string, funName: string) {
        this.misc.set(funKey, [[], funName]);
      }

      addExampleVariable(
        existential: TS.Type,
        argTypes: TS.Type[],
        retType: TS.Type,
        loc: SL.Srcloc,
        checkFunction: ExampleTypeInfo['checkFunction'],
        functionName: string
      ): void {
        this.constraints.addExampleVariable(existential, argTypes, retType, loc, checkFunction, functionName);
      }

      addMiscExampleType(funKey: string, typ: TS.Type) {
        if (this.misc.has(funKey)) {
          const cur = this.misc.get(funKey)!;
          cur[0].unshift(typ);
        }
      }

      getDataType(type : TJ.Variant<TS.Type, 't-name'>): TS.DataType | false {
        const resolvedType = resolveAlias(type, this)
        switch(resolvedType.$name) {
          case 't-name': {
            const modName = resolvedType.dict['module-name'];
            const idName = nameToName(resolvedType.dict.id);
            const idKey = nameToKey(resolvedType.dict.id);
            switch(modName.$name) {
              case 'module-uri': {
                if (this.modules.has(modName.dict.uri)) {
                  const mod = this.modules.get(modName.dict.uri)!;
                  const modTypes = mapFromStringDict(mod.dict.types);
                  if (modTypes.has(idName)) {
                    return modTypes.get(idName)!;
                  } else {
                    throw new InternalCompilerError(`No type ${typeKey(type)} available on ${prettyIsh(mod)}`)
                  }
                } else if (modName.dict.uri === "builtin") {
                  if (this.dataTypes.has(idKey)) {
                    return this.dataTypes.get(idKey)!;
                  } else {
                    return false;
                  }
                } else {
                  throw new InternalCompilerError(`No module available with the name '${modName}'`);
                }
              }
              case 'local': {
                if (this.dataTypes.has(idKey)) {
                  return this.dataTypes.get(idKey)!;
                } else {
                  return false;
                }
              }
              case 'dependency':
                throw new InternalCompilerError(`Should not get dependency in typechecker; got ${prettyIsh(modName)}`);
              default:
                throw new ExhaustiveSwitchError(modName);
            }
          }
          default:
            throw new InternalCompilerError(`Should not resolve an alias to a ${resolvedType.$name} in getDataType`);
        }
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
          LOG(`Solving level (depth ${this.constraints.levels.length}) ${this.constraints.curLevel().name}:\n${String(this.constraints)}\nCurrent bindings:\n`);
          for (let [name, type] of this.binds) {
            LOG(`${name} => ${typeKey(type)}\n`);
          }
          LOG("\n");
          const result = this.constraints.solveLevel(this);
          LOG(`With solution: ${String(result)}\n\n`);
          return result;
        }
        catch(e) {
          LOG(`Got an exception while solving: ${this.constraints.toString()}\n\n`);
          throw e;
        }
      }

      solveAndResolveType(t : TS.Type) : TS.Type {
        const solution = this.solveLevel()
        this.substituteInBinds(solution);
        this.substituteInMisc(solution);
        let result = solution.apply(t);
        if(result.$name === 't-app' && (result.dict.onto.$name === 't-app' || result.dict.onto.$name === 't-forall')) {
          result = simplifyTApp(result, this);
        }
        LOG(`Solved and resolved: ${typeKey(t)} ===> ${typeKey(result)}\n`);
        LOG(`under solution ${solution.toString()}\n\n`);
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
                if (aliased === undefined) {
                  LOG(`Found no alias for ${typeKey(type)} among ${[...context.aliases.keys()].join(',')}\n`);
                  return type;
                }
                return setLocAndInferred(aliased, l, inferred);
              } else {
                const modtyp = context.modules.get(mod);
                if (modtyp === undefined) {
                  throw new InternalCompilerError(`Module not found with name ${mod}`)
                }
                const dataType = callMethod(modtyp.dict.types, 'get', nameToName(id));
                switch(dataType.$name) {
                  case 'some': {
                    LOG(`Found type ${typeKey(type)} as ${dataType.dict.value.dict.name}, but ignoring that and returning type\n`);
                    return type;
                  }
                  case 'none': {
                    const aliased = callMethod(modtyp.dict.aliases, 'get', nameToName(id));
                    switch(aliased.$name) {
                      case 'none': {
                        LOG(`Did not find ${typeKey(type)} in modTyp.aliases, retruning type itself\n`);
                        return type;
                      }
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

    function setTypeLoc<T extends TS.Type>(typ: T, loc: SL.Srcloc): TJ.Variant<TS.Type, T['$name']> {
      return map<SL.Srcloc | TS.Type, T>({
        "builtin": (_self, _oldLoc) => loc,
        "srcloc": (_self, _oldLoc) => loc,
        "t-record": (self, tRecord: TJ.Variant<TS.Type, 't-record'>) => {
          return TS['t-record'].app(
            stringDictFromMap(mapMapValues(mapFromStringDict(tRecord.dict.fields), (a) => map(self, a))),
            loc,
            tRecord.dict.inferred
          );
        }
      }, typ);
    }

    function setInferred(type: TS.Type, inferred: boolean): TS.Type {
      const newType = Object.create(Object.getPrototypeOf(type));
      Object.assign(newType, type);
      newType.dict.inferred = inferred;
      return newType;
    }

    function setLocAndInferred(type: TS.Type, loc: SL.Srcloc, inferred: boolean): TS.Type {
      return setTypeLoc(setInferred(type, inferred), loc);
    }

    function substitute<T extends TS.Type>(
      type: T,
      newType: TS.Type,
      typeVar: TJ.Variant<TS.Type, 't-var' | 't-existential'>
    ): (T['$name'] extends (typeof typeVar)['$name'] ? TS.Type : TJ.Variant<TS.Type, T['$name']>) {
      return map<TS.Type>({
        // Note: t-forall doesn't need to be capture-avoiding thanks to resolve-names
        // so we don't need to handle it specially

        "t-record": (self, t: TJ.Variant<TS.Type, 't-record'>) => {
          const fields = mapFromStringDict(t.dict.fields);
          for(let [name, val] of fields.entries()) {
            fields.set(name, map(self, val));
          }
          return TS['t-record'].app(stringDictFromMap(fields), t.dict.l, t.dict.inferred);
        },
        "t-var": (_self, t: TJ.Variant<TS.Type, 't-var'>) => {
          if (typeVar.$name === "t-var" && sameName(t.dict.id, typeVar.dict.id)) {
            return setTypeLoc(newType, type.dict.l);
          } else {
            return t;
          }
        },
        "t-existential": (_self, t: TJ.Variant<TS.Type, 't-existential'>) => {
          // inferred existentials keep their locations
          // this is along the lines of inferred argument types etc
          // uninferred existentials are used to equate different pieces of code
          // they should not keep their location
          if (typeVar.$name === "t-existential" && sameName(t.dict.id, typeVar.dict.id)) {
            if (t.dict.inferred) {
              return setTypeLoc(newType, type.dict.l);
            } else {
              return newType;
            }
          } else {
            return t;
          }
        }
      }, type);
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
        return setTypeLoc(context.binds.get(idKey)!, blameLoc);
      }
      else if(context.globalTypes.has(idKey)) {
        return setTypeLoc(context.globalTypes.get(idKey)!, blameLoc);
      }
      else {
        throw new TypeCheckFailure(CS['unbound-id'].app(idExpr));
      }
    }

    function removeRefinementAndForalls(typ : TS.Type): TS.Type {
      return map<TS.Type>({
        't-forall': (visitor, forall : TJ.Variant<TS.Type, 't-forall'>) => {
          const { introduces, onto } = forall.dict;
          let ret = onto;
          for (const aVar of listToArray(introduces)) {
            ret = substitute(ret, newExistential(aVar.dict.l, false), aVar);
          }
          return map(visitor, ret);
        },
        't-data-refinement': (visitor, dataRefinement: TJ.Variant<TS.Type, 't-data-refinement'>) => {
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
                genFields.set(curField, generalizeType(curType, nextFields.get(curField)!));
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
    function instantiateForallWithFreshVars(type : TS.Type, system : ConstraintSystem, deep: boolean): TS.Type {
      switch (type.$name) {
        case 't-forall': {
          const { introduces, onto } = type.dict;
          let newOnto = deep ? instantiateForallWithFreshVars(onto, system, deep) : onto;
          const introducesArr = listToArray(introduces);
          const newExistentials = introducesArr.map((i) => newExistential(i.dict.l, false));
          for (let i = 0; i < newExistentials.length; i++) {
            newOnto = substitute(newOnto, newExistentials[i], introducesArr[i]);
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
      // runtime['RUNGAS'] = Infinity;
      // runtime['GAS'] = Infinity;
      // logger.app(val, runtime.ffi.makeNone());
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
          let newOnto: TS.Type = onto.dict.onto;
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

    function substituteVariant(variant: TS.TypeVariant, newType: TS.Type, typeVar: TJ.Variant<TS.Type, 't-var' | 't-existential'>): TS.TypeVariant {
      switch(variant.$name) {
        case 't-variant': {
          const fields = listToArray(variant.dict.fields).map(t => runtime.makeTuple([...t.vals]));
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
    function substituteFields(fields: Map<string, TS.Type>, newType: TS.Type, typeVar: TJ.Variant<TS.Type, 't-var' | 't-existential'>): void {
      for (let [f, fType] of fields) {
        fields.set(f, substitute(fType, newType, typeVar));
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
              throw new TypeCheckFailure(CS['incorrect-type'].app(typeKey(aOnto), aOnto.dict.l, "a polymorphic type", l));
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
          const instantiated = instantiateForallWithFreshVars(typ, context.constraints, true);
          return instantiateObjectType(instantiated, context);
        }
        default: {
          throw new TypeCheckFailure(CS['incorrect-type'].app(typeKey(typ), typ.dict.l, "an object type", typ.dict.l));
        }
      }
    }

    function instantiateDataType(typ: TS.Type, context: Context): TS.DataType {
      function helper(typ : TS.Type, context: Context): TS.DataType {
        switch (typ.$name) {
          case 't-name': {
            const dataType = context.getDataType(typ);
            if (dataType) { return dataType; }
            LOG(`Searching for ${typeKey(typ)} in `);
            LOG("Known datatypes:");
            context.dataTypes.forEach((data, key) => {
              LOG(`Key: ${key} ==> `);
              LOG(`Data fields: ${[...mapFromStringDict(data.dict.fields).keys()].join(", ")}\n`);
              LOG(`Data variants: ${listToArray(data.dict.variants).map((v) => v.dict.name).join(", ")}\n`);
            });
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`Expected a data type but got ${typeKey(typ)}`, typ.dict.l));
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
            const newTyp = instantiateForallWithFreshVars(typ, context.constraints, true);
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
      context.addLevel(`_checking(${e.$name}) at ${formatSrcloc(e.dict.l, false)} against expectTyp ${typeKey(expectTyp)}`);
      function solveAndReturn() {
        const solution = context.solveLevel();
        return;
      }
      expectTyp = resolveAlias(expectTyp, context);
      if(expectTyp.$name === 't-app' && (expectTyp.dict.onto.$name === 't-app' || expectTyp.dict.onto.$name === 't-forall')) {
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
        case 's-check': return solveAndReturn();
        case 's-check-expr': {
          synthesis(e, topLevel, context);
          return solveAndReturn();
        }
        case 's-paren': {
          checkSynthesis(e.dict.expr, expectTyp, topLevel, context);
          return solveAndReturn();
        }
        case 's-construct': {
          const constr = A['s-app'].app(
            e.dict.l,
            A['s-dot'].app(e.dict.l, e.dict.constructor, "make"),
            runtime.ffi.makeList([A['s-array'].app(e.dict.l, e.dict.values)])
          );
          checkSynthesis(constr, expectTyp, topLevel, context);
          return solveAndReturn();
        }
        case 's-instantiate':
        case 's-op':
        case 's-extend':
        case 's-update':
        case 's-tuple-get':
        case 's-app':
        case 's-prim-app':
        case 's-id':
        case 's-id-var-modref':
        case 's-id-modref':
        case 's-id-var':
        case 's-id-letrec':
        case 's-srcloc':
        case 's-num':
        case 's-frac':
        case 's-rfrac':
        case 's-bool':
        case 's-str':
        case 's-dot':
        case 's-get-bang':
        case 's-spy-block':
        case 's-for': {
          const result = checkSynthesis(e, expectTyp, topLevel, context);
          LOG(`checkSynthesis on ${e.$name} of ${typeKey(expectTyp)} produced ${typeKey(result)}\n`);
          return solveAndReturn();
        }
        case 's-array': {
          switch(expectTyp.$name) {
            case 't-app': {
              if (typeKey(tArrayName) === typeKey(expectTyp.dict.onto)) {
                const paramType = listToArray(expectTyp.dict.args)[0];
                for (let value of listToArray(e.dict.values)) {
                  checking(value, paramType, false, context);
                }
                return solveAndReturn();
              } else {
                throw new TypeCheckFailure(CS['incorrect-type-expression'].app(typeKey(tArrayName), e.dict.l, typeKey(expectTyp), expectTyp.dict.l, e));
              }
            }
            default: {
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app("a raw array", e.dict.l, typeKey(expectTyp), expectTyp.dict.l, e));
            }
          }
        }
        case 's-let-expr': {
          // TODO(Ben): ignoreChecker?
          const rhsResult : TS.Type[] = [];
          const binds = listToArray(e.dict.binds);
          for (const lb of binds) {
            rhsResult.push(synthesisLetBind(lb, context));
          }
          checking(e.dict.body, expectTyp, topLevel, context);
          for (const b of binds) {
            context.removeBinding(nameToKey((b.dict.b as TJ.Variant<A.Bind, "s-bind">).dict.id));
          }
          return solveAndReturn();
        }
        case 's-letrec': {
          const binds = listToArray(e.dict.binds);
          handleLetrecBindings(binds, topLevel, context);
          checking(e.dict.body, expectTyp, topLevel, context);
          for (const b of binds) {
            context.removeBinding(nameToKey((b.dict.b as TJ.Variant<A.Bind, "s-bind">).dict.id));
          }
          return solveAndReturn();
        }
        case 's-block': {
          const stmts = listToArray(e.dict.stmts);
          const lastStmt = stmts.pop()!;
          const top = TS['t-top'].app(e.dict.l, false);
          for (let stmt of stmts) {
            checking(stmt, top, topLevel, context);
          }
          checking(lastStmt, expectTyp, topLevel, context);
          return solveAndReturn();
        }
        case 's-assign': {
          const idType = lookupId(e.dict.l, nameToKey(e.dict.id), e, context);
          switch(idType.$name) {
            case 't-ref': {
              checking(e.dict.value, idType.dict.typ, topLevel, context);
              return solveAndReturn();
            }
            default: {
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app(typeKey(idType), e.dict.l, typeKey(TS['t-ref'].app(idType, e.dict.l, false)), e.dict.l, e));
            }
          }
        }
        case 's-if-else': {
          const branches = listToArray(e.dict.branches);
          for (let b of branches) {
            checking(b.dict.test, tBoolean(b.dict.l), false, context);
            checking(b.dict.body, expectTyp, false, context);
          }
          checking(e.dict._else, expectTyp, false, context);
          return solveAndReturn();
        }
        case 's-cases': {
          checkingCases(e.dict.l, e.dict.typ, e.dict.val, listToArray(e.dict.branches), false, expectTyp, context);
          return solveAndReturn();
        }
        case 's-cases-else': {
          checkingCases(e.dict.l, e.dict.typ, e.dict.val, listToArray(e.dict.branches), e.dict._else, expectTyp, context);
          return solveAndReturn();
        }
        case 's-obj': {
          const newExpectTyp = instantiateObjectType(expectTyp, context);
          switch(newExpectTyp.$name) {
            case 't-record': {
              const fields = listToArray(e.dict.fields);
              const fieldTypes = collectMembers(fields, true, context);
              const tempObjectType = TS['t-record'].app(stringDictFromMap(fieldTypes), e.dict.l, false);
              context.addConstraint(tempObjectType, expectTyp);
              for (let field of fields) {
                // NOTE(Ben): this appears to only be used for its side effects of checking each field
                toTypeMember(field, fieldTypes.get(field.dict.name)!, tempObjectType, true, context);
              }
              return solveAndReturn();
            }
            default:
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app(typeKey(expectTyp), expectTyp.dict.l, "an object type", e.dict.l, e));
          }
        }
        case 's-check-test': {
          const result = synthesisCheckTest(e, context);
          context.addConstraint(result, expectTyp);
          return solveAndReturn();
        }
        case 's-data':
        case 's-user-block':
        case 's-fun':
        case 's-var':
        case 's-let':
        case 's-when':
        case 's-if-pipe':
        case 's-if-pipe-else':
        case 's-if':
        case 's-reactor':
          throw new InternalCompilerError(`${e.$name} should already have been desugared`);
        case 's-type':
        case 's-newtype':
        case 's-rec':
          throw new InternalCompilerError(`${e.$name} should be removed by resolve-scope`);
        case 's-data-expr':
          throw new InternalCompilerError("s-data-expr should have been handled by s-letrec");
        case 's-prim-val': // NOTE(joe Jan22) Needs to be implemented
        case 's-bracket':  // NOTE(joe Jan22) Needs to be implemented
        case 's-contract':
          throw new InternalCompilerError(`checking for ${e.$name} not implemented`);
        case 's-app-enriched':
          throw new InternalCompilerError(`checking for ${e.$name} should not happen; should be introduced after tc`);
        case 's-module':
          throw new InternalCompilerError(`checking for ${e.$name} should not happen; should only by synthed`);
        case 's-table':
        case 's-table-extend':
        case 's-table-extract':
        case 's-table-filter':
        case 's-table-filter':
        case 's-table-order':
        case 's-table-select':
        case 's-table-update':
        case 's-load-table':
          throw new InternalCompilerError(`_checking switch ${e.$name} not even mentioned`);
        default:
          throw new ExhaustiveSwitchError(e);
      }
    }

    function checkingCases(
      l: SL.Srcloc,
      ann: A.Ann,
      val: A.Expr,
      branches: A.CasesBranch[],
      maybeElse: A.Expr | false,
      expectType: TS.Type,
      context: Context): void {
      handleCases(
        l, 
        ann,
        val,
        branches,
        maybeElse,
        expectType,
        (_l, _branchTypes, elseBranch, context) => {
          checking(elseBranch, expectType, false, context);
        },
        (_l, _branchTypes, _context) => {
          return;
        },
        context);
    }

    function synthesisCases(
      l: SL.Srcloc,
      ann: A.Ann,
      val: A.Expr,
      branches: A.CasesBranch[],
      maybeElse: A.Expr | false,
      context: Context): TS.Type {
      return handleCases(
        l,
        ann,
        val,
        branches,
        maybeElse,
        false,
        (l, branchTypes, elseBranch, context) => {
          const elseType = synthesis(elseBranch, false, context);
          return setTypeLoc(meetBranchTypes([elseType, ...branchTypes], l, context), l);
        },
        (l, branchTypes, context) => {
          return setTypeLoc(meetBranchTypes(branchTypes, l, context), l);
        },
        context
      );
    }


    function handleCases<T>(l: SL.Srcloc,
      ann: A.Ann,
      val: A.Expr,
      branches: A.CasesBranch[],
      maybeElse: A.Expr | false,
      maybeExpect: TS.Type | false,
      hasElse: (
        l: SL.Srcloc,
        branchTypes: TS.Type[],
        elseBranch: A.Expr,
        context: Context,
      ) => T,
      noElse: (
        l: SL.Srcloc,
        branchTypes: TS.Type[],
        context: Context,
      ) => T,
      context: Context): T {
      const typ = toType(ann, context);
      if (typ) {
        context.addLevel(`handleCases at ${formatSrcloc(l, false)}`);
        const casesType = addExistentialsToDataName(typ, context);
        let valType = synthesis(val, false, context);
        context.addConstraint(valType, casesType);
        valType = context.solveAndResolveType(valType);

        const dataType = instantiateDataType(valType, context);
        const variants = listToArray(dataType.dict.variants);
        // Branch map contains all the variants of the data type for which
        // we haven't yet found a CasesBranch that match it
        const branchMap = new Map<string, TS.TypeVariant>();
        for (const v of variants) {
          branchMap.set(v.dict.name, v);
        }
        
        let maybeKeyToUpdate : string | false = false;
        switch(val.$name) {
          case 's-id':
          case 's-id-var':
          case 's-id-letrec': maybeKeyToUpdate = nameToKey(val.dict.id);
        }
        const branchResults: TS.Type[] = [];
        for (const b of branches) {
          if (maybeKeyToUpdate) {
            context.addBinding(maybeKeyToUpdate, TS['t-data-refinement'].app(valType, b.dict.name, l, true));
          }
          branchResults.push(handleBranch(dataType, l, b, maybeExpect, branchMap, context));
        }
        if (maybeElse) {
          if (branchMap.size === 0) {
            throw new TypeCheckFailure(CS['unnecessary-else-branch'].app(typeKey(typ), l));
          } else {
            return hasElse(l, branchResults, maybeElse, context);
          }
        } else {
          if (branchMap.size === 0) {
            return noElse(l, branchResults, context);
          } else {
            throw new TypeCheckFailure(CS['non-exhaustive-pattern'].app(runtime.ffi.makeList([...branchMap.values()]), typeKey(typ), l));
          }
        }
      } else {
        throw new TypeCheckFailure(CS['cant-typecheck'].app("Could not resole type on cases expression", l));
      }
    }

    function handleBranch(
      dataType: TS.DataType,
      casesLoc: SL.Srcloc,
      branch: A.CasesBranch,
      maybeCheck: TS.Type | false,
      branchMap: Map<string, TS.TypeVariant>,
      context: Context): TS.Type {

      // Technically, branchMap might lose members over time, as we handle them.
      // But that's ok!  Duplicate branch names are handled in well-formedness,
      // so we'll only encounter *unique* names here
      if (branchMap.has(branch.dict.name)) {
        const tv = branchMap.get(branch.dict.name)!;
        switch(tv.$name) {
          case 't-variant': {
            switch(branch.$name) {
              case 's-singleton-cases-branch':
                throw new TypeCheckFailure(CS['cases-singleton-mismatch'].app(branch.dict.name, branch.dict.l, false));
              case 's-cases-branch': {
                const args = listToArray(branch.dict.args);
                const fields = listToArray(tv.dict.fields);
                if (args.length !== fields.length) {
                  throw new TypeCheckFailure(CS['incorrect-number-of-bindings'].app(branch, tv));
                }
                context.addLevel();
                for (let i = 0; i < args.length; i++) {
                  const bind = args[i].dict.bind as TJ.Variant<A.Bind, "s-bind">;
                  const maybeType = toType(bind.dict.ann, context);
                  if (maybeType) {
                    context.addConstraint(maybeType, fields[i].vals[1]);
                    context.addBinding(nameToKey(bind.dict.id), maybeType);
                  } else {
                    context.addBinding(nameToKey(bind.dict.id), fields[i].vals[1]);
                  }
                }
                const solution = context.solveLevel();
                context.substituteInBinds(solution);
                branchMap.delete(branch.dict.name);
                let ret: TS.Type;
                if (maybeCheck) {
                  checking(branch.dict.body, maybeCheck, false, context);
                  ret = maybeCheck;
                } else {
                  ret = synthesis(branch.dict.body, false, context);
                }
                branchMap.delete(branch.dict.name);
                for (let a of args) {
                  context.removeBinding(nameToKey((a.dict.bind as TJ.Variant<A.Bind, "s-bind">).dict.id));
                }
                return ret;
              }
              default: throw new ExhaustiveSwitchError(branch);
            }
          }
          case 't-singleton-variant': {
            switch(branch.$name) {
              case 's-cases-branch':
                throw new TypeCheckFailure(CS['cases-singleton-mismatch'].app(branch.dict.name, branch.dict.l, true));
              case 's-singleton-cases-branch': {
                branchMap.delete(branch.dict.name);
                if (maybeCheck) {
                  checking(branch.dict.body, maybeCheck, false, context);
                  return maybeCheck;
                } else {
                  return synthesis(branch.dict.body, false, context);
                }
              }
              default: throw new ExhaustiveSwitchError(branch);
            }
          }
          default: throw new ExhaustiveSwitchError(tv);
        }
      } else {
        throw new TypeCheckFailure(CS['unnecessary-branch'].app(branch, dataType, casesLoc));
      }
    }

    function addExistentialsToDataName(typ: TS.Type, context: Context): TS.Type {
      switch(typ.$name) {
        case 't-name': {
          const dataType = context.getDataType(typ);
          if (dataType) {
            const params = listToArray(dataType.dict.params);
            if (params.length === 0) {
              return typ;
            }
            const newExistentials = params.map((a) => newExistential(a.dict.l, false));
            const newType = TS['t-app'].app(typ, runtime.ffi.makeList(newExistentials), typ.dict.l, typ.dict.inferred);
            context.addVariableSet(newExistentials);
            return newType;
          } else {
            LOG(`Failed trying to get ${typeKey(typ)} from context ${context.toString()}`);
            throw new TypeCheckFailure(CS['cant-typecheck'].app(`Expected a data type but got ${typeKey(typ)}`, typ.dict.l));
          }
        }
        default: return typ;
      }
    }

    function handleLetrecBindings(
      binds: A.LetrecBind[],
      topLevel: boolean,
      context: Context): void {
      context.addLevel("handleLetrecBindings");
      const { dataBindings, bindings: { bindingsToType, collectedTypes } } = collectLetrecBindings(binds, topLevel, context);
      context.addDictToBindings(collectedTypes);
      for (let dataBinding of dataBindings) {
        handleDatatype(dataBinding.dataBinding, dataBinding.variants, context);
      }
      for (let binding of bindingsToType) {
        const b = binding.dict.b as TJ.Variant<A.Bind, 's-bind'>;
        const { l: l2, value } = binding.dict; 
        const expectedType = collectedTypes.get(nameToKey(b.dict.id))!;
        if (context.constraints.curLevel().exampleTypes.has(typeKey(expectedType))) {
          const partialType = context.constraints.curLevel().exampleTypes.get(typeKey(expectedType))!.annTypes;
          if (value.$name === 's-lam') {
            context.testInferenceData = {
              name: b.dict.id,
              argTypes: partialType.argTypes,
              retType: partialType.retType,
              loc: partialType.loc,
              existential: expectedType,
            };
            const _check = value.dict._check;
            if (_check.$name === 'none') {
              throw new InternalCompilerError("Original type-checker assumed this would always be some(value)");
            }
            const checkBlock = _check.dict.value;
            checking(checkBlock, TS['t-top'].app(l2, false), false, context);
            context.testInferenceData = undefined;
          } else {
            throw new InternalCompilerError(`the right hand side should be a lambda; got ${value.$name} at ${formatSrcloc(l2, true)}`);
          }
        } else {
          if (context.misc.has(nameToKey(b.dict.id))) {
            context.miscTestInferenceData = b.dict.id;
          }
          context.addLevel(`handleLetrecBindings for ${nameToKey(b.dict.id)}, with no exampleType for ${typeKey(expectedType)}`);
          const freeVars = freeVariables(expectedType);
          context.addVariableSet([...freeVars.values()]);
          checking(value, expectedType, false, context);
          const solution = context.solveLevel();
          context.substituteInBinds(solution);
          const newType = solution.generalize(solution.apply(expectedType));
          LOG(`${nameToKey(b.dict.id)} had type ${typeKey(expectedType)}, substitution got ${typeKey(newType)}\n`);
          context.addBinding(nameToKey(b.dict.id), newType);
          if (value.$name === "s-lam") {
            if (value.dict._check.$name === "some") {
              const checkLoc = value.dict['_check-loc'] as TJ.Variant<Option<SL.Srcloc>, 'some'>;
              checking(value.dict._check.dict.value, TS['t-top'].app(checkLoc.dict.value, false), false, context);
            }
          }
          context.miscTestInferenceData = undefined;
        }
      }
      const solution = context.solveLevel();
      context.substituteInBinds(solution);
    }

    function handleDatatype(dataTypeBind: A.LetrecBind, bindings: A.LetrecBind[], context: Context): void {
      // TODO(MATT): this should require unifying of same-named methods
      //             should it require unifying types of same-named members?
      // Type checks data types
      // Returns the list of all relevant letrec bindings
      // Use the context returned from this function
      const dataExpr = dataTypeBind.dict.value;
      if (dataExpr.$name !== "s-data-expr") {
        throw new InternalCompilerError(`handleDatatype expected an s-data-expr, but got ${dataExpr.$name}`); 
      }
      context.addLevel(`handleDataType for ${dataExpr.dict.name} at ${formatSrcloc(dataExpr.dict.l, true)}`);
      const branderType = TS['t-name'].app(TS.local, dataExpr.dict.namet, dataExpr.dict.l, false);
      const params = listToArray(dataExpr.dict.params);
      const tVars = params.map((p) => TS['t-var'].app(p, dataExpr.dict.l, false));
      const tVarsList = runtime.ffi.makeList(tVars);
      let appliedBranderType: TS.Type;
      if (tVars.length === 0) {
        appliedBranderType = branderType;
      } else {
        appliedBranderType = TS['t-app'].app(branderType, tVarsList, dataExpr.dict.l, false);
      }

      const variants = listToArray(dataExpr.dict.variants);
      let initialVariantTypes = variants.map((v) => collectVariantConstructor(v, context));
      let predicateType: TS.Type;
      if (tVars.length === 0) {
        predicateType = TS['t-arrow'].app(runtime.ffi.makeList([branderType]), tBoolean(dataExpr.dict.l), dataExpr.dict.l, false);
      } else {
        predicateType = TS['t-forall'].app(tVarsList, TS['t-arrow'].app(runtime.ffi.makeList([TS['t-app'].app(branderType, tVarsList, dataExpr.dict.l, false)]), tBoolean(dataExpr.dict.l), dataExpr.dict.l, false), dataExpr.dict.l, false);
      }
      const dataFields = new Map<string, TS.Type>();
      dataFields.set(`is-${dataExpr.dict.name}`, predicateType);
      for (let varType of initialVariantTypes) {
        const constructorType = makeConstructorType(varType, branderType, tVars);
        dataFields.set(varType.dict.name, constructorType);
        dataFields.set(`is-${varType.dict.name}`, predicateType);
      }
      const dataTypeBindId = (dataTypeBind.dict.b as TJ.Variant<A.Bind, 's-bind'>).dict.id;
      context.addBinding(nameToKey(dataTypeBindId), TS['t-record'].app(stringDictFromMap(dataFields), dataExpr.dict.l, false));
      for (const binding of bindings) {
        const bindingType = synthesis(binding.dict.value, false, context);
        context.addBinding(nameToKey((binding.dict.b as TJ.Variant<A.Bind, 's-bind'>).dict.id), bindingType);
      }
      initialVariantTypes = variants.map((v) => collectVariant(v, context));
      const variantTypesMap = new Map(initialVariantTypes.map((v) => [v.dict.name, v]));
      const initialSharedFieldTypes = collectMembers(listToArray(dataExpr.dict['shared-members']), true, context);
      const initialDataType = TS['t-data'].app(
        dataExpr.dict.name,
        tVarsList, 
        runtime.ffi.makeList(initialVariantTypes),
        stringDictFromMap(initialSharedFieldTypes),
        dataExpr.dict.l);
      context.dataTypes.set(nameToKey(dataExpr.dict.namet), initialDataType);
      mergeCommonFields(initialVariantTypes, dataExpr.dict.l, context);
      for (let variant of variants) {
        const newVariantType = checkVariant(variant, variantTypesMap.get(variant.dict.name)!, branderType, tVars, context);
        variantTypesMap.set(variant.dict.name, newVariantType);
      }
      const variantTypeFields: Map<string, TS.Type>[] = [];
      for (let variant of variants) {
        const variantType = variantTypesMap.get(variant.dict.name)!;
        const allFields = mapFromStringDict(variantType.dict['with-fields']);
        if (variantType.$name === 't-variant') {
          for (let ft of listToArray(variantType.dict.fields)) {
            const [fieldName, fieldType] = ft.vals;
            allFields.set(fieldName, fieldType);
          }
        }
        variantTypeFields.push(allFields);
      }
      let variantsMeet: Map<string, TS.Type>;
      if (variantTypeFields.length === 0) {
        variantsMeet = new Map<string, TS.Type>();
      } else {
        variantsMeet = variantTypeFields.reduce((acc, cur) => meetFields(acc, cur, dataExpr.dict.l, context));
      }
      const extendedSharedFieldTypes = new Map([...variantsMeet, ...initialSharedFieldTypes]);
      const newVariantTypes = runtime.ffi.makeList([...variantTypesMap.values()]);
      const sharedDataType = TS['t-data'].app(dataExpr.dict.name, tVarsList, newVariantTypes, stringDictFromMap(extendedSharedFieldTypes), dataExpr.dict.l);
      context.dataTypes.set(nameToKey(dataExpr.dict.namet), sharedDataType);
      const newSharedFieldTypes = new Map<string, TS.Type>();
      for (let sharedField of listToArray(dataExpr.dict['shared-members'])) {
        // NOTE(Ben/Joe): why isn't this extendedSharedFieldTypes, instead of initialSharedFieldTypes?
        const sharedFieldType = checkSharedField(sharedField, initialSharedFieldTypes, appliedBranderType, context);
        newSharedFieldTypes.set(sharedField.dict.name, sharedFieldType);
      }
      const finalSharedFieldTypes = new Map([...variantsMeet, ...newSharedFieldTypes]);
      const finalDataType = TS['t-data'].app(dataExpr.dict.name, tVarsList, newVariantTypes, stringDictFromMap(finalSharedFieldTypes), dataExpr.dict.l);
      const solution = context.solveLevel();
      const solvedDataType = solution.applyDataType(finalDataType);
      context.dataTypes.set(nameToKey(dataExpr.dict.namet), solvedDataType);
    }

    function mergeCommonFields(variants: TS.TypeVariant[], dataLoc: SL.Srcloc, context: Context): void {
      if (variants.length === 0) return; 
      const fieldsToMerge = new Map<string, TS.Type[]>();
      const allWithFields = variants.map((v) => mapFromStringDict(v.dict['with-fields']));
      const firstMap = allWithFields[0];
      // This essentially computes the intersection of all the WithFields of all of the variants.
      // So we might as well start with the first one in the allWithFields list,
      // and keep only the ones that appear in every set.
      for (let key of firstMap.keys()) {
        let allTyps: TS.Type[] | null = [];
        for (let map of allWithFields) {
          if (map.has(key)) {
            allTyps.push(map.get(key)!);
          } else {
            allTyps = null;
            break;
          }
        }
        if (allTyps) {
          fieldsToMerge.set(key, allTyps);
        }
      }
      for (let [_key, typs] of fieldsToMerge) {
        const mergeExistential = newExistential(dataLoc, false);
        context.addVariable(mergeExistential);
        for (let typ of typs) {
          context.addConstraint(mergeExistential, typ);
        }
      }
    }

    function meetFields(aFields: Map<string, TS.Type>, bFields: Map<string, TS.Type>, loc: SL.Srcloc, context: Context): Map<string, TS.Type> {
      const ret = new Map<string, TS.Type>();
      for (let [aFieldName, aType] of aFields) {
        if (bFields.has(aFieldName)) {
          const tempExistential = newExistential(loc, false);
          context.addLevel();
          context.addVariable(tempExistential);
          aType = instantiateForallWithFreshVars(aType, context.constraints, false);
          const bType = instantiateForallWithFreshVars(bFields.get(aFieldName)!, context.constraints, false);
          context.addConstraint(tempExistential, aType);
          context.addConstraint(tempExistential, bType);
          // NOTE(Ben/Joe): this is changed semantics from type-check.arr:
          // in the original, if solveLevel failed then this line of code
          // would just discard the current field, and proceed with the remaining ones.
          // Instead, this will currently abort the entire type-checker with a unification error.
          // We can restore the original semantics *iff* context.solveLevel is transactional.
          const solution = context.solveLevel();
          const meetType = solution.generalize(solution.apply(tempExistential));
          ret.set(aFieldName, meetType);
        }
      }
      return ret;
    }

    /** Checks with-members on a variant */
    function checkVariant(variant: A.Variant, variantType: TS.TypeVariant, dataType: TS.Type, tVars: TS.Type[], context: Context): TS.TypeVariant {
      const innerType = tVars.length === 0 ? dataType : TS['t-app'].app(dataType, runtime.ffi.makeList(tVars), dataType.dict.l, false);
      const refinedType = TS['t-data-refinement'].app(innerType, variant.dict.name, dataType.dict.l, false);

      const withMembersTypeMap = mapFromStringDict(variantType.dict['with-fields']);
      for (let member of listToArray(variant.dict['with-members'])) {
        const memberType = withMembersTypeMap.get(member.dict.name)!;
        const checkedMemberType = toTypeMember(member, memberType, refinedType, true, context);
        withMembersTypeMap.set(member.dict.name, checkedMemberType);
      }
      const { name, l } = variantType.dict;
      switch(variantType.$name) {
        case 't-variant': 
          return TS['t-variant'].app(name, variantType.dict.fields, stringDictFromMap(withMembersTypeMap), l);
        case 't-singleton-variant':
          return TS['t-singleton-variant'].app(name, stringDictFromMap(withMembersTypeMap), l);
        default: throw new ExhaustiveSwitchError(variantType);
      }
    }
    function checkSharedField(field: A.Member, fieldTypes: Map<string, TS.Type>, dataType: TS.Type, context: Context): TS.Type {
      const fieldType = fieldTypes.get(field.dict.name)!;
      return toTypeMember(field, fieldType, dataType, true, context);
    }

    function addSelfType(funType: TS.Type, selfType: TS.Type): TS.Type {
      switch(funType.$name) {
        case 't-arrow': {
          const { args, ret, l, inferred } = funType.dict;
          return TS['t-arrow'].app(runtime.ffi.makeList([selfType, ...listToArray(args)]), ret, l, inferred);
        }
        case 't-forall': {
          const { introduces, onto, l, inferred } = funType.dict;
          switch(onto.$name) {
            case 't-arrow': {
              const { args, ret, l: lOnto, inferred: infOnto } = onto.dict;
              return TS['t-forall'].app(
                introduces, 
                TS['t-arrow'].app(runtime.ffi.makeList([selfType, ...listToArray(args)]), ret, lOnto, infOnto),
                l,
                inferred
              );
            }
            default:
              throw new InternalCompilerError(`method type is not a function (this shouldn't happen): ${typeKey(funType)}`);
          }
        }
        default:
          throw new InternalCompilerError(`method type is not a function (this shouldn't happen): ${typeKey(funType)}`);
      }
    }
    function removeSelfType(funType: TS.Type): TS.Type {
      switch(funType.$name) {
        case 't-arrow': {
          const { args, ret, l, inferred } = funType.dict;
          if (args.$name === 'empty') {
            throw new InternalCompilerError(`function type has no arguments (this shouldn't happen): ${typeKey(funType)}`);
          }
          return TS['t-arrow'].app(args.dict.rest, ret, l, inferred);
        }
        case 't-forall': {
          const { introduces, onto, l, inferred } = funType.dict;
          switch(onto.$name) {
            case 't-arrow': {
              const { args, ret, l: lOnto, inferred: infOnto } = onto.dict;
              if (args.$name === 'empty') {
                throw new InternalCompilerError(`function type has no arguments (this shouldn't happen): ${typeKey(funType)}`);
              }
              return TS['t-forall'].app(introduces, TS['t-arrow'].app(args.dict.rest, ret, lOnto, infOnto), l, inferred);
            }
            default:
              throw new InternalCompilerError(`method type is not a function (this shouldn't happen): ${typeKey(funType)}`);
          }
        }
        default:
          throw new InternalCompilerError(`method type is not a function (this shouldn't happen): ${typeKey(funType)}`);
      }
    }
    function toTypeMember(member: A.Member, typ: TS.Type, selfType: TS.Type, typeCheckFunctions: boolean, context: Context): TS.Type {
      switch(member.$name) {
        case 's-data-field': {
          const { value } = member.dict;
          switch(value.$name) {
            case 's-lam': {
              if (typeCheckFunctions) {
                checking(value, typ, false, context);
              }
              return typ;
            }
            default: return typ;
          }
        }
        case 's-method-field': {
          // TODO(alex): TC limitations means cannot implement _equality() as a with-member
          //   See tests-new/simple-output/custom-equal-always.arr for details
          const { l, name, params, args, ann, doc, body, "_check-loc": checkLoc, _check, blocky } = member.dict;
          const newType = addSelfType(typ, selfType);
          const methodAsLam = A['s-lam'].app(l, name, params, args, ann, doc, body, checkLoc, _check, blocky);
          const checkedType = checkFun(l, body, params, args, ann, newType, methodAsLam, context);
          return removeSelfType(checkedType);
        }
        case 's-mutable-field': throw new InternalCompilerError(`toTypeMember: mutable fields not handled yet (field name is ${member.dict.name})`);
        default: throw new ExhaustiveSwitchError(member);
      }
    }

    function makeConstructorType(variantType: TS.TypeVariant, branderType: TS.Type, params: TJ.Variant<TS.Type, 't-var'>[]): TS.Type {
      let innerType = branderType;
      if (params.length > 0) {
        innerType = TS['t-app'].app(branderType, runtime.ffi.makeList(params), variantType.dict.l, false);
      }
      const refinedType = setTypeLoc(TS['t-data-refinement'].app(innerType, variantType.dict.name, variantType.dict.l, false), variantType.dict.l);
      switch(variantType.$name) {
        case 't-variant': {
          const fieldTypes = listToArray(variantType.dict.fields).map((field) => {
            const [_fieldName, typ] = field.vals;
            return (typ.$name === 't-ref' ? typ.dict.typ : typ);
          });
          let ret: TS.Type = TS['t-arrow'].app(runtime.ffi.makeList(fieldTypes), refinedType, variantType.dict.l, false);
          if (params.length > 0) {
            ret = TS['t-forall'].app(runtime.ffi.makeList(params), ret, variantType.dict.l, false);
          }
          return ret;
        }
        case 't-singleton-variant': {
          if (params.length === 0) {
            return refinedType;
          } else {
            return TS['t-forall'].app(runtime.ffi.makeList(params), refinedType, variantType.dict.l, false);
          }
        }
        default: throw new ExhaustiveSwitchError(variantType);
      }
    }

    function collectVariantConstructor(variant: A.Variant, context: Context): TS.TypeVariant {
      switch(variant.$name) {
        case 's-variant': {
          const typeMembers: PTuple<[string, TS.Type]>[] = [];
          const members = listToArray(variant.dict.members);
          for (const member of members) {
            const bind = member.dict.bind as TJ.Variant<A.Bind, 's-bind'>;
            const maybeType = toType(bind.dict.ann, context);
            if (maybeType) {
              let typ: TS.Type;
              switch(member.dict['member-type'].$name) {
              case 's-normal': typ = setTypeLoc(maybeType, member.dict.l); break;
              case 's-mutable': typ =TS['t-ref'].app(setTypeLoc(maybeType, member.dict.l), member.dict.l, false); break;
              default: throw new ExhaustiveSwitchError(member.dict['member-type']);
              }
              typeMembers.push(runtime.makeTuple([nameToName(bind.dict.id), typ]));
            } else {
              throw new TypeCheckFailure(CS['cant-typecheck'].app("No type annotation provided on member", member.dict.l));
            }
          }
          return TS['t-variant'].app(variant.dict.name, runtime.ffi.makeList(typeMembers), stringDictFromMap(new Map<string, TS.Type>()), variant.dict.l);
        }
        case 's-singleton-variant': {
          return TS['t-singleton-variant'].app(variant.dict.name, stringDictFromMap(new Map<string, TS.Type>()), variant.dict.l);
        }
        default: throw new ExhaustiveSwitchError(variant);
      }
    }

    function collectVariant(variant: A.Variant, context: Context): TS.TypeVariant {
      switch(variant.$name) {
        case 's-variant': {
          const typeMembers: PTuple<[string, TS.Type]>[] = [];
          const members = listToArray(variant.dict.members);
          for (const member of members) {
            const bind = member.dict.bind as TJ.Variant<A.Bind, 's-bind'>;
            const maybeType = toType(bind.dict.ann, context);
            if (maybeType) {
              let typ: TS.Type;
              switch(member.dict['member-type'].$name) {
                case 's-normal': typ = setTypeLoc(maybeType, member.dict.l); break;
                case 's-mutable': typ =TS['t-ref'].app(setTypeLoc(maybeType, member.dict.l), member.dict.l, false); break;
                default: throw new ExhaustiveSwitchError(member.dict['member-type']);
              }
              typeMembers.push(runtime.makeTuple([nameToName(bind.dict.id), typ]));
            } else {
              throw new TypeCheckFailure(CS['cant-typecheck'].app("No type annotation provided on member", member.dict.l));
            }
          }
          const typeWithMembers = collectMembers(listToArray(variant.dict['with-members']), true, context);
          return TS['t-variant'].app(variant.dict.name, runtime.ffi.makeList(typeMembers), stringDictFromMap(typeWithMembers), variant.dict.l);
        }
        case 's-singleton-variant': {
          const typeWithMembers = collectMembers(listToArray(variant.dict['with-members']), true, context);
          return TS['t-singleton-variant'].app(variant.dict.name, stringDictFromMap(typeWithMembers), variant.dict.l);
        }
        default: throw new ExhaustiveSwitchError(variant);
      }
    }

    function collectMembers(members: A.Member[], collectFunctions: boolean, context: Context): Map<string, TS.Type> {
      const ret = new Map<string, TS.Type>();
      for (let member of members) {
        const memberType = collectMember(member, collectFunctions, context);
        ret.set(member.dict.name, memberType);
      }
      return ret;
    }
    function collectMember(member: A.Member, collectFunctions: boolean, context: Context): TS.Type {
      switch(member.$name) {
        case 's-data-field': {
          const { value } = member.dict;
          switch(value.$name) {
            case 's-lam': {
              const { l, params, args, ann } = value.dict;
              const argsArray = listToArray(args as List<TJ.Variant<A.Bind, 's-bind'>>);
              if (collectFunctions) {
                const bindings = collectBindings(argsArray, context);
                return lamToType(bindings, l, params, argsArray, ann, false, context).arrow;
              } else {
                return synthesis(value, true, context);
              }
            }
            default: {
              return synthesis(value, true, context);
            }
          }
        }
        case 's-method-field': {
          const { l, name, params, args, ann, doc, body, "_check-loc": checkLoc, _check, blocky } = member.dict;
          if (args.$name === 'empty') {
            const memberAsFunction = A['s-fun'].app(l, name, params, args, ann, doc, body, checkLoc, _check, blocky);
            // NOTE: This was a type-error in the original type-checker, and method-missing-self would give a contract error
            throw new TypeCheckFailure(CS['method-missing-self'].app(memberAsFunction));
          } else {
            const argsArray = listToArray(args.dict.rest as List<TJ.Variant<A.Bind, 's-bind'>>);
            const bindings = collectBindings(argsArray, context);
            return lamToType(bindings, l, params, argsArray, ann, !collectFunctions, context).arrow;
          }
        }
        case 's-mutable-field': throw new InternalCompilerError("Type checker does not handle mutable fields yet");
        default: throw new ExhaustiveSwitchError(member);
      }
    }

    type CollectedLetrecBindings = {
      dataBindings: {
        dataBinding: A.LetrecBind, 
        variants: A.LetrecBind[],
      }[],
      bindings: {
        bindingsToType: A.LetrecBind[],
        collectedTypes: Map<string, TS.Type>
      },
    }

    function collectLetrecBindings(binds: A.LetrecBind[], topLevel: boolean, context: Context): CollectedLetrecBindings {
      const ret: CollectedLetrecBindings = {
        dataBindings: [],
        bindings: {
          bindingsToType: [], 
          collectedTypes: new Map<string, TS.Type>()
        },
      };
      for (let i = 0; i < binds.length; i++) {
        const firstBind = binds[i];
        const firstValue = firstBind.dict.value;
        switch(firstValue.$name) {
          case 's-data-expr': {
            const variants = listToArray(firstValue.dict.variants);
            const numDataBinds = (2 * variants.length) + 1 // foo(), is-foo(), and Foo
            LOG(`i = ${i}, numDataBinds = ${numDataBinds} in\n`);
            for (let cur = 0; cur < binds.length; cur++) {
              LOG(`  [${cur} => ${formatSrcloc(binds[cur].dict.l, true)}, ${nameToKey((binds[cur].dict.b as TJ.Variant<A.Bind, "s-bind">).dict.id)}]\n`);
            }
            const dataBinds = binds.slice(i + 1, i + 1 + numDataBinds);
            i += numDataBinds; // skip over all the processed data-bindings
            ret.dataBindings.push({
              dataBinding: firstBind,
              variants: dataBinds,
            });
            break;
          }
          default: {
            if (firstBind.dict.b.$name !== "s-bind") { throw new InternalCompilerError(`${firstBind.dict.b.$name} should have been desugared`); }
            const key = nameToKey(firstBind.dict.b.dict.id);
            const collected = collectBindings([firstBind.dict.b], context);
            context.addDictToBindings(collected);
            const initialType = collected.get(key)!;
            if (initialType.$name === 't-existential') {
              if (firstValue.$name === 's-lam') {
                const args = listToArray(firstValue.dict.args) as TJ.Variant<A.Bind, "s-bind">[];
                const argColl = collectBindings(args, context);
                const { _check, l: lamL, params, ann, } = firstValue.dict;
                switch(_check.$name) {
                  case 'some': {
                    const checkBlock = _check.dict.value;
                    const { arrow: lamType } = lamToType(argColl, lamL, params, args, ann, false, context);
                    LOG(prettyIsh({
                      functionName: nameToName(firstBind.dict.b.dict.id),
                      annotatedType: typeKey(lamType),
                      checkBlock: String(checkBlock),
                    }));
                    switch(lamType.$name) {
                      case 't-arrow': {
                        const freeVars = freeVariables(lamType);
                        if (freeVars.size() > 0) {
                          const newExists = newExistential(lamType.dict.l, true);
                          context.addVariable(newExists);
                          context.addExampleVariable(
                            newExists,
                            listToArray(lamType.dict.args),
                            lamType.dict.ret,
                            lamType.dict.l, 
                            (typ, context) => {
                              checking(firstValue, typ, topLevel, context);
                              return typ;
                            },
                            nameToName(firstBind.dict.b.dict.id));
                          collected.set(key, newExists);
                        } else {
                          context.addMiscExampleVariable(key, nameToName(firstBind.dict.b.dict.id));
                          collected.set(key, lamType);
                        }
                        break;
                      }
                      default: {
                        context.addMiscExampleVariable(key, nameToName(firstBind.dict.b.dict.id));
                        const { arrow: lamType } = lamToType(argColl, lamL, params, args, ann, topLevel, context);
                        collected.set(key, lamType);
                      }
                    }
                    break;
                  }
                  case 'none': {
                    const { arrow: lamType } = lamToType(argColl, lamL, params, args, ann, topLevel, context);
                    collected.set(key, lamType);
                    break;
                  }
                  default: throw new ExhaustiveSwitchError(_check);
                }
              }
            }
            ret.bindings.bindingsToType.push(firstBind);
            ret.bindings.collectedTypes.set(key, collected.get(key)!);
          }
        }
      }
      return ret;
    }

    // TODO(MATT): this should not generalize the arguments
    function checkFun(funLoc : SL.Srcloc, body : A.Expr, params : List<A.Name>, args : List<A.Bind>, retAnn : A.Ann, expectTyp : TS.Type, original : A.Expr, context : Context) : TS.Type {
      context.addLevel(`checkFun at ${formatSrcloc(funLoc, false)} against expectTyp ${typeKey(expectTyp)}`);
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
            const typ = lamBindings.get(key)!;
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
            const typ = lamBindings.get(nameToKey(argsArray[i].dict.id))!;
            context.addConstraint(typ, expectArgs[i]);
          }

          LOG(`Checking body at ${formatSrcloc(body.dict.l, true)}\n`);
          checking(body, expectTyp.dict.ret, false, context);
          LOG(`DONE checking body at ${formatSrcloc(body.dict.l, true)}\n`);

          // NOTE(Ben): the original code never removed the arguments from the context
          for (let key of lamBindings.keys()) {
            context.removeBinding(key);
          }

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
        default:
          throw new InternalCompilerError(`Unexpected expectTyp.$name ${expectTyp.$name} in checkFun`);
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
        }
        context.addVariable(newTyp);
        bindings.set(nameToKey(b.dict.id), newTyp);
      }
      return bindings;
    }

    function synthesisAppFun(appLoc : SL.Srcloc, fun : A.Expr, args : A.Expr[], context: Context) : TS.Type{
      const newType = synthesis(fun, false, context);
      return newType;
    }

    function synthesisCheckTest(e: TJ.Variant<A.Expr,"s-check-test">, context: Context): TS.Type {
      function createResult() {
        const resultType = newExistential(e.dict.l, false);
        context.addVariable(resultType);
        return resultType;
      }
      function synthesisEquivalent() {
        switch(e.dict.right.$name) {
          case 'some': {
            const leftTyp = synthesis(e.dict.left, false, context);
            const rightTyp = synthesis(e.dict.right.dict.value, false, context);
            context.addConstraint(leftTyp, rightTyp);
            return createResult();
          }
          case 'none': {
            throw new InternalCompilerError("Expected test to have a right-hand side");
          }
        }
      }
      function synthesisRefinement() {
        switch(e.dict.refinement.$name) {
          case 'none': return synthesisEquivalent();
          case 'some': {
            switch(e.dict.right.$name) {
              case 'none': throw new InternalCompilerError("is/is-not tests must have a right-hand side");
              case 'some': {
                const leftTyp = synthesis(e.dict.left, false, context);
                const rightTyp = synthesis(e.dict.right.dict.value, false, context);
                const refinementTyp = synthesis(e.dict.refinement.dict.value, false, context);
                const arrowArgs = runtime.ffi.makeList([leftTyp, rightTyp]);
                context.addConstraint(refinementTyp, TS['t-arrow'].app(arrowArgs, tBoolean(e.dict.l), e.dict.l, false));
                return createResult();
              }
            }
          }
        }
      }
      function synthesisPredicate() : TS.Type {
        switch(e.dict.right.$name) {
          case 'none': throw new InternalCompilerError("satisfies/violates tests must have a right-hand side");
          case 'some': {
            const leftTyp = synthesis(e.dict.left, false, context);
            const predTyp = synthesis(e.dict.right.dict.value, false, context);
            const arrowArgs = runtime.ffi.makeList([leftTyp]);
            context.addConstraint(predTyp, TS['t-arrow'].app(arrowArgs, tBoolean(e.dict.l), e.dict.l, false));
            return createResult();
          }
        }
      }
      function synthesisString() : TS.Type {
        synthesis(e.dict.left, false, context);
        switch(e.dict.right.$name) {
          case 'none': throw new InternalCompilerError("raises(-not) tests must have a right-hand side");
          case 'some': {
            checking(e.dict.right.dict.value, tString(e.dict.right.dict.value.dict.l), false, context);
            return createResult();
          }
        }
      }
      function synthesisException() : TS.Type {
        switch(e.dict.right.$name) {
          case 'none': throw new InternalCompilerError("raises-(satisfies/violates) tests must have a right-hand side");
          case 'some': {
            const _ = synthesis(e.dict.left, false, context);
            const predicateType = synthesis(e.dict.right.dict.value, false, context);
            const arrowArgs = runtime.ffi.makeList([TS['t-top'].app(e.dict.l, false)]);
            context.addConstraint(predicateType, TS['t-arrow'].app(arrowArgs, tBoolean(e.dict.l), e.dict.l, false));
            return createResult();
          }
        }
      }

      switch(e.dict.op.$name) {
        case 's-op-is': return synthesisRefinement();
        case 's-op-is-roughly': return synthesisEquivalent();
        case 's-op-is-not-roughly': return synthesisRefinement();
        case 's-op-is-op': return synthesisEquivalent();
        case 's-op-is-not': return synthesisRefinement();
        case 's-op-is-not-op': return synthesisEquivalent();
        case 's-op-satisfies': return synthesisPredicate();
        case 's-op-satisfies-not': return synthesisPredicate();
        case 's-op-raises': return synthesisString();
        case 's-op-raises-not': {
          synthesis(e.dict.left, false, context);
          return createResult();
        }
        case 's-op-raises-other': return synthesisString();
        case 's-op-raises-satisfies': return synthesisException();
        case 's-op-raises-violates': return synthesisException();
        default: {
          throw new ExhaustiveSwitchError(e.dict.op);
        }
      }
    }

    function synthesisSpine(funType : TS.Type, original: TJ.Variant<A.Expr, 's-app'>, args : A.Expr[], appLoc : SL.Srcloc, context : Context) : TS.Type {
      context.addLevel(`synthesisSpine(${original.$name}) at ${formatSrcloc(original.dict.l, false)}`);
      function wrapReturn(t : TS.Type) {
        return setTypeLoc(context.solveAndResolveType(t), appLoc);
      }
      LOG(`funType before instantiation: ${typeKey(funType)}\n`);
      funType = instantiateForallWithFreshVars(funType, context.constraints, true);
      LOG(`funType after instantiation: ${typeKey(funType)}\n`);
      switch(funType.$name) {
        case "t-arrow": {
          const argTypes = listToArray(funType.dict.args);
          if(args.length !== argTypes.length) {
            throw new TypeCheckFailure(CS['incorrect-number-of-args'].app(original, funType));
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
      switch(e.$name) {
        // Handle these constant cases without needing to create a level
        case 's-srcloc': return tSrcloc(e.dict.l);
        case 's-num':
        case 's-frac':
        case 's-rfrac': return tNumber(e.dict.l);
        case 's-bool': return tBoolean(e.dict.l);
        case 's-str': return tString(e.dict.l);
      }
      context.addLevel(`synthesis(${e.$name}) at ${formatSrcloc(e.dict.l, false)}`);
      return context.solveAndResolveType(_synthesis(e, topLevel, context));
    }

    function _synthesis(e : A.Expr, topLevel : boolean, context : Context) : TS.Type {
      switch(e.$name) {
        // If we somehow wind up here, return the correct types anyway,
        // but these cases should have been handled in `synthesis` itself
        case 's-srcloc': return tSrcloc(e.dict.l);
        case 's-num':
        case 's-frac':
        case 's-rfrac': return tNumber(e.dict.l);
        case 's-bool': return tBoolean(e.dict.l);
        case 's-str': return tString(e.dict.l);

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
        case 's-user-block': return synthesis(e.dict.body, topLevel, context);
        case 's-let-expr': {
          // TODO(Ben): ignoreChecker?
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
        case 's-id':
        case 's-id-letrec': {
          const idTyp = lookupId(e.dict.l, nameToKey(e.dict.id), e, context);
          return idTyp;
        }
        case 's-id-var': {
          const idTyp = lookupId(e.dict.l, nameToKey(e.dict.id), e, context);
          if (idTyp.$name === 't-ref') {
            return setTypeLoc(idTyp.dict.typ, e.dict.l);
          } else {
            throw new TypeCheckFailure(CS['incorrect-type-expression'].app(
              typeKey(idTyp), e.dict.l,
              typeKey(TS['t-ref'].app(idTyp, e.dict.l, false)), e.dict.l,
              e));
          }
        }
        case 's-assign': {
          const idTyp = lookupId(e.dict.l, nameToKey(e.dict.id), e, context);
          if (idTyp.$name === 't-ref') {
            checking(e.dict.value, idTyp.dict.typ, topLevel, context);
            return idTyp.dict.typ;
          } else {
            throw new TypeCheckFailure(CS['incorrect-type-expression'].app(
              typeKey(idTyp), e.dict.l,
              typeKey(TS['t-ref'].app(idTyp, e.dict.l, false)), e.dict.l,
              e));
          }
        }
        case 's-id-var-modref':
        case 's-id-modref': {
          const modTyps = context.modules.get(e.dict.uri)!;
          const providedTypes = modTyps.dict.provides;
          const fields = mapFromStringDict(providedTypes.dict.fields);
          if (fields.has(e.dict.name)) {
            return fields.get(e.dict.name)!;
          } else {
            throw new InternalCompilerError(`should be caught in unbound-ids: no such name on module ${e.dict.uri}: ${e.dict.name}`);
          }
        }
        case 's-app':
          const args = listToArray(e.dict.args);
          const funType = synthesisAppFun(e.dict.l, e.dict._fun, args, context);
          return synthesisSpine(funType, e, args, e.dict.l, context);
        case 's-prim-app': {
          const arrowType = lookupId(e.dict.l, e.dict._fun, e, context);
          const eAsApp = A['s-app'].app(e.dict.l, A['s-id'].app(e.dict.l, A['s-name'].app(A['dummy-loc'], e.dict._fun)), e.dict.args);
          const result = synthesisSpine(arrowType, eAsApp, listToArray(e.dict.args), e.dict.l, context);
          return setTypeLoc(result, e.dict.l);
        }
        case 's-tuple': {
          const eltTyps: TS.Type[] = [];
          for(let elt of listToArray(e.dict.fields)) {
            eltTyps.push(synthesis(elt, false, context));
          }
          return TS['t-tuple'].app(runtime.ffi.makeList(eltTyps), e.dict.l, false);
        }
        case 's-tuple-get': {
          const newType = synthesis(e.dict.tup, topLevel, context); // TODO(joe): toplevel should be false?
          return synthesisTupleIndex(e.dict.l, newType.dict.l, newType, e.dict.index, context);
        }
        case 's-paren': return synthesis(e.dict.expr, topLevel, context);
        case 's-lam': {
          return synthesisFun(e.dict.l, e.dict.body, e.dict.params, e.dict.args, e.dict.ann, e, topLevel, context);
        }
        case 's-construct': {
          const constr = A['s-app'].app(
            e.dict.l,
            A['s-dot'].app(e.dict.l, e.dict.constructor, "make"),
            runtime.ffi.makeList([A['s-array'].app(e.dict.l, e.dict.values)])
          );
            return synthesis(constr, topLevel, context);
        }
        case 's-op': {
          const desugaredOp = desugarSOp(e);
          if (desugaredOp.$name === "s-op") {
            const { l, "op-l": opL, op, left, right } = desugaredOp.dict;
            return synthesisOp(topLevel, l, op, opL, left, right, context);
          } else {
            return synthesis(desugaredOp, topLevel, context);
          }
        }
        case 's-dot': {
          const newType = synthesis(e.dict.obj, topLevel, context);
          const fieldType = synthesisField(e.dict.l, newType, e.dict.field, context);
          switch(fieldType.$name) {
            case 't-ref': {
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app(
                typeKey(fieldType),
                fieldType.dict.l,
                "a non-ref type",
                e.dict.l,
                e
              ));
            }
            default: {
              return setTypeLoc(fieldType, e.dict.l);
            }

          }
        }
        case 's-get-bang': {
          const objType = synthesis(e.dict.obj, topLevel, context);
          const fieldType = synthesisField(e.dict.l, objType, e.dict.field, context);
          switch(fieldType.$name) {
            case 't-ref': {
              return setTypeLoc(fieldType.dict.typ, e.dict.l);
            }
            default: {
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app(
                typeKey(fieldType),
                fieldType.dict.l,
                "a ref type",
                e.dict.l,
                e
              ));
            }
          }
        }
        case 's-array': {
          const types : TS.Type[] = [];
          const values = listToArray(e.dict.values);
          for (let value of values) {
            types.push(synthesis(value, false, context));
          }
          const meetType = meetBranchTypes(types, e.dict.l, context);
          return tArray(setTypeLoc(meetType, e.dict.l), e.dict.l)
        }
        case 's-cases': 
          return synthesisCases(e.dict.l, e.dict.typ, e.dict.val, listToArray(e.dict.branches), false, context);
        case 's-cases-else':
          return synthesisCases(e.dict.l, e.dict.typ, e.dict.val, listToArray(e.dict.branches), e.dict._else, context);
        case 's-letrec': {
          const binds = listToArray(e.dict.binds);
          handleLetrecBindings(binds, topLevel, context);
          const ret = synthesis(e.dict.body, topLevel, context);
          for (const b of binds) {
            context.removeBinding(nameToKey((b.dict.b as TJ.Variant<A.Bind, "s-bind">).dict.id));
          }
          return ret;
        }
        case 's-if-else': {
          const branches = listToArray(e.dict.branches);
          const branchTypes: TS.Type[] = [];
          for (let b of branches) {
            checking(b.dict.test, tBoolean(b.dict.l), false, context);
            branchTypes.push(synthesis(b.dict.body, false, context));
          }
          branchTypes.push(synthesis(e.dict._else, false, context));
          return meetBranchTypes(branchTypes, e.dict.l, context);
        }
        case 's-obj': {
          const fields = listToArray(e.dict.fields);
          const fieldTypes = collectMembers(fields, false, context);
          const initialObjectType = TS['t-record'].app(stringDictFromMap(fieldTypes), e.dict.l, false);
          const newFieldTypes = new Map<string, TS.Type>();
          for (let field of fields) {
            const newFieldType = toTypeMember(field, fieldTypes.get(field.dict.name)!, initialObjectType, false, context);
            newFieldTypes.set(field.dict.name, newFieldType);
          }
          return TS['t-record'].app(stringDictFromMap(newFieldTypes), e.dict.l, false);
        }
        case 's-for': 
          return synthesis(desugarSFor(e), topLevel, context);
        case 's-extend': {
          const superType = synthesis(e.dict.supe, topLevel, context);
          return setTypeLoc(synthesisExtend(e, superType, context), e.dict.l);
        }
        case 's-spy-block': {
          const contents = listToArray(e.dict.contents);
          if (e.dict.message.$name === 'some') {
            synthesis(e.dict.message.dict.value, topLevel, context);
          }
          for (let spyField of contents) {
            synthesis(spyField.dict.value, topLevel, context);
          }
          return tNothing(e.dict.l);
        }
        case 's-check': {
          synthesis(e.dict.body, false, context);
          const resultType = newExistential(e.dict.l, false);
          context.addVariable(resultType);
          return resultType;
        }
        case 's-check-test': {
          return synthesisCheckTest(e, context);
        }
        case 's-get-bang': {
          const objType = synthesis(e.dict.obj, topLevel, context);
          const fieldType = synthesisField(e.dict.l, objType, e.dict.field, context);
          switch(fieldType.$name) {
            case 't-ref': {
              return setTypeLoc(fieldType.dict.typ, e.dict.l);
            }
            default: {
              throw new TypeCheckFailure(CS['incorrect-type-expression'].app(
                typeKey(fieldType),
                fieldType.dict.l,
                "a ref type",
                e.dict.l,
                e
              ));
            }
          }
        }
        case 's-update': {
          const objType = synthesis(e.dict.supe, topLevel, context);
          return synthesisUpdate(e.dict.l, objType, e.dict.fields, context);
        }
        case 's-instantiate':
        case 's-check-expr':
          throw new InternalCompilerError(`TODO: _synthesis switch ${e.$name}`);
        case 's-data':
        case 's-fun':
        case 's-var':
        case 's-let':
        case 's-when':
        case 's-if-pipe':
        case 's-if-pipe-else':
        case 's-if':
          throw new InternalCompilerError(`${e.$name} should already have been desugared`);
        case 's-data-expr':
          throw new InternalCompilerError("s-data-expr should have been handled by s-letrec");
        case 's-prim-val':
        case 's-bracket': // s-bracket needs to be implemented by looking up/checking .get-value
        case 's-type':
        case 's-newtype':
        case 's-rec':
        case 's-contract':
          throw new InternalCompilerError(`_synthesis for ${e.$name} not implemented`);
        case 's-app-enriched':
        case 's-reactor':
        case 's-table':
        case 's-table-extend':
        case 's-table-extract':
        case 's-table-filter':
        case 's-table-filter':
        case 's-table-order':
        case 's-table-select':
        case 's-table-update':
        case 's-load-table':
          throw new InternalCompilerError(`_synthesis switch ${e.$name} not even mentioned`);
        default:
          throw new ExhaustiveSwitchError(e);
      }
    }

    function synthesisUpdate(updateLoc : SL.Srcloc, objType : TS.Type, fields : List<A.Member>, context : Context) : TS.Type {
      objType = instantiateObjectType(objType, context);
      switch(objType.$name) {
        case 't-record': {
          throw new TypeCheckFailure(CS['incorrect-type'].app(typeKey(objType), objType.dict.l, "a datatype with at least one ref field", updateLoc));
        }
        case 't-existential': {
          throw new TypeCheckFailure(CS['unable-to-infer'].app(objType.dict.l));
        }
        default: {
          const dataType = instantiateDataType(objType, context);
          const fieldsArray = listToArray(fields);
          fieldsArray.forEach(field => {
            const fieldTyp = callMethod(dataType.dict.fields, "get", field.dict.name);
            switch(fieldTyp.$name) {
              case 'none': {
                throw new TypeCheckFailure(CS['object-missing-field'].app(
                  field.dict.name, typeKey(objType), objType.dict.l, updateLoc
                ));
              }
              case 'some': {
                const oldType = fieldTyp.dict.value;
                switch(oldType.$name) {
                  case 't-ref': {
                    if(field.$name === 's-method-field') {
                      throw new InternalCompilerError('s-method-field in mutable update expression')
                    }
                    checking(field.dict.value, oldType.dict.typ, false, context);
                    break;
                  }
                  default: {
                    // NOTE(joe/ben): This error message should be a custom constructor for
                    // "expected a ref field but this field wasn't"
                    throw new TypeCheckFailure(CS['incorrect-type'].app(
                      typeKey(oldType), oldType.dict.l, typeKey(TS['t-ref'].app(oldType, updateLoc, false)), updateLoc
                    ));
                  }
                }
              }
            }
          });
          return objType;
        }
      }
    }

    function synthesisExtend(extend: TJ.Variant<A.Expr, 's-extend'>, superType: TS.Type, context: Context): TS.Type {
      const { l, supe: obj } = extend.dict;
      const fields = listToArray(extend.dict.fields);
      /** Type-check fields
       * Note(Ben): do we actually need to synthesize any types here?
       * Note(Ben): this is the only place in the typechecker (as far as I can tell)
       * that genuinely could produce multiple type errors.
       */
      function fieldLookup(objType: TS.Type, availableFields: Map<string, TS.Type>) {
        const errors: CS.CompileError[] = [];
        for (const field of fields) {
          switch(field.$name) {
            case 's-data-field': {
              if (availableFields.has(field.dict.name)) {
                try { 
                  checking(field.dict.value, availableFields.get(field.dict.name)!, false, context); 
                } catch (e) {
                  if (e instanceof TypeCheckFailure) {
                    errors.push(...e.errs);
                  } else {
                    throw e;
                  }
                }
              } else {
                errors.push(CS['object-missing-field'].app(field.dict.name, typeKey(objType), objType.dict.l, field.dict.l));
              }
              break;
            }
            default: throw new InternalCompilerError(`synthesisExtend for ${field.$name} not implemented yet`);
          }
        }
        if (errors.length > 0) {
          throw new TypeCheckFailure(...errors);
        }
      }
      const newMembers = collectMembers(fields, false, context);
      const objType = instantiateObjectType(superType, context);
      switch(objType.$name) {
        case 't-record': {
          for (let [fieldName, fieldType] of mapFromStringDict(objType.dict.fields)) {
            newMembers.set(fieldName, fieldType);
          }
          return TS['t-record'].app(stringDictFromMap(newMembers), l, objType.dict.inferred);
        }
        case 't-name': {
          const concreteDataType = instantiateDataType(objType, context);
          const availableFields = mapFromStringDict(concreteDataType.dict.fields);
          fieldLookup(objType, availableFields);
          // NOTE(Ben): I think the original fieldLookup function returns 
          // the type of the last field it examines, but that seems weird and wrong.
          return objType;
        }
        case 't-data-refinement': {
          // NOTE(alex): Only allow extend on data variants iff the field exists and match
          //   the data variant's field type.
          // This allows the type of an extend expression on a data variant is that data variant.
          // Previously, this behavior was not supported and exposed runtime-implementations of
          //   data variants and removed the type.
          const concreteDataType = instantiateDataType(objType.dict['data-type'], context);
          const variants = new Map(listToArray(concreteDataType.dict.variants).map((v) => [v.dict.name, v]));
          if (!variants.has(objType.dict['variant-name'])) {
            throw new InternalCompilerError(`Invalid variant: '${objType.dict['variant-name']}'`);
          }
          const concreteVariant = variants.get(objType.dict['variant-name'])!;
          const availableFields = mapFromStringDict(concreteVariant.dict['with-fields']);
          if (concreteVariant.$name === 't-variant') {
            for (let field of listToArray(concreteVariant.dict.fields)) {
              const [fieldName, fieldType] = field.vals;
              availableFields.set(fieldName, fieldType);
            }
          }
          fieldLookup(objType, availableFields);
          return objType;
        }
        case 't-existential':
          throw new TypeCheckFailure(CS['unable-to-infer'].app(l));
        default:
          throw new TypeCheckFailure(CS['incorrect-type-expression'].app(typeKey(objType), objType.dict.l, "an object type", l, obj));
      }
    }

    function desugarSFor(e : TJ.Variant<A.Expr, 's-for'>): A.Expr {
      const binds: A.Bind[] = [];
      const args: A.Expr[] = [];
      for (let fb of listToArray(e.dict.bindings)) {
        const { bind, value } = fb.dict;
        binds.push(bind);
        args.push(value);
      }
      const { l, ann, body } = e.dict;
      const lambdaFor = A['s-lam'].app(l, "", runtime.ffi.makeList([]), runtime.ffi.makeList(binds), ann, "", body, runtime.ffi.makeNone(), runtime.ffi.makeNone(), true);
      return A['s-app'].app(l, e.dict.iterator, runtime.ffi.makeList([lambdaFor, ...args]));
    }

    const flatPrimApp = A['prim-app-info-c'].app(false);
    function desugarSOp(sOp : TJ.Variant<A.Expr, "s-op">) : A.Expr {
      const { l, op, left, right } = sOp.dict;
      switch(op) {
        case "op==": return A['s-prim-app'].app(l, "equal-always", runtime.ffi.makeList([left, right]), flatPrimApp);
        case "op<>":
          return A['s-prim-app'].app(l, "not", runtime.ffi.makeList([
            A['s-prim-app'].app(l, "equal-always", runtime.ffi.makeList([left, right]), flatPrimApp)
          ]), flatPrimApp);
        case "op^": return A['s-app'].app(l, right, runtime.ffi.makeList([left]));
        default: return sOp;
      }
    }

    const opNamesAsFunctions = {
      "op+": "_plus",
      "op-": "_minus",
      "op*": "_times",
      "op/": "_divide",
      "op<": "_lessthan",
      "op>": "_greaterthan",
      "op<=": "_lessequal",
      "op>=": "_greaterequal",
    };
    function synthesisOp(topLevel: boolean, appLoc: SL.Srcloc, op: string, opLoc: SL.Srcloc, left: A.Expr, right: A.Expr, context: Context): TS.Type {
      if (op === "opand" || op === "opor") {
        const tBool = tBoolean(opLoc);
        checking(left, tBool, topLevel, context);
        checking(right, tBool, topLevel, context);
        return tBool;
      } else {
        if (opNamesAsFunctions[op]) {
          const opName = opNamesAsFunctions[op];
          const objExists = newExistential(left.dict.l, false);
          const otherType = newExistential(right.dict.l, false);
          const retType = newExistential(appLoc, false);
          const arrowType = TS['t-arrow'].app(runtime.ffi.makeList([objExists, otherType]), retType, appLoc, false);
          context.addVariable(objExists);
          context.addVariable(otherType);
          context.addVariable(retType);
          context.addFieldConstraint(objExists, opName,
            TS['t-arrow'].app(runtime.ffi.makeList([otherType]), retType, appLoc, false));
          const argsArray = [left, right];
          return synthesisSpine(arrowType,
            A['s-app'].app(appLoc, A['s-id'].app(opLoc, A['s-global'].app(opName)), runtime.ffi.makeList(argsArray)),
            argsArray, appLoc, context
          );
        } else {
          throw new InternalCompilerError(`unknown op: '${op}'`);
        }
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

      context.addLevel(`synthesisFun at ${formatSrcloc(l, false)}`);
      const argsArray = listToArray(args) as TJ.Variant<A.Bind, "s-bind">[];
      const collected = collectBindings(argsArray, context);
      const { arrow, ret } = lamToType(collected, l, params, argsArray, ann, topLevel, context);
      context.addDictToBindings(collected);
      checking(body, ret, false, context);
      return context.solveAndResolveType(setRetType(arrow, ret));
    }

    function synthesisField(accessLoc : SL.Srcloc, objType: TS.Type, fieldName: string, context: Context): TS.Type {
      objType = instantiateObjectType(objType, context);
      switch (objType.$name) {
        case 't-record': {
          const fields = mapFromStringDict(objType.dict.fields);
          if (fields.has(fieldName)) {
            return fields.get(fieldName)!;
          } else {
            const synthesizedType = newExistential(accessLoc, false);
            context.addVariable(synthesizedType);
            context.addFieldConstraint(objType, fieldName, synthesizedType);
            return synthesizedType;
          }
        }
        case 't-existential': {
          const synthesizedType = newExistential(accessLoc, false);
          context.addVariable(synthesizedType);
          context.addFieldConstraint(objType, fieldName, synthesizedType);
          return synthesizedType;
        }
        default: {
          const dataType = instantiateDataType(objType, context);
          const fields = mapFromStringDict(dataType.dict.fields);
          if (fields.has(fieldName)) {
            return fields.get(fieldName)!;
          } else {
            throw new TypeCheckFailure(CS['object-missing-field'].app(fieldName, typeKey(objType), objType.dict.l, accessLoc));
          }
        }
      }
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
      const argTypes: TS.Type[] = [];
      for(let arg of args) {
        const argType = collected.get(nameToKey(arg.dict.id));
        if (!argType) {
          throw new InternalCompilerError(`Could not find ${nameToKey(arg.dict.id)} in ${prettyIsh(mapMapValues(collected, typeKey))}`);
        }
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
          const newType = instantiateForallWithFreshVars(tupType, context.constraints, true);
          return tupleView(accessLoc, tupTypeLoc, newType, context);
        case 't-existential':
          throw new TypeCheckFailure(CS['unable-to-infer'].app(tupType.dict.l));
        default:
          throw new TypeCheckFailure(CS['incorrect-type'].app(typeKey(tupType), tupTypeLoc, "a tuple type", accessLoc));
      }

    }

    function synthesisLetBind(binding : A.LetBind, context : Context) : TS.Type {
      const b = (binding.dict.b as TJ.Variant<A.Bind, "s-bind">);
      context.addLevel(`synthesisLetBind(${binding.$name})(${nameToKey(b.dict.id)}) at ${formatSrcloc(binding.dict.l, false)}`);
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
      switch(binding.$name) {
        case 's-let-bind':
          context.addBinding(nameToKey(b.dict.id), annTyp);
          const ret = context.solveAndResolveType(annTyp);
          context.addBinding(nameToKey(b.dict.id), ret);
          return ret;
        case 's-var-bind': {
          const refType = TS['t-ref'].app(annTyp, binding.dict.l, false);
          context.addBinding(nameToKey(b.dict.id), refType);
          const ret = context.solveAndResolveType(refType);
          context.addBinding(nameToKey(b.dict.id), ret);
          return ret;
        }
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

    function typeCheck(program: A.Program, compileEnv : CS.CompileEnvironment, postCompileEnv : CS.ComputedEnvironment, modules : MutableStringDict<CS.Loadable>, options: CompileOptions): CS.CompileResult<TCS.Typed> {
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
          const origin = globTs.get(g)!;
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
          const thismod = contextGlobMods.get(vbind.dict.origin.dict['uri-of-definition'])!;
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

      //LOG(`\n\nContext from modules:\n${contextFromModules.toString()}\n\n`);

      try {
        checking(program.dict.block, TS['t-top'].app(program.dict.l, false), true, contextFromModules);
      }
      catch(e) {
        console.error("XXX Got a type-checking error", e);
        // LOG("YYY Got a type-checking error " + require('util').inspect(e, {depth:null}) + "\n");
        // if (e instanceof TypeCheckFailure) {
        //   return CS.err.app(runtime.ffi.makeList(e.errs));
        // }
        return runtime.ffi.throwMessageException(String(e));
      }

      const info = gatherProvides(provides[0], contextFromModules);
      return CS.ok.app(typed.app(program, info));
    }

    const exports: Exports['dict']['values']['dict'] = {
      'type-check': runtime.makeFunction(typeCheck),
      'empty-context': emptyContext.toPyretContext(),
    }
    return runtime.makeModuleReturn(exports, {});
  }
})
