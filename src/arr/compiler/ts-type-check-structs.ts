
import type { Type, DataType, ModuleType } from './ts-type-structs';
import type * as C from './ts-compile-structs';
import type { List, PFunction, PTuple, StringDict } from './ts-impl-types';
import type * as A from './ts-ast';
import type { Variant, PyretObject } from './ts-codegen-helpers';
///////////////////////////// OLD Types ///////////////////////////
export type PathElement = 
  | { $name: "arg-path", dict: { 'arg-num': number } }
  | { $name: "ret-path", dict: {} }
  | { $name: "app-path", dict: { 'type-num': number } }
  | { $name: "record-path", dict: { 'field-name': string } }
  | { $name: "ref-path", dict: {} }
  | { $name: "tuple-path", dict: { 'tuple-index': number } }

export type TCInfo = 
  | {
    $name: "tc-info",
    dict: 
      {
        'types': StringDict<Type>,
        'aliases': StringDict<Type>,
        'data-types': StringDict<DataType>
      }
  }

export type Context = 
  | {
    $name: "typing-context",
    dict: 
      {
        'global-types': StringDict<Type>,
        'aliases': StringDict<Type>,
        'data-types': StringDict<DataType>,
        'modules': StringDict<ModuleType>,
        'module-names': StringDict<string>,
        'binds': StringDict<Type>,
        'constraints': ConstraintSystem,
        'info': TCInfo,
        'misc': StringDict<PTuple<[List<Type>, string]>>
      }
  }

export type ConstraintSolution = 
  | {
    $name: "constraint-solution",
    dict: 
      { 'variables': Set<Type>, 'substitutions': StringDict<PTuple<[Type, Type]>> }
  }

export type ConstraintSystem = 
  | {
    $name: "constraint-system",
    dict: 
      {
        'variables': Set<Type>,
        'constraints': List<PTuple<[Type, Type]>>,
        'refinement-constraints': List<PTuple<[Type, Type]>>,
        'field-constraints': StringDict<PTuple<[Type, StringDict<List<Type>>]>>,
        'example-types': StringDict<PTuple<[
              Type,
              { dict: { 'arg-types' : List<Type>, 'ret-type' : Type, loc : A.Srcloc } },
              List<Type>,
              PFunction<((t: Type, c : Context) => TypingResult)>,
              string
	]>>,
        'next-system': ConstraintSystem
      }
  }
  | { $name: "no-constraints", dict: {} }

export type TypingResult = 
  | {
    $name: "typing-result",
    dict: { 'ast': A.Expr, 'typ': Type, 'out-context': Context }
  }
  | { $name: "typing-error", dict: { 'errors': List<C.CompileError> } }

export type FoldResult<V> = 
  | { $name: "fold-result", dict: { 'v': V, 'context': Context } }
  | { $name: "fold-errors", dict: { 'errors': List<C.CompileError> } }

export type Typed = 
  | { $name: "typed", dict: { 'ast': A.Program, 'info': TCInfo } }

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
'arg-path': PFunction< (arg_num: number) => Variant<PathElement, 'arg-path'> >

'ret-path': Variant<PathElement, 'ret-path'>

'app-path': PFunction< (type_num: number) => Variant<PathElement, 'app-path'> >

'record-path': 
  PFunction< (field_name: string) => Variant<PathElement, 'record-path'> >

'ref-path': Variant<PathElement, 'ref-path'>

'tuple-path': 
  PFunction< (tuple_index: number) => Variant<PathElement, 'tuple-path'> >

'tc-info': 
  PFunction<
    (
        types: StringDict<Type>,
        aliases: StringDict<Type>,
        data_types: StringDict<DataType>
      ) => Variant<TCInfo, 'tc-info'>
  >

'typing-context': 
  PFunction<
    (
        global_types: StringDict<Type>,
        aliases: StringDict<Type>,
        data_types: StringDict<DataType>,
        modules: StringDict<ModuleType>,
        module_names: StringDict<string>,
        binds: StringDict<Type>,
        constraints: ConstraintSystem,
        info: TCInfo,
        misc: StringDict<PTuple<[List<Type>, string]>>
      ) => Variant<Context, 'typing-context'>
  >

'constraint-solution': 
  PFunction<
    (variables: Set<Type>, substitutions: StringDict<PTuple<[Type, Type]>>) => Variant<ConstraintSolution, 'constraint-solution'>
  >

'constraint-system': 
  PFunction<
    (
        variables: Set<Type>,
        constraints: List<PTuple<[Type, Type]>>,
        refinement_constraints: List<PTuple<[Type, Type]>>,
        field_constraints: StringDict<PTuple<[Type, StringDict<List<Type>>]>>,
        example_types: StringDict<PTuple<[
              Type,
              {dict: { 'arg-types' : List<Type>, 'ret-type' : Type, loc : A.Srcloc }},
              List<Type>,
              PFunction<((t: Type, c: Context) => TypingResult)>,
              string
	]>>,
        next_system: ConstraintSystem
      ) => Variant<ConstraintSystem, 'constraint-system'>
  >

'no-constraints': Variant<ConstraintSystem, 'no-constraints'>

'typing-result': 
  PFunction<
    (ast: A.Expr, typ: Type, out_context: Context) => Variant<TypingResult, 'typing-result'>
  >

'typing-error': 
  PFunction<
    (errors: List<C.CompileError>) => Variant<TypingResult, 'typing-error'>
  >

'fold-result': 
  PFunction< <V>(v: V, context: Context) => Variant<FoldResult<V>, 'fold-result'> >

'fold-errors': 
  PFunction<
    (errors: List<C.CompileError>) => Variant<FoldResult<any>, 'fold-errors'>
  >

'typed': PFunction< (ast: A.Program, info: TCInfo) => Variant<Typed, 'typed'> >

'empty-info': PFunction< () => TCInfo>

}}}}