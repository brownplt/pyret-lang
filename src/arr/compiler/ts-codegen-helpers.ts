import * as J from 'estree';
import type { List, MutableStringDict, StringDict, PMethod, PFunction } from './ts-impl-types';
import type * as A from './ts-ast';
import type * as E from 'escodegen';

export type Variant<T, V> = T & { $name: V };

export type PyretObject = {
  dict: any
};

/** The supertype of all possible Pyret data values. */
// TODO: add the remaining reflective accessors to this definition
type PyretDataValue = {
  $name: string,
  dict: {
    [property: string]: any
  }
};

type SrclocContext = {
  compileSrcloc: (l : A.Srcloc, cache?: boolean) => J.Expression,
}


type DropFirst<T extends unknown[]> = ((...p: T) => void) extends ((p1: infer P1, ...rest: infer R) => void) ? R : never

type SDExports = {
  dict: { values: { dict: {
    'make-mutable-string-dict': PFunction<<T>() => MutableStringDict<T>>
    'is-mutable-string-dict': PFunction<<T>(val: any) => val is MutableStringDict<T>>,
    'make-string-dict': PFunction<<T>() => StringDict<T>>,
    'is-string-dict': PFunction<<T>(val: any) => val is StringDict<T>>,
    'map-keys': PFunction<<T, U>(f: ((key: T) => U), isd: StringDict<T>) => List<U>>,
    'map-keys-now': PFunction<<T, U>(f: ((key: T) => U), msd: MutableStringDict<T>) => List<U>>,
    'fold-keys': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, isd: StringDict<T>) => U>,
    'fold-keys-now': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, msd: MutableStringDict<T>) => U>,
    'each-key': PFunction<<T>(f: ((key: T) => void), isd: StringDict<T>) => void>,
    'each-key-now': PFunction<<T>(f: ((key: T) => void), msd: MutableStringDict<T>) => void>,
  }}}
}

export type Visitor<T extends PyretDataValue, Ret = any, E = any> = {
  [method in T["$name"]]?: 
    ((self: Visitor<T, Ret, E>, val: Variant<T, method>) => Ret) |
    ((self: Visitor<T, Ret, E>, val: Variant<T, method>, extra: E) => Ret);
};
export interface Exports {
  ArrayExpression : (values : J.ArrayExpression['elements'], compact?: boolean) => J.ArrayExpression,
  AssignmentExpression : (lhs : J.MemberExpression | J.Identifier, rhs: J.Expression) => J.AssignmentExpression,
  BinaryExpression : (op: J.BinaryOperator, left: J.Expression, rhs: J.Expression) => J.BinaryExpression,
  BlockStatement : (stmts: J.Statement[]) => J.BlockStatement,
  BracketExpression : (object: J.Expression, property: J.Expression) => J.MemberExpression,
  BreakStatement : J.BreakStatement,
  CallExpression : (callee : J.Expression, args: J.Expression[]) => J.CallExpression,
  Case : (value: J.Expression, body: J.Statement[]) => J.SwitchCase,
  ConditionalExpression : (cond: J.Expression, thn: J.Expression, els: J.Expression) => J.ConditionalExpression,
  Default : (body: J.Statement[]) => J.SwitchCase,
  DotExpression : (object: J.Expression, property: string) => J.MemberExpression,
  ExhaustiveSwitchError : new(v: never, message?: string) => Error,
  ExpressionStatement : (e : J.Expression) => J.ExpressionStatement,
  FunctionExpression : (id: A.Name, args: A.Name[], body: J.BlockStatement) => J.FunctionExpression,
  Getter : (name: string, value: J.Expression) => J.Property,
  Identifier : (id: A.Name) => J.Identifier,
  IfStatement : (test: J.Expression, thn: J.Statement, els: J.Statement | null) => J.IfStatement,
  InternalCompilerError : new(message?: string) => Error,
  Literal : (a: any) => J.Literal | J.UnaryExpression,
  LogicalExpression : (operator : J.LogicalOperator, left: J.Expression, right: J.Expression) => J.LogicalExpression,
  MakeName : (start: number) => {
    reset: () => void,
    sUnderscore: (l : A.Srcloc) => A.Name,
    sName: (s: string, l: A.Srcloc) => A.Name,
    sGlobal: (s: string) => A.Name,
    sModuleGlobal: (s: string) => A.Name,
    sTypeGlobal: (s: string) => A.Name,
    makeAtom: (base: string) => A.Name,
    isSUnderscore: (v : A.Name) => boolean,
    isSName: (v: A.Name) => boolean,
    isSGlobal: (v: A.Name) => boolean,
    isSModuleGlobal: (v: A.Name) => boolean,
    isSAtom: (v: A.Name) => boolean,
  },
  MethodCallExpression : (obj: J.Expression, field : string, args: J.Expression[]) => J.CallExpression,
  ObjectExpression : (properties: J.Property[]) => J.ObjectExpression,
  Program : (body: J.Statement[]) => J.Program,
  Property : (name : string, value : J.Expression) => J.Property,
  ReturnStatement : (e : J.Expression) => J.ReturnStatement,
  ShouldHaveDesugared: new(loc: A.Srcloc, variant: string) => Error,
  SwitchStatement : (value: J.Expression, cases: J.SwitchCase[]) => J.SwitchStatement,
  TODOError : new(message?: string) => Error,
  UnaryExpression : (operator: J.UnaryOperator, argument : J.Expression) => J.UnaryExpression,
  Var : (id : A.Name, expr : J.Expression | undefined | null) => J.Declaration,
  bindToName: (b: A.Bind) => A.Name,
  listToArray : <T>(list: List<T>) => T[],
  nameToKey : (name: A.Name) => string,
  nameToName : (name: A.Name) => string,
  nameToSourceString: (name: A.Name) => string,
  sameName: (n1: A.Name, n2: A.Name) => boolean,
  dummyLoc : A.Srcloc,
  compileSrcloc: (context: SrclocContext, l : A.Srcloc, cache?: boolean) => J.Expression,
  beforeSrcloc: (s1 : A.Srcloc, s2 : A.Srcloc) => boolean,
  formatSrcloc: (loc: A.Srcloc, showFile: boolean) => string,
  visit: (<T extends PyretDataValue, E = any>(v : Visitor<T, any, E>, d : PyretDataValue, extra: E) => void)
       & (<T extends PyretDataValue>(v : Visitor<T, any, undefined>, d : PyretDataValue) => void),
  map: (<T extends PyretDataValue, Ret extends T = T, E = any>(v : Visitor<T, any, E>, d : T, extra: E) => Ret)
     & (<T extends PyretDataValue, Ret extends T = T>(v : Visitor<T, any, undefined>, d : T) => Ret),
  callMethod: <Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) => ReturnType<O["dict"][Name]["full_meth"]>,
  mapFromStringDict: <T>(s : StringDict<T>) => Map<string, T>,
  mapFromMutableStringDict: <T>(s : MutableStringDict<T>) => Map<string, T>,
  stringDictFromMap: <T>(m : Map<string, T>) => StringDict<T>,
  mutableStringDictFromMap: <T>(m : Map<string, T>) => MutableStringDict<T>,
}

({
  requires: [
    { 'import-type': 'builtin', name: 'string-dict' },
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
  ],
  nativeRequires: ['escodegen'],
  provides: {
    values: {
      ArrayExpression : 'tany',
      AssignmentExpression : 'tany',
      BinaryExpression : 'tany',
      BlockStatement : 'tany',
      BracketExpression : 'tany',
      BreakStatement : 'tany',
      CallExpression : 'tany',
      Case : 'tany',
      ConditionalExpression : 'tany',
      Default : 'tany',
      DotExpression : 'tany',
      ExhaustiveSwitchError: 'tany',
      ExpressionStatement : 'tany',
      FunctionExpression : 'tany',
      Getter : 'tany',
      Identifier : 'tany',
      IfStatement : 'tany',
      InternalCompilerError: 'tany',
      Literal : 'tany',
      LogicalExpression : 'tany',
      MakeName: 'tany',
      MethodCallExpression : 'tany',
      ObjectExpression : 'tany',
      Program : 'tany',
      Property : 'tany',
      ReturnStatement : 'tany',
      ShouldHaveDesugared: 'tany',
      SwitchStatement : 'tany',
      TODOError: 'tany',
      UnaryExpression : 'tany',
      Var : 'tany',
      bindToName: 'tany',
      listToArray : 'tany',
      nameToKey : 'tany',
      nameToName : 'tany',
      nameToSourceString: 'tany',
      dummyLoc : 'tany',
      compileSrcloc: 'tany',
    },
  },
  theModule: function(runtime, _, __, SD: SDExports, Ain: A.Exports, escodegen: (typeof E)) {
    const A = Ain.dict.values.dict;
    class ExhaustiveSwitchError extends Error {
      constructor(v: never, message?: string) {
        super(`Switch is not exhaustive on \`${JSON.stringify(v)}\`: ${message}`);
      }
    }
    class InternalCompilerError extends Error {
      constructor(message: string) {
        super(`Internal compile error (compiler bug/broken invariant): ${message}`);
      }
    }
    class ShouldHaveDesugared extends Error {
      constructor(loc: A.Srcloc, variant: string) {
        super(`Expression form should have been desugared (compiler bug/broken invariant): ${variant} at ${JSON.stringify(loc)}`);
      }
    }
    class TODOError extends Error {
      constructor(message: string) {
        super(`Incomplete feature in compiler: ${message}`);
      }
    }
    
    function ExpressionStatement(e : J.Expression) : J.ExpressionStatement {
      return { type: "ExpressionStatement", expression: e };
    }
    function CallExpression(callee : J.Expression, args: Array<J.Expression>) : J.CallExpression {
      return {
        type: "CallExpression",
        callee: callee,
        arguments: args,
        optional: false
      };
    }
    function MethodCallExpression(obj : J.Expression, field : string, args: Array<J.Expression>) : J.CallExpression {
      return {
        type: "CallExpression",
        callee: DotExpression(obj, field),
        arguments: args,
        optional: false
      };
    }
    function DotExpression(object : J.Expression, property : string) : J.MemberExpression {
      return {
        type: "MemberExpression",
        object: object,
        property: Literal(property),
        computed: true,
        optional: false
      }
    }
    function BracketExpression(object : J.Expression, property : J.Expression) : J.MemberExpression {
      return {
        type: "MemberExpression",
        object: object,
        property: property,
        computed: true,
        optional: false
      }
    }
    function Literal(a : any) : J.Literal | J.UnaryExpression {
      if(a === undefined) { throw new InternalCompilerError("undefined given to Literal"); }
      if(typeof a === "number" && isNaN(a)) { throw new InternalCompilerError(`NaN as numeric literal: ${a}`); }
      if (typeof a === 'number' && (a < 0 || (a === 0 && (1 / a < 0)))) {
        return UnaryExpression('-', { type: 'Literal', value: 0 - a });
      }
      return {
        type: "Literal",
        value: a
      };
    }
    function Identifier(id : A.Name) : J.Identifier {
      return {
        type: "Identifier",
        name: nameToSourceString(id)
      };
    }
    function ReturnStatement(e : J.Expression) : J.ReturnStatement {
      return {
        type: "ReturnStatement",
        argument: e
      }
    }
    function Var(id : A.Name, expr : J.Expression | null) : J.Declaration {
      return {
        type: "VariableDeclaration",
        kind: "var",
        declarations: [ { type: "VariableDeclarator", id: Identifier(id), init: expr }]
      };
    }
    function FunctionExpression(id : A.Name, args : Array<A.Name>, body : J.BlockStatement) : J.FunctionExpression {
      return {
        type: "FunctionExpression",
        id: Identifier(id),
        params: args.map(Identifier),
        body
      };
    }
    function AssignmentExpression(lhs : J.MemberExpression | J.Identifier, rhs : J.Expression) : J.AssignmentExpression {
      return {
        type: "AssignmentExpression",
        operator: "=",
        left: lhs,
        right: rhs
      }
    }
    function BinaryExpression(op : J.BinaryOperator, left : J.Expression, right : J.Expression) : J.BinaryExpression {
      return {
        type: "BinaryExpression",
        operator: op,
        left: left,
        right: right
      };
    }
    function ConditionalExpression(cond : J.Expression, thn : J.Expression, els : J.Expression) : J.ConditionalExpression {
      return {
        type: "ConditionalExpression",
        test: cond,
        consequent: thn,
        alternate: els
      };
    }
    function ArrayExpression(values : J.ArrayExpression['elements'], compact?: boolean) : J.ArrayExpression {
      let ans = {
        type: "ArrayExpression" as const,
        elements: values
      };
      if (compact) {
        ans['x-verbatim-content'] = {
          content: escodegen.generate(ans, {
            format: {
              compact: true,
              indent: { style: '' },
            },
          }),
          precedence: escodegen.Precedence.Assignment,
        };
      }
      return ans;
    }
    function ObjectExpression(properties : Array<J.Property>) : J.ObjectExpression {
      return {
        type: "ObjectExpression",
        properties: properties
      };
    }
    function Getter(name : string, value : J.Expression) : J.Property {
      return {
        type: "Property",
        kind: "get",
        key: Literal(name),
        value: value,
        method: false,
        shorthand: false,
        computed: true
      };
    }
    function Property(name : string, value : J.Expression) : J.Property {
      return {
        type: "Property",
        kind: "init",
        key: Literal(name),
        value: value,
        method: false,
        shorthand: false,
        computed: false
      };
    }
    function Program(body : Array<J.Statement>) : J.Program {
      return {
        sourceType: "module",
        type: "Program",
        body: body
      };
    }
    
    function UnaryExpression(operator: J.UnaryOperator, argument : J.Expression) : J.UnaryExpression {
      return {
        type: "UnaryExpression",
        operator,
        argument,
        prefix: true,
      };
    }
    
    function LogicalExpression(operator : J.LogicalOperator, left: J.Expression, right: J.Expression) : J.LogicalExpression {
      return {
        type: "LogicalExpression",
        left,
        right,
        operator,
      };
    }
    
    function SwitchStatement(value : J.Expression, cases : Array<J.SwitchCase>) : J.SwitchStatement {
      return {
        type: "SwitchStatement",
        cases: cases,
        discriminant: value
      };
    }
    
    function Case(value : J.Expression, body : Array<J.Statement>) : J.SwitchCase {
      return {
        type: "SwitchCase",
        test: value,
        consequent: body
      }
    }
    
    function Default(body : Array<J.Statement>) : J.SwitchCase {
      return {
        type: "SwitchCase",
        test: null,
        consequent: body
      }
    }
    
    const BreakStatement : J.BreakStatement = { type: 'BreakStatement' };
    function BlockStatement(stmts : J.Statement[]) : J.BlockStatement {
      return {
        type: 'BlockStatement',
        body: stmts,
      }
    }
    
    function IfStatement(test: J.Expression, thn: J.Statement, els: J.Statement | null): J.IfStatement {
      return {
        type: 'IfStatement',
        test,
        consequent: thn,
        alternate: els,
      };
    }
    
    // deliberately making pyret values here instead of synthesizing object 
    function MakeName(start : number) {
      var count = start;
      function atom(base : string) : A.Name {
        count = count + 1;
        return A['s-atom'].app(base, count);
      }
      return {
        reset: () => count = start,
        sUnderscore: (l : A.Srcloc) : A.Name => (A['s-underscore'].app(l)),
        sName: (s : string, l : A.Srcloc) : A.Name => (A['s-name'].app(l, s)),
        sGlobal: (s : string) : A.Name => (A['s-global'].app(s)),
        sModuleGlobal: (s : string) : A.Name => (A['s-module-global'].app(s)),
        sTypeGlobal: (s : string) : A.Name => (A['s-type-global'].app(s)),
        makeAtom : atom,
        isSUnderscore: v => v.$name === "s-underscore",
        isSName: v => v.$name === "s-name",
        isSGlobal: v => v.$name === "s-global",
        isSModuleGlobal: v => v.$name === "s-module-global",
        isSAtom: v => v.$name === "s-atom",
      };
    }
    
    // NOTE(joe): Function version of to-sourcestring() method on Name in ast.arr
    function nameToSourceString(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "_";
        case "s-name": return name.dict.s;
        case "s-global": return name.dict.s;
        case "s-module-global": return "$module$" + name.dict.s;
        case "s-type-global": return "$type$" + name.dict.s;
        case "s-atom": return name.dict.base + String(name.dict.serial);
      }
    }
    
    function bindToName(b : A.Bind): A.Name {
      switch(b.$name) {
        case 's-bind': return b.dict.id;
        case 's-tuple-bind': throw new ShouldHaveDesugared(b.dict.l, "Got an s-tuple-bind, which should have been desugared");
      }
    }
    
    // NOTE(joe): Function version of toname() method on Name in ast.arr
    function nameToName(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "_";
        case "s-atom": return name.dict.base;
        default: return name.dict.s;
      }
    }
    
    function nameToKey(name : A.Name) : string {
      switch(name.$name) {
        case "s-underscore": return "underscore#";
        case "s-name": return "name#" + name.dict.s;
        case "s-global": return "global#" + name.dict.s;
        case "s-module-global": return "mglobal#" + name.dict.s;
        case "s-type-global": return "tglobal#" + name.dict.s;
        case "s-atom": return "atom#" + name.dict.base + "#" + String(name.dict.serial);
      }
    }

    function sameName(n1: A.Name, n2: A.Name): boolean {
      switch(n1.$name) {
        case 's-atom': 
          return n2.$name === n1.$name && 
                n2.dict.base === n1.dict.base &&
                n2.dict.serial === n1.dict.serial;
        case 's-underscore': return n2.$name === n1.$name;
        case 's-global':  
          return n2.$name === n1.$name && 
                 n2.dict.s === n1.dict.s;
        case 's-module-global':
          return n2.$name === n1.$name && 
                n2.dict.s === n1.dict.s;
        case 's-type-global':
          return n2.$name === n1.$name && 
                 n2.dict.s === n1.dict.s;
        case 's-name':
          return n2.$name === n1.$name && 
                 n2.dict.s === n1.dict.s;
        default:
          throw new ExhaustiveSwitchError(n1);
      }
    }
    
    const dummyLoc : A.Srcloc = {
      $name: "builtin",
      dict: { 'module-name': "dummy location" }
    };
    
    function compileSrcloc(context : SrclocContext, l : A.Srcloc, cache?: boolean) : J.Expression {
      return context.compileSrcloc(l, cache)
    }

    function beforeSrcloc(s1 : A.Srcloc, s2 : A.Srcloc) : boolean {
      switch(s1.$name) {
        case "builtin": {
          switch(s2.$name) {
            case "builtin": return s1.dict['module-name'] < s2.dict['module-name'];
            case "srcloc": return false;
          }
        }
        case "srcloc": {
          switch(s2.$name) {
            case "builtin": return true;
            case "srcloc": { return s1.dict['start-char'] < s2.dict['start-char']; }
          }
        }
      }
    }
    
    function listToArray<T>(list: List<T>): T[] {
      const ret: T[] = [];
      let cur = list;
      while (cur.$name === 'link') {
        ret.push(cur.dict.first);
        cur = cur.dict.rest;
      }
      return ret;
    }

    // mimics the Srcloc//format method from ast.arr
    function formatSrcloc(loc: A.Srcloc, showFile: boolean): string {
      switch(loc.$name) {
        case 'builtin': return `<builtin ${loc.dict['module-name']}>`;
        case 'srcloc': 
          if (showFile) {
            const start = `${loc.dict.source}:${loc.dict['start-line']}:${loc.dict['start-column']}`;
            const end = `${loc.dict['end-line']}:${loc.dict['end-column']}`;
            return `${start}-${end}`;
          } else {
            return `line ${loc.dict['start-line']}, column ${loc.dict['start-column']}`;
          }
      }
    }
    
    /**
     * `visit<T>` will traverse an entire Pyret data value, looking for
     * any nested values whose `$name` fields indicate they overlap with type `T`,
     * and will call any matching visitor methods within the visitor object.
     */
    function visit<T extends PyretDataValue, E = any>(v : Visitor<T, any, E>, d : PyretDataValue, extra: E) {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Visit failed: " + JSON.stringify(d)); }
      if(d.$name in v) { v[d.$name](v, d, extra); }
      else {
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            visit(v, subd as any, extra);
          }
        }
      }
    }

    /**
     * `map<T, R, E>` will traverse an entire data value of type `T`,
     * and will call any matching visitor methods within the visitor.
     * The return value of type `R` is by default the same as type `A`, but can be different if needed.
     * Note that `R <: A` in order for any uniformly-transformed values to be type-correct:
     * if the visitor doesn't modify the values directly, the result will be the same type of
     * `PyretDataValue` as was given.
     * If present, `E` describes any extra data that should be recursively passed down
     * as context for each nested call.
     * 
     * @param `T` describes the input data being traversed, with enough detail to describe
     *   all the constructors being examined (via visitor methods) and all the data
     *   types to which the visitor is recursively applied (even if they are not examined explicitly).
     * @param `R` describes the intended output type
     * @param `E` optionally describes any context information being passed down through recursive calls.
     * @param `v` is the visitor itself
     * @param `d` is the initial data value being examined
     * @param `extra` optionally provides context information.
     */
    function map<
      T extends PyretDataValue,
      Ret extends T = T,
      E = any,
    >(
      v : Visitor<T, Ret, E>,
      d : T,
      extra?: E,
    ) : Ret {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Map failed: " + JSON.stringify(d)); }
      if(d.$name in v) { return v[d.$name](v, d, extra); }
      else {
        const newObj : Ret = Object.create(Object.getPrototypeOf(d));
        for(const [k, meta] of Object.entries(d)) {
          if(k !== "dict") { newObj[k] = meta; }
        }
        newObj.dict = Object.create(Object.getPrototypeOf(d.dict));
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            const result = map<T, Ret, E>(v, subd, extra);
            newObj.dict[k] = result;
          }
          else {
            newObj.dict[k] = subd;
          }
        }
        return newObj;
      }
    }

    function callMethod<Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) : ReturnType<O["dict"][Name]["full_meth"]> {
      return obj.dict[name].full_meth(obj, ...args);
    }
    function mapFromStringDict<T>(s : StringDict<T>) : Map<string, T> {
      const m : Map<string, T> = new Map();
      for (let valKey of listToArray(callMethod(s, 'keys-list'))) {
        m.set(valKey, callMethod(s, "get-value", valKey));
      }
      return m;
    }
    function mapFromMutableStringDict<T>(s : MutableStringDict<T>) : Map<string, T> {
      const m : Map<string, T> = new Map();
      for (let valKey of listToArray(callMethod(s, 'keys-list-now'))) {
        m.set(valKey, callMethod(s, "get-value-now", valKey));
      }
      return m;
    }
    function stringDictFromMap<T>(m : Map<string, T>): StringDict<T> {
      return callMethod(mutableStringDictFromMap(m), 'freeze');
    }
    function mutableStringDictFromMap<T>(m : Map<string, T>): MutableStringDict<T> {
      const s = SD.dict.values.dict['make-mutable-string-dict'].app<T>();
      for (const [k, v] of m.entries()) {
        callMethod(s, 'set-now', k, v);
      }
      return s;
    }

    return runtime.makeJSModuleReturn({
      ArrayExpression,
      AssignmentExpression,
      BinaryExpression,
      BlockStatement,
      BracketExpression,
      BreakStatement,
      CallExpression,
      Case,
      ConditionalExpression,
      Default,
      DotExpression,
      ExhaustiveSwitchError,
      ExpressionStatement,
      FunctionExpression,
      Getter,
      Identifier,
      IfStatement,
      InternalCompilerError,
      Literal,
      LogicalExpression,
      MakeName,
      MethodCallExpression,
      ObjectExpression,
      Program,
      Property,
      ReturnStatement,
      ShouldHaveDesugared,
      SwitchStatement,
      TODOError,
      UnaryExpression,
      Var,
      bindToName,
      listToArray,
      nameToKey,
      nameToName,
      nameToSourceString,
      sameName,
      dummyLoc,
      compileSrcloc,
      beforeSrcloc,
      formatSrcloc,
      visit,
      map,
      callMethod,
      mapFromStringDict,
      mapFromMutableStringDict,
      stringDictFromMap,
      mutableStringDictFromMap,
    });
  }
})