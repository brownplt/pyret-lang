import * as J from 'estree';
import type { List } from './ts-impl-types';
import type * as A from './ts-ast';

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

export type Visitor<T extends PyretDataValue, Ret = any> = {
  [method in T["$name"]]?: (self: Visitor<T, Ret>, val: Variant<T, method>) => Ret;
};
export interface Exports {
  ArrayExpression : (values : J.Expression[]) => J.ArrayExpression,
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
  IfStatement : (test: J.Expression, thn: J.Statement, els: J.Statement) => J.IfStatement,
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
  Var : (id : A.Name, expr : J.Expression) => J.Declaration,
  bindToName: (b: A.Bind) => A.Name,
  listToArray : <T>(list: List<T>) => T[],
  nameToKey : (name: A.Name) => string,
  nameToName : (name: A.Name) => string,
  nameToSourceString: (name: A.Name) => string,
  sameName: (n1: A.Name, n2: A.Name) => boolean,
  dummyLoc : A.Srcloc,
  compileSrcloc: (context: any, l : A.Srcloc) => J.Expression,
  formatSrcloc: (loc: A.Srcloc, showFile: boolean) => string,
  visit: <T extends PyretDataValue>(v : Visitor<T>, d : PyretDataValue) => void,
  map: <
      T extends PyretDataValue,
      Ret extends T = T,
    >(
      v : Visitor<T>,
      d : T,
    ) => Ret,
}

({
  requires: [],
  nativeRequires: [],
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
  theModule: function(runtime, _, __) {
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
    function Var(id : A.Name, expr : J.Expression) : J.Declaration {
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
    function ArrayExpression(values : Array<J.Expression>) : J.ArrayExpression {
      return {
        type: "ArrayExpression",
        elements: values
      }
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
    
    function IfStatement(test: J.Expression, thn: J.Statement, els: J.Statement): J.IfStatement {
      return {
        type: 'IfStatement',
        test,
        consequent: thn,
        alternate: els,
      };
    }
    
    
    function MakeName(start : number) {
      var count = start;
      function atom(base : string) : A.Name {
        count = count + 1;
        return { $name: "s-atom", dict: { base: base, serial: count }};
      }
      return {
        reset: () => count = start,
        sUnderscore: (l : A.Srcloc) : A.Name => ({ $name: "s-underscore", dict: { l }}),
        sName: (s : string, l : A.Srcloc) : A.Name => ({ $name: "s-name", dict: { s, l }}),
        sGlobal: (s : string) : A.Name => ({ $name: "s-global", dict: { s }}),
        sModuleGlobal: (s : string) : A.Name => ({ $name: "s-module-global", dict: { s }}),
        sTypeGlobal: (s : string) : A.Name => ({ $name: "s-type-global", dict: { s }}),
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
    
    function compileSrcloc(_context : any, l : A.Srcloc) : J.Expression {
      switch(l.$name) {
        case "builtin": return ArrayExpression([Literal(l.dict['module-name'])]);
        case "srcloc":
          return ArrayExpression([
            Literal(l.dict.source),
            Literal(l.dict['start-line']),
            Literal(l.dict['start-column']),
            Literal(l.dict['start-char']),
            Literal(l.dict['end-line']),
            Literal(l.dict['end-column']),
            Literal(l.dict['end-char']),
          ]);
      }
    }
    
    function listToArray<T>(list: List<T>): T[] {
      const ret = [];
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
    function visit<T extends PyretDataValue>(v : Visitor<T>, d : PyretDataValue) {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Visit failed: " + JSON.stringify(d)); }
      if(d.$name in v) { v[d.$name](v, d); }
      else {
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            visit(v, subd as any);
          }
        }
      }
    }

    /**
     * `map<T, R>` will traverse an entire data value of type `T`,
     * and will call any matching visitor methods within the visitor.
     * The return value of type `R` is by default the same as type `A`, but can be different if needed.
     * Note that `R <: A` in order for any uniformly-transformed values to be type-correct:
     * if the visitor doesn't modify the values directly, the result will be the same type of
     * `PyretDataValue` as was given.
     */
    function map<
      T extends PyretDataValue,
      Ret extends T = T,
    >(
      v : Visitor<T>,
      d : T,
    ) : Ret {
      if(typeof d !== "object" || !("$name" in d)) { throw new Error("Map failed: " + JSON.stringify(d)); }
      if(d.$name in v) { return v[d.$name](v, d); }
      else {
        const newObj : Ret = Object.create(Object.getPrototypeOf(d));
        for(const [k, meta] of Object.entries(d)) {
          if(k !== "dict") { newObj[k] = meta; }
        }
        newObj.dict = Object.create(Object.getPrototypeOf(d.dict));
        for(const [k, subd] of Object.entries(d.dict)) {
          if(typeof subd === 'object' && "$name" in subd) {
            const result = map<T>(v, subd as any);
            newObj.dict[k] = result;
          }
          else {
            newObj.dict[k] = subd;
          }
        }
        return newObj;
      }
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
      formatSrcloc,
      visit,
      map,
    });
  }
})