// Port of src/arr/compiler/ast-util.arr

import * as A from './ast';
import * as SL from './srcloc';
import * as CS from './compile-structs';
import * as T from './type-structs';
import { DefaultMapVisitor, DefaultIterVisitor, PostScopeMapVisitor, PostScopeIterVisitor } from './ast-visitors';
import { raise, mapSet, mapGetValue, map2, field, asVariant, nonNull } from './shared';

export type URI = string;
export type Loc = SL.Loc;

export type NameOrigin = T.NameOrigin;
export const local = T.local;
export const moduleUri = (uri: string): T.ModuleUri => new T.ModuleUri(uri);
export const dependency = (dep: string): T.Dependency => new T.Dependency(dep);

// Pyret object-extension (`self.{field: v}`): copy the visitor, preserving its
// prototype (so subclass overrides survive), with some own fields replaced.
function extendVisitor<V>(self: V, fields: object): V {
  const copy = Object.assign(Object.create(Object.getPrototypeOf(self)), self);
  return Object.assign(copy, fields);
}

export function okLast(stmt: A.Expr): boolean {
  return !(
    A.isSLet(stmt) ||
    A.isSVar(stmt) ||
    A.isSRec(stmt) ||
    A.isSFun(stmt) ||
    A.isSData(stmt) ||
    A.isSContract(stmt) ||
    A.isSCheck(stmt) ||
    A.isSType(stmt) ||
    A.isSNewtype(stmt)
  );
}

export function checkers(l: Loc): A.Expr {
  return new A.SApp(l, new A.SDot(l, new A.SPrimVal(l, 'builtins'), 'current-checker'), []);
}

export function appendNothingIfNecessary(prog: A.Program): A.Program {
  const body = prog.block;
  if (A.isSBlock(body)) {
    const stmts = body.stmts;
    if (stmts.length === 0) {
      return new A.SProgram(prog.l, prog._use, prog._provide, prog.providedTypes, prog.provides, prog.imports,
        new A.SBlock(body.l, [new A.SPrimVal(body.l, 'nothing')]));
    } else {
      const lastStmt = stmts[stmts.length - 1];
      if (okLast(lastStmt)) { return prog; }
      else {
        return new A.SProgram(prog.l, prog._use, prog._provide, prog.providedTypes, prog.provides, prog.imports,
          new A.SBlock(body.l, [...stmts, new A.SPrimVal(A.dummyLoc, 'nothing')]));
      }
    }
  } else {
    return prog;
  }
}

export function wrapIfNeeded(exp: A.Expr): A.Expr {
  const l = exp.l;
  if (okLast(exp) && !A.isSSpyBlock(exp)) {
    return new A.SApp(l, new A.SDot(l, new A.SPrimVal(l, 'builtins'), 'trace-value'),
      [new A.SSrcloc(l, l), exp]);
  } else { return exp; }
}

export function wrapToplevels(prog: A.Program): A.Program {
  const body = prog.block;
  const newBody = A.isSBlock(body)
    ? new A.SBlock(body.l, body.stmts.map(wrapIfNeeded))
    : wrapIfNeeded(body);
  return new A.SProgram(prog.l, prog._use, prog._provide, prog.providedTypes, prog.provides, prog.imports, newBody);
}

export function countApps(expr: { visit(visitor: any): any }): number {
  let count = 0;
  const visitor = new (class extends DefaultIterVisitor {
    sApp(node: A.SApp): boolean {
      count = count + 1;
      const fv = node._fun.visit(this);
      return fv && node.args.map((a) => a.visit(this)).every((v) => v);
    }
  })();
  expr.visit(visitor);
  return count;
}

// ---------- data BindingInfo ----------

export abstract class BindingInfoBase {
  abstract get $name(): string;
}
// Some "primitive" value supplied by the initial environment
export class BPrim extends BindingInfoBase {
  get $name(): 'b-prim' { return 'b-prim'; }
  constructor(public name: string) { super(); }
}
// Some module supplied by the initial environment
export class BDict extends BindingInfoBase {
  get $name(): 'b-dict' { return 'b-dict'; }
  constructor(public dict: Map<string, BindingInfo>) { super(); }
}
// This name is bound to some expression that we can't interpret yet
export class BExp extends BindingInfoBase {
  get $name(): 'b-exp' { return 'b-exp'; }
  constructor(public exp: A.Expr) { super(); }
}
// A field lookup off some binding that isn't a b-dict
export class BDot extends BindingInfoBase {
  get $name(): 'b-dot' { return 'b-dot'; }
  constructor(public base: BindingInfo, public name: string) { super(); }
}
// A type
export class BTyp extends BindingInfoBase {
  get $name(): 'b-typ' { return 'b-typ'; }
}
// imported from a module
export class BImport extends BindingInfoBase {
  get $name(): 'b-import' { return 'b-import'; }
  constructor(public imp: A.ImportType) { super(); }
}
// Any unknown value
export class BUnknown extends BindingInfoBase {
  get $name(): 'b-unknown' { return 'b-unknown'; }
}
export type BindingInfo = BPrim | BDict | BExp | BDot | BTyp | BImport | BUnknown;
export const bTyp: BTyp = new BTyp();
export const bUnknown: BUnknown = new BUnknown();
export function isBPrim(x: any): x is BPrim { return x instanceof BPrim; }
export function isBDict(x: any): x is BDict { return x instanceof BDict; }
export function isBExp(x: any): x is BExp { return x instanceof BExp; }
export function isBDot(x: any): x is BDot { return x instanceof BDot; }
export function isBTyp(x: any): x is BTyp { return x instanceof BTyp; }
export function isBImport(x: any): x is BImport { return x instanceof BImport; }
export function isBUnknown(x: any): x is BUnknown { return x instanceof BUnknown; }

// ---------- data Binding ----------

export class EBind {
  get $name(): 'e-bind' { return 'e-bind'; }
  constructor(public loc: Loc, public mut: boolean, public info: BindingInfo) {}
}
export type Binding = EBind;
export function isEBind(x: any): x is EBind { return x instanceof EBind; }

export type BindingEnv = Map<string, Binding>;

export function bindExp(e: A.Expr, env: BindingEnv): Binding | undefined {
  switch (e.$name) {
    case 's-dot': {
      const eb = bindExp(e.obj, env);
      if (eb !== undefined) {
        const b = eb.info;
        if (isBDict(b)) {
          // NOTE: the Pyret source looks the field name up via `name.key()`,
          // but `name` is a String there; this branch is only reachable if a
          // b-dict is ever constructed, which never happens in this module.
          if (b.dict.has(e.field)) { return new EBind(A.dummyLoc, false, mapGetValue(b.dict, e.field)); }
          else { return new EBind(A.dummyLoc, false, new BDot(b, e.field)); }
        } else {
          return new EBind(A.dummyLoc, false, new BDot(b, e.field));
        }
      } else {
        return undefined;
      }
    }
    case 's-id':
      return env.get(e.id.key());
    case 's-id-var':
      return env.get(e.id.key());
    case 's-id-letrec':
      return env.get(e.id.key());
    default:
      return new EBind(A.dummyLoc, false, new BExp(e));
  }
}

export function bindOrUnknown(e: A.Expr, env: BindingEnv): BindingInfo {
  const b = bindExp(e, env);
  if (b === undefined) { return bUnknown; }
  else {
    if (!isEBind(b)) {
      console.error("b isn't a binding for expr " + String(e).substring(0, 100));
      console.error(b);
    }
    return b.info;
  }
}

export function bindingTypeEnvFromEnv(env: CS.CompileEnvironment): BindingEnv {
  const acc: BindingEnv = new Map();
  for (const name of env.globals.types.keys()) {
    acc.set(new A.STypeGlobal(name).key(), new EBind(A.dummyLoc, false, bTyp));
  }
  return acc;
}
export function bindingEnvFromEnv(env: CS.CompileEnvironment): BindingEnv {
  const acc: BindingEnv = new Map();
  for (const name of env.globals.values.keys()) {
    acc.set(new A.SGlobal(name).key(), new EBind(A.dummyLoc, false, new BPrim(name)));
  }
  return acc;
}

// ---------- env-threading visitors ----------

export interface EnvBindHandlers<E, TE> {
  sLetrecBind(lrb: A.LetrecBind, env: E): E;
  sLetBind(lb: A.LetBind, env: E): E;
  sBind(b: A.Bind, env: E): E;
  sHeader(imp: A.Import, env: E, typeEnv: TE): { valEnv: E; typeEnv: TE };
  sTypeLetBind(tlb: A.TypeLetBind, env: E, typeEnv: TE): { valEnv: E; typeEnv: TE };
  sParamBind(l: Loc, param: A.Name, typeEnv: TE): TE;
}

export class DefaultEnvMapVisitor<E, TE> extends PostScopeMapVisitor {
  env: E;
  typeEnv: TE;
  bindHandlers: EnvBindHandlers<E, TE>;

  constructor(initialEnv: E, initialTypeEnv: TE, bindHandlers: EnvBindHandlers<E, TE>) {
    super();
    this.env = initialEnv;
    this.typeEnv = initialTypeEnv;
    this.bindHandlers = bindHandlers;
  }

  sProgram(node: A.SProgram): A.Program {
    const visitUse = this.option(node._use);
    const visitProvide = node._provide.visit(this);
    const visitProvideTypes = node.providedTypes.visit(this);
    const visitImports = node.imports.map((i: A.Import) => i.visit(this));
    let importedEnvs = { valEnv: this.env, typeEnv: this.typeEnv };
    for (const i of visitImports) {
      importedEnvs = this.bindHandlers.sHeader(i, importedEnvs.valEnv, importedEnvs.typeEnv);
    }
    const visitBody = node.block.visit(extendVisitor(this, { env: importedEnvs.valEnv, typeEnv: importedEnvs.typeEnv }));
    // MARK(joe/ben)
    return new A.SProgram(node.l, visitUse, visitProvide, visitProvideTypes, node.provides, visitImports, visitBody);
  }

  sScopeBlock(node: A.SScopeBlock): A.Expr {
    let env = this.env;
    let typeEnv = this.typeEnv;
    const newEntries: A.ScopeEntry[] = [];
    for (const entry of node.entries) {
      if (A.isSScopeLet(entry)) {
        const bs: A.LetBind[] = [];
        for (const b of entry.binds) {
          const newBind = b.visit(extendVisitor(this, { env, typeEnv }));
          env = this.bindHandlers.sLetBind(newBind, env);
          bs.push(newBind);
        }
        newEntries.push(new A.SScopeLet(entry.l, bs));
      } else if (A.isSScopeTypeLet(entry)) {
        const bs: A.TypeLetBind[] = [];
        for (const b of entry.binds) {
          const updated = this.bindHandlers.sTypeLetBind(b, env, typeEnv);
          const visitEnvs = extendVisitor(this, { env: updated.valEnv, typeEnv: updated.typeEnv });
          const newBind = b.visit(visitEnvs);
          env = updated.valEnv;
          typeEnv = updated.typeEnv;
          bs.push(newBind);
        }
        newEntries.push(new A.SScopeTypeLet(entry.l, bs));
      } else if (A.isSScopeLetrec(entry)) {
        let bindEnv = env;
        for (const b of entry.binds) {
          bindEnv = this.bindHandlers.sLetrecBind(b, bindEnv);
        }
        const newVisitor = extendVisitor(this, { env: bindEnv, typeEnv });
        newEntries.push(new A.SScopeLetrec(entry.l, entry.binds.map((b: A.LetrecBind) => b.visit(newVisitor))));
        env = bindEnv;
      } else {
        newEntries.push(entry.visit(extendVisitor(this, { env, typeEnv })));
      }
    }
    const tail = node.tail.visit(extendVisitor(this, { env, typeEnv }));
    return new A.SScopeBlock(node.l, newEntries, tail);
  }

  sLam(node: A.SLam): A.Expr {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    const newArgs = node.args.map((a: A.Bind) => a.visit(withParams));
    let argsEnv = withParams.env;
    for (const newArg of node.args) {
      argsEnv = this.bindHandlers.sBind(newArg, argsEnv);
    }
    const withArgs = extendVisitor(withParams, { env: argsEnv });
    const newBody = node.body.visit(withArgs);
    const newCheck = withArgs.option(node._check);
    return new A.SLam(node.l, node.name, node.params, newArgs, node.ann.visit(withArgs), node.doc, newBody, node._checkLoc, newCheck, node.blocky);
  }

  sCasesElse(node: A.SCasesElse): A.Expr {
    return new A.SCasesElse(node.l, node.typ.visit(this), node.val.visit(this),
      node.branches.map((b: A.CasesBranch) => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sCasesBranch(node: A.SCasesBranch): A.CasesBranch {
    const newArgs = node.args.map((a: A.CasesBind) => a.visit(this));
    let argsEnv = this.env;
    for (const arg of node.args.map((a: A.CasesBind) => a.bind)) {
      argsEnv = this.bindHandlers.sBind(arg, argsEnv);
    }
    return new A.SCasesBranch(node.l, node.patLoc, node.name, newArgs, node.body.visit(extendVisitor(this, { env: argsEnv })));
  }

  sSingletonCasesBranch(node: A.SSingletonCasesBranch): A.CasesBranch {
    return new A.SSingletonCasesBranch(node.l, node.patLoc, node.name, node.body.visit(this));
  }

  sDataExpr(node: A.SDataExpr): A.Expr {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    return new A.SDataExpr(node.l, node.name, node.namet.visit(withParams), node.params,
      node.mixins.map((m: A.Expr) => m.visit(withParams)), node.variants.map((v: A.Variant) => v.visit(withParams)),
      node.sharedMembers.map((m: A.Member) => m.visit(withParams)), node._checkLoc, withParams.option(node._check));
  }

  sMethod(node: A.SMethod): A.Expr {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    const newArgs = node.args.map((a: A.Bind) => a.visit(withParams));
    let argsEnv = withParams.env;
    for (const arg of newArgs) {
      argsEnv = this.bindHandlers.sBind(arg, argsEnv);
    }
    const withArgsEnv = extendVisitor(withParams, { env: argsEnv });
    const newBody = node.body.visit(withArgsEnv);
    const newCheck = withArgsEnv.option(node._check);
    // NOTE: the Pyret source constructs `A.s-method(l, params, new-args, ...)`
    // here, dropping the name and blocky fields (a latent arity bug); we
    // construct the full node as clearly intended.
    return new A.SMethod(node.l, node.name, node.params, newArgs, node.ann.visit(withArgsEnv), node.doc, newBody, node._checkLoc, newCheck, node.blocky);
  }
}

export function defaultEnvMapVisitor<E, TE>(
    initialEnv: E,
    initialTypeEnv: TE,
    bindHandlers: EnvBindHandlers<E, TE>): DefaultEnvMapVisitor<E, TE> {
  return new DefaultEnvMapVisitor(initialEnv, initialTypeEnv, bindHandlers);
}

export class DefaultEnvIterVisitor<E, TE> extends PostScopeIterVisitor {
  env: E;
  typeEnv: TE;
  bindHandlers: EnvBindHandlers<E, TE>;

  constructor(initialEnv: E, initialTypeEnv: TE, bindHandlers: EnvBindHandlers<E, TE>) {
    super();
    this.env = initialEnv;
    this.typeEnv = initialTypeEnv;
    this.bindHandlers = bindHandlers;
  }

  sProgram(node: A.SProgram): boolean {
    // NOTE: the Pyret source says `self.option.visit(_use)`; the intent is
    // `self.option(_use)`.
    if (this.option(node._use) && node._provide.visit(this) && node.providedTypes.visit(this)) {
      let importedEnvs = { valEnv: this.env, typeEnv: this.typeEnv };
      for (const i of node.imports) {
        importedEnvs = this.bindHandlers.sHeader(i, importedEnvs.valEnv, importedEnvs.typeEnv);
      }
      const newVisitor = extendVisitor(this, { env: importedEnvs.valEnv, typeEnv: importedEnvs.typeEnv });
      return node.imports.every((i: A.Import) => i.visit(newVisitor)) && node.block.visit(newVisitor);
      // MARK(joe/ben): provides
    } else {
      return false;
    }
  }

  sScopeBlock(node: A.SScopeBlock): boolean {
    let env = this.env;
    let typeEnv = this.typeEnv;
    for (const entry of node.entries) {
      if (A.isSScopeLet(entry)) {
        for (const b of entry.binds) {
          const thisEnv = this.bindHandlers.sLetBind(b, env);
          const newBind = b.visit(extendVisitor(this, { env, typeEnv }));
          env = thisEnv;
          if (!newBind) { return false; }
        }
      } else if (A.isSScopeTypeLet(entry)) {
        for (const b of entry.binds) {
          const updated = this.bindHandlers.sTypeLetBind(b, env, typeEnv);
          const visitEnvs = extendVisitor(this, { env: updated.valEnv, typeEnv: updated.typeEnv });
          const newBind = b.visit(visitEnvs);
          env = updated.valEnv;
          typeEnv = updated.typeEnv;
          if (!newBind) { return false; }
        }
      } else if (A.isSScopeLetrec(entry)) {
        let bindEnv = env;
        for (const b of entry.binds) {
          bindEnv = this.bindHandlers.sLetrecBind(b, bindEnv);
        }
        const newVisitor = extendVisitor(this, { env: bindEnv, typeEnv });
        if (!entry.binds.every((b: A.LetrecBind) => b.visit(newVisitor))) { return false; }
        env = bindEnv;
      } else {
        if (!entry.visit(extendVisitor(this, { env, typeEnv }))) { return false; }
      }
    }
    return node.tail.visit(extendVisitor(this, { env, typeEnv }));
  }

  sLam(node: A.SLam): boolean {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    const visitArgs = node.args.every((a: A.Bind) => a.visit(withParams));
    let argsEnv = withParams.env;
    for (const arg of node.args) {
      argsEnv = this.bindHandlers.sBind(arg, argsEnv);
    }
    const withArgs = extendVisitor(withParams, { env: argsEnv });
    return visitArgs &&
      node.ann.visit(withArgs) &&
      node.body.visit(withArgs) &&
      withArgs.option(node._check);
  }

  sCasesElse(node: A.SCasesElse): boolean {
    return node.typ.visit(this)
      && node.val.visit(this)
      && node.branches.every((b: A.CasesBranch) => b.visit(this))
      && node._else.visit(this);
  }

  sCasesBranch(node: A.SCasesBranch): boolean {
    const visitArgs = node.args.every((a: A.CasesBind) => a.visit(this));
    let argsEnv = this.env;
    for (const arg of node.args.map((a: A.CasesBind) => a.bind)) {
      argsEnv = this.bindHandlers.sBind(arg, argsEnv);
    }
    return visitArgs
      && node.body.visit(extendVisitor(this, { env: argsEnv }));
  }

  // s-singleton-cases-branch introduces no new bindings, so default visitor is fine

  sDataExpr(node: A.SDataExpr): boolean {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    return node.namet.visit(withParams)
      && node.mixins.every((m: A.Expr) => m.visit(withParams))
      && node.variants.every((v: A.Variant) => v.visit(withParams))
      && node.sharedMembers.every((m: A.Member) => m.visit(withParams))
      && withParams.option(node._check);
  }

  sMethod(node: A.SMethod): boolean {
    let newTypeEnv = this.typeEnv;
    for (const param of node.params) {
      newTypeEnv = this.bindHandlers.sParamBind(node.l, param, newTypeEnv);
    }
    const withParams = extendVisitor(this, { typeEnv: newTypeEnv });
    let argsEnv = this.env;
    for (const arg of node.args) {
      argsEnv = this.bindHandlers.sBind(arg, argsEnv);
    }
    const withArgsEnv = extendVisitor(withParams, { env: argsEnv });
    return node.args.every((a: A.Bind) => a.visit(withParams)) &&
      node.ann.visit(withArgsEnv) &&
      node.body.visit(withArgsEnv) &&
      withArgsEnv.option(node._check);
  }
}

export function defaultEnvIterVisitor<E, TE>(
    initialEnv: E,
    initialTypeEnv: TE,
    bindHandlers: EnvBindHandlers<E, TE>): DefaultEnvIterVisitor<E, TE> {
  return new DefaultEnvIterVisitor(initialEnv, initialTypeEnv, bindHandlers);
}

export const bindingHandlers: EnvBindHandlers<BindingEnv, BindingEnv> = {
  sHeader(imp: A.Import, env: BindingEnv, typeEnv: BindingEnv): { valEnv: BindingEnv; typeEnv: BindingEnv } {
    // NOTE: the fields accessed here (vals-name, types-name, values, types,
    // import-type) come from the long-gone s-import-complete header form;
    // this handler is only reachable through the legacy binding-env visitors.
    const impAny = imp as any;
    const withVname = mapSet(env, impAny.valsName.key(), new EBind(imp.l, false, bUnknown));
    const withTname = mapSet(typeEnv, impAny.typesName.key(), new EBind(imp.l, false, bTyp));
    let withVnames = withVname;
    for (const v of impAny.values as A.Name[]) {
      withVnames = mapSet(withVnames, v.key(), new EBind(imp.l, false, new BImport(impAny.importType)));
    }
    let withTnames = withTname;
    for (const t of impAny.types as A.Name[]) {
      withTnames = mapSet(withTnames, t.key(), new EBind(imp.l, false, new BImport(impAny.importType)));
    }
    return {
      valEnv: withVnames,
      typeEnv: withTnames,
    };
  },
  sParamBind(l: Loc, param: A.Name, typeEnv: BindingEnv): BindingEnv {
    return mapSet(typeEnv, param.key(), new EBind(l, false, bTyp));
  },
  sTypeLetBind(tlb: A.TypeLetBind, env: BindingEnv, typeEnv: BindingEnv): { valEnv: BindingEnv; typeEnv: BindingEnv } {
    switch (tlb.$name) {
      case 's-type-bind':
        return {
          valEnv: env,
          typeEnv: mapSet(typeEnv, tlb.name.key(), new EBind(tlb.l, false, bTyp)),
        };
      case 's-newtype-bind':
        return {
          valEnv: mapSet(env, tlb.namet.key(), new EBind(tlb.l, false, bUnknown)),
          typeEnv: mapSet(typeEnv, tlb.name.key(), new EBind(tlb.l, false, bTyp)),
        };
      default:
        throw raise('Unknown TypeLetBind in bindingHandlers: ' + (tlb as any).$name);
    }
  },
  sLetBind(lb: A.LetBind, env: BindingEnv): BindingEnv {
    switch (lb.$name) {
      case 's-let-bind':
        return mapSet(env, field(lb.b, 'id').key(), new EBind(lb.l, false, bindOrUnknown(lb.value, env)));
      case 's-var-bind':
        return mapSet(env, field(lb.b, 'id').key(), new EBind(lb.l, true, bUnknown));
      default:
        throw raise('Unknown LetBind in bindingHandlers: ' + (lb as any).$name);
    }
  },
  sLetrecBind(lrb: A.LetrecBind, env: BindingEnv): BindingEnv {
    return mapSet(env, field(lrb.b, 'id').key(),
      new EBind(lrb.l, false, bindOrUnknown(lrb.value, env)));
  },
  sBind(b: A.Bind, env: BindingEnv): BindingEnv {
    return mapSet(env, field(b, 'id').key(), new EBind(b.l, false, bUnknown));
  },
};

export function bindingEnvMapVisitor(initialEnv: CS.CompileEnvironment): DefaultEnvMapVisitor<BindingEnv, BindingEnv> {
  return defaultEnvMapVisitor(bindingEnvFromEnv(initialEnv), bindingTypeEnvFromEnv(initialEnv), bindingHandlers);
}
export function bindingEnvIterVisitor(initialEnv: CS.CompileEnvironment): DefaultEnvIterVisitor<BindingEnv, BindingEnv> {
  return defaultEnvIterVisitor(bindingEnvFromEnv(initialEnv), bindingTypeEnvFromEnv(initialEnv), bindingHandlers);
}

class LinkListVisitor extends DefaultEnvMapVisitor<BindingEnv, BindingEnv> {
  sApp(node: A.SApp): A.Expr {
    const f = node._fun;
    const defaultApp = (): A.Expr =>
      new A.SApp(node.l, f.visit(this), node.args.map((a: A.Expr) => a.visit(this)));
    if (A.isSDot(f) && (f.field === '_plus')) {
      const target = f.obj;
      switch (target.$name) {
        case 's-app': {
          const b = bindOrUnknown(target._fun, this.env);
          if (isBPrim(b)) {
            if (b.name === 'list:link') {
              return new A.SApp(target.l, target._fun, [target.args[0],
                new A.SApp(node.l, new A.SDot(f.l, target.args[1], f.field), node.args).visit(this)]);
            } else if (b.name === 'list:empty') {
              return node.args[0].visit(this);
            } else {
              return defaultApp();
            }
          } else {
            return defaultApp();
          }
        }
        case 's-id': {
          const b = bindOrUnknown(target, this.env);
          if (isBPrim(b)) {
            if (b.name === 'list:empty') {
              return node.args[0].visit(this);
            } else {
              return defaultApp();
            }
          } else {
            return defaultApp();
          }
        }
        case 's-dot': {
          const b = bindOrUnknown(target, this.env);
          if (isBPrim(b)) {
            if (b.name === 'list:empty') {
              return node.args[0].visit(this);
            } else {
              return defaultApp();
            }
          } else {
            return defaultApp();
          }
        }
        default:
          return defaultApp();
      }
    } else {
      return defaultApp();
    }
  }
}

export function linkListVisitor(initialEnv: CS.CompileEnvironment): DefaultEnvMapVisitor<BindingEnv, BindingEnv> {
  return new LinkListVisitor(bindingEnvFromEnv(initialEnv), bindingTypeEnvFromEnv(initialEnv), bindingHandlers);
}

export function badAssignments(initialEnv: CS.CompileEnvironment, ast: A.Program): CS.CompileError[] {
  let errors: CS.CompileError[] = []; // THE MUTABLE LIST OF ERRORS
  function addError(err: CS.CompileError): void { errors = [err, ...errors]; }
  ast.visit(new (class extends DefaultEnvIterVisitor<BindingEnv, BindingEnv> {
    sAssign(node: A.SAssign): boolean {
      const b = bindExp(new A.SId(node.l, node.id), this.env);
      if (b !== undefined) {
        if (!b.mut) {
          // NOTE: the Pyret source here calls
          // `CS.bad-assignment(id.toname(), loc, b.loc)`, a stale three-argument
          // call; bad-assignment takes (iuse :: A.Expr, idef :: Loc), so we
          // build the s-assign use site as resolve-scope does.
          addError(new CS.BadAssignment(new A.SAssign(node.l, node.id, node.value), b.loc));
        }
      }
      return node.value.visit(this);
    }
  })(bindingEnvFromEnv(initialEnv), bindingTypeEnvFromEnv(initialEnv), bindingHandlers));
  return errors;
}

class InlineLams extends PostScopeMapVisitor {
  sApp(node: A.SApp): A.Expr {
    const f = node._fun;
    if (A.isSLam(f)) {
      const l = f.l;
      const args = f.args;
      const ann = f.ann;
      const body = f.body;
      if (args.length === node.args.length) {
        const a = A.globalNames.makeAtom('inline_body');
        const letBinds = map2((arg: A.Bind, exp: A.Expr): A.LetBind =>
          new A.SLetBind(arg.l, arg, exp.visit(this)), args, node.args);
        switch (ann.$name) {
          case 'a-blank': return A.sScopeLetBlock(l, letBinds, body.visit(this));
          case 'a-any': return A.sScopeLetBlock(l, letBinds, body.visit(this));
          default:
            return A.sScopeLetBlock(l,
              [...letBinds, new A.SLetBind(body.l, new A.SBind(l, false, a, ann), body.visit(this))],
              new A.SId(l, a));
        }
      } else {
        return new A.SApp(node.l, f.visit(this), node.args.map((e: A.Expr) => e.visit(this)));
      }
    } else {
      return new A.SApp(node.l, f.visit(this), node.args.map((e: A.Expr) => e.visit(this)));
    }
  }
}

export const inlineLams: DefaultMapVisitor = new InlineLams();

// ---------- data Scope ----------

export abstract class ScopeBase {
  abstract get $name(): string;
}
export class NoS extends ScopeBase {
  get $name(): 'no-s' { return 'no-s'; }
}
export class FunS extends ScopeBase {
  get $name(): 'fun-s' { return 'fun-s'; }
  constructor(public id: A.Name) { super(); }
}
export class MethodS extends ScopeBase {
  get $name(): 'method-s' { return 'method-s'; }
  constructor(public selfId: A.Name, public name: string) { super(); }
}
export class PartialFunS extends ScopeBase {
  get $name(): 'partial-fun-s' { return 'partial-fun-s'; }
  constructor(public id: A.Name) { super(); }
}
export class PartialMethodS extends ScopeBase {
  get $name(): 'partial-method-s' { return 'partial-method-s'; }
  constructor(public name: string) { super(); }
}
export type Scope = NoS | FunS | MethodS | PartialFunS | PartialMethodS;
export const noS: NoS = new NoS();
export function isNoS(x: any): x is NoS { return x instanceof NoS; }
export function isFunS(x: any): x is FunS { return x instanceof FunS; }
export function isMethodS(x: any): x is MethodS { return x instanceof MethodS; }
export function isPartialFunS(x: any): x is PartialFunS { return x instanceof PartialFunS; }
export function isPartialMethodS(x: any): x is PartialMethodS { return x instanceof PartialMethodS; }

// set-recursive-visitor is to replace s-app with s-app-enhanced with correct is-recursive
// but with incorrect is-tail (all false)
// postcondition: no s-app
class SetRecursiveVisitor extends PostScopeMapVisitor {
  scope: Scope = noS;

  clearScope(): this {
    return extendVisitor(this, { scope: noS } as Partial<this>);
  }

  // Return a Boolean indicating whether a call with `f` is recursive or not
  isRecursive(f: A.Expr): boolean {
    const scope = this.scope;
    switch (scope.$name) {
      case 'fun-s': return A.isSIdLetrec(f) && (f.id.key() === scope.id.key());
      case 'method-s': return false; // TODO(Oak, 15 Jan 2016): Don't care about method for now
      case 'no-s': return false;
      case 'partial-method-s': return false; // Do not actually find a method: `{ a : lam(): id(1) end }`
      case 'partial-fun-s': return raise('Error while querying: after partial-fun-s should immediately be fun-s');
      default:
        throw raise('Unknown Scope: ' + (scope as any).$name);
    }
  }

  // Activate fun-s
  activateFun(): this {
    const scope = this.scope;
    switch (scope.$name) {
      case 'partial-fun-s': return extendVisitor(this, { scope: new FunS(scope.id) } as Partial<this>);
      case 'no-s': return this; // no letrec, meaning it's just normal lambda: `lam(x): x + 1 end(5)`, for example
      case 'fun-s': return this.clearScope(); // lam in function: `fun foo() lam(): 1 end end`
      case 'method-s': return this.clearScope(); // lam in method: `{ foo(self): lam(): 1 end end }`
      case 'partial-method-s': return this.clearScope(); // lam in object: `{ a : lam(): id(1) end }`
      default:
        throw raise('Unknown Scope: ' + (scope as any).$name);
    }
  }

  // Return an environment with a binding containing function's name
  collectFunName(binding: A.LetrecBind): this {
    if (A.isSLam(binding.value)) {
      return extendVisitor(this, { scope: new PartialFunS(field(binding.b, 'id')) } as Partial<this>);
    } else {
      return this;
    }
  }

  // Return an environment with method's self id
  collectMethodSelf(selfBind: A.Bind): this {
    const scope = this.scope;
    switch (scope.$name) {
      case 'partial-method-s': return extendVisitor(this, { scope: new MethodS(field(selfBind, 'id'), scope.name) } as Partial<this>);
      case 'no-s': return this.clearScope(); // `method(self): 1 end`
      case 'fun-s': return this.clearScope(); // fun foo(): method(self): 1 end end
      case 'method-s': return this.clearScope(); // { a(self1): method(self2): 1 end end }
      case 'partial-fun-s': return raise('Error while collecting self: after partial-fun-s should immediately be fun-s');
      default:
        throw raise('Unknown Scope: ' + (scope as any).$name);
    }
  }

  // Return an environment with a method's name
  collectMethodName(methodName: string): this {
    return extendVisitor(this, { scope: new PartialMethodS(methodName) } as Partial<this>);
  }

  sApp(node: A.SApp): A.Expr {
    return new A.SAppEnriched(
      node.l,
      node._fun.visit(this),
      node.args.map((e: A.Expr) => e.visit(this)),
      new A.AppInfoC(this.isRecursive(node._fun), false));
  }

  sLam(node: A.SLam): A.Expr {
    return new A.SLam(
      node.l,
      node.name,
      node.params.map((p: A.Name) => p.visit(this.clearScope())),
      node.args.map((a: A.Bind) => a.visit(this.clearScope())),
      node.ann.visit(this.clearScope()),
      node.doc,
      node.body.visit(this.activateFun()),
      node._checkLoc,
      this.clearScope().option(node._check),
      node.blocky);
  }

  sMethod(node: A.SMethod): A.Expr {
    return new A.SMethod(
      node.l,
      node.name,
      node.params.map((p: A.Name) => p.visit(this)),
      node.args.map((a: A.Bind) => a.visit(this)),
      node.ann.visit(this),
      node.doc,
      node.body.visit(this.collectMethodSelf(node.args[0])),
      node._checkLoc,
      this.option(node._check),
      node.blocky);
  }

  sScopeLetrec(node: A.SScopeLetrec): A.ScopeEntry {
    return new A.SScopeLetrec(
      node.l,
      node.binds.map((bind: A.LetrecBind) => bind.visit(this.collectFunName(bind))));
  }

  sDataField(node: A.SDataField): A.Member {
    return new A.SDataField(node.l, node.name, node.value.visit(this.collectMethodName(node.name)));
  }
}

export const setRecursiveVisitor: DefaultMapVisitor = new SetRecursiveVisitor();

// Return whether `ann` is a stateful annotation or not. For now, consider
// all refinements as potentially could be stateful.
export function isStatefulAnn(ann: A.Ann): boolean {
  // TODO(Oak, 26 Jan 2016): make sure below are correct when static type checker lands
  switch (ann.$name) {
    case 'a-blank': return false;
    case 'a-any': return false;
    case 'a-name': return false;
    case 'a-type-var': return false;
    case 'a-arrow': return false;
    case 'a-arrow-argnames': return false;
    case 'a-method': return false;
    case 'a-record': return ann.fields.map((f: A.AField) => f.ann).every(isStatefulAnn);
    case 'a-tuple': return ann.fields.every(isStatefulAnn);
    case 'a-app': return isStatefulAnn(ann.ann);
    case 'a-pred': return true; // TODO(Oak, 21 Jan 2016): true for now. Could refine later
    case 'a-dot': return true; // TODO(Oak, 7 Feb 2016): true for now. Could refine later
    case 'a-checked': return raise('NYI');
    default:
      throw raise('Unknown Ann in isStatefulAnn: ' + (ann as any).$name);
  }
}

// set-tail-visitor is to correct is-tail in s-app-enriched
// precondition: no s-app
class SetTailVisitor extends PostScopeMapVisitor {
  isTail: boolean = false;

  private noTail(): this {
    return extendVisitor(this, { isTail: false } as Partial<this>);
  }

  sModule(node: A.SModule): A.Expr {
    const noTail = this.noTail();
    return new A.SModule(
      node.l,
      node.answer.visit(noTail),
      node.definedModules.map((dm: A.DefinedModule) => dm.visit(noTail)),
      node.definedValues.map((dv: A.DefinedValue) => dv.visit(noTail)),
      node.definedTypes.map((dt: A.DefinedType) => dt.visit(noTail)),
      node.checks.visit(noTail));
  }

  // skip s-num, s-frac, s-str, s-undefined, s-bool, s-id, s-id-var, s-id-letrec, s-srcloc
  // because it has no s-app-enriched

  // skip s-data-expr because it couldn't be at the tail position

  // skip s-if-else because all positions which could have s-app-enriched could be in the tail position

  sIfBranch(node: A.SIfBranch): A.IfBranch {
    return new A.SIfBranch(node.l, node.test.visit(this.noTail()), node.body.visit(this));
  }

  sCasesElse(node: A.SCasesElse): A.Expr {
    return new A.SCasesElse(node.l, node.typ.visit(this), node.val.visit(this.noTail()),
      node.branches.map((b: A.CasesBranch) => b.visit(this)), node._else.visit(this), false);
  }

  sBlock(node: A.SBlock): A.Expr {
    const len = node.stmts.length; // can be sure that len >= 1
    const prefix = node.stmts.slice(0, len - 1);
    const suffix = node.stmts.slice(len - 1);
    return new A.SBlock(
      node.l,
      [...prefix.map((s: A.Expr) => s.visit(this.noTail())),
       ...suffix.map((s: A.Expr) => s.visit(this))]);
  }

  sScopeBlock(node: A.SScopeBlock): A.Expr {
    // type-let binds keep the CURRENT flag: the Pyret original skips
    // s-type-let-expr, so its default traversal carries is-tail into them
    const noTail = this.noTail();
    const newEntries: A.ScopeEntry[] = node.entries.map((entry) => {
      if (A.isSScopeTypeLet(entry)) {
        return entry.visit(this);
      }
      return entry.visit(noTail);
    });
    return new A.SScopeBlock(node.l, newEntries, node.tail.visit(this));
  }

  sCheckExpr(node: A.SCheckExpr): A.Expr {
    return new A.SCheckExpr(
      node.l,
      node.expr.visit(this.noTail()),
      node.ann.visit(this.noTail()));
  }

  sLam(node: A.SLam): A.Expr {
    return new A.SLam(
      node.l,
      node.name,
      node.params.map((p: A.Name) => p.visit(this.noTail())),
      node.args.map((a: A.Bind) => a.visit(this.noTail())),
      node.ann.visit(this.noTail()),
      node.doc,
      node.body.visit(extendVisitor(this, { isTail: !isStatefulAnn(node.ann) } as Partial<this>)),
      node._checkLoc,
      this.noTail().option(node._check),
      node.blocky);
  }

  sMethod(node: A.SMethod): A.Expr {
    return new A.SMethod(
      node.l,
      node.name,
      node.params.map((p: A.Name) => p.visit(this.noTail())),
      node.args.map((a: A.Bind) => a.visit(this.noTail())),
      node.ann.visit(this.noTail()),
      node.doc,
      node.body.visit(extendVisitor(this, { isTail: !isStatefulAnn(node.ann) } as Partial<this>)),
      node._checkLoc,
      this.noTail().option(node._check),
      node.blocky);
  }

  sArray(node: A.SArray): A.Expr {
    return new A.SArray(node.l, node.values.map((v: A.Expr) => v.visit(this.noTail())));
  }

  sAppEnriched(node: A.SAppEnriched): A.Expr {
    return new A.SAppEnriched(
      node.l,
      node._fun.visit(this.noTail()),
      node.args.map((e: A.Expr) => e.visit(this.noTail())),
      new A.AppInfoC(node.appInfo.isRecursive, this.isTail));
  }

  sPrimApp(node: A.SPrimApp): A.Expr {
    return new A.SPrimApp(node.l, node._fun, node.args.map((a: A.Expr) => a.visit(this.noTail())), node.appInfo);
  }

  // skip s-instantiate because all positions which could have s-app-enriched could be in the tail position

  sDot(node: A.SDot): A.Expr {
    return new A.SDot(node.l, node.obj.visit(this.noTail()), node.field);
  }

  // skip s-ref because it has no s-app-enriched -- what is s-ref anyway

  sGetBang(node: A.SGetBang): A.Expr {
    return new A.SGetBang(node.l, node.obj.visit(this.noTail()), node.field);
  }

  sAssign(node: A.SAssign): A.Expr {
    return new A.SAssign(node.l, node.id.visit(this.noTail()), node.value.visit(this.noTail()));
  }

  sObj(node: A.SObj): A.Expr {
    return new A.SObj(node.l, node.fields.map((f: A.Member) => f.visit(this.noTail())));
  }

  sUpdate(node: A.SUpdate): A.Expr {
    return new A.SUpdate(node.l, node.supe.visit(this.noTail()), node.fields.map((f: A.Member) => f.visit(this.noTail())));
  }

  sExtend(node: A.SExtend): A.Expr {
    return new A.SExtend(node.l, node.supe.visit(this.noTail()), node.fields.map((f: A.Member) => f.visit(this.noTail())));
  }
}

export const setTailVisitor: DefaultMapVisitor = new SetTailVisitor();

export function valueDelaysExecOf(name: A.Name, expr: A.Expr): boolean {
  return A.isSLam(expr) || A.isSMethod(expr);
}

class LetrecVisitor extends PostScopeMapVisitor {
  env: Map<string, boolean> = new Map();

  sIdLetrec(node: A.SIdLetrec): A.Expr {
    return new A.SIdLetrec(node.l, node.id, mapGetValue(this.env, node.id.key()));
  }

  sScopeBlock(node: A.SScopeBlock): A.Expr {
    let cur: LetrecVisitor = this;
    const newEntries: A.ScopeEntry[] = [];
    for (const entry of node.entries) {
      if (A.isSScopeLetrec(entry)) {
        const bindEnvs = entry.binds.map((b1: A.LetrecBind, i: number) => {
          const rhsIsDelayed = valueDelaysExecOf(field(b1.b, 'id'), b1.value);
          const acc = new Map(cur.env);
          entry.binds.forEach((b2: A.LetrecBind, j: number) => {
            const key = field(b2.b, 'id').key();
            if (i < j) {
              acc.set(key, false);
            } else if (i === j) {
              acc.set(key, rhsIsDelayed);
            } else {
              acc.set(key, true);
            }
          });
          return acc;
        });
        const newBinds = map2((b: A.LetrecBind, bindEnv: Map<string, boolean>) =>
          b.visit(extendVisitor(cur, { env: bindEnv } as Partial<LetrecVisitor>)), entry.binds, bindEnvs);
        const bodyEnv = mapSet(bindEnvs[bindEnvs.length - 1],
          field(entry.binds[entry.binds.length - 1].b, 'id').key(), true);
        newEntries.push(new A.SScopeLetrec(entry.l, newBinds));
        cur = extendVisitor(cur, { env: bodyEnv } as Partial<LetrecVisitor>);
      } else {
        newEntries.push(entry.visit(cur));
      }
    }
    return new A.SScopeBlock(node.l, newEntries, node.tail.visit(cur));
  }
}

export const letrecVisitor: DefaultMapVisitor = new LetrecVisitor();

class StripAnnotationsVisitor extends DefaultMapVisitor {
  aBlank(node: A.ABlank): A.Ann { return A.aBlank; }
  aAny(node: A.AAny): A.Ann { return A.aBlank; }
  aName(node: A.AName): A.Ann { return A.aBlank; }
  aTypeVar(node: A.ATypeVar): A.Ann { return A.aBlank; }
  aArrow(node: A.AArrow): A.Ann { return A.aBlank; }
  aArrowArgnames(node: A.AArrowArgnames): A.Ann { return A.aBlank; }
  aMethod(node: A.AMethod): A.Ann { return A.aBlank; }
  aRecord(node: A.ARecord): A.Ann { return A.aBlank; }
  aTuple(node: A.ATuple): A.Ann { return A.aBlank; }
  aApp(node: A.AApp): A.Ann { return A.aBlank; }
  aPred(node: A.APred): A.Ann { return A.aBlank; }
  aDot(node: A.ADot): A.Ann { return A.aBlank; }
  aField(node: A.AField): A.AField { return new A.AField(node.l, node.name, A.aBlank); }
}

export const stripAnnotationsVisitor: DefaultMapVisitor = new StripAnnotationsVisitor();

export function makeRenamer(replacements: Map<string, A.Name>): DefaultMapVisitor {
  return new (class extends DefaultMapVisitor {
    sAtom(node: A.SAtom): A.Name {
      const a = new A.SAtom(node.base, node.serial);
      const k = a.key();
      if (replacements.has(k)) {
        return mapGetValue(replacements, k);
      } else {
        return a;
      }
    }
  })();
}

export function wrapExtraImports(p: A.Program, env: CS.ExtraImports): A.Program {
  let fullImports: A.Import[];
  if (p._use !== undefined) {
    fullImports = [new A.SInclude(A.dummyLoc, p._use.mod), ...p.imports];
  } else {
    // TODO(Joe/Ben Dec 2020) in the future we will desugar this case into
    // `s-include(p.l, default-namespace)` instead of relying on ExtraImports,
    // and let modules decide for themselves if they want something else
    /*
      NOTE(Ben): I've moved the existing p.imports *after* these generated imports,
      so that any user-requested imports have to coexist in the global environment,
      rather than globals having to coexist in the user's environment.
      Additionally, this allows for better srcloc reporting: suppose the user's program says
        `import some from option`
      which is already existing in the global scope.  The global import will have
      srcloc=A.dummy-loc, but the deliberate import will have srcloc within the file,
      which will ensure the error message refers to that explicit location.
      (I can't change how `resolve-scope:add-value-name` or `resolve-scope:make-import-atom-for`
      handle this case, because we haven't finished resolving names to know whether the name
      collision is acceptable or not.)
    */
    const l = A.dummyLoc;
    let lst: A.Import[] = [];
    for (const i of env.imports) {
      const nameToUse: A.Name = i.asName === '_' ? A.globalNames.makeAtom('$extra-import') : new A.SName(l, i.asName);
      let astDep: A.ImportType;
      switch (i.dependency.$name) {
        case 'builtin':
          astDep = new A.SConstImport(p.l, i.dependency.modname);
          break;
        case 'dependency':
          astDep = new A.SSpecialImport(p.l, i.dependency.protocol, i.dependency.arguments);
          break;
        default:
          throw raise('Unknown Dependency in wrapExtraImports: ' + (i.dependency as any).$name);
      }
      const importLine = new A.SImport(p.l, astDep, nameToUse);
      const includeLine =
        new A.SIncludeFrom(p.l, [nameToUse], [
          ...i.values.map((v: string): A.IncludeSpec =>
            new A.SIncludeName(l, new A.SModuleRef(l, [new A.SName(l, v)], undefined))),
          ...i.types.map((t: string): A.IncludeSpec =>
            new A.SIncludeType(l, new A.SModuleRef(l, [new A.SName(l, t)], undefined))),
        ]);
      lst = [importLine, includeLine, ...lst];
    }
    fullImports = [...lst, ...p.imports];
  }
  return new A.SProgram(p.l, p._use, p._provide, p.providedTypes, p.provides, fullImports, p.block);
}

export function importToDep(imp: A.ImportType): CS.AnyDependency {
  switch (imp.$name) {
    // crossover compatibility
    case 's-const-import': return new CS.Builtin(imp.mod);
    case 's-special-import': return new CS.Dependency(imp.kind, imp.args);
    default:
      throw raise('Unknown ImportType in importToDep: ' + (imp as any).$name);
  }
}

export function somePred<X>(pred: (x: X) => boolean, o: X | undefined): X {
  if (o === undefined) {
    return raise('Expected some but got none');
  } else {
    if (!pred(o)) {
      raise('Predicate failed for ' + String(o));
    }
    return o;
  }
}

export const isSDataExpr = A.isSDataExpr;

export const isTName = T.isTName;
export type NameChanger = (t: T.TName) => T.Type;

// TODO(MATT): a-blank should have location
export function annToTyp(a: A.Ann, uri: URI, compileEnv: CS.CompileEnvironment): T.Type {
  const att = (x: A.Ann): T.Type => annToTyp(x, uri, compileEnv);
  switch (a.$name) {
    case 'a-blank': return new T.TTop(A.dummyLoc, false);
    case 'a-any': return new T.TTop(a.l, false);
    case 'a-name': {
      const id = a.id;
      switch (id.$name) {
        case 's-type-global': {
          const origin = compileEnv.globals.types.get(id.s);
          if (origin === undefined) {
            return raise('Name not found in globals.types: ' + id.s);
          } else {
            // ```include from string-dict: type StringDict as SD end```
            return new T.TName(new T.ModuleUri(origin.uriOfDefinition), origin.originalName, a.l, false);
          }
        }
        case 's-atom': return new T.TName(new T.ModuleUri(uri), id, a.l, false);
        default: return raise('Bad name found in ann-to-typ: ' + id.key());
      }
    }
    case 'a-type-var':
      return new T.TVar(a.id, a.l, false);
    case 'a-arrow':
      return new T.TArrow(a.args.map(att), att(a.ret), a.l, false);
    case 'a-arrow-argnames':
      return new T.TArrow(a.args.map((arg: A.AField) => att(arg.ann)), att(a.ret), a.l, false);
    case 'a-method':
      return raise('Cannot provide a raw method');
    case 'a-record': {
      const members: T.TypeMembers = new Map();
      for (const f of a.fields) {
        members.set(f.name, att(f.ann));
      }
      return new T.TRecord(members, a.l, false);
    }
    case 'a-tuple':
      return new T.TTuple(a.fields.map(att), a.l, false);
    case 'a-app':
      return new T.TApp(att(a.ann), a.args.map(att), a.l, false);
    case 'a-pred':
      // TODO(joe): give more info than this to type checker?  only needed dynamically, right?
      return att(a.ann);
    case 'a-dot':
      // TODO(joe): maybe-b = resolved.module-bindings.get-now(obj.key())
      // Then use the information to provide the right a-dot type by looking
      // it up on the module.
      return new T.TTop(a.l, false);
    case 'a-checked':
      return raise('a-checked should only be generated by the type-checker');
    default:
      throw raise('Unknown Ann in annToTyp: ' + (a as any).$name);
  }
}

export function getNameSpecKey(ns: A.NameSpec): string {
  switch (ns.$name) {
    case 's-star': return raise('Should not get star name-specs in type-checker');
    case 's-module-ref':
      if (ns.path.length !== 1) { raise('Path for a module-ref should always be length 1'); }
      return ns.path[0].key();
    case 's-local-ref':
      return ns.name.key();
    default:
      throw raise('Unknown NameSpec in getNameSpecKey: ' + (ns as any).$name);
  }
}
export function getNameSpecKeyAndName(ns: A.NameSpec): [string, string] {
  switch (ns.$name) {
    case 's-star': return raise('Should not get star name-specs in type-checker');
    case 's-module-ref': {
      if (ns.asName === undefined) { raise('Should always have an as-name post resolve-scope'); }
      if (ns.path.length !== 1) { raise('Path for a module-ref should always be length 1'); }
      return [ns.path[0].key(), ns.asName!.toname()];
    }
    default:
      throw raise('Unknown NameSpec in getNameSpecKeyAndName: ' + (ns as any).$name);
  }
}
export function getNameSpecAtomAndName(ns: A.NameSpec): [A.Name, string] {
  switch (ns.$name) {
    case 's-star': return raise('Should not get star name-specs in type-checker');
    case 's-module-ref': {
      if (ns.asName === undefined) { raise('Should always have an as-name post resolve-scope'); }
      if (ns.path.length !== 1) { raise('Path for a module-ref should always be length 1'); }
      return [ns.path[0], ns.asName!.toname()];
    }
    default:
      throw raise('Unknown NameSpec in getNameSpecAtomAndName: ' + (ns as any).$name);
  }
}

export function getNamedProvides(resolved: CS.NameResolution, uri: URI, compileEnv: CS.CompileEnvironment): CS.Provides {
  const att = (x: A.Ann): T.Type => annToTyp(x, uri, compileEnv);

  function collectSharedFields(vs: A.Variant[]): Map<string, T.Type> {
    if (vs.length === 0) {
      return new Map();
    } else {
      const initMembers = membersToTMembers(vs[0].withMembers);
      let sharedMembers = initMembers;
      for (const v of vs.slice(1)) {
        for (const m of v.withMembers) {
          if (sharedMembers.has(m.name)) {
            const existingMemType = mapGetValue(sharedMembers, m.name);
            const thisMemType = memberToTMember(m);
            if (existingMemType.equals(thisMemType)) {
              // keep sharedMembers
            } else {
              sharedMembers.delete(m.name);
            }
          }
        }
      }
      return sharedMembers;
    }
  }

  function vMembersToTMembers(ms: A.VariantMember[]): T.VariantField[] {
    const members: T.VariantField[] = [];
    for (let i = ms.length - 1; i >= 0; i--) {
      const m = ms[i];
      const typ = A.isSMutable(m.memberType)
        ? new T.TRef(att(field(m.bind, 'ann')), m.l, false)
        : att(field(m.bind, 'ann'));
      members.unshift([field(m.bind, 'id').toname(), typ]);
    }
    return members;
  }

  function memberToTMember(m: A.Member): T.Type {
    switch (m.$name) {
      case 's-data-field':
        return new T.TTop(m.l, false);
      case 's-mutable-field':
        // NOTE: the Pyret source calls `T.t-ref(ann-to-typ(ann), false)`,
        // missing the loc argument; we pass the member's loc.
        return new T.TRef(att(m.ann), m.l, false);
      case 's-method-field': {
        const arrowPart =
          new T.TArrow(m.args.map((a: A.Bind) => att(field(a, 'ann'))), att(m.ann), m.l, false);
        if (m.params.length === 0) { return arrowPart; }
        else {
          const tvars: T.Type[] = m.params.map((p: A.Name) => new T.TVar(p, m.l, false));
          return new T.TForall(tvars, arrowPart, m.l, false);
        }
      }
      default:
        throw raise('Unknown Member in memberToTMember: ' + (m as any).$name);
    }
  }

  function membersToTMembers(ms: A.Member[]): Map<string, T.Type> {
    const members: Map<string, T.Type> = new Map();
    for (const m of ms) {
      members.set(m.name, memberToTMember(m));
    }
    return members;
  }

  function dataExprToDatatype(exp: A.SDataExpr): T.DataType {
    const l = exp.l;
    const tvars: T.Type[] = exp.params.map((tvar: A.Name) => new T.TVar(tvar, l, false));

    const tvariants: T.TypeVariant[] = exp.variants.map((tv: A.Variant) => {
      switch (tv.$name) {
        case 's-variant':
          return new T.TVariant(
            tv.name,
            vMembersToTMembers(tv.members),
            membersToTMembers(tv.withMembers),
            tv.l);
        case 's-singleton-variant':
          return new T.TSingletonVariant(
            tv.name,
            membersToTMembers(tv.withMembers),
            tv.l);
        default:
          throw raise('Unknown Variant in dataExprToDatatype: ' + (tv as any).$name);
      }
    });

    const sharedAcrossVariants = collectSharedFields(exp.variants);
    const sharedFields = membersToTMembers(exp.sharedMembers);
    const allSharedFields = sharedFields;
    for (const key of sharedAcrossVariants.keys()) {
      if (allSharedFields.has(key)) {
        // keep
      } else {
        allSharedFields.set(key, mapGetValue(sharedAcrossVariants, key));
      }
    }

    return new T.TData(
      exp.name,
      tvars,
      tvariants,
      allSharedFields,
      l);
  }

  const env = asVariant(resolved.env, CS.ComputedEnv);
  const provideBlocks = resolved.ast.provides;
  // NOTE(joe): assume the provide block is resolved
  const pb = provideBlocks[0];
  const provideSpecs = pb.specs;

  const mpSpecs = provideSpecs.filter(A.isSProvideModule);
  const modProvides: Map<string, URI> = new Map();
  for (const m of mpSpecs) {
    const ns = m.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const modInfo = compileEnv.providesByUriValue(ns.uri);
        const modUri = mapGetValue(modInfo.modules, ns.name.toname());
        modProvides.set(ns.asName.toname(), modUri);
        break;
      }
      case 's-local-ref': {
        const mb = mapGetValue(env.moduleBindings, ns.name.key());
        modProvides.set(nonNull(ns.asName).toname(), mb.uri);
        break;
      }
      default:
        throw raise('All provides should be resolved to local or remote refs');
    }
  }

  const vpSpecs = provideSpecs.filter(A.isSProvideName);
  const valProvides: Map<string, CS.ValueExport> = new Map();
  for (const v of vpSpecs) {
    const ns = v.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const originName = ns.name.toname();
        const valExport = compileEnv.valueByUriValue(ns.uri, originName);
        const origin = valExport.origin;
        const correctedOrigin = new CS.BindOrigin(
          (ns.asName as any).l,
          origin.definitionBindSite,
          origin.newDefinition,
          origin.uriOfDefinition,
          origin.originalName);
        valProvides.set(ns.asName.toname(), new CS.VAlias(correctedOrigin, originName));
        break;
      }
      case 's-local-ref': {
        const asName = nonNull(ns.asName);
        const vb = mapGetValue(env.bindings, ns.name.key());
        // NOTE(joe/ben): The as-name below has important semantic meaning.
        // This makes it so if you provide the same definition with
        // multiple names, each gets a separate identity from the POV of
        // the module system.

        // This is _different_ from (rename-out) in Racket, a closely
        // related feature. In Racket,
        //
        // (provide x (rename-out (x y))) (define x 10)
        //
        // is different from
        //
        // (provide x y) (define x 10) (define y x)
        //
        // In Pyret,
        // provide: x, x as y end
        // x = 10
        //
        // is (from the module system's POV) the same as
        //
        // provide: x, y end
        // x = 10
        // y = x
        const correctedOrigin = new CS.BindOrigin(
          (asName as any).l,
          vb.origin.definitionBindSite,
          vb.origin.newDefinition,
          vb.origin.uriOfDefinition,
          asName);
        const providedValue = CS.isVbVar(vb.binder)
          ? new CS.VVar(correctedOrigin, att(vb.ann))
          : new CS.VJustType(correctedOrigin, att(vb.ann));
        valProvides.set(asName.toname(), providedValue);
        break;
      }
      default:
        throw raise('Unknown NameSpec in getNamedProvides: ' + (ns as any).$name);
    }
  }

  const tpSpecs = provideSpecs.filter(A.isSProvideType);
  const typProvides: Map<string, T.Type> = new Map();
  for (const t of tpSpecs) {
    const ns = t.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const remoteTyp = compileEnv.typeByUriValue(ns.uri, ns.name.toname());
        typProvides.set(ns.asName.toname(), remoteTyp);
        break;
      }
      case 's-local-ref': {
        const tb = mapGetValue(env.typeBindings, ns.name.key());
        const typ = CS.isTbNone(tb.typ) ? new T.TTop(ns.l, false) : tb.typ.typ;
        typProvides.set(nonNull(ns.asName).toname(), typ);
        break;
      }
      default:
        throw raise('Unknown NameSpec in getNamedProvides: ' + (ns as any).$name);
    }
  }

  const dpSpecs = provideSpecs.filter(A.isSProvideData);
  const dataProvides: Map<string, CS.DataExport> = new Map();
  for (const d of dpSpecs) {
    const ns = d.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const originName = ns.name.toname();
        const dataExport = compileEnv.datatypeByUriValue(ns.uri, originName);
        const origin = dataExport.origin;
        const correctedOrigin = new CS.BindOrigin(
          (ns.asName as any).l,
          origin.definitionBindSite,
          false, // NOTE(joe/ben): This seems like it ought to be false,
                 // but writing a test where that matters isn't really
                 // doable, so it could also be origin.new-definition
                 // without changing behavior
          origin.uriOfDefinition,
          origin.originalName);
        dataProvides.set(ns.asName.toname(), new CS.DAlias(correctedOrigin, originName));

        // TODO(joe): do remote lookup here to get a better location than SL.builtin for the origin
        break;
      }
      case 's-local-ref': {
        const exp = asVariant(mapGetValue(env.datatypes, ns.name.toname()), A.SDataExpr);
        dataProvides.set(nonNull(ns.asName).toname(),
          new CS.DType(new CS.BindOrigin(ns.l, exp.l, true, uri, ns.name), dataExprToDatatype(exp)));
        break;
      }
      default:
        throw raise('Unknown NameSpec in getNamedProvides: ' + (ns as any).$name);
    }
  }

  const provs = new CS.Provides(
    uri,
    modProvides,
    valProvides,
    typProvides,
    dataProvides);
  return provs;
}

export function canonicalizeMembers(ms: T.TypeMembers, uri: URI, tn: NameChanger): T.TypeMembers {
  return T.typeMemberMap(ms, (_: string, typ: T.Type) => canonicalizeNames(typ, uri, tn));
}

export function canonicalizeFields(ms: T.VariantField[], uri: URI, tn: NameChanger): T.VariantField[] {
  return ms.map(([name, typ]): T.VariantField => [name, canonicalizeNames(typ, uri, tn)]);
}

export function canonicalizeVariant(v: T.TypeVariant, uri: URI, tn: NameChanger): T.TypeVariant {
  const c = (ms: T.TypeMembers): T.TypeMembers => canonicalizeMembers(ms, uri, tn);
  switch (v.$name) {
    case 't-variant':
      return new T.TVariant(v.name, canonicalizeFields(v.fields, uri, tn), c(v.withFields), v.l);
    case 't-singleton-variant':
      return new T.TSingletonVariant(v.name, c(v.withFields), v.l);
    default:
      throw raise('Unknown TypeVariant in canonicalizeVariant: ' + (v as any).$name);
  }
}

export function canonicalizeDataExport(de: CS.DataExport, uri: URI, tn: NameChanger): CS.DataExport {
  switch (de.$name) {
    case 'd-alias': return de;
    case 'd-type': return new CS.DType(de.origin, canonicalizeDataType(de.typ, uri, tn));
    default:
      throw raise('Unknown DataExport in canonicalizeDataExport: ' + (de as any).$name);
  }
}

export function canonicalizeDataType(dtyp: T.DataType, uri: URI, tn: NameChanger): T.DataType {
  return new T.TData(
    dtyp.name,
    dtyp.params,
    dtyp.variants.map((v: T.TypeVariant) => canonicalizeVariant(v, uri, tn)),
    canonicalizeMembers(dtyp.fields, uri, tn),
    dtyp.l);
}

// TODO(MATT): add all the correct cases to this
export function canonicalizeNames(typ: T.Type, uri: URI, transformName: NameChanger): T.Type {
  const c = (t: T.Type): T.Type => canonicalizeNames(t, uri, transformName);
  switch (typ.$name) {
    case 't-name': return transformName(typ);
    case 't-var': return typ;
    case 't-arrow': return new T.TArrow(typ.args.map(c), c(typ.ret), typ.l, typ.inferred);
    case 't-tuple': return new T.TTuple(typ.elts.map(c), typ.l, typ.inferred);
    case 't-app': return new T.TApp(c(typ.onto), typ.args.map(c), typ.l, typ.inferred);
    case 't-top': return new T.TTop(typ.l, typ.inferred);
    case 't-bot': return new T.TBot(typ.l, typ.inferred);
    case 't-record':
      return new T.TRecord(canonicalizeMembers(typ.fields, uri, transformName), typ.l, typ.inferred);
    case 't-forall': return new T.TForall(typ.introduces.map(c), c(typ.onto), typ.l, typ.inferred);
    case 't-ref': return new T.TRef(c(typ.typ), typ.l, typ.inferred);
    case 't-data-refinement':
      return new T.TDataRefinement(c(typ.dataType), typ.variantName, typ.l, typ.inferred);
    case 't-existential': return typ;
    default:
      throw raise('Unknown Type in canonicalizeNames: ' + (typ as any).$name);
  }
}

export function canonicalizeValueExport(ve: CS.ValueExport, uri: URI, tn: NameChanger): CS.ValueExport {
  switch (ve.$name) {
    case 'v-alias': return new CS.VAlias(ve.origin, ve.originalName);
    case 'v-just-type': return new CS.VJustType(ve.origin, canonicalizeNames(ve.t, uri, tn));
    case 'v-var': return new CS.VVar(ve.origin, canonicalizeNames(ve.t, uri, tn));
    case 'v-fun': return new CS.VFun(ve.origin, canonicalizeNames(ve.t, uri, tn), ve.name, ve.flatness);
    default:
      throw raise('Unknown ValueExport in canonicalizeValueExport: ' + (ve as any).$name);
  }
}

export function findMod(compileEnv: CS.CompileEnvironment, uri: URI): string | undefined {
  for (const depkey of [...compileEnv.myModules.keys()].sort()) {
    const otherUri = mapGetValue(compileEnv.myModules, depkey);
    if (otherUri === uri) { return depkey; }
  }
  return undefined;
}

export function transformDictHelper<V>(canonicalizer: (v: V, uri: URI, tn: NameChanger) => V):
    (d: Map<string, V>, uri: URI, transformer: NameChanger) => Map<string, V> {
  return (d: Map<string, V>, uri: URI, transformer: NameChanger): Map<string, V> => {
    const s: Map<string, V> = new Map();
    for (const v of d.keys()) {
      s.set(v, canonicalizer(mapGetValue(d, v), uri, transformer));
    }
    return s;
  };
}

export const transformValueDict = transformDictHelper<CS.ValueExport>(canonicalizeValueExport);
export const transformDict = transformDictHelper<T.Type>(canonicalizeNames);
export const transformDataDict = transformDictHelper<CS.DataExport>(canonicalizeDataExport);

export function transformProvides(provides: CS.Provides, compileEnv: CS.CompileEnvironment, transformer: NameChanger): CS.Provides {
  const newVals = transformValueDict(provides.values, provides.fromUri, transformer);
  const newAliases = transformDict(provides.aliases, provides.fromUri, transformer);
  const newDataDefinitions = transformDataDict(provides.dataDefinitions, provides.fromUri, transformer);
  return new CS.Provides(provides.fromUri, provides.modules, newVals, newAliases, newDataDefinitions);
}

/**
 * Produces a new provides data structure that has no `dependency` NameOrigins
 * in the types, by looking up dependencies in the compile environment.
 * Also produces an error if there is a module URI or dependency that is not
 * mentioned in the compile-env.
 */
export function canonicalizeProvides(provides: CS.Provides, compileEnv: CS.CompileEnvironment): CS.Provides {
  const transformer: NameChanger = (t: T.TName): T.Type => {
    const origin = t.moduleName;
    switch (origin.$name) {
      case 'local': return new T.TName(new T.ModuleUri(provides.fromUri), t.id, t.l, t.inferred);
      case 'module-uri': return t;
      case 'dependency': {
        const providesForD = compileEnv.providesByDepKey(origin.dep);
        if (providesForD !== undefined) {
          return new T.TName(moduleUri(providesForD.fromUri), t.id, t.l, t.inferred);
        } else {
          return raise('Unknown module dependency for type: ' + String(t) + ' in provides for ' + provides.fromUri);
        }
      }
      default:
        throw raise('Unknown NameOrigin in canonicalizeProvides: ' + (origin as any).$name);
    }
  };
  return transformProvides(provides, compileEnv, transformer);
}

/**
 * Produces a new provides data structure that has no `module-uri` NameOrigins
 * in the types, by looking up uris in the compile environment, or using `local`.
 *
 * Also produces an error if there is a module URI or dependency that is not
 * mentioned in the compile-env.
 */
export function localizeProvides(provides: CS.Provides, compileEnv: CS.CompileEnvironment): CS.Provides {
  const transformer: NameChanger = (t: T.TName): T.Type => {
    const origin = t.moduleName;
    switch (origin.$name) {
      case 'local': return t;
      case 'module-uri':
        if (origin.uri === provides.fromUri) {
          return new T.TName(T.local, t.id, t.l, t.inferred);
        } else {
          return t;
        }
      case 'dependency': {
        const providesForD = compileEnv.myModules.get(origin.dep);
        if (providesForD !== undefined) {
          return t;
        } else {
          return raise('Unknown module dependency for type: ' + String(t) + ' in provides for ' + provides.fromUri);
        }
      }
      default:
        throw raise('Unknown NameOrigin in localizeProvides: ' + (origin as any).$name);
    }
  };
  return transformProvides(provides, compileEnv, transformer);
}

// TODO(MATT): this does not actually get the provided module values
// NOTE: `typed` is TCS.Typed from type-check-structs, which is not yet ported;
// it is accessed structurally (typed.ast, typed.info.types, typed.info.aliases,
// typed.info.dataTypes, all keyed Maps).
export function getTypedProvides(resolved: CS.NameResolution, typed: any, uri: URI, compileEnv: CS.CompileEnvironment): CS.Provides {
  const transformer = (t: T.TName): T.Type => {
    if (T.isTName(t)) {
      return new T.TName(t.moduleName, new A.STypeGlobal(t.id.toname()), t.l, t.inferred);
    } else {
      return t;
    }
  };
  const c = (typ: T.Type): T.Type => canonicalizeNames(typ, uri, transformer);
  const env = asVariant(resolved.env, CS.ComputedEnv);
  const provideBlocks = (typed.ast as A.Program).provides;
  const pb = provideBlocks[0];
  const provideSpecs = pb.specs;

  const mpSpecs = provideSpecs.filter(A.isSProvideModule);
  const modProvides: Map<string, URI> = new Map();
  for (const m of mpSpecs) {
    const ns = m.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const modInfo = compileEnv.providesByUriValue(ns.uri);
        const modUri = mapGetValue(modInfo.modules, ns.name.toname());
        modProvides.set(ns.asName.toname(), modUri);
        break;
      }
      case 's-local-ref': {
        const mb = mapGetValue(env.moduleBindings, ns.name.key());
        modProvides.set(nonNull(ns.asName).toname(), mb.uri);
        break;
      }
      default:
        throw raise('All provides should be resolved to local or remote refs');
    }
  }

  const vpSpecs = provideSpecs.filter(A.isSProvideName);
  const valProvides: Map<string, CS.ValueExport> = new Map();
  for (const v of vpSpecs) {
    const ns = v.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const originName = ns.name.toname();
        const valExport = compileEnv.valueByUriValue(ns.uri, originName);
        valProvides.set(ns.asName.toname(), new CS.VAlias(valExport.origin, originName));
        break;
      }
      case 's-local-ref': {
        const asName = nonNull(ns.asName);
        const tcTyp = mapGetValue(typed.info.types as Map<string, T.Type>, ns.name.key());
        const vb = mapGetValue(env.bindings, ns.name.key());
        const correctedOrigin = new CS.BindOrigin(
          (asName as any).l,
          vb.origin.definitionBindSite,
          vb.origin.newDefinition,
          vb.origin.uriOfDefinition,
          asName);
        // TODO(joe): Still have v-var questions here
        valProvides.set(asName.toname(), new CS.VJustType(correctedOrigin, c(tcTyp)));
        break;
      }
      default:
        throw raise('Unknown NameSpec in getTypedProvides: ' + (ns as any).$name);
    }
  }

  const tpSpecs = provideSpecs.filter(A.isSProvideType);
  const typProvides: Map<string, T.Type> = new Map();
  for (const t of tpSpecs) {
    const ns = t.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const remoteTyp = compileEnv.typeByUriValue(ns.uri, ns.name.toname());
        typProvides.set(ns.asName.toname(), remoteTyp);
        break;
      }
      case 's-local-ref': {
        const key = ns.name.key();
        const dataTyp = (typed.info.dataTypes as Map<string, T.Type>).get(key);
        if (dataTyp !== undefined) {
          // NOTE: the Pyret source says `tp.set(name, c(typ))`, using the Name
          // itself as the string-dict key; we use its string form.
          typProvides.set(ns.name.toname(), c(dataTyp));
        } else {
          const typ = mapGetValue(typed.info.aliases as Map<string, T.Type>, key);
          typProvides.set(nonNull(ns.asName).toname(), c(typ));
        }
        break;
      }
      default:
        throw raise('Unknown NameSpec in getTypedProvides: ' + (ns as any).$name);
    }
  }

  const dpSpecs = provideSpecs.filter(A.isSProvideData);
  const dataProvides: Map<string, CS.DataExport> = new Map();
  for (const d of dpSpecs) {
    const ns = d.nameSpec;
    switch (ns.$name) {
      case 's-remote-ref': {
        const originName = ns.name.toname();
        const dataExport = compileEnv.datatypeByUriValue(ns.uri, originName);
        const origin = dataExport.origin;
        dataProvides.set(ns.asName.toname(), new CS.DAlias(origin, originName));
        break;
      }
      case 's-local-ref': {
        const exp = asVariant(mapGetValue(env.datatypes, ns.name.toname()), A.SDataExpr);
        const origin = new CS.BindOrigin(ns.l, exp.l, true, uri, ns.name);
        dataProvides.set(nonNull(ns.asName).toname(),
          new CS.DType(origin,
            canonicalizeDataType(mapGetValue(typed.info.dataTypes as Map<string, T.DataType>, exp.namet.key()), uri, transformer)));
        break;
      }
      default:
        throw raise('Unknown NameSpec in getTypedProvides: ' + (ns as any).$name);
    }
  }

  const provs = new CS.Provides(
    uri,
    modProvides,
    valProvides,
    typProvides,
    dataProvides);
  return provs;
}
