// Port of the default visitors from src/arr/trove/ast.arr (lines 1880-3739):
// default-map-visitor, default-iter-visitor, dummy-loc-visitor.
// Visitor methods receive the node itself (see CONVENTIONS.md), not spread fields.

import * as A from './ast';
import { Loc, dummyLoc } from './srcloc';
import { InternalCompilerError } from './shared';

export class DefaultMapVisitor {
  protected option<T extends { visit(v: any): any }>(x: T | undefined): any {
    return x === undefined ? undefined : x.visit(this);
  }

  sUnderscore(node: A.SUnderscore): A.Name {
    return new A.SUnderscore(node.l);
  }

  sName(node: A.SName): A.Name {
    return new A.SName(node.l, node.s);
  }

  sTypeGlobal(node: A.STypeGlobal): A.Name {
    return new A.STypeGlobal(node.s);
  }

  sModuleGlobal(node: A.SModuleGlobal): A.Name {
    return new A.SModuleGlobal(node.s);
  }

  sGlobal(node: A.SGlobal): A.Name {
    return new A.SGlobal(node.s);
  }

  sAtom(node: A.SAtom): A.Name {
    return new A.SAtom(node.base, node.serial);
  }

  sStar(node: A.SStar): A.NameSpec {
    return new A.SStar(node.l, node.hidden.map(h => h.visit(this)));
  }

  sModuleRef(node: A.SModuleRef): A.NameSpec {
    return new A.SModuleRef(node.l, node.path.map(p => p.visit(this)), this.option(node.asName));
  }

  sLocalRef(node: A.SLocalRef): A.NameSpec {
    return new A.SLocalRef(node.l, node.name.visit(this), node.asName.visit(this));
  }

  sRemoteRef(node: A.SRemoteRef): A.NameSpec {
    return new A.SRemoteRef(node.l, node.uri, node.name.visit(this), node.asName.visit(this));
  }

  sDefinedModule(node: A.SDefinedModule): A.DefinedModule {
    return new A.SDefinedModule(node.name, node.value.visit(this), node.uri);
  }

  sDefinedValue(node: A.SDefinedValue): A.DefinedValue {
    return new A.SDefinedValue(node.name, node.value.visit(this));
  }

  sDefinedVar(node: A.SDefinedVar): A.DefinedValue {
    return new A.SDefinedVar(node.name, node.id.visit(this));
  }

  sDefinedType(node: A.SDefinedType): A.DefinedType {
    return new A.SDefinedType(node.name, node.typ.visit(this));
  }

  sModule(node: A.SModule): A.Expr {
    return new A.SModule(node.l, node.answer.visit(this), node.definedModules.map(dm => dm.visit(this)), node.definedValues.map(dv => dv.visit(this)), node.definedTypes.map(dt => dt.visit(this)), node.checks.visit(this));
  }

  sProgram(node: A.SProgram): A.Program {
    return new A.SProgram(node.l, this.option(node._use), node._provide.visit(this), node.providedTypes.visit(this), node.provides.map(p => p.visit(this)), node.imports.map(i => i.visit(this)), node.block.visit(this));
  }

  sUse(node: A.SUse): A.Use {
    return new A.SUse(node.l, node.n.visit(this), node.mod.visit(this));
  }

  sIncludeFrom(node: A.SIncludeFrom): A.Import {
    return new A.SIncludeFrom(node.l, node.mod.map(m => m.visit(this)), node.specs.map(s => s.visit(this)));
  }

  sIncludeName(node: A.SIncludeName): A.IncludeSpec {
    return new A.SIncludeName(node.l, node.nameSpec.visit(this));
  }

  sIncludeData(node: A.SIncludeData): A.IncludeSpec {
    return new A.SIncludeData(node.l, node.nameSpec.visit(this), node.hidden.map(h => h.visit(this)));
  }

  sIncludeType(node: A.SIncludeType): A.IncludeSpec {
    return new A.SIncludeType(node.l, node.nameSpec.visit(this));
  }

  sIncludeModule(node: A.SIncludeModule): A.IncludeSpec {
    return new A.SIncludeModule(node.l, node.nameSpec.visit(this));
  }

  sInclude(node: A.SInclude): A.Import {
    return new A.SInclude(node.l, node.mod.visit(this));
  }

  sImport(node: A.SImport): A.Import {
    return new A.SImport(node.l, node.file.visit(this), node.name.visit(this));
  }

  sConstImport(node: A.SConstImport): A.ImportType {
    return new A.SConstImport(node.l, node.mod);
  }

  sSpecialImport(node: A.SSpecialImport): A.ImportType {
    return new A.SSpecialImport(node.l, node.kind, node.args);
  }

  sImportTypes(node: A.SImportTypes): A.Import {
    return new A.SImportTypes(node.l, node.file, node.name.visit(this), node.types.visit(this));
  }

  sImportFields(node: A.SImportFields): A.Import {
    return new A.SImportFields(node.l, node.fields.map(f => f.visit(this)), node.file);
  }

  sProvide(node: A.SProvide): A.Provide {
    return new A.SProvide(node.l, node.block.visit(this));
  }

  sProvideAll(node: A.SProvideAll): A.Provide {
    return new A.SProvideAll(node.l);
  }

  sProvideNone(node: A.SProvideNone): A.Provide {
    return new A.SProvideNone(node.l);
  }

  sProvideTypes(node: A.SProvideTypes): A.ProvideTypes {
    return new A.SProvideTypes(node.l, node.ann.map(a => a.visit(this)));
  }

  sProvideTypesAll(node: A.SProvideTypesAll): A.ProvideTypes {
    return new A.SProvideTypesAll(node.l);
  }

  sProvideTypesNone(node: A.SProvideTypesNone): A.ProvideTypes {
    return new A.SProvideTypesNone(node.l);
  }

  sProvideBlock(node: A.SProvideBlock): A.ProvideBlock {
    return new A.SProvideBlock(node.l, node.path.map(p => p.visit(this)), node.specs.map(s => s.visit(this)));
  }

  sProvideName(node: A.SProvideName): A.ProvideSpec {
    return new A.SProvideName(node.l, node.nameSpec.visit(this));
  }

  sProvideData(node: A.SProvideData): A.ProvideSpec {
    return new A.SProvideData(node.l, node.nameSpec.visit(this), node.hidden.map(h => h.visit(this)));
  }

  sProvideType(node: A.SProvideType): A.ProvideSpec {
    return new A.SProvideType(node.l, node.nameSpec.visit(this));
  }

  sProvideModule(node: A.SProvideModule): A.ProvideSpec {
    return new A.SProvideModule(node.l, node.nameSpec.visit(this));
  }

  sBind(node: A.SBind): A.Bind {
    return new A.SBind(node.l, node.shadows, node.id.visit(this), node.ann.visit(this));
  }

  sTupleBind(node: A.STupleBind): A.Bind {
    return new A.STupleBind(node.l, node.fields.map(f => f.visit(this)), this.option(node.asName));
  }

  sVarBind(node: A.SVarBind): A.LetBind {
    return new A.SVarBind(node.l, node.b.visit(this), node.value.visit(this));
  }

  sLetBind(node: A.SLetBind): A.LetBind {
    return new A.SLetBind(node.l, node.b.visit(this), node.value.visit(this));
  }

  sTypeBind(node: A.STypeBind): A.TypeLetBind {
    return new A.STypeBind(node.l, node.name.visit(this), node.params.map(p => p.visit(this)), node.ann.visit(this));
  }

  sNewtypeBind(node: A.SNewtypeBind): A.TypeLetBind {
    return new A.SNewtypeBind(node.l, node.name.visit(this), node.namet.visit(this));
  }

  sTypeLetExpr(node: A.STypeLetExpr): A.Expr {
    return new A.STypeLetExpr(node.l, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sTemplate(node: A.STemplate): A.Expr {
    return new A.STemplate(node.l);
  }

  sLetExpr(node: A.SLetExpr): A.Expr {
    return new A.SLetExpr(node.l, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sLetrecBind(node: A.SLetrecBind): A.LetrecBind {
    return new A.SLetrecBind(node.l, node.b.visit(this), node.value.visit(this));
  }

  sLetrec(node: A.SLetrec): A.Expr {
    return new A.SLetrec(node.l, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sHintExp(node: A.SHintExp): A.Expr {
    return new A.SHintExp(node.l, node.hints, node.exp.visit(this));
  }

  sInstantiate(node: A.SInstantiate): A.Expr {
    return new A.SInstantiate(node.l, node.expr.visit(this), node.params.map(p => p.visit(this)));
  }

  sBlock(node: A.SBlock): A.Expr {
    return new A.SBlock(node.l, node.stmts.map(s => s.visit(this)));
  }

  sScopeLet(node: A.SScopeLet): A.ScopeEntry {
    return new A.SScopeLet(node.l, node.binds.map(b => b.visit(this)));
  }

  sScopeTypeLet(node: A.SScopeTypeLet): A.ScopeEntry {
    return new A.SScopeTypeLet(node.l, node.binds.map(b => b.visit(this)));
  }

  sScopeLetrec(node: A.SScopeLetrec): A.ScopeEntry {
    return new A.SScopeLetrec(node.l, node.binds.map(b => b.visit(this)));
  }

  sScopeStmt(node: A.SScopeStmt): A.ScopeEntry {
    return new A.SScopeStmt(node.l, node.stmt.visit(this));
  }

  sScopeBlock(node: A.SScopeBlock): A.Expr {
    return new A.SScopeBlock(node.l, node.entries.map(e => e.visit(this)), node.tail.visit(this));
  }

  sUserBlock(node: A.SUserBlock): A.Expr {
    return new A.SUserBlock(node.l, node.body.visit(this));
  }

  sFun(node: A.SFun): A.Expr {
    return new A.SFun(node.l, node.name, node.params, node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._checkLoc, this.option(node._check), node.blocky);
  }

  sType(node: A.SType): A.Expr {
    return new A.SType(node.l, node.name.visit(this), node.params.map(p => p.visit(this)), node.ann.visit(this));
  }

  sNewtype(node: A.SNewtype): A.Expr {
    return new A.SNewtype(node.l, node.name.visit(this), node.namet.visit(this));
  }

  sVar(node: A.SVar): A.Expr {
    return new A.SVar(node.l, node.name.visit(this), node.value.visit(this));
  }

  sRec(node: A.SRec): A.Expr {
    return new A.SRec(node.l, node.name.visit(this), node.value.visit(this));
  }

  sLet(node: A.SLet): A.Expr {
    return new A.SLet(node.l, node.name.visit(this), node.value.visit(this), node.keywordVal);
  }

  sRef(node: A.SRef): A.Expr {
    return new A.SRef(node.l, this.option(node.ann));
  }

  sWhen(node: A.SWhen): A.Expr {
    return new A.SWhen(node.l, node.test.visit(this), node.block.visit(this), node.blocky);
  }

  sContract(node: A.SContract): A.Expr {
    return new A.SContract(node.l, node.name.visit(this), node.params.map(p => p.visit(this)), node.ann.visit(this));
  }

  sAssign(node: A.SAssign): A.Expr {
    return new A.SAssign(node.l, node.id.visit(this), node.value.visit(this));
  }

  sIfBranch(node: A.SIfBranch): A.IfBranch {
    return new A.SIfBranch(node.l, node.test.visit(this), node.body.visit(this));
  }

  sIfPipeBranch(node: A.SIfPipeBranch): A.IfPipeBranch {
    return new A.SIfPipeBranch(node.l, node.test.visit(this), node.body.visit(this));
  }

  sIf(node: A.SIf): A.Expr {
    return new A.SIf(node.l, node.branches.map(b => b.visit(this)), node.blocky);
  }

  sIfElse(node: A.SIfElse): A.Expr {
    return new A.SIfElse(node.l, node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sIfPipe(node: A.SIfPipe): A.Expr {
    return new A.SIfPipe(node.l, node.branches.map(b => b.visit(this)), node.blocky);
  }

  sIfPipeElse(node: A.SIfPipeElse): A.Expr {
    return new A.SIfPipeElse(node.l, node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sCasesBind(node: A.SCasesBind): A.CasesBind {
    return new A.SCasesBind(node.l, node.fieldType, node.bind.visit(this));
  }

  sCasesBranch(node: A.SCasesBranch): A.CasesBranch {
    return new A.SCasesBranch(node.l, node.patLoc, node.name, node.args.map(a => a.visit(this)), node.body.visit(this));
  }

  sSingletonCasesBranch(node: A.SSingletonCasesBranch): A.CasesBranch {
    return new A.SSingletonCasesBranch(node.l, node.patLoc, node.name, node.body.visit(this));
  }

  sCases(node: A.SCases): A.Expr {
    return new A.SCases(node.l, node.typ.visit(this), node.val.visit(this), node.branches.map(b => b.visit(this)), node.blocky);
  }

  sCasesElse(node: A.SCasesElse): A.Expr {
    return new A.SCasesElse(node.l, node.typ.visit(this), node.val.visit(this), node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sOp(node: A.SOp): A.Expr {
    return new A.SOp(node.l, node.opL, node.op, node.left.visit(this), node.right.visit(this));
  }

  sCheckTest(node: A.SCheckTest): A.Expr {
    return new A.SCheckTest(node.l, node.op.visit(this), this.option(node.refinement), node.left.visit(this), this.option(node.right), this.option(node.cause));
  }

  sOpIs(node: A.SOpIs): A.CheckOp { return new A.SOpIs(node.l); }
  sOpIsRoughly(node: A.SOpIsRoughly): A.CheckOp { return new A.SOpIsRoughly(node.l); }
  sOpIsNotRoughly(node: A.SOpIsNotRoughly): A.CheckOp { return new A.SOpIsNotRoughly(node.l); }
  sOpIsOp(node: A.SOpIsOp): A.CheckOp { return new A.SOpIsOp(node.l, node.op); }
  sOpIsNot(node: A.SOpIsNot): A.CheckOp { return new A.SOpIsNot(node.l); }
  sOpIsNotOp(node: A.SOpIsNotOp): A.CheckOp { return new A.SOpIsNotOp(node.l, node.op); }
  sOpSatisfies(node: A.SOpSatisfies): A.CheckOp { return new A.SOpSatisfies(node.l); }
  sOpSatisfiesNot(node: A.SOpSatisfiesNot): A.CheckOp { return new A.SOpSatisfiesNot(node.l); }
  sOpRaises(node: A.SOpRaises): A.CheckOp { return new A.SOpRaises(node.l); }
  sOpRaisesOther(node: A.SOpRaisesOther): A.CheckOp { return new A.SOpRaisesOther(node.l); }
  sOpRaisesNot(node: A.SOpRaisesNot): A.CheckOp { return new A.SOpRaisesNot(node.l); }
  sOpRaisesSatisfies(node: A.SOpRaisesSatisfies): A.CheckOp { return new A.SOpRaisesSatisfies(node.l); }
  sOpRaisesViolates(node: A.SOpRaisesViolates): A.CheckOp { return new A.SOpRaisesViolates(node.l); }

  sCheckExpr(node: A.SCheckExpr): A.Expr {
    return new A.SCheckExpr(node.l, node.expr.visit(this), node.ann.visit(this));
  }

  sParen(node: A.SParen): A.Expr {
    return new A.SParen(node.l, node.expr.visit(this));
  }

  sLam(node: A.SLam): A.Expr {
    return new A.SLam(node.l, node.name, node.params.map(p => p.visit(this)), node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._checkLoc, this.option(node._check), node.blocky);
  }

  sMethod(node: A.SMethod): A.Expr {
    return new A.SMethod(node.l, node.name, node.params.map(p => p.visit(this)), node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._checkLoc, this.option(node._check), node.blocky);
  }

  sExtend(node: A.SExtend): A.Expr {
    return new A.SExtend(node.l, node.supe.visit(this), node.fields.map(f => f.visit(this)));
  }

  sUpdate(node: A.SUpdate): A.Expr {
    return new A.SUpdate(node.l, node.supe.visit(this), node.fields.map(f => f.visit(this)));
  }

  sTuple(node: A.STuple): A.Expr {
    return new A.STuple(node.l, node.fields.map(f => f.visit(this)));
  }

  sTupleGet(node: A.STupleGet): A.Expr {
    return new A.STupleGet(node.l, node.tup.visit(this), node.index, node.indexLoc);
  }

  sObj(node: A.SObj): A.Expr {
    return new A.SObj(node.l, node.fields.map(f => f.visit(this)));
  }

  sArray(node: A.SArray): A.Expr {
    return new A.SArray(node.l, node.values.map(v => v.visit(this)));
  }

  sConstruct(node: A.SConstruct): A.Expr {
    return new A.SConstruct(node.l, node.modifier, node.constructorVal.visit(this), node.values.map(v => v.visit(this)));
  }

  sReactor(node: A.SReactor): A.Expr {
    return new A.SReactor(node.l, node.fields.map(f => f.visit(this)));
  }

  sTable(node: A.STable): A.Expr {
    return new A.STable(node.l, node.headers.map(h => h.visit(this)), node.rows.map(r => r.visit(this)));
  }

  sTableRow(node: A.STableRow): A.TableRow {
    return new A.STableRow(node.l, node.elems.map(e => e.visit(this)));
  }

  sLoadTable(node: A.SLoadTable): A.Expr {
    return new A.SLoadTable(node.l, node.headers.map(h => h.visit(this)), node.spec.map(s => s.visit(this)));
  }

  sFieldName(node: A.SFieldName): A.FieldName {
    return new A.SFieldName(node.l, node.name, node.ann.visit(this));
  }

  sApp(node: A.SApp): A.Expr {
    return new A.SApp(node.l, node._fun.visit(this), node.args.map(a => a.visit(this)));
  }

  sAppEnriched(node: A.SAppEnriched): A.Expr {
    return new A.SAppEnriched(node.l, node._fun.visit(this), node.args.map(a => a.visit(this)), node.appInfo);
  }

  sPrimApp(node: A.SPrimApp): A.Expr {
    return new A.SPrimApp(node.l, node._fun, node.args.map(a => a.visit(this)), node.appInfo);
  }

  sPrimVal(node: A.SPrimVal): A.Expr {
    return new A.SPrimVal(node.l, node.name);
  }

  sId(node: A.SId): A.Expr {
    return new A.SId(node.l, node.id.visit(this));
  }

  sIdVar(node: A.SIdVar): A.Expr {
    return new A.SIdVar(node.l, node.id.visit(this));
  }

  sIdLetrec(node: A.SIdLetrec): A.Expr {
    return new A.SIdLetrec(node.l, node.id.visit(this), node.safe);
  }

  sIdVarModref(node: A.SIdVarModref): A.Expr {
    return new A.SIdVarModref(node.l, node.id.visit(this), node.uri, node.name);
  }

  sIdModref(node: A.SIdModref): A.Expr {
    return new A.SIdModref(node.l, node.id.visit(this), node.uri, node.name);
  }

  // The Pyret source says `s-undefined(self)` here, which is a latent bug; the
  // intended behavior (preserve the loc) is ported.
  sUndefined(node: A.SUndefined): A.Expr {
    return new A.SUndefined(node.l);
  }

  sSrcloc(node: A.SSrcloc): A.Expr {
    return new A.SSrcloc(node.l, node.loc);
  }

  sNum(node: A.SNum): A.Expr {
    return new A.SNum(node.l, node.n);
  }

  sFrac(node: A.SFrac): A.Expr {
    return new A.SFrac(node.l, node.num, node.den);
  }

  sRfrac(node: A.SRfrac): A.Expr {
    return new A.SRfrac(node.l, node.num, node.den);
  }

  sBool(node: A.SBool): A.Expr {
    return new A.SBool(node.l, node.b);
  }

  sStr(node: A.SStr): A.Expr {
    return new A.SStr(node.l, node.s);
  }

  sDot(node: A.SDot): A.Expr {
    return new A.SDot(node.l, node.obj.visit(this), node.field);
  }

  sGetBang(node: A.SGetBang): A.Expr {
    return new A.SGetBang(node.l, node.obj.visit(this), node.field);
  }

  sBracket(node: A.SBracket): A.Expr {
    return new A.SBracket(node.l, node.obj.visit(this), node.key.visit(this));
  }

  sData(node: A.SData): A.Expr {
    return new A.SData(
      node.l,
      node.name,
      node.params.map(p => p.visit(this)),
      node.mixins.map(m => m.visit(this)),
      node.variants.map(v => v.visit(this)),
      node.sharedMembers.map(s => s.visit(this)),
      node._checkLoc,
      this.option(node._check)
    );
  }

  sDataExpr(node: A.SDataExpr): A.Expr {
    return new A.SDataExpr(
      node.l,
      node.name,
      node.namet.visit(this),
      node.params.map(p => p.visit(this)),
      node.mixins.map(m => m.visit(this)),
      node.variants.map(v => v.visit(this)),
      node.sharedMembers.map(s => s.visit(this)),
      node._checkLoc,
      this.option(node._check)
    );
  }

  sFor(node: A.SFor): A.Expr {
    return new A.SFor(node.l, node.iterator.visit(this), node.bindings.map(b => b.visit(this)), node.ann.visit(this), node.body.visit(this), node.blocky);
  }

  sCheck(node: A.SCheck): A.Expr {
    return new A.SCheck(node.l, node.name, node.body.visit(this), node.keywordCheck);
  }

  sDataField(node: A.SDataField): A.Member {
    return new A.SDataField(node.l, node.name, node.value.visit(this));
  }

  sMutableField(node: A.SMutableField): A.Member {
    return new A.SMutableField(node.l, node.name, node.ann.visit(this), node.value.visit(this));
  }

  sMethodField(node: A.SMethodField): A.Member {
    return new A.SMethodField(
      node.l,
      node.name,
      node.params.map(p => p.visit(this)),
      node.args.map(a => a.visit(this)),
      node.ann.visit(this),
      node.doc,
      node.body.visit(this),
      node._checkLoc,
      this.option(node._check),
      node.blocky
    );
  }

  sForBind(node: A.SForBind): A.ForBind {
    return new A.SForBind(node.l, node.bind.visit(this), node.value.visit(this));
  }

  sColumnBinds(node: A.SColumnBinds): A.ColumnBinds {
    return new A.SColumnBinds(node.l, node.binds.map(b => b.visit(this)), node.table.visit(this));
  }

  sVariantMember(node: A.SVariantMember): A.VariantMember {
    return new A.SVariantMember(node.l, node.memberType, node.bind.visit(this));
  }

  sVariant(node: A.SVariant): A.Variant {
    return new A.SVariant(node.l, node.constrLoc, node.name, node.members.map(m => m.visit(this)), node.withMembers.map(w => w.visit(this)));
  }

  sSingletonVariant(node: A.SSingletonVariant): A.Variant {
    return new A.SSingletonVariant(node.l, node.name, node.withMembers.map(w => w.visit(this)));
  }

  sColumnSort(node: A.SColumnSort): A.ColumnSort {
    return new A.SColumnSort(node.l, node.column.visit(this), node.direction);
  }

  sTableExtend(node: A.STableExtend): A.Expr {
    return new A.STableExtend(node.l, node.columnBinds.visit(this), node.extensions.map(e => e.visit(this)));
  }

  sTableUpdate(node: A.STableUpdate): A.Expr {
    return new A.STableUpdate(node.l, node.columnBinds.visit(this), node.updates.map(u => u.visit(this)));
  }

  sTableFilter(node: A.STableFilter): A.Expr {
    return new A.STableFilter(node.l, node.columnBinds.visit(this), node.predicate.visit(this));
  }

  sTableSelect(node: A.STableSelect): A.Expr {
    return new A.STableSelect(node.l, node.columns.map(c => c.visit(this)), node.table.visit(this));
  }

  sTableOrder(node: A.STableOrder): A.Expr {
    return new A.STableOrder(node.l, node.table.visit(this), node.ordering.map(o => o.visit(this)));
  }

  sTableExtract(node: A.STableExtract): A.Expr {
    return new A.STableExtract(node.l, node.column.visit(this), node.table.visit(this));
  }

  sTableExtendField(node: A.STableExtendField): A.TableExtendField {
    return new A.STableExtendField(node.l, node.name, node.value.visit(this), node.ann.visit(this));
  }

  sTableExtendReducer(node: A.STableExtendReducer): A.TableExtendField {
    return new A.STableExtendReducer(node.l, node.name, node.reducer.visit(this), node.col.visit(this), node.ann.visit(this));
  }

  sSanitize(node: A.SSanitize): A.LoadTableSpec {
    return new A.SSanitize(node.l, node.name.visit(this), node.sanitizer.visit(this));
  }

  sTableSrc(node: A.STableSrc): A.LoadTableSpec {
    return new A.STableSrc(node.l, node.src.visit(this));
  }

  sSpyBlock(node: A.SSpyBlock): A.Expr {
    return new A.SSpyBlock(node.l, this.option(node.message), node.contents.map(c => c.visit(this)));
  }

  sSpyExpr(node: A.SSpyExpr): A.SpyField {
    return new A.SSpyExpr(node.l, node.name, node.value.visit(this), node.implicitLabel);
  }

  aBlank(node: A.ABlank): A.Ann { return node; }
  aAny(node: A.AAny): A.Ann { return new A.AAny(node.l); }
  aName(node: A.AName): A.Ann { return new A.AName(node.l, node.id.visit(this)); }
  aTypeVar(node: A.ATypeVar): A.Ann { return new A.ATypeVar(node.l, node.id.visit(this)); }
  aArrow(node: A.AArrow): A.Ann {
    return new A.AArrow(node.l, node.args.map(a => a.visit(this)), node.ret.visit(this), node.useParens);
  }
  aArrowArgnames(node: A.AArrowArgnames): A.Ann {
    return new A.AArrowArgnames(node.l, node.args.map(a => a.visit(this)), node.ret.visit(this), node.useParens);
  }
  aMethod(node: A.AMethod): A.Ann {
    return new A.AMethod(node.l, node.args.map(a => a.visit(this)), node.ret.visit(this));
  }
  aRecord(node: A.ARecord): A.Ann {
    return new A.ARecord(node.l, node.fields.map(f => f.visit(this)));
  }
  aTuple(node: A.ATuple): A.Ann {
    return new A.ATuple(node.l, node.fields.map(f => f.visit(this)));
  }
  aApp(node: A.AApp): A.Ann {
    return new A.AApp(node.l, node.ann.visit(this), node.args.map(a => a.visit(this)));
  }
  aPred(node: A.APred): A.Ann {
    return new A.APred(node.l, node.ann.visit(this), node.exp.visit(this));
  }
  aDot(node: A.ADot): A.Ann {
    return new A.ADot(node.l, node.obj.visit(this), node.field);
  }
  aField(node: A.AField): A.AField {
    return new A.AField(node.l, node.name, node.ann.visit(this));
  }
}

export class DefaultIterVisitor {
  protected option<T extends { visit(v: any): any }>(x: T | undefined): boolean {
    return x === undefined ? true : x.visit(this);
  }

  sUnderscore(node: A.SUnderscore): boolean {
    return true;
  }

  sName(node: A.SName): boolean {
    return true;
  }

  sGlobal(node: A.SGlobal): boolean {
    return true;
  }

  sTypeGlobal(node: A.STypeGlobal): boolean {
    return true;
  }

  sModuleGlobal(node: A.SModuleGlobal): boolean {
    return true;
  }

  sAtom(node: A.SAtom): boolean {
    return true;
  }

  sStar(node: A.SStar): boolean {
    return node.hidden.every(h => h.visit(this));
  }

  sModuleRef(node: A.SModuleRef): boolean {
    return node.path.every(p => p.visit(this)) && this.option(node.asName);
  }

  sLocalRef(node: A.SLocalRef): boolean {
    return node.name.visit(this) && node.asName.visit(this);
  }

  sRemoteRef(node: A.SRemoteRef): boolean {
    return node.name.visit(this) && node.asName.visit(this);
  }

  sDefinedModule(node: A.SDefinedModule): boolean {
    return node.value.visit(this);
  }

  sDefinedValue(node: A.SDefinedValue): boolean {
    return node.value.visit(this);
  }

  sDefinedVar(node: A.SDefinedVar): boolean {
    return node.id.visit(this);
  }

  sDefinedType(node: A.SDefinedType): boolean {
    return node.typ.visit(this);
  }

  sModule(node: A.SModule): boolean {
    return node.answer.visit(this) && node.definedModules.every(dm => dm.visit(this)) && node.definedValues.every(dv => dv.visit(this)) && node.definedTypes.every(dt => dt.visit(this)) && node.checks.visit(this);
  }

  sProgram(node: A.SProgram): boolean {
    return this.option(node._use)
      && node._provide.visit(this)
      && node.providedTypes.visit(this)
      && node.provides.every(p => p.visit(this))
      && node.imports.every(i => i.visit(this))
      && node.block.visit(this);
  }

  sUse(node: A.SUse): boolean {
    return node.n.visit(this) && node.mod.visit(this);
  }

  sImport(node: A.SImport): boolean {
    return node.file.visit(this) && node.name.visit(this);
  }

  sInclude(node: A.SInclude): boolean {
    return node.mod.visit(this);
  }

  sIncludeFrom(node: A.SIncludeFrom): boolean {
    return node.mod.every(m => m.visit(this)) && node.specs.every(s => s.visit(this));
  }

  sIncludeName(node: A.SIncludeName): boolean {
    return node.nameSpec.visit(this);
  }

  sIncludeData(node: A.SIncludeData): boolean {
    return node.nameSpec.visit(this) && node.hidden.every(h => h.visit(this));
  }

  sIncludeType(node: A.SIncludeType): boolean {
    return node.nameSpec.visit(this);
  }

  sIncludeModule(node: A.SIncludeModule): boolean {
    return node.nameSpec.visit(this);
  }

  sConstImport(node: A.SConstImport): boolean {
    return true;
  }

  sSpecialImport(node: A.SSpecialImport): boolean {
    return true;
  }

  sImportTypes(node: A.SImportTypes): boolean {
    return node.name.visit(this) && node.types.visit(this);
  }

  sImportFields(node: A.SImportFields): boolean {
    return node.fields.every(f => f.visit(this));
  }

  sProvide(node: A.SProvide): boolean {
    return node.block.visit(this);
  }

  sProvideAll(node: A.SProvideAll): boolean {
    return true;
  }

  sProvideNone(node: A.SProvideNone): boolean {
    return true;
  }

  sProvideTypes(node: A.SProvideTypes): boolean {
    return node.ann.every(a => a.visit(this));
  }

  sProvideTypesAll(node: A.SProvideTypesAll): boolean {
    return true;
  }

  sProvideTypesNone(node: A.SProvideTypesNone): boolean {
    return true;
  }

  sProvideBlock(node: A.SProvideBlock): boolean {
    return node.path.every(p => p.visit(this)) && node.specs.every(s => s.visit(this));
  }

  sProvideName(node: A.SProvideName): boolean {
    return node.nameSpec.visit(this);
  }

  sProvideData(node: A.SProvideData): boolean {
    return node.nameSpec.visit(this) && node.hidden.every(h => h.visit(this));
  }

  sProvideType(node: A.SProvideType): boolean {
    return node.nameSpec.visit(this);
  }

  sProvideModule(node: A.SProvideModule): boolean {
    return node.nameSpec.visit(this);
  }

  sTemplate(node: A.STemplate): boolean {
    return true;
  }

  sBind(node: A.SBind): boolean {
    return node.id.visit(this) && node.ann.visit(this);
  }

  sTupleBind(node: A.STupleBind): boolean {
    return node.fields.every(f => f.visit(this)) && this.option(node.asName);
  }

  sVarBind(node: A.SVarBind): boolean {
    return node.b.visit(this) && node.value.visit(this);
  }

  sLetBind(node: A.SLetBind): boolean {
    return node.b.visit(this) && node.value.visit(this);
  }

  sTypeBind(node: A.STypeBind): boolean {
    return node.name.visit(this) && node.ann.visit(this) && node.params.every(p => p.visit(this));
  }

  sNewtypeBind(node: A.SNewtypeBind): boolean {
    return node.name.visit(this) && node.namet.visit(this);
  }

  sTypeLetExpr(node: A.STypeLetExpr): boolean {
    return node.binds.every(b => b.visit(this)) && node.body.visit(this);
  }

  sLetExpr(node: A.SLetExpr): boolean {
    return node.binds.every(b => b.visit(this)) && node.body.visit(this);
  }

  sLetrecBind(node: A.SLetrecBind): boolean {
    return node.b.visit(this) && node.value.visit(this);
  }

  sLetrec(node: A.SLetrec): boolean {
    return node.binds.every(b => b.visit(this)) && node.body.visit(this);
  }

  sHintExp(node: A.SHintExp): boolean {
    return node.exp.visit(this);
  }

  sInstantiate(node: A.SInstantiate): boolean {
    return node.expr.visit(this) && node.params.every(p => p.visit(this));
  }

  sBlock(node: A.SBlock): boolean {
    return node.stmts.every(s => s.visit(this));
  }

  sScopeLet(node: A.SScopeLet): boolean {
    return node.binds.every(b => b.visit(this));
  }

  sScopeTypeLet(node: A.SScopeTypeLet): boolean {
    return node.binds.every(b => b.visit(this));
  }

  sScopeLetrec(node: A.SScopeLetrec): boolean {
    return node.binds.every(b => b.visit(this));
  }

  sScopeStmt(node: A.SScopeStmt): boolean {
    return node.stmt.visit(this);
  }

  sScopeBlock(node: A.SScopeBlock): boolean {
    return node.entries.every(e => e.visit(this)) && node.tail.visit(this);
  }

  sUserBlock(node: A.SUserBlock): boolean {
    return node.body.visit(this);
  }

  sFun(node: A.SFun): boolean {
    return node.params.every(p => p.visit(this))
      && node.args.every(a => a.visit(this)) && node.ann.visit(this) && node.body.visit(this) && this.option(node._check);
  }

  sType(node: A.SType): boolean {
    return node.name.visit(this) && node.ann.visit(this) && node.params.every(p => p.visit(this));
  }

  sNewtype(node: A.SNewtype): boolean {
    return node.name.visit(this) && node.namet.visit(this);
  }

  sVar(node: A.SVar): boolean {
    return node.name.visit(this) && node.value.visit(this);
  }

  sRec(node: A.SRec): boolean {
    return node.name.visit(this) && node.value.visit(this);
  }

  sLet(node: A.SLet): boolean {
    return node.name.visit(this) && node.value.visit(this);
  }

  sRef(node: A.SRef): boolean {
    return this.option(node.ann);
  }

  sWhen(node: A.SWhen): boolean {
    return node.test.visit(this) && node.block.visit(this);
  }

  sContract(node: A.SContract): boolean {
    return node.name.visit(this) && node.params.every(p => p.visit(this)) && node.ann.visit(this);
  }

  sAssign(node: A.SAssign): boolean {
    return node.id.visit(this) && node.value.visit(this);
  }

  sIfBranch(node: A.SIfBranch): boolean {
    return node.test.visit(this) && node.body.visit(this);
  }

  sIfPipeBranch(node: A.SIfPipeBranch): boolean {
    return node.test.visit(this) && node.body.visit(this);
  }

  sIf(node: A.SIf): boolean {
    return node.branches.every(b => b.visit(this));
  }

  sIfElse(node: A.SIfElse): boolean {
    return node.branches.every(b => b.visit(this)) && node._else.visit(this);
  }

  sIfPipe(node: A.SIfPipe): boolean {
    return node.branches.every(b => b.visit(this));
  }

  sIfPipeElse(node: A.SIfPipeElse): boolean {
    return node.branches.every(b => b.visit(this)) && node._else.visit(this);
  }

  sCasesBind(node: A.SCasesBind): boolean {
    return node.bind.visit(this);
  }

  sCasesBranch(node: A.SCasesBranch): boolean {
    return node.args.every(a => a.visit(this)) && node.body.visit(this);
  }

  sSingletonCasesBranch(node: A.SSingletonCasesBranch): boolean {
    return node.body.visit(this);
  }

  sCases(node: A.SCases): boolean {
    return node.typ.visit(this) && node.val.visit(this) && node.branches.every(b => b.visit(this));
  }

  sCasesElse(node: A.SCasesElse): boolean {
    return node.typ.visit(this) && node.val.visit(this) && node.branches.every(b => b.visit(this)) && node._else.visit(this);
  }

  sOp(node: A.SOp): boolean {
    return node.left.visit(this) && node.right.visit(this);
  }

  sCheckTest(node: A.SCheckTest): boolean {
    return node.op.visit(this) && this.option(node.refinement) && node.left.visit(this) && this.option(node.right) && this.option(node.cause);
  }

  sOpIs(node: A.SOpIs): boolean { return true; }
  sOpIsRoughly(node: A.SOpIsRoughly): boolean { return true; }
  sOpIsNotRoughly(node: A.SOpIsNotRoughly): boolean { return true; }
  sOpIsOp(node: A.SOpIsOp): boolean { return true; }
  sOpIsNot(node: A.SOpIsNot): boolean { return true; }
  sOpIsNotOp(node: A.SOpIsNotOp): boolean { return true; }
  sOpSatisfies(node: A.SOpSatisfies): boolean { return true; }
  sOpSatisfiesNot(node: A.SOpSatisfiesNot): boolean { return true; }
  sOpRaises(node: A.SOpRaises): boolean { return true; }
  sOpRaisesOther(node: A.SOpRaisesOther): boolean { return true; }
  sOpRaisesNot(node: A.SOpRaisesNot): boolean { return true; }
  sOpRaisesSatisfies(node: A.SOpRaisesSatisfies): boolean { return true; }
  sOpRaisesViolates(node: A.SOpRaisesViolates): boolean { return true; }

  sCheckExpr(node: A.SCheckExpr): boolean {
    return node.expr.visit(this) && node.ann.visit(this);
  }

  sParen(node: A.SParen): boolean {
    return node.expr.visit(this);
  }

  sLam(node: A.SLam): boolean {
    return node.params.every(p => p.visit(this))
      && node.args.every(a => a.visit(this)) && node.ann.visit(this) && node.body.visit(this) && this.option(node._check);
  }

  sMethod(node: A.SMethod): boolean {
    return node.params.every(p => p.visit(this)) && node.args.every(a => a.visit(this)) && node.ann.visit(this) && node.body.visit(this) && this.option(node._check);
  }

  sExtend(node: A.SExtend): boolean {
    return node.supe.visit(this) && node.fields.every(f => f.visit(this));
  }

  sUpdate(node: A.SUpdate): boolean {
    return node.supe.visit(this) && node.fields.every(f => f.visit(this));
  }

  sTuple(node: A.STuple): boolean {
    return node.fields.every(f => f.visit(this));
  }

  sTupleGet(node: A.STupleGet): boolean {
    return node.tup.visit(this);
  }

  sObj(node: A.SObj): boolean {
    return node.fields.every(f => f.visit(this));
  }

  sArray(node: A.SArray): boolean {
    return node.values.every(v => v.visit(this));
  }

  sConstruct(node: A.SConstruct): boolean {
    return node.constructorVal.visit(this) && node.values.every(v => v.visit(this));
  }

  sReactor(node: A.SReactor): boolean {
    return node.fields.every(f => f.visit(this));
  }

  sTable(node: A.STable): boolean {
    return node.headers.every(h => h.visit(this)) && node.rows.every(r => r.visit(this));
  }

  sTableRow(node: A.STableRow): boolean {
    return node.elems.every(e => e.visit(this));
  }

  sLoadTable(node: A.SLoadTable): boolean {
    return node.headers.every(h => h.visit(this)) && node.spec.every(s => s.visit(this));
  }

  sFieldName(node: A.SFieldName): boolean {
    return true;
  }

  sApp(node: A.SApp): boolean {
    return node._fun.visit(this) && node.args.every(a => a.visit(this));
  }

  sPrimApp(node: A.SPrimApp): boolean {
    return node.args.every(a => a.visit(this));
  }

  sPrimVal(node: A.SPrimVal): boolean {
    return true;
  }

  sId(node: A.SId): boolean {
    return node.id.visit(this);
  }

  sIdVar(node: A.SIdVar): boolean {
    return node.id.visit(this);
  }

  sIdLetrec(node: A.SIdLetrec): boolean {
    return node.id.visit(this);
  }

  sIdVarModref(node: A.SIdVarModref): boolean {
    return node.id.visit(this);
  }

  sIdModref(node: A.SIdModref): boolean {
    return node.id.visit(this);
  }

  sUndefined(node: A.SUndefined): boolean {
    return true;
  }

  sSrcloc(node: A.SSrcloc): boolean {
    return true;
  }

  sNum(node: A.SNum): boolean {
    return true;
  }

  sFrac(node: A.SFrac): boolean {
    return true;
  }

  sRfrac(node: A.SRfrac): boolean {
    return true;
  }

  sBool(node: A.SBool): boolean {
    return true;
  }

  sStr(node: A.SStr): boolean {
    return true;
  }

  sDot(node: A.SDot): boolean {
    return node.obj.visit(this);
  }

  sGetBang(node: A.SGetBang): boolean {
    return node.obj.visit(this);
  }

  sBracket(node: A.SBracket): boolean {
    return node.obj.visit(this) && node.key.visit(this);
  }

  sData(node: A.SData): boolean {
    return node.params.every(p => p.visit(this))
      && node.mixins.every(m => m.visit(this))
      && node.variants.every(v => v.visit(this))
      && node.sharedMembers.every(s => s.visit(this))
      && this.option(node._check);
  }

  sDataExpr(node: A.SDataExpr): boolean {
    return node.namet.visit(this)
      && node.params.every(p => p.visit(this))
      && node.mixins.every(m => m.visit(this))
      && node.variants.every(v => v.visit(this))
      && node.sharedMembers.every(s => s.visit(this))
      && this.option(node._check);
  }

  sFor(node: A.SFor): boolean {
    return node.iterator.visit(this) && node.bindings.every(b => b.visit(this)) && node.ann.visit(this) && node.body.visit(this);
  }

  sCheck(node: A.SCheck): boolean {
    return node.body.visit(this);
  }

  sDataField(node: A.SDataField): boolean {
    return node.value.visit(this);
  }

  sMutableField(node: A.SMutableField): boolean {
    return node.ann.visit(this) && node.value.visit(this);
  }

  // NOTE: the Pyret source visits `args` twice here (and never visits params);
  // this is mirrored faithfully.
  sMethodField(node: A.SMethodField): boolean {
    return node.args.every(a => a.visit(this))
      && node.args.every(a => a.visit(this))
      && node.ann.visit(this)
      && node.body.visit(this)
      && this.option(node._check);
  }

  sForBind(node: A.SForBind): boolean {
    return node.bind.visit(this) && node.value.visit(this);
  }

  sColumnBinds(node: A.SColumnBinds): boolean {
    return node.binds.every(b => b.visit(this)) && node.table.visit(this);
  }

  sVariantMember(node: A.SVariantMember): boolean {
    return node.bind.visit(this);
  }

  sVariant(node: A.SVariant): boolean {
    return node.members.every(m => m.visit(this)) && node.withMembers.every(w => w.visit(this));
  }

  sSingletonVariant(node: A.SSingletonVariant): boolean {
    return node.withMembers.every(w => w.visit(this));
  }

  sColumnSort(node: A.SColumnSort): boolean {
    return node.column.visit(this);
  }

  sTableExtend(node: A.STableExtend): boolean {
    return node.columnBinds.visit(this) && node.extensions.every(e => e.visit(this));
  }

  sTableUpdate(node: A.STableUpdate): boolean {
    return node.columnBinds.visit(this) && node.updates.every(u => u.visit(this));
  }

  sTableFilter(node: A.STableFilter): boolean {
    return node.columnBinds.visit(this) && node.predicate.visit(this);
  }

  sTableSelect(node: A.STableSelect): boolean {
    return node.columns.every(c => c.visit(this)) && node.table.visit(this);
  }

  sTableOrder(node: A.STableOrder): boolean {
    return node.table.visit(this) && node.ordering.every(o => o.visit(this));
  }

  sTableExtract(node: A.STableExtract): boolean {
    return node.column.visit(this) && node.table.visit(this);
  }

  sTableExtendField(node: A.STableExtendField): boolean {
    return node.value.visit(this) && node.ann.visit(this);
  }

  sTableExtendReducer(node: A.STableExtendReducer): boolean {
    return node.reducer.visit(this) && node.col.visit(this) && node.ann.visit(this);
  }

  sSanitize(node: A.SSanitize): boolean {
    return node.name.visit(this) && node.sanitizer.visit(this);
  }

  sTableSrc(node: A.STableSrc): boolean {
    return node.src.visit(this);
  }

  sSpyBlock(node: A.SSpyBlock): boolean {
    return this.option(node.message) && node.contents.every(c => c.visit(this));
  }

  sSpyExpr(node: A.SSpyExpr): boolean {
    return node.value.visit(this);
  }

  aBlank(node: A.ABlank): boolean {
    return true;
  }

  aAny(node: A.AAny): boolean {
    return true;
  }

  aName(node: A.AName): boolean {
    return true;
  }

  aTypeVar(node: A.ATypeVar): boolean {
    return true;
  }

  aArrow(node: A.AArrow): boolean {
    return node.args.every(a => a.visit(this)) && node.ret.visit(this);
  }

  aArrowArgnames(node: A.AArrowArgnames): boolean {
    return node.args.every(a => a.visit(this)) && node.ret.visit(this);
  }

  aMethod(node: A.AMethod): boolean {
    return node.args.every(a => a.visit(this)) && node.ret.visit(this);
  }

  aRecord(node: A.ARecord): boolean {
    return node.fields.every(f => f.visit(this));
  }

  aTuple(node: A.ATuple): boolean {
    return node.fields.every(f => f.visit(this));
  }

  aApp(node: A.AApp): boolean {
    return node.ann.visit(this) && node.args.every(a => a.visit(this));
  }

  aPred(node: A.APred): boolean {
    return node.ann.visit(this) && node.exp.visit(this);
  }

  aDot(node: A.ADot): boolean {
    return node.obj.visit(this);
  }

  aField(node: A.AField): boolean {
    return node.ann.visit(this);
  }
}

// ---------- post-resolve-scope visitors ----------

export function eliminatedScopeForm(node: A.STypeLetExpr | A.SLetExpr | A.SLetrec): never {
  throw new InternalCompilerError(
    node.$name + ' is not a post-resolve-scope form; desugar-scope replaces it with s-scope-block');
}

export class PostScopeMapVisitor extends DefaultMapVisitor {
  sTypeLetExpr(node: A.STypeLetExpr): A.Expr { return eliminatedScopeForm(node); }
  sLetExpr(node: A.SLetExpr): A.Expr { return eliminatedScopeForm(node); }
  sLetrec(node: A.SLetrec): A.Expr { return eliminatedScopeForm(node); }
}

export class PostScopeIterVisitor extends DefaultIterVisitor {
  sTypeLetExpr(node: A.STypeLetExpr): boolean { return eliminatedScopeForm(node); }
  sLetExpr(node: A.SLetExpr): boolean { return eliminatedScopeForm(node); }
  sLetrec(node: A.SLetrec): boolean { return eliminatedScopeForm(node); }
}

export class DummyLocVisitor {
  protected option<T extends { visit(v: any): any }>(x: T | undefined): any {
    return x === undefined ? undefined : x.visit(this);
  }

  sUnderscore(node: A.SUnderscore): A.Name {
    return new A.SUnderscore(dummyLoc);
  }

  sName(node: A.SName): A.Name {
    return new A.SName(dummyLoc, node.s);
  }

  sGlobal(node: A.SGlobal): A.Name {
    return new A.SGlobal(node.s);
  }

  sTypeGlobal(node: A.STypeGlobal): A.Name {
    return new A.STypeGlobal(node.s);
  }

  sModuleGlobal(node: A.SModuleGlobal): A.Name {
    return new A.SModuleGlobal(node.s);
  }

  sAtom(node: A.SAtom): A.Name {
    return new A.SAtom(node.base, node.serial);
  }

  sStar(node: A.SStar): A.NameSpec {
    return new A.SStar(dummyLoc, node.hidden.map(h => h.visit(this)));
  }

  sModuleRef(node: A.SModuleRef): A.NameSpec {
    return new A.SModuleRef(dummyLoc, node.path.map(p => p.visit(this)), this.option(node.asName));
  }

  sLocalRef(node: A.SLocalRef): A.NameSpec {
    return new A.SLocalRef(dummyLoc, node.name.visit(this), node.asName.visit(this));
  }

  sRemoteRef(node: A.SRemoteRef): A.NameSpec {
    return new A.SRemoteRef(dummyLoc, node.uri, node.name.visit(this), node.asName.visit(this));
  }

  sDefinedModule(node: A.SDefinedModule): A.DefinedModule {
    return new A.SDefinedModule(node.name, node.value.visit(this), node.uri);
  }

  sDefinedValue(node: A.SDefinedValue): A.DefinedValue {
    return new A.SDefinedValue(node.name, node.value.visit(this));
  }

  sDefinedVar(node: A.SDefinedVar): A.DefinedValue {
    return new A.SDefinedVar(node.name, node.id.visit(this));
  }

  sDefinedType(node: A.SDefinedType): A.DefinedType {
    return new A.SDefinedType(node.name, node.typ.visit(this));
  }

  sModule(node: A.SModule): A.Expr {
    return new A.SModule(dummyLoc,
      node.answer.visit(this), node.definedModules.map(dm => dm.visit(this)), node.definedValues.map(dv => dv.visit(this)), node.definedTypes.map(dt => dt.visit(this)), node.checks.visit(this));
  }

  sProgram(node: A.SProgram): A.Program {
    return new A.SProgram(dummyLoc, this.option(node._use), node._provide.visit(this), node.providedTypes.visit(this), node.provides.map(p => p.visit(this)), node.imports.map(i => i.visit(this)), node.block.visit(this));
  }

  sUse(node: A.SUse): A.Use {
    return new A.SUse(dummyLoc, node.n.visit(this), node.mod.visit(this));
  }

  sConstImport(node: A.SConstImport): A.ImportType {
    return new A.SConstImport(dummyLoc, node.mod);
  }

  sSpecialImport(node: A.SSpecialImport): A.ImportType {
    return new A.SSpecialImport(dummyLoc, node.kind, node.args);
  }

  sImport(node: A.SImport): A.Import {
    return new A.SImport(dummyLoc, node.file.visit(this), node.name.visit(this));
  }

  sIncludeFrom(node: A.SIncludeFrom): A.Import {
    return new A.SIncludeFrom(dummyLoc, node.mod.map(m => m.visit(this)), node.specs.map(s => s.visit(this)));
  }

  sIncludeName(node: A.SIncludeName): A.IncludeSpec {
    return new A.SIncludeName(dummyLoc, node.nameSpec.visit(this));
  }

  sIncludeData(node: A.SIncludeData): A.IncludeSpec {
    return new A.SIncludeData(dummyLoc, node.nameSpec.visit(this), node.hidden.map(h => h.visit(this)));
  }

  sIncludeType(node: A.SIncludeType): A.IncludeSpec {
    return new A.SIncludeType(dummyLoc, node.nameSpec.visit(this));
  }

  sIncludeModule(node: A.SIncludeModule): A.IncludeSpec {
    return new A.SIncludeModule(dummyLoc, node.nameSpec.visit(this));
  }

  sInclude(node: A.SInclude): A.Import {
    return new A.SInclude(dummyLoc, node.mod.visit(this));
  }

  sImportTypes(node: A.SImportTypes): A.Import {
    return new A.SImportTypes(dummyLoc, node.file.visit(this), node.name.visit(this), node.types.visit(this));
  }

  sImportFields(node: A.SImportFields): A.Import {
    return new A.SImportFields(dummyLoc, node.fields.map(f => f.visit(this)), node.file.visit(this));
  }

  sProvideBlock(node: A.SProvideBlock): A.ProvideBlock {
    return new A.SProvideBlock(dummyLoc, node.path.map(p => p.visit(this)), node.specs.map(s => s.visit(this)));
  }

  sProvideName(node: A.SProvideName): A.ProvideSpec {
    return new A.SProvideName(dummyLoc, node.nameSpec.visit(this));
  }

  sProvideData(node: A.SProvideData): A.ProvideSpec {
    return new A.SProvideData(dummyLoc, node.nameSpec.visit(this), node.hidden.map(h => h.visit(this)));
  }

  sProvideType(node: A.SProvideType): A.ProvideSpec {
    return new A.SProvideType(dummyLoc, node.nameSpec.visit(this));
  }

  sProvideModule(node: A.SProvideModule): A.ProvideSpec {
    return new A.SProvideModule(dummyLoc, node.nameSpec.visit(this));
  }

  sProvide(node: A.SProvide): A.Provide {
    return new A.SProvide(dummyLoc, node.block.visit(this));
  }

  sProvideAll(node: A.SProvideAll): A.Provide {
    return new A.SProvideAll(dummyLoc);
  }

  sProvideNone(node: A.SProvideNone): A.Provide {
    return new A.SProvideNone(dummyLoc);
  }

  sProvideTypes(node: A.SProvideTypes): A.ProvideTypes {
    return new A.SProvideTypes(dummyLoc, node.ann.map(a => a.visit(this)));
  }

  sProvideTypesAll(node: A.SProvideTypesAll): A.ProvideTypes {
    return new A.SProvideTypesAll(dummyLoc);
  }

  sProvideTypesNone(node: A.SProvideTypesNone): A.ProvideTypes {
    return new A.SProvideTypesNone(dummyLoc);
  }

  sBind(node: A.SBind): A.Bind {
    return new A.SBind(dummyLoc, node.shadows, node.id.visit(this), node.ann.visit(this));
  }

  sTupleBind(node: A.STupleBind): A.Bind {
    return new A.STupleBind(dummyLoc, node.fields.map(f => f.visit(this)), this.option(node.asName));
  }

  sVarBind(node: A.SVarBind): A.LetBind {
    return new A.SVarBind(dummyLoc, node.b.visit(this), node.value.visit(this));
  }

  sLetBind(node: A.SLetBind): A.LetBind {
    return new A.SLetBind(dummyLoc, node.b.visit(this), node.value.visit(this));
  }

  // NOTE: the Pyret source does not visit any children here.
  sTypeBind(node: A.STypeBind): A.TypeLetBind {
    return new A.STypeBind(dummyLoc, node.name, node.params, node.ann);
  }

  // NOTE: the Pyret source keeps the original loc here.
  sNewtypeBind(node: A.SNewtypeBind): A.TypeLetBind {
    return new A.SNewtypeBind(node.l, node.name.visit(this), node.namet.visit(this));
  }

  sTemplate(node: A.STemplate): A.Expr {
    return new A.STemplate(dummyLoc);
  }

  sTypeLetExpr(node: A.STypeLetExpr): A.Expr {
    return new A.STypeLetExpr(dummyLoc, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sLetExpr(node: A.SLetExpr): A.Expr {
    return new A.SLetExpr(dummyLoc, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sLetrecBind(node: A.SLetrecBind): A.LetrecBind {
    return new A.SLetrecBind(dummyLoc, node.b.visit(this), node.value.visit(this));
  }

  sLetrec(node: A.SLetrec): A.Expr {
    return new A.SLetrec(dummyLoc, node.binds.map(b => b.visit(this)), node.body.visit(this), node.blocky);
  }

  sHintExp(node: A.SHintExp): A.Expr {
    return new A.SHintExp(dummyLoc, node.hints, node.exp.visit(this));
  }

  sInstantiate(node: A.SInstantiate): A.Expr {
    return new A.SInstantiate(dummyLoc, node.expr.visit(this), node.params.map(p => p.visit(this)));
  }

  sBlock(node: A.SBlock): A.Expr {
    return new A.SBlock(dummyLoc, node.stmts.map(s => s.visit(this)));
  }

  sScopeLet(node: A.SScopeLet): A.ScopeEntry {
    return new A.SScopeLet(dummyLoc, node.binds.map(b => b.visit(this)));
  }

  sScopeTypeLet(node: A.SScopeTypeLet): A.ScopeEntry {
    return new A.SScopeTypeLet(dummyLoc, node.binds.map(b => b.visit(this)));
  }

  sScopeLetrec(node: A.SScopeLetrec): A.ScopeEntry {
    return new A.SScopeLetrec(dummyLoc, node.binds.map(b => b.visit(this)));
  }

  sScopeStmt(node: A.SScopeStmt): A.ScopeEntry {
    return new A.SScopeStmt(dummyLoc, node.stmt.visit(this));
  }

  sScopeBlock(node: A.SScopeBlock): A.Expr {
    return new A.SScopeBlock(dummyLoc, node.entries.map(e => e.visit(this)), node.tail.visit(this));
  }

  sUserBlock(node: A.SUserBlock): A.Expr {
    return new A.SUserBlock(dummyLoc, node.body.visit(this));
  }

  sFun(node: A.SFun): A.Expr {
    return new A.SFun(dummyLoc, node.name, node.params.map(p => p.visit(this)), node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._checkLoc === undefined ? undefined : dummyLoc, this.option(node._check), node.blocky);
  }

  sType(node: A.SType): A.Expr {
    return new A.SType(dummyLoc, node.name.visit(this), node.params.map(p => p.visit(this)), node.ann.visit(this));
  }

  sNewtype(node: A.SNewtype): A.Expr {
    return new A.SNewtype(dummyLoc, node.name.visit(this), node.namet.visit(this));
  }

  sVar(node: A.SVar): A.Expr {
    return new A.SVar(dummyLoc, node.name.visit(this), node.value.visit(this));
  }

  sRec(node: A.SRec): A.Expr {
    return new A.SRec(dummyLoc, node.name.visit(this), node.value.visit(this));
  }

  sLet(node: A.SLet): A.Expr {
    return new A.SLet(dummyLoc, node.name.visit(this), node.value.visit(this), node.keywordVal);
  }

  // The Pyret source says `s-ref(self, dummy-loc, self.option(ann))`, passing
  // the visitor as an extra first argument; the intended behavior is ported.
  sRef(node: A.SRef): A.Expr {
    return new A.SRef(dummyLoc, this.option(node.ann));
  }

  sWhen(node: A.SWhen): A.Expr {
    return new A.SWhen(dummyLoc, node.test.visit(this), node.block.visit(this), node.blocky);
  }

  sContract(node: A.SContract): A.Expr {
    return new A.SContract(dummyLoc, node.name.visit(this), node.params.map(p => p.visit(this)), node.ann.visit(this));
  }

  sAssign(node: A.SAssign): A.Expr {
    return new A.SAssign(dummyLoc, node.id.visit(this), node.value.visit(this));
  }

  sIfBranch(node: A.SIfBranch): A.IfBranch {
    return new A.SIfBranch(dummyLoc, node.test.visit(this), node.body.visit(this));
  }

  sIfPipeBranch(node: A.SIfPipeBranch): A.IfPipeBranch {
    return new A.SIfPipeBranch(dummyLoc, node.test.visit(this), node.body.visit(this));
  }

  sIf(node: A.SIf): A.Expr {
    return new A.SIf(dummyLoc, node.branches.map(b => b.visit(this)), node.blocky);
  }

  sIfElse(node: A.SIfElse): A.Expr {
    return new A.SIfElse(dummyLoc, node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sIfPipe(node: A.SIfPipe): A.Expr {
    return new A.SIfPipe(dummyLoc, node.branches.map(b => b.visit(this)), node.blocky);
  }

  sIfPipeElse(node: A.SIfPipeElse): A.Expr {
    return new A.SIfPipeElse(dummyLoc, node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sCasesBind(node: A.SCasesBind): A.CasesBind {
    return new A.SCasesBind(dummyLoc, node.fieldType, node.bind.visit(this));
  }

  sCasesBranch(node: A.SCasesBranch): A.CasesBranch {
    return new A.SCasesBranch(dummyLoc, dummyLoc, node.name, node.args.map(a => a.visit(this)), node.body.visit(this));
  }

  sSingletonCasesBranch(node: A.SSingletonCasesBranch): A.CasesBranch {
    return new A.SSingletonCasesBranch(dummyLoc, dummyLoc, node.name, node.body.visit(this));
  }

  sCases(node: A.SCases): A.Expr {
    return new A.SCases(dummyLoc, node.typ.visit(this), node.val.visit(this), node.branches.map(b => b.visit(this)), node.blocky);
  }

  sCasesElse(node: A.SCasesElse): A.Expr {
    return new A.SCasesElse(dummyLoc, node.typ.visit(this), node.val.visit(this), node.branches.map(b => b.visit(this)), node._else.visit(this), node.blocky);
  }

  sOp(node: A.SOp): A.Expr {
    return new A.SOp(dummyLoc, dummyLoc, node.op, node.left.visit(this), node.right.visit(this));
  }

  sCheckTest(node: A.SCheckTest): A.Expr {
    return new A.SCheckTest(dummyLoc, node.op.visit(this), this.option(node.refinement), node.left.visit(this), this.option(node.right), this.option(node.cause));
  }

  sOpIs(node: A.SOpIs): A.CheckOp { return new A.SOpIs(dummyLoc); }
  sOpIsRoughly(node: A.SOpIsRoughly): A.CheckOp { return new A.SOpIsRoughly(dummyLoc); }
  sOpIsNotRoughly(node: A.SOpIsNotRoughly): A.CheckOp { return new A.SOpIsNotRoughly(dummyLoc); }
  sOpIsOp(node: A.SOpIsOp): A.CheckOp { return new A.SOpIsOp(dummyLoc, node.op); }
  sOpIsNot(node: A.SOpIsNot): A.CheckOp { return new A.SOpIsNot(dummyLoc); }
  sOpIsNotOp(node: A.SOpIsNotOp): A.CheckOp { return new A.SOpIsNotOp(dummyLoc, node.op); }
  sOpSatisfies(node: A.SOpSatisfies): A.CheckOp { return new A.SOpSatisfies(dummyLoc); }
  sOpSatisfiesNot(node: A.SOpSatisfiesNot): A.CheckOp { return new A.SOpSatisfiesNot(dummyLoc); }
  sOpRaises(node: A.SOpRaises): A.CheckOp { return new A.SOpRaises(dummyLoc); }
  sOpRaisesOther(node: A.SOpRaisesOther): A.CheckOp { return new A.SOpRaisesOther(dummyLoc); }
  sOpRaisesNot(node: A.SOpRaisesNot): A.CheckOp { return new A.SOpRaisesNot(dummyLoc); }
  sOpRaisesSatisfies(node: A.SOpRaisesSatisfies): A.CheckOp { return new A.SOpRaisesSatisfies(dummyLoc); }
  sOpRaisesViolates(node: A.SOpRaisesViolates): A.CheckOp { return new A.SOpRaisesViolates(dummyLoc); }

  sParen(node: A.SParen): A.Expr {
    return new A.SParen(dummyLoc, node.expr.visit(this));
  }

  // NOTE: the Pyret source replaces the name with "" and keys the new
  // _check-loc off of _check (not _check-loc); both are mirrored.
  sLam(node: A.SLam): A.Expr {
    return new A.SLam(dummyLoc, "", node.params.map(p => p.visit(this)), node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._check === undefined ? undefined : dummyLoc, this.option(node._check), node.blocky);
  }

  // NOTE: the Pyret source replaces the name with "".
  sMethod(node: A.SMethod): A.Expr {
    return new A.SMethod(dummyLoc, "", node.params.map(p => p.visit(this)), node.args.map(a => a.visit(this)), node.ann.visit(this), node.doc, node.body.visit(this), node._checkLoc === undefined ? undefined : dummyLoc, this.option(node._check), node.blocky);
  }

  sExtend(node: A.SExtend): A.Expr {
    return new A.SExtend(dummyLoc, node.supe.visit(this), node.fields.map(f => f.visit(this)));
  }

  sUpdate(node: A.SUpdate): A.Expr {
    return new A.SUpdate(dummyLoc, node.supe.visit(this), node.fields.map(f => f.visit(this)));
  }

  sTuple(node: A.STuple): A.Expr {
    return new A.STuple(dummyLoc, node.fields.map(f => f.visit(this)));
  }

  sTupleGet(node: A.STupleGet): A.Expr {
    return new A.STupleGet(dummyLoc, node.tup.visit(this), node.index, dummyLoc);
  }

  sObj(node: A.SObj): A.Expr {
    return new A.SObj(dummyLoc, node.fields.map(f => f.visit(this)));
  }

  sArray(node: A.SArray): A.Expr {
    return new A.SArray(dummyLoc, node.values.map(v => v.visit(this)));
  }

  sConstruct(node: A.SConstruct): A.Expr {
    return new A.SConstruct(dummyLoc, node.modifier, node.constructorVal.visit(this), node.values.map(v => v.visit(this)));
  }

  sReactor(node: A.SReactor): A.Expr {
    return new A.SReactor(dummyLoc, node.fields.map(f => f.visit(this)));
  }

  sTable(node: A.STable): A.Expr {
    return new A.STable(dummyLoc, node.headers.map(h => h.visit(this)), node.rows.map(r => r.visit(this)));
  }

  sTableRow(node: A.STableRow): A.TableRow {
    return new A.STableRow(dummyLoc, node.elems.map(e => e.visit(this)));
  }

  sFieldName(node: A.SFieldName): A.FieldName {
    return new A.SFieldName(dummyLoc, node.name, node.ann.visit(this));
  }

  sLoadTable(node: A.SLoadTable): A.Expr {
    return new A.SLoadTable(dummyLoc, node.headers.map(h => h.visit(this)), node.spec.map(s => s.visit(this)));
  }

  sApp(node: A.SApp): A.Expr {
    return new A.SApp(dummyLoc, node._fun.visit(this), node.args.map(a => a.visit(this)));
  }

  sPrimApp(node: A.SPrimApp): A.Expr {
    return new A.SPrimApp(dummyLoc, node._fun, node.args.map(a => a.visit(this)), node.appInfo);
  }

  sPrimVal(node: A.SPrimVal): A.Expr {
    return new A.SPrimVal(dummyLoc, node.name);
  }

  sId(node: A.SId): A.Expr {
    return new A.SId(dummyLoc, node.id.visit(this));
  }

  sIdVar(node: A.SIdVar): A.Expr {
    return new A.SIdVar(dummyLoc, node.id.visit(this));
  }

  sIdLetrec(node: A.SIdLetrec): A.Expr {
    return new A.SIdLetrec(dummyLoc, node.id.visit(this), node.safe);
  }

  sIdVarModref(node: A.SIdVarModref): A.Expr {
    return new A.SIdVarModref(dummyLoc, node.id.visit(this), node.uri, node.name);
  }

  sIdModref(node: A.SIdModref): A.Expr {
    return new A.SIdModref(dummyLoc, node.id.visit(this), node.uri, node.name);
  }

  // The Pyret source says `s-undefined(self)` here, which is a latent bug; the
  // intended behavior (dummy loc) is ported.
  sUndefined(node: A.SUndefined): A.Expr {
    return new A.SUndefined(dummyLoc);
  }

  sSrcloc(node: A.SSrcloc): A.Expr {
    return new A.SSrcloc(dummyLoc, node.loc);
  }

  sNum(node: A.SNum): A.Expr {
    return new A.SNum(dummyLoc, node.n);
  }

  sFrac(node: A.SFrac): A.Expr {
    return new A.SFrac(dummyLoc, node.num, node.den);
  }

  sRfrac(node: A.SRfrac): A.Expr {
    return new A.SRfrac(dummyLoc, node.num, node.den);
  }

  sBool(node: A.SBool): A.Expr {
    return new A.SBool(dummyLoc, node.b);
  }

  sStr(node: A.SStr): A.Expr {
    return new A.SStr(dummyLoc, node.s);
  }

  sDot(node: A.SDot): A.Expr {
    return new A.SDot(dummyLoc, node.obj.visit(this), node.field);
  }

  sGetBang(node: A.SGetBang): A.Expr {
    return new A.SGetBang(dummyLoc, node.obj.visit(this), node.field);
  }

  sBracket(node: A.SBracket): A.Expr {
    return new A.SBracket(dummyLoc, node.obj.visit(this), node.key.visit(this));
  }

  sData(node: A.SData): A.Expr {
    return new A.SData(
      dummyLoc,
      node.name,
      node.params.map(p => p.visit(this)),
      node.mixins.map(m => m.visit(this)),
      node.variants.map(v => v.visit(this)),
      node.sharedMembers.map(s => s.visit(this)),
      node._checkLoc === undefined ? undefined : dummyLoc,
      this.option(node._check)
    );
  }

  sDataExpr(node: A.SDataExpr): A.Expr {
    return new A.SDataExpr(
      dummyLoc,
      node.name,
      node.namet.visit(this),
      node.params.map(p => p.visit(this)),
      node.mixins.map(m => m.visit(this)),
      node.variants.map(v => v.visit(this)),
      node.sharedMembers.map(s => s.visit(this)),
      node._checkLoc === undefined ? undefined : dummyLoc,
      this.option(node._check)
    );
  }

  sFor(node: A.SFor): A.Expr {
    return new A.SFor(dummyLoc, node.iterator.visit(this), node.bindings.map(b => b.visit(this)), node.ann.visit(this), node.body.visit(this), node.blocky);
  }

  sCheck(node: A.SCheck): A.Expr {
    return new A.SCheck(dummyLoc, node.name, node.body.visit(this), node.keywordCheck);
  }

  sDataField(node: A.SDataField): A.Member {
    return new A.SDataField(dummyLoc, node.name, node.value.visit(this));
  }

  sMutableField(node: A.SMutableField): A.Member {
    return new A.SMutableField(dummyLoc, node.name, node.ann.visit(this), node.value.visit(this));
  }

  sMethodField(node: A.SMethodField): A.Member {
    return new A.SMethodField(
      dummyLoc,
      node.name,
      node.params.map(p => p.visit(this)),
      node.args.map(a => a.visit(this)),
      node.ann.visit(this),
      node.doc,
      node.body.visit(this),
      node._checkLoc === undefined ? undefined : dummyLoc,
      this.option(node._check),
      node.blocky
    );
  }

  sForBind(node: A.SForBind): A.ForBind {
    return new A.SForBind(dummyLoc, node.bind.visit(this), node.value.visit(this));
  }

  sColumnBinds(node: A.SColumnBinds): A.ColumnBinds {
    return new A.SColumnBinds(dummyLoc, node.binds.map(b => b.visit(this)), node.table.visit(this));
  }

  sVariantMember(node: A.SVariantMember): A.VariantMember {
    return new A.SVariantMember(dummyLoc, node.memberType, node.bind.visit(this));
  }

  sVariant(node: A.SVariant): A.Variant {
    return new A.SVariant(dummyLoc, dummyLoc, node.name, node.members.map(m => m.visit(this)), node.withMembers.map(w => w.visit(this)));
  }

  sSingletonVariant(node: A.SSingletonVariant): A.Variant {
    return new A.SSingletonVariant(dummyLoc, node.name, node.withMembers.map(w => w.visit(this)));
  }

  sColumnSort(node: A.SColumnSort): A.ColumnSort {
    return new A.SColumnSort(dummyLoc, node.column.visit(this), node.direction);
  }

  sTableExtend(node: A.STableExtend): A.Expr {
    return new A.STableExtend(dummyLoc, node.columnBinds.visit(this), node.extensions.map(e => e.visit(this)));
  }

  sTableUpdate(node: A.STableUpdate): A.Expr {
    return new A.STableUpdate(dummyLoc, node.columnBinds.visit(this), node.updates.map(u => u.visit(this)));
  }

  sTableFilter(node: A.STableFilter): A.Expr {
    return new A.STableFilter(dummyLoc, node.columnBinds.visit(this), node.predicate.visit(this));
  }

  sTableSelect(node: A.STableSelect): A.Expr {
    return new A.STableSelect(dummyLoc, node.columns.map(c => c.visit(this)), node.table.visit(this));
  }

  sTableOrder(node: A.STableOrder): A.Expr {
    return new A.STableOrder(dummyLoc, node.table.visit(this), node.ordering.map(o => o.visit(this)));
  }

  sTableExtract(node: A.STableExtract): A.Expr {
    return new A.STableExtract(dummyLoc, node.column.visit(this), node.table.visit(this));
  }

  sTableExtendField(node: A.STableExtendField): A.TableExtendField {
    return new A.STableExtendField(dummyLoc, node.name, node.value.visit(this), node.ann.visit(this));
  }

  sTableExtendReducer(node: A.STableExtendReducer): A.TableExtendField {
    return new A.STableExtendReducer(dummyLoc, node.name, node.reducer.visit(this), node.col.visit(this), node.ann.visit(this));
  }

  sSanitize(node: A.SSanitize): A.LoadTableSpec {
    return new A.SSanitize(dummyLoc, node.name.visit(this), node.sanitizer.visit(this));
  }

  sTableSrc(node: A.STableSrc): A.LoadTableSpec {
    return new A.STableSrc(dummyLoc, node.src.visit(this));
  }

  sSpyBlock(node: A.SSpyBlock): A.Expr {
    return new A.SSpyBlock(dummyLoc, this.option(node.message), node.contents.map(c => c.visit(this)));
  }

  sSpyExpr(node: A.SSpyExpr): A.SpyField {
    return new A.SSpyExpr(dummyLoc, node.name, node.value.visit(this), node.implicitLabel);
  }

  aBlank(node: A.ABlank): A.Ann { return node; }
  aAny(node: A.AAny): A.Ann { return new A.AAny(dummyLoc); }
  aName(node: A.AName): A.Ann { return new A.AName(dummyLoc, node.id.visit(this)); }
  aTypeVar(node: A.ATypeVar): A.Ann { return new A.ATypeVar(dummyLoc, node.id.visit(this)); }
  aArrow(node: A.AArrow): A.Ann {
    return new A.AArrow(dummyLoc, node.args.map(a => a.visit(this)), node.ret.visit(this), node.useParens);
  }
  aArrowArgnames(node: A.AArrowArgnames): A.Ann {
    return new A.AArrowArgnames(dummyLoc, node.args.map(a => a.visit(this)), node.ret.visit(this), node.useParens);
  }
  aMethod(node: A.AMethod): A.Ann {
    return new A.AMethod(dummyLoc, node.args.map(a => a.visit(this)), node.ret.visit(this));
  }
  aRecord(node: A.ARecord): A.Ann {
    return new A.ARecord(dummyLoc, node.fields.map(f => f.visit(this)));
  }
  aTuple(node: A.ATuple): A.Ann {
    return new A.ATuple(dummyLoc, node.fields.map(f => f.visit(this)));
  }
  aApp(node: A.AApp): A.Ann {
    return new A.AApp(dummyLoc, node.ann.visit(this), node.args.map(a => a.visit(this)));
  }
  aPred(node: A.APred): A.Ann {
    return new A.APred(dummyLoc, node.ann.visit(this), node.exp.visit(this));
  }
  aDot(node: A.ADot): A.Ann {
    return new A.ADot(dummyLoc, node.obj.visit(this), node.field);
  }
  aField(node: A.AField): A.AField {
    return new A.AField(dummyLoc, node.name, node.ann.visit(this));
  }
}
