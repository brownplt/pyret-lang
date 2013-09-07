#lang pyret

provide {
  AField: AField,
  ASTList: ASTList,
  Ann:  Ann,
  Bind: Bind,
  CasesBranch: CasesBranch,
  Expr: Expr,
  ForBind: ForBind,
  Header: Header,
  IfBranch: IfBranch,
  ImportType: ImportType,
  List: List,
  Member: Member,
  Program: Program,
  SourceAST: SourceAST,
  TAField: TAField,
  TASTList: TASTList,
  TAnn: TAnn,
  TBind: TBind,
  TCaseBranch: TCaseBranch,
  TCasesBranch: TCasesBranch,
  TExpr: TExpr,
  TForBind: TForBind,
  THeader: THeader,
  TIfBranch: TIfBranch,
  TImportType: TImportType,
  TMember: TMember,
  TProgram: TProgram,
  TVariant: TVariant,
  Variant: Variant,
  field-equals: field-equals,
  from-ast: from-ast,
  funlam_tosource: funlam_tosource,
  generic-hash-key: generic-hash-key,
  generic-loose-equals: generic-loose-equals,
  generic-pretty: generic-pretty,
  generic-print: generic-print,
  generic-to-labelled: generic-to-labelled,
  generic-topprint: generic-topprint,
  generic-tostring: generic-tostring,
  loose-field-equals: loose-field-equals,
  transpose: transpose,
  a_any: a_any,
  a_app: a_app,
  a_arrow: a_arrow,
  a_blank: a_blank,
  a_dot: a_dot,
  a_field: a_field,
  a_method: a_method,
  a_name: a_name,
  a_pred: a_pred,
  a_record: a_record,
  ast_list: ast_list,
  is-a_any: stamp(a_any, is-a_any),
  is-a_app: stamp(a_app, is-a_app),
  is-a_arrow: stamp(a_arrow, is-a_arrow),
  is-a_blank: stamp(a_blank, is-a_blank),
  is-a_dot: stamp(a_dot, is-a_dot),
  is-a_field: stamp(a_field, is-a_field),
  is-a_method: stamp(a_method, is-a_method),
  is-a_name: stamp(a_name, is-a_name),
  is-a_pred: stamp(a_pred, is-a_pred),
  is-a_record: stamp(a_record, is-a_record),
  is-ast_list: stamp(ast_list, is-ast_list),
  is-s_app: stamp(s_app, is-s_app),
  is-s_assign: stamp(s_assign, is-s_assign),
  is-s_bind: stamp(s_bind, is-s_bind),
  is-s_block: stamp(s_block, is-s_block),
  is-s_bool: stamp(s_bool, is-s_bool),
  is-s_bracket: stamp(s_bracket, is-s_bracket),
  is-s_cases: stamp(s_cases, is-s_cases),
  is-s_cases_branch: stamp(s_cases_branch, is-s_cases_branch),
  is-s_cases_else: stamp(s_cases_else, is-s_cases_else),
  is-s_colon: stamp(s_colon, is-s_colon),
  is-s_colon_bracket: stamp(s_colon_bracket, is-s_colon_bracket),
  is-s_const_import: stamp(s_const_import, is-s_const_import),
  is-s_data: stamp(s_data, is-s_data),
  is-s_data_field: stamp(s_data_field, is-s_data_field),
  is-s_mutable_field: stamp(s_mutable_field, is-s_mutable_field),
  is-s_once_field: stamp(s_once_field, is-s_once_field),
  is-s_dot: stamp(s_dot, is-s_dot),
  is-s_extend: stamp(s_extend, is-s_extend),
  is-s_file_import: stamp(s_file_import, is-s_file_import),
  is-s_for: stamp(s_for, is-s_for),
  is-s_for_bind: stamp(s_for_bind, is-s_for_bind),
  is-s_fun: stamp(s_fun, is-s_fun),
  is-s_id: stamp(s_id, is-s_id),
  is-s_if: stamp(s_if, is-s_if),
  is-s_if_branch: stamp(s_if_branch, is-s_if_branch),
  is-s_if_else: stamp(s_if_else, is-s_if_else),
  is-s_import: stamp(s_import, is-s_import),
  is-s_lam: stamp(s_lam, is-s_lam),
  is-s_left_app: stamp(s_left_app, is-s_left_app),
  is-s_let: stamp(s_let, is-s_let),
  is-s_list: stamp(s_list, is-s_list),
  is-s_method: stamp(s_method, is-s_method),
  is-s_method_field: stamp(s_method_field, is-s_method_field),
  is-s_not: stamp(s_not, is-s_not),
  is-s_num: stamp(s_num, is-s_num),
  is-s_obj: stamp(s_obj, is-s_obj),
  is-s_op: stamp(s_op, is-s_op),
  is-s_paren: stamp(s_paren, is-s_paren),
  is-s_program: stamp(s_program, is-s_program),
  is-s_provide: stamp(s_provide, is-s_provide),
  is-s_provide_all: stamp(s_provide_all, is-s_provide_all),
  is-s_singleton_variant: stamp(s_singleton_variant, is-s_singleton_variant),
  is-s_str: stamp(s_str, is-s_str),
  is-s_try: stamp(s_try, is-s_try),
  is-s_var: stamp(s_var, is-s_var),
  is-s_variant: stamp(s_variant, is-s_variant),
  is-s_variant_member: stamp(s_variant_member, is-s_variant_member),
  is-s_when: stamp(s_when, is-s_when),
  is-t_a_any: stamp(t_a_any, is-t_a_any),
  is-t_a_app: stamp(t_a_app, is-t_a_app),
  is-t_a_arrow: stamp(t_a_arrow, is-t_a_arrow),
  is-t_a_blank: stamp(t_a_blank, is-t_a_blank),
  is-t_a_dot: stamp(t_a_dot, is-t_a_dot),
  is-t_a_field: stamp(t_a_field, is-t_a_field),
  is-t_a_method: stamp(t_a_method, is-t_a_method),
  is-t_a_name: stamp(t_a_name, is-t_a_name),
  is-t_a_pred: stamp(t_a_pred, is-t_a_pred),
  is-t_a_record: stamp(t_a_record, is-t_a_record),
  is-t_app: stamp(t_app, is-t_app),
  is-t_assign: stamp(t_assign, is-t_assign),
  is-t_ast_list: stamp(t_ast_list, is-t_ast_list),
  is-t_bind: stamp(t_bind, is-t_bind),
  is-t_block: stamp(t_block, is-t_block),
  is-t_bool: stamp(t_bool, is-t_bool),
  is-t_bracket: stamp(t_bracket, is-t_bracket),
  is-t_case: stamp(t_case, is-t_case),
  is-t_case_branch: stamp(t_case_branch, is-t_case_branch),
  is-t_cases: stamp(t_cases, is-t_cases),
  is-t_cases_branch: stamp(t_cases_branch, is-t_cases_branch),
  is-t_cases_else: stamp(t_cases_else, is-t_cases_else),
  is-t_colon: stamp(t_colon, is-t_colon),
  is-t_colon_bracket: stamp(t_colon_bracket, is-t_colon_bracket),
  is-t_const_import: stamp(t_const_import, is-t_const_import),
  is-t_data: stamp(t_data, is-t_data),
  is-t_data_field: stamp(t_data_field, is-t_data_field),
  is-t_dot: stamp(t_dot, is-t_dot),
  is-t_extend: stamp(t_extend, is-t_extend),
  is-t_file_import: stamp(t_file_import, is-t_file_import),
  is-t_for: stamp(t_for, is-t_for),
  is-t_for_bind: stamp(t_for_bind, is-t_for_bind),
  is-t_check: stamp(t_check, is-t_check),
  is-t_fun: stamp(t_fun, is-t_fun),
  is-t_id: stamp(t_id, is-t_id),
  is-t_if: stamp(t_if, is-t_if),
  is-t_if_branch: stamp(t_if_branch, is-t_if_branch),
  is-t_if_else: stamp(t_if_else, is-t_if_else),
  is-t_import: stamp(t_import, is-t_import),
  is-t_lam: stamp(t_lam, is-t_lam),
  is-t_left_app: stamp(t_left_app, is-t_left_app),
  is-t_let: stamp(t_let, is-t_let),
  is-t_list: stamp(t_list, is-t_list),
  is-t_method: stamp(t_method, is-t_method),
  is-t_method_field: stamp(t_method_field, is-t_method_field),
  is-t_not: stamp(t_not, is-t_not),
  is-t_num: stamp(t_num, is-t_num),
  is-t_obj: stamp(t_obj, is-t_obj),
  is-t_op: stamp(t_op, is-t_op),
  is-t_paren: stamp(t_paren, is-t_paren),
  is-t_program: stamp(t_program, is-t_program),
  is-t_provide: stamp(t_provide, is-t_provide),
  is-t_provide_all: stamp(t_provide_all, is-t_provide_all),
  is-t_singleton_variant: stamp(t_singleton_variant, is-t_singleton_variant),
  is-t_str: stamp(t_str, is-t_str),
  is-t_try: stamp(t_try, is-t_try),
  is-t_var: stamp(t_var, is-t_var),
  is-t_variant: stamp(t_variant, is-t_variant),
  is-t_when: stamp(t_when, is-t_when),
  s_app: s_app,
  s_assign: s_assign,
  s_bind: s_bind,
  s_block: s_block,
  s_bool: s_bool,
  s_bracket: s_bracket,
  s_cases: s_cases,
  s_cases_branch: s_cases_branch,
  s_cases_else: s_cases_else,
  s_colon: s_colon,
  s_colon_bracket: s_colon_bracket,
  s_const_import: s_const_import,
  s_data: s_data,
  s_data_field: s_data_field,
  s_mutable_field: s_mutable_field,
  s_once_field: s_once_field,
  s_dot: s_dot,
  s_extend: s_extend,
  s_file_import: s_file_import,
  s_for: s_for,
  s_for_bind: s_for_bind,
  s_fun: s_fun,
  s_id: s_id,
  s_if: s_if,
  s_if_branch: s_if_branch,
  s_if_else: s_if_else,
  s_import: s_import,
  s_lam: s_lam,
  s_left_app: s_left_app,
  s_let: s_let,
  s_list: s_list,
  s_method: s_method,
  s_method_field: s_method_field,
  s_not: s_not,
  s_num: s_num,
  s_obj: s_obj,
  s_op: s_op,
  s_paren: s_paren,
  s_program: s_program,
  s_provide: s_provide,
  s_provide_all: s_provide_all,
  s_singleton_variant: s_singleton_variant,
  s_str: s_str,
  s_try: s_try,
  s_var: s_var,
  s_variant: s_variant,
  s_when: s_when,
  t_a_any: t_a_any,
  t_a_app: t_a_app,
  t_a_arrow: t_a_arrow,
  t_a_blank: t_a_blank,
  t_a_dot: t_a_dot,
  t_a_field: t_a_field,
  t_a_method: t_a_method,
  t_a_name: t_a_name,
  t_a_pred: t_a_pred,
  t_a_record: t_a_record,
  t_app: t_app,
  t_assign: t_assign,
  t_ast_list: t_ast_list,
  t_bind: t_bind,
  t_block: t_block,
  t_bool: t_bool,
  t_bracket: t_bracket,
  t_case: t_case,
  t_case_branch: t_case_branch,
  t_cases: t_cases,
  t_cases_branch: t_cases_branch,
  t_cases_else: t_cases_else,
  t_colon: t_colon,
  t_colon_bracket: t_colon_bracket,
  t_const_import: t_const_import,
  t_data: t_data,
  t_data_field: t_data_field,
  t_dot: t_dot,
  t_extend: t_extend,
  t_file_import: t_file_import,
  t_for: t_for,
  t_for_bind: t_for_bind,
  t_check: t_check,
  t_fun: t_fun,
  t_id: t_id,
  t_if: t_if,
  t_if_branch: t_if_branch,
  t_if_else: t_if_else,
  t_import: t_import,
  t_lam: t_lam,
  t_left_app: t_left_app,
  t_let: t_let,
  t_list: t_list,
  t_method: t_method,
  t_method_field: t_method_field,
  t_not: t_not,
  t_num: t_num,
  t_obj: t_obj,
  t_op: t_op,
  t_paren: t_paren,
  t_program: t_program,
  t_provide: t_provide,
  t_provide_all: t_provide_all,
  t_singleton_variant: t_singleton_variant,
  t_str: t_str,
  t_try: t_try,
  t_var: t_var,
  t_variant: t_variant,
  t_when: t_when,
} end

import "pprint.arr" as PP
import ast as A
import "labelled-tree.arr" as LT

List = list.List

INDENT = 2

fun stamp(tag, checker):
  checker.{unique-tag: tag}
end

fun SourceAST(e :: Any) -> Bool:
  Program(e) or
  Header(e) or
  ImportType(e) or
  Expr(e) or
  Bind(e) or
  Member(e) or
  ForBind(e) or
  Variant(e) or
  IfBranch(e) or
  CasesBranch(e) or
  Ann(e) or
  AField(e) or
  ASTList(e)
end

fun from-ast(ast):
  fun h(a):
    fun err(t):
      raise("Encountered unknown " + t + ": " + tostring(a))
    end
    if List(a):
      ast_list(a.map(h))
    else if A.Program(a):
      cases(A.Program) a:
        | s_program(_, imports, block) => s_program(h(imports), h(block))
      end
    else if A.Header(a): 
      cases(A.Header) a:
        | s_import(_, file, name) => s_import(h(file), h(name))
        | s_provide(_, block) => s_provide(h(block))
        | s_provide_all(_) => s_provide_all
      end
    else if A.ImportType(a):
      cases(A.ImportType) a:
        | s_file_import(_, file) => s_file_import(h(file))
        | s_const_import(_, module) => s_const_import(h(module))
      end
    else if A.Expr(a):
      cases(A.Expr) a:
        | s_block(_, stmts) => s_block(h(stmts))
        | s_fun(_, name, params, args, ann, doc, body, _check) =>
          s_fun(h(name), h(params), h(args), h(ann), h(doc), h(body), h(_check))
        | s_var(_, name, value) => s_var(h(name), h(value))
        | s_let(_, name, value) => s_let(h(name), h(value))
        | s_when(_, test, block) => s_when(h(test), h(block))
        | s_assign(_, id, value) => s_assign(h(id), h(value))
        | s_if(_, branches) => s_if(h(branches))
        | s_if_else(_, branches, _else) => s_if_else(h(branches), h(_else))
        | s_cases(_, type, val, branches) => s_cases(h(type), h(val), h(branches))
        | s_cases_else(_, type, val, branches, _else) =>
          s_cases_else(h(type), h(val), h(branches), h(_else))
        | s_try(_, body, id, _except) => s_try(h(body), h(id), h(_except))
        | s_op(_, op, left, right) => s_op(h(op), h(left), h(right))
        | s_not(_, expr) => s_not(h(expr))
        | s_paren(_, expr) => s_paren(h(expr))
        | s_lam(_, params, args, ann, doc, body, _check) =>
          s_lam(h(params), h(args), h(ann), h(doc), h(body), h(_check))
        | s_method(_, args, ann, doc, body, _check) =>
          s_method(h(args), h(ann), h(doc), h(body), h(_check))
        | s_extend(_, super, fields) => s_extend(h(super), h(fields))
        | s_obj(_, fields) => s_obj(h(fields))
        | s_list(_, values) => s_list(h(values))
        | s_app(_, _fun, args) => s_app(h(_fun), h(args))
        | s_left_app(_, obj, _fun, args) => s_left_app(h(obj), h(_fun), h(args))
        | s_id(_, id) => s_id(h(id))
        | s_num(_, n) => s_num(h(n))
        | s_bool(_, b) => s_bool(h(b))
        | s_str(_, s) => s_str(h(s))
        | s_dot(_, obj, field) => s_dot(h(obj), h(field))
        | s_bracket(_, obj, field) => s_bracket(h(obj), h(field))
        | s_colon(_, obj, field) => s_colon(h(obj), h(field))
        | s_colon_bracket(_, obj, field) => s_colon_bracket(h(obj), h(field))
        | s_data(_, name, params, mixins, variants, shared_members, _check) =>
          s_data(h(name), h(params), h(mixins), h(variants), h(shared_members), h(_check))
        | s_for(_, iterator, bindings, ann, body) =>
          s_for(h(iterator), h(bindings), h(ann), h(body))
        | s_check(_, body) => s_check(h(body))
        | else => raise("Missed an expr: " + torepr(a))
      end
    else if A.Bind(a):
      cases(A.Bind) a:
        | s_bind(_, id, ann) => s_bind(h(id), h(ann))
      end
    else if A.Member(a):
      cases(A.Member) a:
        | s_data_field(_, name, value) => s_data_field(h(name), h(value))
        | s_mutable_field(_, name, value, ann) => s_mutable_field(h(name), h(value), h(ann))
        | s_once_field(_, name, value, ann) => s_once_field(h(name), h(value), h(ann))
        | s_method_field(_, name, args, ann, doc, body, _check) =>
          s_method_field(h(name), h(args), h(ann), h(doc), h(body), h(_check))
      end
    else if A.ForBind(a):
      cases(A.ForBind) a:
        | s_for_bind(_, bind, value) => s_for_bind(h(bind), h(value))
      end
    else if A.VariantMember(a):
      cases(A.VariantMember) a:
        | s_variant_member(_, type, value) => s_variant_member(type, h(value))
      end
    else if A.Variant(a):
      cases(A.Variant) a:
        | s_variant(_, name, binds, with_members) => s_variant(h(name), h(binds), h(with_members))
        | s_singleton_variant(_, name, with_members) => s_singleton_variant(h(name), h(with_members))
      end
    else if A.IfBranch(a):
      cases(A.IfBranch) a:
        | s_if_branch(_, test, body) => s_if_branch(h(test), h(body))
      end
    else if A.CasesBranch(a):
      cases(A.CasesBranch) a:
        | s_cases_branch(_, name, args, body) => s_cases_branch(h(name), h(args), h(body))
      end
    else if A.Ann(a):
      cases(A.Ann) a:
        | a_blank => a_blank
        | a_any => a_any
        | a_name(_, id) => a_name(h(id))
        | a_arrow(_, args, ret) => a_arrow(h(args), h(ret))
        | a_method(_, args, ret) => a_method(h(args), h(ret))
        | a_record(_, fields) => a_record(h(fields))
        | a_app(_, ann, args) => a_app(h(ann), h(args))
        | a_pred(_, ann, exp) => a_pred(h(ann), h(exp))
        | a_dot(_, obj, field) => a_dot(h(obj), h(field))
      end
    else if A.AField(a):
      cases(A.AField) a:
        | a_field(_, name, ann) => a_field(h(name), h(ann))
      end
    else: a
    end
  end
  h(ast)
end


fun generic-topprint(obj):
  fun h(cur-obj):
    if builtins.has-field(cur-obj, "fields") and
      builtins.has-field(cur-obj, "node-name"): # what we need for generic printing
      label = generic-topprint(cur-obj.node-name())
      fields = cur-obj.fields()
      pp-fields = for list.map(f from fields):
        if list.List(f):
          PP.label-align-surround(
            PP.string(""),
            PP.lbrack,
            PP.commabreak,
            f.map(h),
            PP.rbrack)
        else:
          h(f)
        end
      end
      PP.label-align-surround(
        label,
        PP.lparen,
        PP.commabreak,
        pp-fields,
        PP.rparen)
    else:
      if String(cur-obj):
        PP.string("\"" + cur-obj + "\"")
      else:
        PP.string(tostring(cur-obj))
      end
    end
  end
  h(obj)
end

fun generic-pretty(obj,width): generic-topprint(obj).pretty(width) end

data TProgram:
  | t_program with: tostring(self): "TProgram" end
end
data THeader:
  | t_import with: tostring(self): "TImport" end
  | t_provide with: tostring(self): "TProvide" end
  | t_provide_all with: tostring(self): "TProvideAll" end
end
data TImportType:
  | t_file_import with: tostring(self): "TFileImport" end
  | t_const_import with: tostring(self): "TConstImport" end
end
data TExpr:
  | t_block with: tostring(self): "TBlock" end
  | t_fun with: tostring(self): "TFun" end
  | t_var with: tostring(self): "TVar" end
  | t_let with: tostring(self): "TLet" end
  | t_when with: tostring(self): "TWhen" end
  | t_assign with: tostring(self): "TAssign" end
  | t_case with: tostring(self): "TCase" end
  | t_if with: tostring(self): "TIf" end
  | t_if_else with: tostring(self): "TIfElse" end
  | t_cases with: tostring(self): "TCases" end
  | t_cases_else with: tostring(self): "TCasesElse" end
  | t_try with: tostring(self): "TTry" end
  | t_op with: tostring(self): "TOp" end
  | t_not with: tostring(self): "TNot" end
  | t_paren with: tostring(self): "TParen" end
  | t_lam with: tostring(self): "TLam" end
  | t_method with: tostring(self): "TMethod" end
  | t_extend with: tostring(self): "TExtend" end
  | t_obj with: tostring(self): "TObj" end
  | t_list with: tostring(self): "TList" end
  | t_app with: tostring(self): "TApp" end
  | t_left_app with: tostring(self): "TLeftApp" end
  | t_id with: tostring(self): "TId" end
  | t_num with: tostring(self): "TNum" end
  | t_bool with: tostring(self): "TBool" end
  | t_str with: tostring(self): "TStr" end
  | t_dot with: tostring(self): "TDot" end
  | t_bracket with: tostring(self): "TBracket" end
  | t_colon with: tostring(self): "TColon" end
  | t_colon_bracket with: tostring(self): "TColonBracket" end
  | t_data with: tostring(self): "TData" end
  | t_for with: tostring(self): "TFor" end
  | t_check with: tostring(self): "TCheck" end
end
data TBind:
  | t_bind with: tostring(self): "TBind" end
end
data TMember:
  | t_data_field with: tostring(self): "TDataField" end
  | t_mutable_field with: tostring(self): "TMutableField" end
  | t_once_field with: tostring(self): "TOnceField" end
  | t_method_field with: tostring(self): "TMethodField" end
end
data TForBind:
  | t_for_bind with: tostring(self): "TForBind" end
end
data TVariantMember:
  | t_variant_member with: tostring(self): "TVariant" end
end
data TVariant:
  | t_variant with: tostring(self): "TVariant" end
  | t_singleton_variant with: tostring(self): "TSingletonVariant" end
end
data TCaseBranch:
  | t_case_branch with: tostring(self): "TCaseBranch" end
end
data TIfBranch:
  | t_if_branch with: tostring(self): "TIfBranch" end
end
data TCasesBranch:
  | t_cases_branch with: tostring(self): "TCasesBranch" end
end
data TAnn:
  | t_a_blank with: tostring(self): "TABlank" end
  | t_a_any with: tostring(self): "TAAny" end
  | t_a_name with: tostring(self): "TAName" end
  | t_a_arrow with: tostring(self): "TAArrow" end
  | t_a_method with: tostring(self): "TAMethod" end
  | t_a_record with: tostring(self): "TARecord" end
  | t_a_app with: tostring(self): "TAApp" end
  | t_a_pred with: tostring(self): "TAPred" end
  | t_a_dot with: tostring(self): "TADot" end
end
data TAField:
  | t_a_field with: tostring(self): "TAField" end
end



# to-labelled function for all ast-nodes
fun generic-to-labelled(ast, T):
  
  fun h(a):
    n =
      if ASTList(a):
        cases(ASTList) a:
          | ast_list(fields) =>
            node = T.node(a.node-name(), "ASTList", fields.map(h))
            node.{mk-ast(l): ast_list(l.children.map(fun (c): c.mk-ast() end)) end}
        end
      else if Program(a):
        cases(Program) a:
          | s_program(imports, block) =>
            node = T.node(a.node-name(), "", [h(imports), h(block)])
            node.{mk-ast(l): s_program(l.mkc(0), l.mkc(1)) end}
        end
      else if Header(a):
        cases(Header) a:
          | s_import(file, name) =>
            node = T.node(a.node-name(), "", [h(file), h(name)])
            node.{mk-ast(l): s_import(l.mkc(0), l.mkc(1)) end}
          | s_provide(block) =>
            node = T.node(a.node-name(), "", [h(block)])
            node.{mk-ast(l): s_provide(l.mkc(0)) end}
          | s_provide_all =>
            node = T.node(a.node-name(), "", [])
            node.{mk-ast(l): s_provide_all end}
        end
      else if ImportType(a):
        cases(ImportType) a:
          | s_file_import(file) =>
            node = T.node(a.node-name(), "", [h(file)])
            node.{mk-ast(l): s_file_import(l.mkc(0)) end}
          | s_const_import(module) =>
            node = T.node(a.node-name(), "", [h(module)])
            node.{mk-ast(l): s_const_import(l.mkc(0)) end}
        end
      else if Expr(a):
        cases(Expr) a:
          | s_block(stmts) =>
            node = T.node(a.node-name(), "", [h(stmts)])
            node.{mk-ast(l): s_block(l.mkc(0)) end}
          | s_fun(name, params, args, ann, doc, body, _check) =>
            node = T.node(a.node-name(), "", [h(name), h(params), h(args), h(ann), h(doc), h(body), h(_check)])
            node.{mk-ast(l): s_fun(l.mkc(0), l.mkc(1), l.mkc(2), l.mkc(3), l.mkc(4), l.mkc(5), l.mkc(6), l.mkc(7)) end}
          | s_var(name, value) =>
            node = T.node(a.node-name(), "", [h(name), h(value)])
            node.{mk-ast(l): s_var(l.mkc(0), l.mkc(1)) end}
          | s_let(name, value) =>
            node = T.node(a.node-name(), "", [h(name), h(value)])
            node.{mk-ast(l): s_var(l.mkc(0), l.mkc(1)) end}
          | s_when(test, block) =>
            node = T.node(a.node-name(), "", [h(test), h(block)])
            node.{mk-ast(l): s_when(l.mkc(0), l.mkc(1)) end}
          | s_assign(id, value) =>
            node = T.node(a.node-name(), "", [h(id), h(value)])
            node.{mk-ast(l): s_assign(l.mkc(0), l.mkc(1)) end}
          | s_if(branches) =>
            node = T.node(a.node-name(), "", [h(branches)])
            node.{mk-ast(l): s_if(l.mkc(0)) end}
          | s_if_else(branches, _else) =>
            node = T.node(a.node-name(), "", [h(branches), h(_else)])
            node.{mk-ast(l): s_if_else(l.mkc(0), l.mkc(1)) end}
          | s_cases(type, val, branches) =>
            node = T.node(a.node-name(), "", [h(type), h(val), h(branches)])
            node.{mk-ast(l): s_cases(l.mkc(0), l.mkc(1), l.mkc(2)) end}
          | s_cases_else(type, val, branches, _else) =>
            node = T.node(a.node-name(), "", [h(type), h(val), h(branches), h(_else)])
            node.{mk-ast(l): s_cases_else(l.mkc(0), l.mkc(1), l.mkc(2), l.mkc(3)) end}
          | s_try(body, id, _except) =>
            node = T.node(a.node-name(), "", [h(body), h(id), h(_except)])
            node.{mk-ast(l): s_try(l.mkc(0), l.mkc(1), l.mkc(2)) end}
          | s_op(op, left, right) =>
            node = T.node(a.node-name(), "", [h(op), h(left), h(right)])
            node.{mk-ast(l): s_op(l.mkc(0), l.mkc(1), l.mkc(2)) end}
          | s_not(expr) =>
            node = T.node(a.node-name(), "", [h(expr)])
            node.{mk-ast(l): s_not(l.mkc(0)) end}
          | s_paren(expr) =>
            node = T.node(a.node-name(), "", [h(expr)])
            node.{mk-ast(l): s_paren(l.mkc(0)) end}
          | s_lam(params, args, ann, doc, body, _check) =>
            node = T.node(a.node-name(), "",[h(params), h(args), h(ann), h(doc), h(body), h(_check)])
            node.{mk-ast(l): s_lam(l.mkc(0),l.mkc(1),l.mkc(2), l.mkc(3),l.mkc(4),l.mkc(5)) end}
          | s_method(args, ann, doc, body, _check) =>
            node = T.node(a.node-name(), "",[h(args), h(ann), h(doc), h(body), h(_check)])
            node.{mk-ast(l): s_method(l.mkc(0),l.mkc(1),l.mkc(2), l.mkc(3),l.mkc(4)) end}
          | s_extend(super, fields) =>
            node = T.node(a.node-name(), "", [h(super), h(fields)])
            node.{mk-ast(l): s_extend(l.mkc(0), l.mkc(1)) end}
          | s_obj(fields) =>
            node = T.node(a.node-name(), "", [h(fields)])
            node.{mk-ast(l): s_obj(l.mkc(0)) end}
          | s_list(values) =>
            node = T.node(a.node-name(), "", [h(values)])
            node.{mk-ast(l): s_list(l.mkc(0)) end}
          | s_app(_fun, args) =>
            node = T.node(a.node-name(), "", [h(_fun), h(args)])
            node.{mk-ast(l): s_app(l.mkc(0), l.mkc(1)) end}
          | s_left_app(obj, _fun, args) =>
            node = T.node(a.node-name(), "", [h(obj), h(_fun), h(args)])
            node.{mk-ast(l): s_left_app(l.mkc(0), l.mkc(1), l.mkc(2)) end}
          | s_id(id) =>
            node = T.node(a.node-name(), "", [h(id)])
            node.{mk-ast(l): s_id(l.mkc(0)) end}
          | s_num(num) =>
            node = T.node(a.node-name(), "", [h(num)])
            node.{mk-ast(l): s_num(l.mkc(0)) end}
          | s_bool(b) =>
            node = T.node(a.node-name(), "", [h(b)])
            node.{mk-ast(l): s_bool(l.mkc(0)) end}
          | s_str(s) =>
            node = T.node(a.node-name(), "", [h(s)])
            node.{mk-ast(l): s_str(l.mkc(0)) end}
          | s_dot(obj, field) =>
            node = T.node(a.node-name(), "", [h(obj), h(field)])
            node.{mk-ast(l): s_dot(l.mkc(0), l.mkc(1)) end}
          | s_bracket(obj, field) =>
            node = T.node(a.node-name(), "", [h(obj), h(field)])
            node.{mk-ast(l): s_bracket(l.mkc(0), l.mkc(1)) end}
          | s_colon(obj, field) =>
            node = T.node(a.node-name(), "", [h(obj), h(field)])
            node.{mk-ast(l): s_colon(l.mkc(0), l.mkc(1)) end}
          | s_colon_bracket(obj, field) =>
            node = T.node(a.node-name(), "", [h(obj), h(field)])
            node.{mk-ast(l): s_colon_bracket(l.mkc(0), l.mkc(1)) end}
          | s_data(name, params, mixins, variants, shared_members, _check) =>
            node = T.node(a.node-name(), "", [h(name), h(params), h(mixins), h(variants), h(shared_members), h(_check)])
            node.{mk-ast(l): s_data(l.mkc(0), l.mkc(1), l.mkc(2), l.mkc(3), l.mkc(4), l.mkc(5)) end}
          | s_for(iterator, bindings, ann, body) =>
            node = T.node(a.node-name(), "", [h(iterator), h(bindings), h(ann), h(body)])
            node.{mk-ast(l): s_for(l.mkc(0), l.mkc(1), l.mkc(2), l.mkc(3)) end}
          | s_check(body) =>
            node = T.node(a.node-name(), "", [h(body)])
            node.{mk-ast(l): s_check(l.mkc(0)) end}
        end
      else if Bind(a):
        cases(Bind) a:
          | s_bind(id, ann) =>
            node = T.node(a.node-name(), "", [h(id), h(ann)])
            node.{mk-ast(l): s_bind(l.mkc(0), l.mkc(1)) end}
        end
      else if Member(a):
        cases(Member) a:
          | s_data_field(name, value) =>
            node = T.node(a.node-name(), "", [h(name), h(value)])
            node.{mk-ast(l): s_data_field(l.mkc(0), l.mkc(1)) end}
          | s_method_field(name, args, ann, doc, body, _check) =>
            node = T.node(a.node-name(), "", [h(name), h(args), h(ann), h(doc), h(body), h(_check)])
            node.{mk-ast(l): s_method_field(l.mkc(0), l.mkc(1), l.mkc(2), l.mkc(3), l.mkc(4), l.mkc(5)) end}
        end
      else if ForBind(a):
        cases(ForBind) a:
          | s_for_bind(bind, value) =>
            node = T.node(a.node-name(), "", [h(bind), h(value)])
            node.{mk-ast(l): s_for_bind(l.mkc(0), l.mkc(1)) end}
        end
      else if Variant(a):
        cases(Variant) a:
          | s_variant(name, binds, with_members) =>
            node = T.node(a.node-name(), "", [h(name), h(binds), h(with_members)])
            node.{mk-ast(l): s_variant(l.mkc(0), l.mkc(1), l.mkc(2)) end}
          | s_singleton_variant(name, with_members) =>
            node = T.node(a.node-name(), "", [h(name), h(with_members)])
            node.{mk-ast(l): s_singleton_variant(l.mkc(0), l.mkc(1)) end}
        end
      else if IfBranch(a):
        cases(IfBranch) a:
          | s_if_branch(test, body) =>
            node = T.node(a.node-name(), "", [h(test), h(body)])
            node.{mk-ast(l): s_if_branch(l.mkc(0), l.mkc(1)) end}
        end
      else if CasesBranch(a):
        cases(CasesBranch) a:
          | s_cases_branch(name, args, body) =>
            node = T.node(a.node-name(), "", [h(name), h(args), h(body)])
            node.{mk-ast(l): s_cases_branch(l.mkc(0), l.mkc(1), l.mkc(2)) end}
        end
      else if Ann(a):
        cases(Ann) a:
          | a_blank =>
            node = T.node(a.node-name(), "", [])
            node.{mk-ast(l): a_blank end}
          | a_any =>
            node = T.node(a.node-name(), "", [])
            node.{mk-ast(l): a_any end}
          | a_name(id) =>
            node = T.node(a.node-name(), "", [h(id)])
            node.{mk-ast(l): a_name(l.mkc(0)) end}
          | a_arrow(args, ret) =>
            node = T.node(a.node-name(), "", [h(args), h(ret)])
            node.{mk-ast(l): a_arrow(l.mkc(0), l.mkc(1)) end}
          | a_method(args, ret) =>
            node = T.node(a.node-name(), "", [h(args), h(ret)])
            node.{mk-ast(l): a_method(l.mkc(0), l.mkc(1)) end}
          | a_record(fields) =>
            node = T.node(a.node-name(), "", [h(fields)])
            node.{mk-ast(l): a_record(l.mkc(0)) end}
          | a_app(ann, args) =>
            node = T.node(a.node-name(), "", [h(ann), h(args)])
            node.{mk-ast(l): a_app(l.mkc(0), l.mkc(1)) end}
          | a_pred(ann, exp) =>
            node = T.node(a.node-name(), "", [h(ann), h(exp)])
            node.{mk-ast(l): a_pred(l.mkc(0), l.mkc(1)) end}
          | a_dot(obj, field) =>
            node = T.node(a.node-name(), "", [h(obj), h(field)])
            node.{mk-ast(l): a_dot(l.mkc(0), l.mkc(1)) end}
        end
      else if AField(a):
        cases(AField) a:
          | a_field(name, ann) =>
            node = T.node(a.node-name(), "", [h(name), h(ann)])
            node.{mk-ast(l): a_field(l.mkc(0), l.mkc(1)) end}
        end
      else if Number(a) or String(a) or Bool(a):
        node = T.leaf(a, a)
        node.{mk-ast(l): a end}
      else:
        print("SHOULD BE IMPOSSIBLE: Missing case: " + tostring(a))
        node = T.leaf(tostring(a), tostring(a))
        node.{mk-ast(l): a end}
      end
    n.{ast: a}
  end
  h(ast)
end

fun generic-hash-key(obj):
  obj.node-name() + tostring(obj.arity())
end

fun generic-tostring(obj):
  for list.fold(acc from "", line from obj.pretty(160)):
    if acc == "": line
    else: acc + "\n" + line
    end
  end
end

fun generic-print(obj,width):
  list.each(print, generic-pretty(obj, width))
end

fun field-equals(o1, o2):
  for list.fold2(acc from true, f1 from o1.fields(), f2 from o2.fields()):
    acc and (f1 == f2)
  end
end

fun loose-field-equals(o1, o2):
  for list.fold2(acc from {eq:true, first:false}, f1 from o1.fields(), f2 from o2.fields()):
    f-eq = 
      if (not (SourceAST(f1) and SourceAST(f2))): f1 == f2
      else: acc.eq and (f1.loose-equals(f2)).eq end
    res = {eq:f-eq, first: (not acc.first) and f-eq}
    # when res.first: print("LFE FIRST: " + tostring(o1) + "\n-----\n" + tostring(o2)) end
    # when not res.eq: print(tostring(f1) + "\nl=/=\n" + tostring(f2)) end
    res
  end
end

fun generic-loose-equals(o1, o2):
  feq = o1.equals(o2, loose-field-equals)
  ofeq = if Bool(feq): {eq:feq, first: feq} else: feq end
  is-leq =
    ofeq.eq or
  (is-s_str(o2) and (o2.s.substring(0,6).contains("IGNORE"))) or
  (is-s_str(o1) and (o1.s.substring(0,6).contains("IGNORE")))
  res = {eq:is-leq, first: (not ofeq.first) and is-leq}
  # when res.first: print("GLE FIRST") end
  # when not res.eq: print(tostring(o1) + "\nl=/=\n" + tostring(o2)) end
  res
end

fun funlam_tosource(funtype, name, params, args :: ASTList,
    ann :: Ann, doc :: String, body :: Expr, _check :: Expr) -> PP.PPrintDoc:
  typarams =
    if is-nothing(params): PP.empty
    else: PP.surround-separate(INDENT, 0, PP.empty, PP.langle, PP.commabreak, PP.rangle,
        params.fields().map(fun(p): PP.string(p) end))
    end
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.fields().map(fun(a): a.tosource() end)))
  ftype = funtype + typarams
  fname = 
    if is-nothing(name): ftype
    else if PP.is-empty(ftype): PP.string(name)
    else: ftype + PP.string(" " + name)
    end
  fann =
    if is-a_blank(ann) or is-nothing(ann): PP.empty
    else: PP.break(1) + PP.string("-> ") + ann.tosource()
    end
  header = PP.group(fname + arg-list + fann + PP.string(":"))
  checker = _check.tosource()
  footer =
    if PP.is-empty(checker): PP.string("end")
    else: PP.surround(INDENT, 1, PP.string("where:"), _check.tosource(), PP.string("end"))
    end
  docstr =
    if is-nothing(doc) or (doc == ""): PP.empty
    else: PP.string("doc: ") + PP.dquote(PP.string(doc)) + PP.hardline
    end
  PP.surround(INDENT, 1, header, docstr + body.tosource(), footer)
end


data Program:
  | s_program(imports :: ASTList, block :: Expr) with:
    fields(self): [self.imports, self.block] end,
    node-name(self): t_program end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  print-source(self, width): list.each(print, self.tosource().pretty(width)) end,
  hash-key(self): generic-hash-key(self) end,
  tosource(self):
    PP.group(
      PP.flow_map(PP.hardline, fun(i): i.tosource() end, self.imports.fields())
        + PP.hardline
        + self.block.tosource()
      )
  end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_program(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data Header:
  | s_import(file :: ImportType, name :: String) with:
    fields(self): [self.file, self.name] end,
    node-name(self): t_import end,
    tosource(self):
      PP.flow([PP.string("import"), PP.quote(PP.string(self.file)),
          PP.string("as"), PP.string(self.name)])
    end
  | s_provide(block :: Expr) with:
    fields(self): [self.block] end,
    node-name(self): t_provide end,
    tosource(self):
      PP.soft-surround(INDENT, 1, PP.string("provide"),
        self.block.tosource(), PP.string("end"))
    end
  | s_provide_all with:
    fields(self): [] end,
    node-name(self): t_provide_all end,
    tosource(self): PP.string("provide *") end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_import(self) and is-s_import(other): feq-fun(self, other)
    else if is-s_provide(self) and is-s_provide(other): feq-fun(self, other)
    else if is-s_provide_all(self): is-s_provide_all(other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data ImportType:
  | s_file_import(file :: String) with:
    fields(self): [self.file] end,
    node-name(self): t_file_import end,
    tosource(self): PP.string("import") + PP.break(1) + PP.dquote(PP.string(self.file)) end
  | s_const_import(module :: String) with:
    fields(self): [self.module] end,
    node-name(self): t_const_import end,
    tosource(self): PP.string("import") + PP.break(1) + PP.string(self.module) end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  print-source(self, width): list.each(print, self.tosource().pretty(width)) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_file_import(self) and is-s_file_import(other): feq-fun(self, other)
    else if is-s_const_import(self) and is-s_const_import(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data Expr:
  | s_block(stmts :: ASTList) with:
    fields(self): [self.stmts] end,
    node-name(self): t_block end,
    # List<Expr U Expr>, actually
    tosource(self):
      PP.flow_map(PP.hardline, fun(s):
          s.tosource()
      end, self.stmts.fields()) end
  | s_fun(
      name :: String,
      params :: ASTList, # Type parameters
      args :: ASTList, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
      ) with:
    fields(self): [self.name, self.params, self.args, self.ann,
      self.doc, self.body, self.check] end,
    node-name(self): t_fun end,
    tosource(self):
      funlam_tosource(PP.string("fun"),
        self.name, self.params, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_var(name :: Bind, value :: Expr)  with:
    fields(self): [self.name, self.value] end,
    node-name(self): t_var end,
    tosource(self):
      PP.string("var ")
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + PP.string(" =") + PP.break(1) + self.value.tosource()))
    end
  | s_let(name :: Bind, value :: Expr)  with:
    fields(self): [self.name, self.value] end,
    node-name(self): t_let end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.name.tosource() + PP.string(" =") + PP.break(1) + self.value.tosource()))
    end
  | s_when(test :: Expr, block :: Expr) with:
    fields(self): [self.test, self.block] end,
    node-name(self): t_when end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        PP.string("when") + PP.parens(self.test.tosource()) + PP.string(":"),
        self.block.tosource(),
        PP.string("end"))
    end
  | s_assign(id :: String, value :: Expr) with:
    fields(self): [self.id, self.value] end,
    node-name(self): t_assign end,
    tosource(self):
      PP.nest(INDENT, PP.string(self.id) + PP.string(" :=") + PP.break(1) + self.value.tosource())
    end
  | s_if(branches :: ASTList) with:
    fields(self): [self.branches] end,
    node-name(self): t_if end,
    tosource(self):
      branches = PP.separate(PP.break(1) + PP.string("else "),
        self.branches.fields().map(fun(b): b.tosource() end))
      PP.group(branches + PP.break(1) + PP.string("end"))
    end      
  | s_if_else(branches :: ASTList, _else :: Expr) with:
    fields(self): [self.branches, self._else] end,
    node-name(self): t_if_else end,
    tosource(self):
      branches = PP.separate(PP.break(1) + PP.string("else "),
        self.branches.fields().map(fun(b): b.tosource() end))
      _else = PP.string("else:") + PP.nest(INDENT, PP.break(1) + self._else.tosource())
      PP.group(branches + PP.break(1) + _else + PP.break(1) + PP.string("end"))
    end
  | s_cases(type :: Ann, val :: Expr, branches :: ASTList) with:
    fields(self): [self.type, self.val, self.branches] end,
    node-name(self): t_cases end,
    tosource(self):
      header = PP.string("cases") + PP.parens(self.type.tosource()) + PP.break(1)
        + self.val.tosource() + PP.string(":")
      PP.surround-separate(INDENT, 1, header + PP.string(" end"),
        PP.group(header), PP.break(1), PP.string("end"),
        self.branches.fields().map(fun(b): PP.group(b.tosource()) end))
    end
  | s_cases_else(type :: Ann, val :: Expr, branches :: ASTList, _else :: Expr) with:
    fields(self): [self.type, self.val, self.branches, self._else] end,
    node-name(self): t_cases_else end,
    tosource(self):
      header = PP.string("cases") + PP.parens(self.type.tosource()) + PP.break(1)
        + self.val.tosource() + PP.string(":")
      body = PP.separate(PP.break(1), self.branches.fields().map(fun(b): PP.group(b.tosource()) end))
        + PP.break(1) + PP.group(PP.string("| else =>") + PP.break(1) + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, PP.string("end"))
    end
  | s_try(body :: Expr, id :: Bind, _except :: Expr)  with:
    fields(self): [self.body, self.id, self._except] end,
    node-name(self): t_try end,
    tosource(self):
      _try = PP.string("try:") + PP.break(1)
        + PP.nest(INDENT, self.body.tosource()) + PP.break(1)
      _except = PP.string("except") + PP.parens(self.id.tosource()) + PP.string(":") + PP.break(1)
        + PP.nest(INDENT, self._except.tosource()) + PP.break(1)
      PP.group(_try + _except + PP.string("end"))
    end
  | s_op(op :: String, left :: Expr, right :: Expr)  with:
    fields(self): [self.op, self.left, self.right] end,
    node-name(self): t_op end,
    tosource(self): PP.infix(INDENT, 1, PP.string(self.op.substring(2, self.op.length())), self.left.tosource(), self.right.tosource()) end
  | s_not(expr :: Expr)  with:
    fields(self): [self.expr] end,
    node-name(self): t_not end,
    tosource(self): PP.nest(INDENT, PP.flow([PP.string("not"), self.expr.tosource()])) end
  | s_paren(expr :: Expr)  with:
    fields(self): [self.expr] end,
    node-name(self): t_paren end,
    tosource(self): PP.parens(self.expr.tosource()) end
  | s_lam(
      params :: ASTList, # Type parameters
      args :: ASTList, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
      )  with:
    fields(self): [self.params, self.args, self.ann,
      self.doc, self.body, self.check] end,
    node-name(self): t_lam end,
    tosource(self):
      funlam_tosource(PP.string("fun"),
        nothing, self.params, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_method(
      args :: ASTList, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
      ) with:
    fields(self): [self.args, self.ann, self.doc, self.body, self.check] end,
    node-name(self): t_method end,
    tosource(self):
      funlam_tosource(PP.string("method"),
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_extend(super :: Expr, flds :: ASTList)  with:
    fields(self): [self.super, self.flds] end,
    node-name(self): t_extend end,
    tosource(self):
      PP.group(self.super.tosource() + PP.string(".")
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.flds.fields().map(fun(f): f.todatafield() end)))
    end
  | s_obj(flds :: ASTList)  with:
    fields(self): [self.flds] end,
    node-name(self): t_obj end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.flds.fields().map(fun(f): f.todatafield() end))
    end
  | s_list(values :: ASTList) with:
    fields(self): [self.values] end,
    node-name(self): t_list end,
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.string("[]"), PP.lbrack, PP.commabreak, PP.rbrack,
        self.values.fields().map(fun(v): v.tosource() end))
    end
  | s_app(_fun :: Expr, args :: ASTList) with:
    fields(self): [self._fun, self.args] end,
    node-name(self): t_app end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.fields().map(fun(f): f.tosource() end)))))
    end
  | s_left_app(obj :: Expr, _fun :: Expr, args :: ASTList) with:
    fields(self): [self.obj, self._fun, self.args] end,
    node-name(self): t_left_app end,
    tosource(self):
      PP.group(self.obj.tosource() + PP.nest(INDENT, PP.break(0) + PP.string(".") + self._fun.tosource())
          + PP.parens(PP.separate(PP.commabreak, self.args.fields().map(fun(f): f.tosource() end))))
    end
  | s_id(id :: String) with:
    fields(self): [self.id] end,
    node-name(self): t_id end,
    tosource(self): PP.string(self.id) end
  | s_num(n :: Number) with:
    fields(self): [self.n] end,
    node-name(self): t_num end,
    tosource(self): PP.number(self.n) end
  | s_bool(b :: Bool) with:
    fields(self): [self.b] end,
    node-name(self): t_bool end,
    tosource(self): PP.string(self.b.tostring()) end
  | s_str(s :: String) with:
    fields(self): [self.s] end,
    node-name(self): t_str end,
    tosource(self): PP.dquote(PP.string(self.s)) end
  | s_dot(obj :: Expr, field :: String) with:
    fields(self): [self.obj, self.field] end,
    node-name(self): t_dot end,
    tosource(self): PP.infix(INDENT, 0, PP.string("."), self.obj.tosource(), PP.string(self.field)) end
  | s_bracket(obj :: Expr, field :: Expr) with:
    fields(self): [self.obj, self.field] end,
    node-name(self): t_bracket end,
    tosource(self): PP.infix(INDENT, 0, PP.string("."), self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s_colon(obj :: Expr, field :: String) with:
    fields(self): [self.obj, self.field] end,
    node-name(self): t_colon end,
    tosource(self): PP.infix(INDENT, 0, PP.string(":"), self.obj.tosource(), PP.string(self.field)) end
  | s_colon_bracket(obj :: Expr, field :: Expr) with:
    fields(self): [self.obj, self.field] end,
    node-name(self): t_bracket end,
    tosource(self): PP.infix(INDENT, 0, PP.string(":"), self.obj.tosource(),
        PP.surround(PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s_data(
      name :: String,
      params :: ASTList, # type params
      mixins :: ASTList, # exprs, of mixins
      variants :: ASTList,
      shared_members :: ASTList,
      check :: Expr
      ) with:
    fields(self): [self.name, self.params, self.variants,
      self.shared_members, self.check] end,
    node-name(self): t_data end,
    tosource(self):
      fun optional_section(lbl, section):
        if PP.is-empty(section): PP.empty
        else: PP.break(1) + PP.group(PP.nest(INDENT, lbl + PP.break(1) + section))
        end
      end
      tys = PP.surround-separate(2*INDENT, 0, PP.empty, PP.langle, PP.commabreak, PP.rangle,
        self.params.fields().map(fun(f): f.tosource() end))
      header = PP.string("data ") + PP.string(self.name) + tys + PP.string(":")
      _deriving =
        PP.surround-separate(INDENT, 0, PP.empty, PP.break(1) + PP.string("deriving "), PP.commabreak, PP.empty, self.mixins.fields().map(fun(m): m.tosource() end))
      variants = PP.separate(PP.break(1) + PP.string("| "),
        PP.string("")^list.link(self.variants.fields().map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional_section(PP.string("sharing:"),
        PP.separate(PP.commabreak, self.shared_members.fields().map(fun(s): s.todatafield() end)))
      _check = optional_section(PP.string("where:"), self.check.tosource())
      footer = PP.break(1) + PP.string("end")
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s_for(
      iterator :: Expr,
      bindings :: ASTList,
      ann :: Ann,
      body :: Expr
      ) with:
    fields(self): [self.iterator, self.bindings, self.ann, self.body] end,
    node-name(self): t_for end,
    tosource(self):
      header = PP.group(PP.string("for ")
          + self.iterator.tosource()
          + PP.surround-separate(2*INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
          self.bindings.fields().map(fun(b): b.tosource() end))
          + PP.group(PP.nest(2*INDENT,
            PP.break(1) + PP.string("->") + PP.break(1) + self.ann.tosource() + PP.string(":"))))
      PP.surround(INDENT, 1, header, self.body.tosource(), PP.string("end"))
    end
  | s_check(body :: Expr) with:
    fields(self): [self.body] end,
    node-name(self): t_check end,
    tosource(self):
      PP.surround(INDENT, 1, PP.string("check:"), self.body.tosource(), PP.string("end"))
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  print-source(self, width): list.each(print, self.tosource().pretty(width)) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_block(self) and is-s_block(other): feq-fun(self, other)
    else if is-s_fun(self) and is-s_fun(other): feq-fun(self, other)
    else if is-s_var(self) and is-s_var(other): feq-fun(self, other)
    else if is-s_let(self) and is-s_let(other): feq-fun(self, other)
    else if is-s_when(self) and is-s_when(other): feq-fun(self, other)
    else if is-s_assign(self) and is-s_assign(other): feq-fun(self, other)
    else if is-s_if(self) and is-s_if(other): feq-fun(self, other)
    else if is-s_if_else(self) and is-s_if_else(other): feq-fun(self, other)
    else if is-s_cases(self) and is-s_cases(other): feq-fun(self, other)
    else if is-s_cases_else(self) and is-s_cases(other): feq-fun(self, other)
    else if is-s_not(self) and is-s_not(other): feq-fun(self, other)
    else if is-s_paren(self) and is-s_paren(other): feq-fun(self, other)
    else if is-s_lam(self) and is-s_lam(other): feq-fun(self, other)
    else if is-s_extend(self) and is-s_extend(other): feq-fun(self, other)
    else if is-s_method(self) and is-s_method(other): feq-fun(self, other)
    else if is-s_obj(self) and is-s_obj(other): feq-fun(self, other)
    else if is-s_list(self) and is-s_list(other): feq-fun(self, other)
    else if is-s_app(self) and is-s_app(other): feq-fun(self, other)
    else if is-s_left_app(self) and is-s_left_app(other): feq-fun(self, other)
    else if is-s_id(self) and is-s_id(other): feq-fun(self, other)
    else if is-s_num(self) and is-s_num(other): feq-fun(self, other)
    else if is-s_bool(self) and is-s_bool(other): feq-fun(self, other)
    else if is-s_str(self) and is-s_str(other): feq-fun(self, other)
    else if is-s_dot(self) and is-s_dot(other): feq-fun(self, other)
    else if is-s_bracket(self) and is-s_bracket(other): feq-fun(self, other)
    else if is-s_colon(self) and is-s_colon(other): feq-fun(self, other)
    else if is-s_const_import(self) and is-s_const_import(other): feq-fun(self, other)
    else if is-s_colon_bracket(self) and is-s_colon_bracket(other): feq-fun(self, other)
    else if is-s_data(self) and is-s_data(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data Bind:
  | s_bind(id :: String, ann :: Ann) with:
    fields(self): [self.id, self.ann] end,
    node-name(self): t_bind end,
    tosource(self):
      if is-a_blank(self.ann): PP.string(self.id)
      else: PP.infix(INDENT, 1, PP.string("::"), PP.string(self.id), self.ann.tosource())
      end
    end
sharing:
  arity(self): self.fields().length()  end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_bind(self) and is-s_bind(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data Member:
  | s_data_field(name :: Expr, value :: Expr) with:
    fields(self): [self.name, self.value] end,
    node-name(self): t_data_field end,
    tosource(self): PP.nest(INDENT, self.name.tosource() + PP.string(": ") + self.value.tosource()) end,
    todatafield(self): PP.nest(INDENT, PP.string(self.name.s) + PP.string(": ") + self.value.tosource()) end
  | s_mutable_field(name :: Expr, ann :: Ann, value :: Expr) with:
    fields(self): [self.name, self.value, self.ann] end,
    node-name(self): t_mutable_field end,
    tosource(self): PP.nest(INDENT, PP.string("mutable") + self.name.tosource() + PP.string("::") + self.ann.tosource() + PP.string(": ") + self.value.tosource()) end,
    todatafield(self): PP.nest(INDENT, PP.string(self.name.s) + PP.string(": ") + self.value.tosource()) end
  | s_once_field(name :: Expr, ann :: Ann, value :: Expr) with:
    fields(self): [self.name, self.value, self.ann] end,
    node-name(self): t_once_field end,
    tosource(self): PP.nest(INDENT, PP.string("once") + self.name.tosource() + PP.string("::") + self.ann.tosource() + PP.string(": ") + self.value.tosource()) end,
    todatafield(self): PP.nest(INDENT, PP.string(self.name.s) + PP.string(": ") + self.value.tosource()) end
  | s_method_field(
      name :: Expr,
      args :: ASTList, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
      ) with:
    fields(self): [self.name, self.args, self.ann,
      self.doc, self.body, self.check] end,
    node-name(self): t_method_field end,
    tosource(self):
      name-part = cases(Expr) self.name:
        | s_str(s) => PP.string(s)
        | else => self.name.tosource()
      end
      funlam_tosource(name-part,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end,
    todatafield(self):
      funlam_tosource(PP.empty, self.name.s, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_data_field(self) and is-s_data_field(other): feq-fun(self, other)
    else if  is-s_method_field(self) and is-s_method_field(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data ForBind:
  | s_for_bind(bind :: Bind, value :: Expr) with:
    fields(self): [self.bind, self.value] end,
    node-name(self): t_for_bind end,
    tosource(self):
      PP.group(self.bind.tosource() + PP.break(1) + PP.string("from") + PP.break(1) + self.value.tosource())
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_for_bind(self) and is-s_for_bind(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data VariantMember:
  | s_variant_member(member_type :: String, bind :: Bind) with:
    fields(self): [self.member_type, self.bind] end,
    node-name(self): t_variant_member end,
    tosource(self):
      PP.string(self.member_type) + self.bind.tosource()
    end
end

data Variant:
  | s_variant(
      name :: String,
      binds :: ASTList,
      with_members :: ASTList
      ) with:
    fields(self): [self.name, self.binds, self.with_members] end,
    node-name(self): t_variant end,
    tosource(self):
      header-nowith = 
        PP.string(self.name)
        + PP.surround-separate(INDENT, 0, PP.empty, PP.lparen, PP.commabreak, PP.rparen,
        self.binds.fields().map(fun(b): b.tosource() end))
      header = PP.group(header-nowith + PP.break(1) + PP.string("with:"))
      withs = self.with_members.fields().map(fun(m): m.todatafield() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, PP.break(1) + PP.separate(PP.commabreak, withs)))
      end
    end
  | s_singleton_variant(
      name :: String,
      with_members :: ASTList
      ) with:
    fields(self): [self.name, self.with_members] end,
    node-name(self): t_singleton_variant end,
    tosource(self):
      header-nowith = PP.string(self.name)
      header = PP.group(header-nowith + PP.break(1) + PP.string("with:"))
      withs = self.with_members.fields().map(fun(m): m.todatafield() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, PP.break(1) + PP.separate(PP.commabreak, withs)))
      end
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_variant(self) and is-s_variant(other): feq-fun(self, other)
    else if is-s_singleton_variant(self) and is-s_singleton_variant(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data IfBranch:
  | s_if_branch(test :: Expr, body :: Expr)  with:
    fields(self): [self.test, self.body] end,
    node-name(self): t_if_branch end,
    tosource(self):
      PP.string("if ")
        + PP.nest(2*INDENT, self.test.tosource()+ PP.string(":"))
        + PP.nest(INDENT, PP.break(1) + self.body.tosource())
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,  
  equals(self, other, feq-fun):
    if is-s_if_branch(self) and is-s_if_branch(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data CasesBranch:
  | s_cases_branch(name :: String, args :: ASTList, body :: Expr) with:
    fields(self): [self.name, self.args, self.body] end,
    node-name(self): t_cases_branch end,
    tosource(self):
      PP.group(PP.string("| " + self.name)
          + PP.surround-separate(INDENT, 0, PP.empty, PP.lparen, PP.commabreak, PP.rparen,
          self.args.fields().map(fun(a): a.tosource() end)) + PP.break(1) + PP.string("=>")) + PP.break(1) +
      self.body.tosource()
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-s_cases_branch(self) and is-s_cases_branch(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data Ann:
  | a_blank with:
    fields(self): [] end,
    node-name(self): t_a_blank end,
    tosource(self): PP.string("Any") end
  | a_any with:
    fields(self): [] end,
    node-name(self): t_a_any end,
    tosource(self): PP.string("Any") end
  | a_name(id :: String)  with:
    fields(self): [self.id] end,
    node-name(self): t_a_name end,
    tosource(self): PP.string(self.id) end
  | a_arrow(args :: ASTList, ret :: Ann)  with:
    fields(self): [self.args, self.ret] end,
    node-name(self): t_a_arrow end,
    tosource(self):
      PP.surround(INDENT, 1, PP.lparen,
        PP.separate(PP.string(" "),
          [PP.separate(PP.commabreak,
            self.args.fields().map(fun(f): f.tosource() end))] + [PP.string("->"), self.ret.tosource()]), PP.rparen)
    end
  | a_method(args :: ASTList, ret :: Ann)  with:
    fields(self): [self.args, self.ret] end,
    node-name(self): t_a_method end,
    tosource(self): PP.string("NYI: A_method") end
  | a_record(flds :: ASTList)  with:
    fields(self): [self.flds] end,
    node-name(self): t_a_record end,
    tosource(self):
      PP.soft-surround(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.flds.fields().map(fun(f): f.tosource() end))
    end
  | a_app(ann :: Ann, args :: ASTList)  with:
    fields(self): [self.ann, self.args] end,
    node-name(self): t_a_app end,
    tosource(self):
      PP.group(self.ann.tosource()
          + PP.group(PP.langle + PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.fields().map(fun(f): f.tosource() end))) + PP.rangle))
    end
  | a_pred(ann :: Ann, exp :: Expr)  with:
    fields(self): [self.ann, self.exp] end,
    node-name(self): t_a_pred end,
    tosource(self): self.ann.tosource() + PP.parens(self.exp.tosource()) end
  | a_dot(obj :: String, field :: String)  with:
    fields(self): [self.obj, self.field] end,
    node-name(self): t_a_dot end,
    tosource(self): PP.string(self.obj + "." + self.field) end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-a_blank(self): is-a_blank(other)
    else if is-a_any(self): is-a_any(other)
    else if is-a_name(self) and is-a_name(other): feq-fun(self, other)
    else if is-a_arrow(self) and is-a_arrow(other): feq-fun(self, other)
    else if is-a_method(self) and is-a_method(other): feq-fun(self, other)
    else if is-a_record(self) and is-a_record(other): feq-fun(self, other)
    else if is-a_app(self) and is-a_app(other): feq-fun(self, other)
    else if is-a_pred(self) and is-a_pred(other): feq-fun(self, other)
    else if is-a_dot(self) and is-a_dot(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data AField:
  | a_field(name :: String, ann :: Ann)  with:
    fields(self): [self.name, self.ann] end,
    node-name(self): t_a_field end,
    tosource(self):
      if is-a_blank(self.ann): PP.string(self.name)
      else: PP.infix(INDENT, 1, PP.string("::"), PP.string(self.name), self.ann.tosource())
      end
    end
sharing:
  arity(self): self.fields().length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self): generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  print-source(self, width): list.each(print, self.tosource().pretty(width)) end, 
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-a_field(self) and is-a_field(other): feq-fun(self, other)
    else: false
    end    
  end,
  _equals(self, other): self.equals(other, field-equals) end
end

data TASTList:
  | t_ast_list with: tostring(self): "TASTList" end
end
data ASTList:
  | ast_list(l :: List) with:
    fields(self): self.l end,
    node-name(self): t_ast_list end,
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.string("[]"), PP.lbrack, PP.commabreak, PP.rbrack,
        self.l.map(fun(v): v.tosource() end))
    end
sharing:
  arity(self): self.l.length() end,
  to-labelled(self, T): generic-to-labelled(self, T) end,
  topprint(self): generic-topprint(self) end,
  pretty(self, width): generic-pretty(self, width) end,
  tostring(self):
  generic-tostring(self) end,
  print(self, width): generic-print(self, width) end,
  print-source(self, width):
    tosrc = self.l.map(fun(f): f.tosource() end)
    list.each(print, PP.group(PP.vert(tosrc)).pretty(width))
  end,
  hash-key(self): generic-hash-key(self) end,
  loose-equals(self, other): generic-loose-equals(self, other) end,
  equals(self, other, feq-fun):
    if is-ast_list(self) and is-ast_list(other): feq-fun(self, other)
    else: false
    end
  end,
  _equals(self, other): self.equals(other, field-equals) end,
  unzip(self):
    ast_list(transpose(self.l.map(fun(n): n.fields() end)).map(ast_list))
  end
end

fun<A> transpose(lol :: list.List<list.List<A>>) -> list.List<list.List<A>>:
  for list.fold(ret from [], l from lol.reverse()):
    cases(list.List) ret:
      | empty => l.map(fun(item): [item] end)
      | else =>
        for list.map2(item from l, acc from ret):
          item^list.link(acc)
        end
    end
  end
end
