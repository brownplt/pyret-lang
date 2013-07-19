#lang pyret

provide *

Loc = error.Location
loc = error.location
List = list.List

data Program:
  | s_program(l :: Loc, imports :: List<Header>, block :: Expr)
end

data Header:
  | s_import(l :: Loc, file :: ImportType, name :: String)
  | s_provide(l :: Loc, block :: Expr)
  | s_provide_all(l :: Loc)
end

data ImportType:
  | s_file_import(file :: String) # import "foo.arr" as F
  | s_const_import(module :: String) # import AST as A
end

data Expr:
  | s_block(l :: Loc, stmts :: List<Expr>) # List<Expr U Expr>, actually
  | s_fun(
      l :: Loc,
      name :: String,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    )
  | s_var(l :: Loc, name :: Bind, value :: Expr)
  | s_let(l :: Loc, name :: Bind, value :: Expr)
  | s_when(l :: Loc, test :: Expr, block :: Expr)
  | s_assign(l :: Loc, id :: String, value :: Expr)
  | s_case(l :: Loc, branches :: List<CaseBranch>)
  | s_if(l :: Loc, branches :: List<IfBranch>)
  | s_if_else(l :: Loc, branches :: List<IfBranch>, _else :: Expr)
  | s_cases(l :: Loc, type :: Expr, val :: Expr, branches :: List<CasesBranch>)
  | s_cases_else(l :: Loc, type :: Expr, val :: Expr, branches :: List<CasesBranch>, _else :: Expr)
  | s_try(l :: Loc, body :: Expr, id :: Bind, _except :: Expr)
  | s_op(l :: Loc, op :: String, left :: Expr, right :: Expr)
  | s_not(l :: Loc, expr :: Expr)
  | s_paren(l :: Loc, expr :: Expr)
  | s_lam(
      l :: Loc, 
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    )
  | s_method(
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    )
  | s_extend(l :: Loc, super :: Expr, fields :: List<Member>)
  | s_obj(l :: Loc, fields :: List<Member>)
  | s_list(l :: Loc, values :: List<Expr>)
  | s_app(l :: Loc, _fun :: Expr, args :: List<Expr>)
  | s_left_app(l :: Loc, obj :: Expr, _fun :: Expr, args :: List<Expr>)
  | s_id(l :: Loc, id :: String)
  | s_num(l :: Loc, n :: Number)
  | s_bool(l :: Loc, b :: Bool)
  | s_str(l :: Loc, s :: String)
  | s_dot(l :: Loc, obj :: Expr, field :: String)
  | s_bracket(l :: Loc, obj :: Expr, field :: Expr)
  | s_colon(l :: Loc, obj :: Expr, field :: String)
  | s_colon_bracket(l :: Loc, obj :: Expr, field :: Expr)
  | s_data(
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      variants :: List<Variant>,
      shared_members :: List<Member>,
      check :: Expr
    )
  | s_for(
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    )
end

data Bind:
  | s_bind(l :: Loc, id :: String, ann :: Ann)
end

data Member:
  | s_data_field(l :: Loc, name :: Expr, value :: Expr)
  | s_method_field(
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    )
end

data ForBind:
  | s_for_bind(l :: Loc, bind :: Bind, value :: Expr)
end

data Variant:
  | s_variant(
      l :: Loc,
      name :: String,
      binds :: List<Bind>,
      with_members :: List<Member>
    )
  | s_singleton_variant(
      l :: Loc,
      name :: String,
      with_members :: List<Member>
    )
end

data CaseBranch:
  | s_case_branch(l :: Loc, expr :: Expr, body :: Expr)
end

data IfBranch:
  | s_if_branch(l :: Loc, test :: Expr, body :: Expr)
end

data CasesBranch:
  | s_cases_branch(l :: Loc, name :: String, args :: List<Bind>, body :: Expr)
end

data Ann:
  | a_blank
  | a_any
  | a_name(l :: Loc, id :: String)
  | a_arrow(l :: Loc, args :: List<Ann>, ret :: Ann)
  | a_method(l :: Loc, args :: List<Ann>, ret :: Ann)
  | a_record(l :: Loc, fields :: List<AField>)
  | a_app(l :: Loc, ann :: Ann, args :: List<Ann>)
  | a_pred(l :: Loc, ann :: Ann, exp :: Expr)
  | a_dot(l :: Loc, obj :: String, field :: String)
end

data AField:
  | a_field(l :: Loc, name :: String, ann :: Ann)
end

