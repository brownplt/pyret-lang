#lang pyret

provide *
import ast as A

Loc = error.Location
Boolean = Bool

data JProgram:
  | j-program(l :: Loc, imports :: List<JHeader>, block :: JHeader)
end

data JHeader:
  | j-import(l :: Loc, file :: JImportType, name :: String)
  | j-provide(l :: Loc, block :: JExpr)
end

data JImportType:
  | j-file-import(file :: String)
  | j-const-import(module :: String)
end

data JExpr:
  | j-num(l :: Loc, n :: Number)
  | j-str(l :: Loc, s :: String)
  | j-bool(l :: Loc, b :: Boolean)
  | j-obj(l :: Loc, fields :: List<JMember>)
  | j-lam(l :: Loc, binds :: List<JBind>, body :: JExpr)

  | j-block(l :: Loc, stmts :: List<JExpr>)
  | j-user-block(l :: Loc, body :: JExpr)

  | j-if-else(l :: Loc, if-cases :: List<JIfCase>, _else :: JExpr)

  | j-try(l :: Loc, try :: JExpr, _except :: JExpr)

  | j-app(l :: Loc, f :: JExpr, args :: List<JExpr>)
  | j-prim(l :: Loc, prim :: JPrim, args :: List<JExpr>)

  | j-let(l :: Loc, bind :: JBind, val :: JExpr)
  | j-var(l :: Loc, bind :: JBind, val :: JExpr)
  | j-id(l :: Loc, id :: String)
  | j-assign(l :: Loc, id :: String, val :: JExpr)

  | j-app-success(l :: Loc, cont :: CUse, expr :: JExpr)
  | j-app-failure(l :: Loc, cont :: CUse, expr :: JExpr)

  | j-app-k(l :: Loc, conts :: CDef, f :: JExpr, args :: List<JExpr>)
  | j-prim-k(l :: Loc, conts :: CDef, prim :: JPrim, args :: List<JExpr>)
end

data Cont:
  | jc-lam(l :: Loc, id :: String, body :: JExpr)
  | jc-id(l :: Loc, id :: String)
end

data CDef:
  | object-cdef(success :: Cont, failure :: Cont)
end

data CUse:
  | object-cuse(id :: String)
end

data JBind:
  | j-bind(l :: Loc, id :: String)
end

data JPrim:
  | get-field
  | get-mutable-field
  | update
  | extend
end

data JIfCase:
  | j-if-case(l :: Loc, test :: JExpr, body :: JExpr)
end

data JMember:
  | j-data-field(l :: Loc, key :: String, value :: JExpr)
end

