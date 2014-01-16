#lang pyret

provide *
import pprint as PP
import format as F

format = F.format

INDENT = 2
break-one = PP.break(1)


data JBlock:
  | j-block(stmts :: List<JStmt>) with:
    tosource(self):
      cases(List) self.stmts:
        | empty => PP.mt-doc
        | else => PP.flow_map(PP.hardline, _.tosource(), self.stmts)
      end
    end
end

data JStmt:
  | j-var(name :: String, rhs :: JExpr) with:
    tosource(self):
      PP.str("var ") + PP.group(PP.nest(INDENT, PP.str(self.name) +
        PP.str(" =") + PP.break(1) + self.rhs.tosource())) + PP.str(";")
    end
  | j-if(cond :: JExpr, consq :: JBlock, alt :: JBlock) with:
    tosource(self):
      alt-doc = self.alt.tosource()
      else-doc =
        if alt-doc == PP.mt-doc: PP.mt-doc
        else: PP.str(" else ") + PP.surround(INDENT, 1, PP.lbrace, alt-doc, PP.rbrace)
        end
      PP.group(PP.str("if") + PP.parens(self.cond.tosource())) + PP.str(" ")
        + PP.surround(INDENT, 1, PP.lbrace, self.consq.tosource(), PP.rbrace)
        + else-doc
    end
  | j-return(expr :: JExpr) with:
    tosource(self):
      PP.str("return ") + self.expr.tosource() + PP.str(";")
    end
  | j-try-catch(body :: JStmt, exn :: String, catch :: JStmt) with:
    tosource(self):
      PP.surround(INDENT, 1, PP.str("try {"), self.body.tosource(), PP.rbrace)
        + PP.surround(INDENT, 1, PP.str(" catch(" + self.exn + ") {"), self.catch.tosource(), PP.rbrace)
    end
  | j-throw(exp :: JExpr) with:
    tosource(self): PP.group(PP.nest(INDENT, PP.str("throw ") + self.exp.tosource())) + PP.str(";") end
  | j-expr(expr :: JExpr) with:
    tosource(self):
      self.expr.tosource() + PP.str(";")
    end
end

data JBinop:
  | j-plus with: tosource(self): PP.str("+") end
  | j-minus with: tosource(self): PP.str("-") end
  | j-times with: tosource(self): PP.str("*") end
  | j-divide with: tosource(self): PP.str("/") end
  | j-and with: tosource(self): PP.str("&&") end
  | j-or with: tosource(self): PP.str("||") end
  | j-lt with: tosource(self): PP.str("<") end
  | j-leq with: tosource(self): PP.str("<=") end
  | j-gt with: tosource(self): PP.str(">") end
  | j-geq with: tosource(self): PP.str(">=") end
  | j-eq with: tosource(self): PP.str("===") end
  | j-equals with: tosource(self): PP.str("==") end
  | j-neq with: tosource(self): PP.str("!==") end
  | j-nequals with: tosource(self): PP.str("!=") end
end

data JUnop:
  | j-incr with: tosource(self): PP.str("++") end
  | j-decr with: tosource(self): PP.str("--") end
  | j-postincr with: tosource(self): PP.str("++") end
  | j-postdecr with: tosource(self): PP.str("--") end
end

data JExpr:
  | j-parens(exp :: JExpr) with:
    tosource(self):
      PP.surround(INDENT, 1, PP.str("("), self.exp.tosource(), PP.str(")"))
    end
  | j-unop(exp :: JExpr, op :: JUnop) with:
    tosource(self):
      cases(JUnop) self.op:
        | j-postincr => self.exp.tosource() + self.op.tosource()
        | j-postdeccr => self.exp.tosource() + self.op.tosource()
        | else => self.op.tosource() + self.exp.tosource()
      end
    end
  | j-binop(left :: JExpr, op :: JBinop, right :: JExpr) with:
    tosource(self): PP.flow([self.left.tosource(), self.op.tosource(), self.right.tosource()]) end
  | j-fun(args :: List<String>, body :: JBlock) with:
    tosource(self):
      arglist = PP.nest(INDENT, PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen, self.args.map(PP.str)))
      header = PP.group(PP.str("function") + arglist)
      PP.surround(INDENT, 1, header + PP.str(" {"), self.body.tosource(), PP.str("}"))
    end
  | j-app(func :: JExpr, args :: List<JExpr>) with:
    tosource(self):
      PP.group(self.func.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | j-method(obj :: JExpr, meth :: String, args :: List<JExpr>) with:
    tosource(self):
      PP.group(PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.meth))
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | j-ternary(test :: JExpr, consq :: JExpr, altern :: JExpr) with:
    tosource(self):
      PP.parens(PP.group(self.test.tosource() +
                         PP.str("?") +
                         self.consq.tosource() +
                         PP.str(":") +
                         self.altern.tosource()))
    end
  | j-assign(name :: String, rhs :: JExpr) with:
    tosource(self):
      PP.nest(INDENT, PP.str(self.name) + PP.str(" =") + break-one + self.rhs.tosource())
    end
  | j-bracket-assign(obj :: JExpr, field :: JExpr, rhs :: JExpr) with:
    tosource(self):
      PP.nest(INDENT, self.obj.tosource() + PP.lbrack + self.field.tosource() + PP.rbrack + PP.str(" =")
          + break-one + self.rhs.tosource())
    end
  | j-dot-assign(obj :: JExpr, name :: String, rhs :: JExpr) with:
    tosource(self):
      PP.nest(INDENT, PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.name)) + PP.str(" =") + break-one + self.rhs.tosource())
    end
  | j-dot(obj :: JExpr, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.field)) end
  | j-bracket(obj :: JExpr, field :: JExpr) with:
    tosource(self): PP.group(self.obj.tosource() +
      PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | j-list(elts :: List<JExpr>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrack + PP.rbrack,
        PP.lbrack, PP.commabreak, PP.rbrack, self.elts.map(_.tosource()))
    end
  | j-obj(fields :: List<JField>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(fun(f): f.tosource() end))
    end
  | j-id(id :: String) with:
    tosource(self): PP.str(self.id) end
  | j-str(s :: String) with:
    tosource(self): PP.str(format("~s", [self.s])) end
  | j-num(n :: Number) with:
    tosource(self): PP.number(self.n) end
  | j-true with:
    tosource(self): PP.str("true") end
  | j-false with:
    tosource(self): PP.str("false") end
  | j-null with:
    tosource(self): PP.str("null") end
  | j-undefined with:
    tosource(self): PP.str("undefined") end
  | j-raw(raw-js :: String) with:
    tosource(self): PP.str(self.raw-js) end
  | j-raw-holes(raw-js :: String, fills :: List<JExpr>, width-tolerance) with:
    tosource(self):
      filldocs = self.fills.map(_.tosource())
      fillstrs = filldocs.map(_.pretty(self.width-tolerance)).map(_.join-str(" "))
      PP.str(format(self.raw-js, fillstrs))
    end
where:
  j-fun(["a","b"], j-block([j-app(j-id("a"), [j-id("b")])])).tosource().pretty(80) is
    ["function(a, b) { a(b) }"]

  j-fun(["RUNTIME", "NAMESPACE"], j-block([
      j-var("print", j-method(j-id("NAMESPACE"), "get", [j-str("print")])),
      j-var("brand", j-method(j-id("NAMESPACE"), "get", [j-str("brand")]))
    ])).tosource().pretty(80)
    is
    [
      "function(RUNTIME, NAMESPACE) {",
      "  var print = NAMESPACE.get(\"print\");",
      "  var brand = NAMESPACE.get(\"brand\");",
      "}"
    ]

  j-null.tosource().pretty(5) is ["null"]

  j-null.tosource().pretty(4) raises "String doesn't fit"

  j-raw-holes("try { ~a } catch(e) { ~a
}", [j-raw("x + y"), j-id("z")], 100000).tosource().pretty(80) is
    ["try { x + y } catch(e) { z
}"]

  j-if(j-true, j-block([j-return(j-false)]), j-block([j-return(j-num(5))]))
    .tosource().pretty(80) is
    ["if(true) { return false; } else { return 5; }"]

  j-bracket(j-true, j-false).tosource().pretty(20) is ["true[false]"]

end

data JField:
  | j-field(name :: String, value :: JExpr) with:
    tosource(self):
      PP.nest(INDENT, PP.dquote(PP.str(self.name)) + PP.str(": ") + self.value.tosource())
    end
end
