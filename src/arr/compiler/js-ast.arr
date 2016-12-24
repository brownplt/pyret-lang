#lang pyret

provide *
provide-types *
import pprint as PP
import format as F
import ast as A
import file("concat-lists.arr") as CL

type CList = CL.ConcatList
clist = CL.clist

format = F.format

INDENT = 2
break-one = PP.sbreak(1)
blank-one = PP.blank(1)

fun string-printer():
  var strs = empty
  {
    append: lam(s): strs := link(s, strs) end,
    get: lam(): for fold(acc from "", s from strs): s + acc end end
  }
end

type Label = { get :: ( -> Number) }

data JBlock:
  | j-block(stmts :: CList<JStmt>) with:
    method label(self): "j-block" end,
    method print-ugly-source(self, printer):
      self.stmts.each(_.print-ugly-source(printer))
    end,
    method tosource(self):
      if self.stmts.is-empty(): PP.mt-doc
      else: PP.vert(self.stmts.map-to-list(_.tosource()))
      end
    end
  | j-block1(stmt :: JStmt) with:
    method label(self): "j-block1" end,
    method print-ugly-source(self, printer): self.stmt.print-ugly-source(printer) end,
    method tosource(self): PP.vert([list: self.stmt.tosource()]) end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end,
  method to-ugly-source(self) block:
    strprint = string-printer()
    self.print-ugly-source(strprint.append)
    strprint.get()
  end
end

data JStmt:
  | j-var(name :: A.Name, rhs :: JExpr) with:
    method label(self): "j-var" end,
    method print-ugly-source(self, printer) block:
      printer("var " + self.name.tosourcestring() + " = ")
      self.rhs.print-ugly-source(printer)
      printer(";\n")
    end,
    method tosource(self):
      PP.group(
        PP.str("var ") + PP.group(PP.nest(INDENT, self.name.to-compiled-source() +
            PP.str(" =") + PP.sbreak(1) + self.rhs.tosource())) + PP.str(";"))
    end
  | j-if1(cond :: JExpr, consq :: JBlock) with:
    method label(self): "j-if1" end,
    method print-ugly-source(self, printer) block:
      printer("if(")
      self.cond.print-ugly-source(printer)
      printer(") {\n")
      self.consq.print-ugly-source(printer)
      printer("}\n")
    end,
    method tosource(self):
      PP.group(PP.str("if") + PP.parens(self.cond.tosource())) + PP.str(" ")
        + PP.surround(INDENT, 1, PP.lbrace, self.consq.tosource(), PP.rbrace)
    end
  | j-if(cond :: JExpr, consq :: JBlock, alt :: JBlock) with:
    method label(self): "j-if" end,
    method print-ugly-source(self, printer) block:
      printer("if(")
      self.cond.print-ugly-source(printer)
      printer(") {\n")
      self.consq.print-ugly-source(printer)
      printer("} else {\n")
      self.alt.print-ugly-source(printer)
      printer("}\n")
    end,
    method tosource(self):
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
    method label(self): "j-return" end,
    method print-ugly-source(self, printer) block:
      printer("return ")
      self.expr.print-ugly-source(printer)
      printer(";\n")
    end,
    method tosource(self):
      PP.str("return ") + self.expr.tosource() + PP.str(";")
    end
  | j-try-catch(body :: JBlock, exn :: A.Name, catch :: JBlock) with:
    method label(self): "j-try-catch" end,
    method print-ugly-source(self, printer) block:
      printer("try {\n")
      self.body.print-ugly-source(printer)
      printer("} catch(" + self.exn.tosourcestring() + ") {\n")
      self.catch.print-ugly-source(printer)
      printer("}\n")
    end,
    method tosource(self):
      PP.surround(INDENT, 1, PP.str("try {"), self.body.tosource(), PP.rbrace)
        + PP.surround(INDENT, 1, PP.str(" catch(" + self.exn.tosourcestring() + ") {"), self.catch.tosource(), PP.rbrace)
    end
  | j-throw(exp :: JExpr) with:
    method label(self): "j-throw" end,
    method print-ugly-source(self, printer) block:
      printer("throw ")
      self.exp.print-ugly-source(printer)
      printer(";\n")
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT, PP.str("throw ") + self.exp.tosource())) + PP.str(";")
    end
  | j-expr(expr :: JExpr) with:
    method label(self): "j-expr" end,
    method print-ugly-source(self, printer) block:
      # (BSL) I wish this weren't necessary
      when is-j-obj(self.expr): printer("(") end
      self.expr.print-ugly-source(printer)
      when is-j-obj(self.expr): printer(")") end
      printer(";\n")
    end,
    method tosource(self):
      if is-j-obj(self.expr): PP.parens(self.expr.tosource())
      else: self.expr.tosource()
      end
        + PP.str(";")
    end
  | j-break with:
    method label(self): "j-break" end,
    method print-ugly-source(self, printer): printer("break;\n") end,
    method tosource(self): PP.str("break;") end
  | j-continue with:
    method label(self): "j-continue" end,
    method print-ugly-source(self, printer): printer("continue;\n") end,
    method tosource(self): PP.str("continue;") end
  | j-switch(exp :: JExpr, branches :: CList<JCase>) with:
    method label(self): "j-switch" end,
    method print-ugly-source(self, printer) block:
      printer("switch(")
      self.exp.print-ugly-source(printer)
      printer(") {\n")
      self.branches.each(_.print-ugly-source(printer))
      printer("}\n")
    end,
    method tosource(self):
      PP.surround(0, 1, PP.group(PP.str("switch") + PP.parens(self.exp.tosource()) + PP.sbreak(1) + PP.lbrace),
        PP.flow-map(PP.hardline, _.tosource(), self.branches.to-list()), PP.rbrace)
    end
  | j-while(cond :: JExpr, body :: JBlock) with:
    method label(self): "j-while" end,
    method print-ugly-source(self, printer) block:
      printer("while(")
      self.cond.print-ugly-source(printer)
      printer(") {\n")
      self.body.print-ugly-source(printer)
      printer("}\n")
    end,
    method tosource(self):
      PP.surround(INDENT, 1, PP.group(PP.str("while") + PP.parens(self.cond.tosource()) + PP.sbreak(1) + PP.lbrace),
        self.body.tosource(), PP.rbrace)
    end
  | j-for(create-var :: Boolean, init :: JExpr, cond :: JExpr, update :: JExpr, body :: JBlock) with:
    method print-ugly-source(self, printer) block:
      printer("for(")
      when self.create-var:
        printer("var ")
      end
      self.init.print-ugly-source(printer)
      printer(";")
      self.cond.print-ugly-source(printer)
      printer(";")
      self.update.print-ugly-source(printer)
      printer(") {\n")
      self.body.print-ugly-source(printer)
      printer("}\n")
    end,
    method tosource(self):
      semi = PP.str(";") + PP.sbreak(1)
      init-src =
        if self.create-var: PP.str("var ") + self.init.tosource()
        else: self.init.tosource()
        end
      PP.surround(INDENT, 1,
        PP.group(PP.str("for")
            + PP.parens(init-src + semi + self.cond.tosource() + semi + self.update.tosource())
            + PP.sbreak(1) + PP.lbrace),
        self.body.tosource(),
        PP.rbrace)
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end,
  method to-ugly-source(self) block:
    strprint = string-printer()
    self.print-ugly-source(strprint.append)
    strprint.get()
  end
end

data JCase:
  | j-case(exp :: JExpr, body :: JBlock) with:
    method label(self): "j-case" end,
    method print-ugly-source(self, printer) block:
      printer("case ")
      self.exp.print-ugly-source(printer)
      printer(": ")
      self.body.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT,
          PP.group(PP.nest(INDENT, PP.str("case ") + self.exp.tosource() + PP.str(":"))) + PP.sbreak(1)
            + self.body.tosource()))
    end
  | j-default(body :: JBlock) with:
    method label(self): "j-default" end,
    method print-ugly-source(self, printer) block:
      printer("default: ")
      self.body.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT, PP.str("default:") + PP.sbreak(1) + self.body.tosource()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end,
  method to-ugly-source(self) block:
    strprint = string-printer()
    self.print-ugly-source(strprint.append)
    strprint.get()
  end
end

data JBinop:
  | j-plus with: method to-ugly-source(self): "+" end
  | j-minus with: method to-ugly-source(self): "-" end
  | j-times with: method to-ugly-source(self): "*" end
  | j-divide with: method to-ugly-source(self): "/" end
  | j-and with: method to-ugly-source(self): "&&" end
  | j-or with: method to-ugly-source(self): "||" end
  | j-lt with: method to-ugly-source(self): "<" end
  | j-leq with: method to-ugly-source(self): "<=" end
  | j-gt with: method to-ugly-source(self): ">" end
  | j-geq with: method to-ugly-source(self): ">=" end
  | j-eq with: method to-ugly-source(self): "===" end
  | j-equals with: method to-ugly-source(self): "==" end
  | j-neq with: method to-ugly-source(self): "!==" end
  | j-nequals with: method to-ugly-source(self): "!=" end
  | j-instanceof with: method to-ugly-source(self): "instanceof" end
sharing:
  method print-ugly-source(self, printer):
    printer(self.to-ugly-source())
  end,
  method tosource(self):
    PP.str(self.to-ugly-source())
  end
end

data JUnop:
  | j-incr with: method to-ugly-source(self): "++" end
  | j-decr with: method to-ugly-source(self): "--" end
  | j-postincr with: method to-ugly-source(self): "++" end
  | j-postdecr with: method to-ugly-source(self): "--" end
  | j-not with: method to-ugly-source(self): "!" end
sharing:
  method print-ugly-source(self, printer):
    printer(self.to-ugly-source())
  end,
  method tosource(self):
    PP.str(self.to-ugly-source())
  end
end

data JExpr:
  | j-parens(exp :: JExpr) with:
    method label(self): "j-parens" end,
    method print-ugly-source(self, printer) block:
      printer("(")
      self.exp.print-ugly-source(printer)
      printer(")")
    end,
    method tosource(self):
      PP.surround(INDENT, 1, PP.str("("), self.exp.tosource(), PP.str(")"))
    end
  | j-raw-code(s :: String) with:
    method print-ugly-source(self, printer):
      printer(self.s)
    end,
    method tosource(self):
      PP.str(self.s)
    end
  | j-unop(exp :: JExpr, op :: JUnop) with:
    method label(self): "j-unop" end,
    method print-ugly-source(self, printer):
      cases(JUnop) self.op block:
        | j-postincr =>
          self.exp.print-ugly-source(printer)
          self.op.print-ugly-source(printer)
        | j-postdeccr =>
          self.exp.print-ugly-source(printer)
          self.op.print-ugly-source(printer)
        | else =>
          self.op.print-ugly-source(printer)
          self.exp.print-ugly-source(printer)
      end
    end,
    method tosource(self):
      cases(JUnop) self.op:
        | j-postincr => self.exp.tosource() + self.op.tosource()
        | j-postdeccr => self.exp.tosource() + self.op.tosource()
        | else => self.op.tosource() + self.exp.tosource()
      end
    end
  | j-binop(left :: JExpr, op :: JBinop, right :: JExpr) with:
    method label(self): "j-binop" end,
    method print-ugly-source(self, printer) block:
      self.left.print-ugly-source(printer)
      printer(" ")
      self.op.print-ugly-source(printer)
      printer(" ")
      self.right.print-ugly-source(printer)
    end,
    method tosource(self): PP.flow([list: self.left.tosource(), self.op.tosource(), self.right.tosource()]) end
  | j-fun(id :: String, args :: CList<A.Name>, body :: JBlock) with:
    method label(self): "j-fun" end,
    method print-ugly-source(self, printer) block:
      printer("function(")
      for CL.each_n(n from 0, arg from self.args) block:
        when n > 0: printer(",") end
        printer(arg.tosourcestring())
      end
      printer(") {\n")
      self.body.print-ugly-source(printer)
      printer("}")
    end,
    method tosource(self):
      arglist = PP.nest(INDENT, PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen, self.args.map-to-list(_.to-compiled-source())))
      header = PP.group(PP.str("function") + arglist)
      PP.surround(INDENT, 1, header + PP.str(" {"), self.body.tosource(), PP.str("}"))
    end
  | j-new(func :: JExpr, args :: CList<JExpr>) with:
    method label(self): "j-new" end,
    method print-ugly-source(self, printer) block:
      printer("new ")
      self.func.print-ugly-source(printer)
      printer("(")
      for CL.each_n(n from 0, arg from self.args) block:
        when n > 0: printer(",") end
        arg.print-ugly-source(printer)
      end
      printer(")")
    end,
    method tosource(self):
      PP.group(PP.str("new ") + self.func.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map-to-list(_.tosource())))))
    end
  | j-app(func :: JExpr, args :: CList<JExpr>) with:
    method label(self): "j-app" end,
    method print-ugly-source(self, printer) block:
      self.func.print-ugly-source(printer)
      printer("(")
      for CL.each_n(n from 0, arg from self.args) block:
        when n > 0: printer(",") end
        arg.print-ugly-source(printer)
      end
      printer(")")
    end,
    method tosource(self):
      PP.group(self.func.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map-to-list(_.tosource())))))
    end
  | j-method(obj :: JExpr, meth :: String, args :: CList<JExpr>) with:
    method label(self): "j-method" end,
    method print-ugly-source(self, printer) block:
      self.obj.print-ugly-source(printer)
      printer(".")
      printer(self.meth)
      printer("(")
      for CL.each_n(n from 0, arg from self.args) block:
        when n > 0: printer(",") end
        arg.print-ugly-source(printer)
      end
      printer(")")
    end,
    method tosource(self):
      PP.group(PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.meth))
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map-to-list(_.tosource())))))
    end
  | j-ternary(test :: JExpr, consq :: JExpr, altern :: JExpr) with:
    method label(self): "j-ternary" end,
    method print-ugly-source(self, printer) block:
      self.test.print-ugly-source(printer)
      printer("?")
      self.consq.print-ugly-source(printer)
      printer(":")
      self.altern.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.parens(
        self.test.tosource()
          + PP.nest(INDENT, break-one + PP.str("?") + blank-one + PP.group(PP.nest(INDENT, self.consq.tosource())))
          + PP.nest(INDENT, break-one + PP.str(":") + blank-one + PP.group(PP.nest(INDENT, self.altern.tosource()))))
    end
  | j-assign(name :: A.Name, rhs :: JExpr) with:
    method label(self): "j-assign" end,
    method print-ugly-source(self, printer) block:
      printer(self.name.tosourcestring())
      printer(" = ")
      self.rhs.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT, self.name.to-compiled-source() + PP.str(" =") + break-one + self.rhs.tosource()))
    end
  | j-bracket-assign(obj :: JExpr, field :: JExpr, rhs :: JExpr) with:
    method label(self): "j-bracket-assign" end,
    method print-ugly-source(self, printer) block:
      self.obj.print-ugly-source(printer)
      printer("[")
      self.field.print-ugly-source(printer)
      printer("]")
      printer(" = ")
      self.rhs.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT, self.obj.tosource() + PP.lbrack + self.field.tosource() + PP.rbrack + PP.str(" =")
            + break-one + self.rhs.tosource()))
    end
  | j-dot-assign(obj :: JExpr, name :: String, rhs :: JExpr) with:
    method label(self): "j-dot-assign" end,
    method print-ugly-source(self, printer) block:
      self.obj.print-ugly-source(printer)
      printer(".")
      printer(self.name)
      printer(" = ")
      self.rhs.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.group(PP.nest(INDENT, PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.name)) + PP.str(" =") + break-one + self.rhs.tosource()))
    end
  | j-dot(obj :: JExpr, field :: String) with:
    method label(self): "j-dot" end,
    method print-ugly-source(self, printer) block:
      self.obj.print-ugly-source(printer)
      printer(".")
      printer(self.field)
    end,
    method tosource(self): PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.field)) end
  | j-bracket(obj :: JExpr, field :: JExpr) with:
    method label(self): "j-bracket" end,
    method print-ugly-source(self, printer) block:
      self.obj.print-ugly-source(printer)
      printer("[")
      self.field.print-ugly-source(printer)
      printer("]")
    end,
    method tosource(self): PP.group(self.obj.tosource() +
      PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | j-list(multi-line :: Boolean, elts :: CList<JExpr>) with:
    method label(self): "j-list" end,
    method print-ugly-source(self, printer) block:
      printer("[")
      sep = if self.multi-line: ",\n" else: "," end
      for CL.each_n(n from 0, elt from self.elts) block:
        when n > 0: printer(sep) end
        elt.print-ugly-source(printer)
      end
      printer("]")
    end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrack + PP.rbrack,
        PP.lbrack, PP.commabreak, PP.rbrack, self.elts.map-to-list(_.tosource()))
    end
  | j-obj(fields :: CList<JField>) with:
    method label(self): "j-obj" end,
    method print-ugly-source(self, printer) block:
      printer("{")
      for CL.each_n(n from 0, field from self.fields) block:
        when n > 0: printer(",\n") end
        field.print-ugly-source(printer)
      end
      printer("}")
    end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map-to-list(_.tosource()))
    end
  | j-id(id :: A.Name) with:
    method label(self): "j-id" end,
    method print-ugly-source(self, printer):
      printer(self.id.tosourcestring())
    end,
    method tosource(self): self.id.to-compiled-source() end
  | j-str(s :: String) with:
    method label(self): "j-str" end,
    method print-ugly-source(self, printer):
      printer(torepr(self.s))
    end,
    method tosource(self): PP.str(torepr(self.s)) end
  | j-num(n :: Number) with:
    method label(self): "j-num" end,
    method print-ugly-source(self, printer):
      printer(tostring(self.n))
    end,
    method tosource(self): PP.number(self.n) end
  | j-true with:
    method label(self): "j-true" end,
    method print-ugly-source(self, printer):
      printer("true")
    end,
    method tosource(self): PP.str("true") end
  | j-false with:
    method label(self): "j-false" end,
    method print-ugly-source(self, printer):
      printer("false")
    end,
    method tosource(self): PP.str("false") end
  | j-null with:
    method label(self): "j-null" end,
    method print-ugly-source(self, printer):
      printer("null")
    end,
    method tosource(self): PP.str("null") end
  | j-undefined with:
    method label(self): "j-undefined" end,
    method print-ugly-source(self, printer):
      printer("undefined")
    end,
    method tosource(self): PP.str("undefined") end
  | j-label(label :: Label) with:
#   TODO(joe): We don't really use label, so ignoring this clash for the moment
#    label(self): "j-label" end,
    method print-ugly-source(self, printer): printer(tostring(self.label.get())) end,
    method tosource(self): PP.number(self.label.get()) end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end,
  method to-ugly-source(self) block:
    strprint = string-printer()
    self.print-ugly-source(strprint.append)
    strprint.get()
  end

where:
  fun j-n-id(name): j-id(A.s-name(A.dummy-loc, name)) end
  j-fun("0", [clist: j-n-id("a").id,j-n-id("b").id],
    j-block([clist: j-app(j-n-id("a"), [clist: j-n-id("b")])])).tosource().pretty(80)
    is [list: "function(a, b) { a(b) }"]

  j-fun("1", [clist: j-n-id("RUNTIME").id, j-n-id("NAMESPACE").id], j-block([clist:
      j-var(j-n-id("print").id, j-method(j-n-id("NAMESPACE"), "get", [clist: j-str("print")])),
      j-var(j-n-id("brand").id, j-method(j-n-id("NAMESPACE"), "get", [clist: j-str("brand")]))
    ])).tosource().pretty(80)
    is
    [list:
      "function(RUNTIME, NAMESPACE) {",
      "  var print = NAMESPACE.get(\"print\");",
      "  var brand = NAMESPACE.get(\"brand\");",
      "}"
    ]

  j-null.tosource().pretty(5) is [list: "null"]

  j-if(j-true, j-block([clist: j-return(j-false)]), j-block([clist: j-return(j-num(5))]))
    .tosource().pretty(80) is
    [list: "if(true) { return false; } else { return 5; }"]

  j-bracket(j-true, j-false).tosource().pretty(20) is [list: "true[false]"]

end

next-j-fun-id = block:
  var n = 0
  lam() block:
    n := n + 1
    tostring(n)
  end
end

fun make-label-sequence(init :: Number) -> ( -> JExpr):
  var next = init
  lam():
    var value = nothing
    j-label({
        get: lam():
            if value == nothing block:
              value := next
              next := next + 1
              value
            else:
              value
            end
        end})
  end
end

data JField:
  | j-field(name :: String, value :: JExpr) with:
    method label(self): "j-field" end,
    method print-ugly-source(self, printer) block:
      printer("\"")
      printer(self.name)
      printer("\":")
      self.value.print-ugly-source(printer)
    end,
    method tosource(self):
      PP.nest(INDENT, PP.dquote(PP.str(self.name)) + PP.str(": ") + self.value.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end,
  method to-ugly-source(self) block:
    strprint = string-printer()
    self.print-ugly-source(strprint.append)
    strprint.get()
  end
end


default-map-visitor = {
  method j-field(self, name, value): j-field(self, name, value.visit(self)) end,
  method j-parens(self, exp): j-parens(exp.visit(self)) end,
  method j-unop(self, exp, op): j-unop(exp.visit(self), op) end,
  method j-binop(self, left, op, right): j-binop(left.visit(self), op, right.visit(self)) end,
  method j-fun(self, id, args, body): j-fun(id, args, body.visit(self)) end,
  method j-new(self, func, args): j-new(func.visit(self), args.map(_.visit(self))) end,
  method j-app(self, func, args): j-app(func.visit(self), args.map(_.visit(self))) end,
  method j-method(self, obj, meth, args): j-method(obj.visit(self), meth, args.map(_.visit(self))) end,
  method j-ternary(self, test, consq, alt): j-ternary(test.visit(self), consq.visit(self), alt.visit(self)) end,
  method j-assign(self, name, rhs): j-assign(name, rhs.visit(self)) end,
  method j-bracket-assign(self, obj, field, rhs): j-bracket-assign(obj.visit(self), field.visit(self), rhs.visit(self)) end,
  method j-dot-assign(self, obj, name, rhs): j-dot-assign(obj.visit(self), name, rhs.visit(self)) end,
  method j-dot(self, obj, name): j-dot(obj.visit(self), name) end,
  method j-bracket(self, obj, field): j-bracket(obj.visit(self), field.visit(self)) end,
  method j-list(self, multi-line, elts): j-list(multi-line, elts.map(_.visit(self))) end,
  method j-obj(self, fields): j-obj(fields.map(_.visit(self))) end,
  method j-id(self, id): j-id(id) end,
  method j-str(self, s): j-str(s) end,
  method j-num(self, n): j-num(n) end,
  method j-true(self): j-true end,
  method j-false(self): j-false end,
  method j-null(self): j-null end,
  method j-undefined(self): j-undefined end,
  method j-label(self, label): j-label(label.visit(self)) end,
  method j-case(self, exp, body): j-case(exp.visit(self), body.visit(self)) end,
  method j-default(self, body): j-default(body.visit(self)) end,
  method j-block(self, stmts): j-block(stmts.map(_.visit(self))) end,
  method j-var(self, name, rhs): j-var(name, rhs.visit(self)) end,
  method j-if1(self, cond, consq): j-if1(cond.visit(self), consq.visit(self)) end,
  method j-if(self, cond, consq, alt): j-if(cond.visit(self), consq.visit(self), alt.visit(self)) end,
  method j-return(self, exp): j-return(exp.visit(self)) end,
  method j-try-catch(self, body, exn, catch): j-try-catch(body.visit(self), exn, catch.visit(self)) end,
  method j-throw(self, exp): j-throw(exp.visit(self)) end,
  method j-expr(self, exp): j-expr(exp.visit(self)) end,
  method j-break(self): j-break end,
  method j-continue(self): j-continue end,
  method j-switch(self, exp, branches): j-switch(exp.visit(self), branches.map(_.visit(self))) end,
  method j-while(self, cond, body): j-while(cond.visit(self), body.visit(self)) end,
  method j-for(self, create-var, init, cond, update, body): j-for(create-var, init.visit(self), cond.visit(self), update.visit(self), body.visit(self)) end
}
