#lang pyret

provide *
import pprint as PP

Loc = error.Location
loc = error.location
List = list.List

INDENT = 2

fun funlam_tosource(funtype, name, params, args :: List<is-s_bind>,
    ann :: Ann, doc :: String, body :: Expr, _check :: Expr) -> PP.PPrintDoc:
  typarams =
    if is-nothing(params): PP.empty
    else: PP.surround-separate(INDENT, 0, PP.empty, PP.langle, PP.commabreak, PP.rangle,
        params.map(fun(p): PP.str(p) end))
    end
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(fun(a): a.tosource() end)))
  ftype = funtype + typarams
  fname = 
    if is-nothing(name): ftype
    else if PP.is-empty(ftype): PP.str(name)
    else: ftype + PP.str(" " + name)
    end
  fann =
    if is-a_blank(ann) or is-nothing(ann): PP.empty
    else: PP.break(1) + PP.str("-> ") + ann.tosource()
    end
  header = PP.group(fname + arg-list + fann + PP.str(":"))
  checker = _check.tosource()
  footer =
    if PP.is-empty(checker): PP.str("end")
    else: PP.surround(INDENT, 1, PP.str("where:"), _check.tosource(), PP.str("end"))
    end
  docstr =
    if is-nothing(doc) or (doc == ""): PP.empty
    else: PP.str("doc: ") + PP.dquote(PP.str(doc)) + PP.hardline
    end
  PP.surround(INDENT, 1, header, docstr + body.tosource(), footer)
end

data Program:
  | s_program(l :: Loc, imports :: List<Header>, block :: Expr) with:
    tosource(self):
      PP.group(
        PP.flow_map(PP.hardline, fun(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.block.tosource()
        )
    end
end

data Header:
  | s_import(l :: Loc, file :: ImportType, name :: String) with:
    tosource(self):
      PP.flow([PP.str("import"), PP.quote(PP.str(self.file)),
          PP.str("as"), PP.str(self.name)])
    end
  | s_provide(l :: Loc, block :: Expr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1, PP.str("provide"),
        self.block.tosource(), PP.str("end"))
    end
  | s_provide_all(l :: Loc) with:
    tosource(self): PP.str("provide *") end
end

data ImportType:
  | s_file_import(file :: String) with:
    tosource(self): PP.str("import") + PP.break(1) + PP.dquote(PP.str(self.file)) end
  | s_const_import(module :: String) with:
    tosource(self): PP.str("import") + PP.break(1) + PP.str(self.module) end
end

data Expr:
  | s_block(l :: Loc, stmts :: List<Expr>) with:
    tosource(self):
      PP.flow_map(PP.hardline, fun(s):
          s.tosource()
      end, self.stmts) end
  | s_user_block(l :: Loc, body :: Expr) with:
    tosource(self):
      PP.str("block: NYI end")
    end
  | s_fun(
      l :: Loc,
      name :: String,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    ) with:
    tosource(self):
      funlam_tosource(PP.str("fun"),
        self.name, self.params, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_var(l :: Loc, name :: Bind, value :: Expr) with:
    tosource(self):
      PP.str("var ")
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + PP.str(" =") + PP.break(1) + self.value.tosource()))
    end
  | s_let(l :: Loc, name :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.name.tosource() + PP.str(" =") + PP.break(1) + self.value.tosource()))
    end
  | s_graph(l :: Loc, bindings :: List<is-s_let>)
  | s_when(l :: Loc, test :: Expr, block :: Expr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1,
        PP.str("when") + PP.parens(self.test.tosource()) + PP.str(":"),
        self.block.tosource(),
        PP.str("end"))
    end
  | s_assign(l :: Loc, id :: String, value :: Expr) with:
    tosource(self):
      PP.nest(INDENT, PP.str(self.id) + PP.str(" :=") + PP.break(1) + self.value.tosource())
    end
  | s_if(l :: Loc, branches :: List<IfBranch>) with:
    tosource(self):
      branches = PP.separate(PP.break(1) + PP.str("else "),
        self.branches.map(fun(b): b.tosource() end))
      PP.group(branches + PP.break(1) + PP.str("end"))
    end      
  | s_if_else(l :: Loc, branches :: List<IfBranch>, _else :: Expr) with:
    tosource(self):
      branches = PP.separate(PP.break(1) + PP.str("else "),
        self.branches.map(fun(b): b.tosource() end))
      _else = PP.str("else:") + PP.nest(INDENT, PP.break(1) + self._else.tosource())
      PP.group(branches + PP.break(1) + _else + PP.break(1) + PP.str("end"))
    end
  | s_cases(l :: Loc, type :: Ann, val :: Expr, branches :: List<CasesBranch>) with:
    tosource(self):
      header = PP.str("cases") + PP.parens(self.type.tosource()) + PP.break(1)
        + self.val.tosource() + PP.str(":")
      PP.surround-separate(INDENT, 1, header + PP.str(" end"),
        PP.group(header), PP.break(1), PP.str("end"),
        self.branches.map(fun(b): PP.group(b.tosource()) end))
    end
  | s_cases_else(l :: Loc, type :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr) with:
    tosource(self):
      header = PP.str("cases") + PP.parens(self.type.tosource()) + PP.break(1)
        + self.val.tosource() + PP.str(":")
      body = PP.separate(PP.break(1), self.branches.map(fun(b): PP.group(b.tosource()) end))
        + PP.break(1) + PP.group(PP.str("| else =>") + PP.break(1) + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, PP.str("end"))
    end
  | s_try(l :: Loc, body :: Expr, id :: Bind, _except :: Expr) with:
    tosource(self):
      _try = PP.str("try:") + PP.break(1)
        + PP.nest(INDENT, self.body.tosource()) + PP.break(1)
      _except = PP.str("except") + PP.parens(self.id.tosource()) + PP.str(":") + PP.break(1)
        + PP.nest(INDENT, self._except.tosource()) + PP.break(1)
      PP.group(_try + _except + PP.str("end"))
    end
  | s_op(l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    tosource(self): PP.infix(INDENT, 1, PP.str(self.op.substring(2, self.op.length())), self.left.tosource(), self.right.tosource()) end
  | s_not(l :: Loc, expr :: Expr) with:
    tosource(self): PP.nest(INDENT, PP.flow([PP.str("not"), self.expr.tosource()])) end
  | s_paren(l :: Loc, expr :: Expr) with:
    tosource(self): PP.parens(self.expr.tosource()) end
  | s_lam(
      l :: Loc,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    ) with:
    tosource(self):
      funlam_tosource(PP.str("fun"),
        nothing, self.params, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_method(
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    ) with:
    tosource(self):
      funlam_tosource(PP.str("method"),
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_extend(l :: Loc, super :: Expr, fields :: List<Member>) with:
    tosource(self):
      PP.group(self.super.tosource() + PP.str(".")
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.flds.map(fun(f): f.todatafield() end)))
    end
  | s_update(l :: Loc, super :: Expr, fields :: List<Member>)
  | s_obj(l :: Loc, fields :: List<Member>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.flds.map(fun(f): f.todatafield() end))
    end
  | s_list(l :: Loc, values :: List<Expr>) with:
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.str("[]"), PP.lbrack, PP.commabreak, PP.rbrack,
        self.values.map(fun(v): v.tosource() end))
    end
  | s_app(l :: Loc, _fun :: Expr, args :: List<Expr>) with:
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | s_left_app(l :: Loc, obj :: Expr, _fun :: Expr, args :: List<Expr>) with:
    tosource(self):
      PP.group(self.obj.tosource() + PP.nest(INDENT, PP.break(0) + PP.str(".") + self._fun.tosource())
          + PP.parens(PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end))))
    end
  | s_id(l :: Loc, id :: String) with:
    tosource(self): PP.str(self.id) end
  | s_num(l :: Loc, n :: Number) with:
    tosource(self): PP.number(self.n) end
  | s_bool(l :: Loc, b :: Bool) with:
    tosource(self): PP.str(self.b.tostring()) end
  | s_str(l :: Loc, s :: String) with:
    tosource(self): PP.squote(PP.str(self.s)) end
  | s_dot(l :: Loc, obj :: Expr, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(), PP.str(self.field)) end
  | s_get_bang(l :: Loc, obj :: Expr, field :: String)
  | s_bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    tosource(self): PP.infix(INDENT, 0, PP.str("."), self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s_colon(l :: Loc, obj :: Expr, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, PP.str(":"), self.obj.tosource(), PP.str(self.field)) end
  | s_colon_bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    tosource(self): PP.infix(INDENT, 0, PP.str(":"), self.obj.tosource(),
        PP.surround(PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s_data(
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared_members :: List<Member>,
      check :: Expr
    ) with:
    tosource(self):
      fun optional_section(lbl, section):
        if PP.is-empty(section): PP.empty
        else: PP.break(1) + PP.group(PP.nest(INDENT, lbl + PP.break(1) + section))
        end
      end
      tys = PP.surround-separate(2*INDENT, 0, PP.empty, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(fun(f): f.tosource() end))
      header = PP.str("data ") + PP.str(self.name) + tys + PP.str(":")
      _deriving =
        PP.surround-separate(INDENT, 0, PP.empty, PP.break(1) + PP.str("deriving "), PP.commabreak, PP.empty, self.mixins.map(fun(m): m.tosource() end))
      variants = PP.separate(PP.break(1) + PP.str("| "),
        PP.str("")^list.link(self.variants.map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional_section(PP.str("sharing:"),
        PP.separate(PP.commabreak, self.shared_members.map(fun(s): s.todatafield() end)))
      _check = optional_section(PP.str("where:"), self.check.tosource())
      footer = PP.break(1) + PP.str("end")
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s_for(
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ) with:
    tosource(self):
      header = PP.group(PP.str("for ")
          + self.iterator.tosource()
          + PP.surround-separate(2*INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
          self.bindings.map(fun(b): b.tosource() end))
          + PP.group(PP.nest(2*INDENT,
            PP.break(1) + PP.str("->") + PP.break(1) + self.ann.tosource() + PP.str(":"))))
      PP.surround(INDENT, 1, header, self.body.tosource(), PP.str("end"))
    end
  | s_check(
      l :: Loc,
      body :: Expr
    ) with:
    tosource(self):
      PP.surround(INDENT, 1, PP.str("check:"), self.body.tosource(), PP.str("end"))
    end
end

data Bind:
  | s_bind(l :: Loc, id :: String, ann :: Ann) with:
    tosource(self):
      if is-a_blank(self.ann): PP.str(self.id)
      else: PP.infix(INDENT, 1, PP.str("::"), PP.str(self.id), self.ann.tosource())
      end
    end
end

data Member:
  | s_data_field(l :: Loc, name :: Expr, value :: Expr) with:
    tosource(self): PP.nest(INDENT, self.name.tosource() + PP.str(": ") + self.value.tosource()) end,
  | s_mutable_field(l :: Loc, name :: Expr, ann :: Ann, value :: Expr) with:
    tosource(self): PP.nest(INDENT, PP.str("mutable") + self.name.tosource() + PP.str("::") + self.ann.tosource() + PP.str(": ") + self.value.tosource()) end,
  | s_once_field(l :: Loc, name :: Expr, ann :: Ann, value :: Expr)
  | s_method_field(
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      check :: Expr
    ) with:
    tosource(self):
      name-part = cases(Expr) self.name:
        | s_str(s) => PP.str(s)
        | else => self.name.tosource()
      end
      funlam_tosource(name-part,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
end

data ForBind:
  | s_for_bind(l :: Loc, bind :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(self.bind.tosource() + PP.break(1) + PP.str("from") + PP.break(1) + self.value.tosource())
    end
end

data VariantMember:
  | s_variant_member(l :: Loc, member_type :: String, bind :: Bind) with:
    tosource(self):
      PP.str(self.member_type) + PP.str(" ") + self.bind.tosource()
    end
end

data Variant:
  | s_variant(
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with_members :: List<Member>
    ) with:
    tosource(self):
      header-nowith = 
        PP.str(self.name)
        + PP.surround-separate(INDENT, 0, PP.empty, PP.lparen, PP.commabreak, PP.rparen,
        self.binds.map(fun(b): b.tosource() end))
      header = PP.group(header-nowith + PP.break(1) + PP.str("with:"))
      withs = self.with_members.map(fun(m): m.todatafield() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, PP.break(1) + PP.separate(PP.commabreak, withs)))
      end
    end
  | s_singleton_variant(
      l :: Loc,
      name :: String,
      with_members :: List<Member>
    ) with:
    tosource(self):
      header-nowith = PP.str(self.name)
      header = PP.group(header-nowith + PP.break(1) + PP.str("with:"))
      withs = self.with_members.map(fun(m): m.todatafield() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, PP.break(1) + PP.separate(PP.commabreak, withs)))
      end
    end
end

data IfBranch:
  | s_if_branch(l :: Loc, test :: Expr, body :: Expr) with:
    tosource(self):
      PP.str("if ")
        + PP.nest(2*INDENT, self.test.tosource()+ PP.str(":"))
        + PP.nest(INDENT, PP.break(1) + self.body.tosource())
    end
end

data CasesBranch:
  | s_cases_branch(l :: Loc, name :: String, args :: List<Bind>, body :: Expr) with:
    tosource(self):
      PP.group(PP.str("| " + self.name)
          + PP.surround-separate(INDENT, 0, PP.empty, PP.lparen, PP.commabreak, PP.rparen,
          self.args.map(fun(a): a.tosource() end)) + PP.break(1) + PP.str("=>")) + PP.break(1) +
      self.body.tosource()
    end
end

data Ann:
  | a_blank with:
    tosource(self): PP.str("Any") end
  | a_any with:
    tosource(self): PP.str("Any") end
  | a_name(l :: Loc, id :: String) with:
    tosource(self): PP.str(self.id) end
  | a_arrow(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    tosource(self):
      PP.surround(INDENT, 1, PP.lparen,
        PP.separate(PP.str(" "),
          [PP.separate(PP.commabreak,
            self.args.map(fun(f): f.tosource() end))] + [PP.str("->"), self.ret.tosource()]), PP.rparen)
    end
  | a_method(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    tosource(self): PP.str("NYI: A_method") end
  | a_record(l :: Loc, fields :: List<AField>) with:
    tosource(self):
      PP.soft-surround(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.flds.map(fun(f): f.tosource() end))
    end
  | a_app(l :: Loc, ann :: Ann, args :: List<Ann>) with:
    tosource(self):
      PP.group(self.ann.tosource()
          + PP.group(PP.langle + PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end))) + PP.rangle))
    end
  | a_pred(l :: Loc, ann :: Ann, exp :: Expr) with:
    tosource(self): self.ann.tosource() + PP.parens(self.exp.tosource()) end
  | a_dot(l :: Loc, obj :: String, field :: String) with:
    tosource(self): PP.str(self.obj + "." + self.field) end
end

data AField:
  | a_field(l :: Loc, name :: String, ann :: Ann) with:
    tosource(self):
      if is-a_blank(self.ann): PP.str(self.name)
      else: PP.infix(INDENT, 1, PP.str("::"), PP.str(self.name), self.ann.tosource())
      end
    end
end
