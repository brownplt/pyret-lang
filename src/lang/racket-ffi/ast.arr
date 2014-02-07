#lang pyret

provide *
import pprint as PP

Loc = error.Location
loc = error.location

INDENT = 2

break-one = PP.break(1)
str-any = PP.str("Any")
str-arrow = PP.str("->")
str-arrowspace = PP.str("-> ")
str-as = PP.str("as")
str-blank = PP.str("")
str-block = PP.str("block:")
str-brackets = PP.str("[]")
str-cases = PP.str("cases")
str-check = PP.str("check:")
str-colon = PP.str(":")
str-coloncolon = PP.str("::")
str-colonspace = PP.str(": ")
str-comment = PP.str("# ")
str-constructor = PP.str("with constructor")
str-data = PP.str("data ")
str-datatype = PP.str("datatype ")
str-deriving = PP.str("deriving ")
str-doc = PP.str("doc: ")
str-elsebranch = PP.str("| else =>")
str-elsecolon = PP.str("else:")
str-elsespace = PP.str("else ")
str-end = PP.str("end")
str-except = PP.str("except")
str-for = PP.str("for ")
str-from = PP.str("from")
str-fun = PP.str("fun")
str-if = PP.str("if ")
str-import = PP.str("import")
str-method = PP.str("method")
str-mutable = PP.str("mutable")
str-not = PP.str("not")
str-period = PP.str(".")
str-pipespace = PP.str("| ")
str-provide = PP.str("provide")
str-provide-star = PP.str("provide *")
str-sharing = PP.str("sharing:")
str-space = PP.str(" ")
str-spacecolonequal = PP.str(" :=")
str-spaceequal = PP.str(" =")
str-thickarrow = PP.str("=>")
str-try = PP.str("try:")
str-use-loc = PP.str("UseLoc")
str-var = PP.str("var ")
str-when = PP.str("when")
str-where = PP.str("where:")
str-with = PP.str("with:")


fun funlam_tosource(funtype, name, params, args :: List<is-s_bind>,
    ann :: Ann, doc :: String, body :: Expr, _check :: Expr) -> PP.PPrintDoc:
  typarams =
    if is-nothing(params): PP.mt-doc
    else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        params.map(PP.str))
    end
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(fun(a): a.tosource() end)))
  ftype = funtype + typarams
  fname =
    if is-nothing(name): ftype
    else if PP.is-mt-doc(ftype): PP.str(name)
    else: ftype + PP.str(" " + name)
    end
  fann =
    if is-a_blank(ann) or is-nothing(ann): PP.mt-doc
    else: break-one + str-arrowspace + ann.tosource()
    end
  header = PP.group(fname + arg-list + fann + str-colon)
  checker = _check.tosource()
  footer =
    if PP.is-mt-doc(checker): str-end
    else: PP.surround(INDENT, 1, str-where, _check.tosource(), str-end)
    end
  docstr =
    if is-nothing(doc) or (doc == ""): PP.mt-doc
    else: str-doc + PP.dquote(PP.str(doc)) + PP.hardline
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
      PP.flow([str-import, PP.quote(PP.str(self.file)),
          str-as, PP.str(self.name)])
    end
  | s_provide(l :: Loc, block :: Expr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1, str-provide,
        self.block.tosource(), str-end)
    end
  | s_provide_all(l :: Loc) with:
    tosource(self): str-provide-star end
end

data ImportType:
  | s_file_import(file :: String) with:
    tosource(self): str-import + break-one + PP.dquote(PP.str(self.file)) end
  | s_const_import(module :: String) with:
    tosource(self): str-import + break-one + PP.str(self.module) end
end

data Hint:
  | h_use_loc(l :: Loc) with:
    tosource(self): str-use-loc + PP.parens(PP.str(self.l.tostring())) end
end

data Expr:
  | s_hint_exp(l :: Loc, hints :: List<Hint>, e :: Expr) with:
    tosource(self):
      PP.flow_map(PP.hardline, fun(h): str-comment + h.tosource() end, self.hints) + PP.hardline
        + self.e.tosource()
    end
  | s_block(l :: Loc, stmts :: List<Expr>) with:
    tosource(self):
      PP.flow_map(PP.hardline, _.tosource(), self.stmts) end
  | s_user_block(l :: Loc, body :: Expr) with:
    tosource(self):
      PP.surround(INDENT, 1, str-block, self.body.tosource(), str-end)
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
      funlam_tosource(str-fun,
        self.name, self.params, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_var(l :: Loc, name :: Bind, value :: Expr) with:
    tosource(self):
      str-var
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + str-spaceequal + break-one + self.value.tosource()))
    end
  | s_let(l :: Loc, name :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.name.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s_graph(l :: Loc, bindings :: List<is-s_let>)
  | s_when(l :: Loc, test :: Expr, block :: Expr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-when + PP.parens(self.test.tosource()) + str-colon,
        self.block.tosource(),
        str-end)
    end
  | s_assign(l :: Loc, id :: String, value :: Expr) with:
    tosource(self):
      PP.nest(INDENT, PP.str(self.id) + str-spacecolonequal + break-one + self.value.tosource())
    end
  | s_if(l :: Loc, branches :: List<IfBranch>) with:
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(fun(b): b.tosource() end))
      PP.group(branches + break-one + str-end)
    end
  | s_if_else(l :: Loc, branches :: List<IfBranch>, _else :: Expr) with:
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(fun(b): b.tosource() end))
      _else = str-elsecolon + PP.nest(INDENT, break-one + self._else.tosource())
      PP.group(branches + break-one + _else + break-one + str-end)
    end
  | s_cases(l :: Loc, type :: Ann, val :: Expr, branches :: List<CasesBranch>) with:
    tosource(self):
      header = str-cases + PP.parens(self.type.tosource()) + break-one
        + self.val.tosource() + str-colon
      PP.surround-separate(INDENT, 1, header + str-space + str-end,
        PP.group(header), break-one, str-end,
        self.branches.map(fun(b): PP.group(b.tosource()) end))
    end
  | s_cases_else(l :: Loc, type :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr) with:
    tosource(self):
      header = str-cases + PP.parens(self.type.tosource()) + break-one
        + self.val.tosource() + str-colon
      body = PP.separate(break-one, self.branches.map(fun(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | s_try(l :: Loc, body :: Expr, id :: Bind, _except :: Expr) with:
    tosource(self):
      _try = str-try + break-one
        + PP.nest(INDENT, self.body.tosource()) + break-one
      _except = str-except + PP.parens(self.id.tosource()) + str-colon + break-one
        + PP.nest(INDENT, self._except.tosource()) + break-one
      PP.group(_try + _except + str-end)
    end
  | s_op(l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    tosource(self): PP.infix(INDENT, 1, PP.str(self.op.substring(2, self.op.length())), self.left.tosource(), self.right.tosource()) end
  | s_check_test(l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    tosource(self): PP.infix(INDENT, 1, PP.str(self.op.substring(2, self.op.length())), self.left.tosource(), self.right.tosource()) end
  | s_not(l :: Loc, expr :: Expr) with:
    tosource(self): PP.nest(INDENT, PP.flow([str-not, self.expr.tosource()])) end
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
      funlam_tosource(str-fun,
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
      funlam_tosource(str-method,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
  | s_extend(l :: Loc, super :: Expr, fields :: List<Member>) with:
    tosource(self):
      PP.group(self.super.tosource() + str-period
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(fun(f): f.tosource() end)))
    end
  | s_update(l :: Loc, super :: Expr, fields :: List<Member>)
  | s_obj(l :: Loc, fields :: List<Member>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(fun(f): f.tosource() end))
    end
  | s_list(l :: Loc, values :: List<Expr>) with:
    tosource(self):
      PP.surround-separate(INDENT, 0, str-brackets, PP.lbrack, PP.commabreak, PP.rbrack,
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
      PP.group(self.obj.tosource() + PP.nest(INDENT, PP.break(0) + str-period + self._fun.tosource())
          + PP.parens(PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end))))
    end
  | s_id(l :: Loc, id :: String) with:
    tosource(self): PP.str(self.id) end
  | s_num(l :: Loc, n :: Number) with:
    tosource(self): PP.number(self.n) end
  | s_bool(l :: Loc, b :: Bool) with:
    tosource(self): PP.str(self.b.tostring()) end
  | s_str(l :: Loc, s :: String) with:
    tosource(self):
      PP.str(torepr(self.s))
    end
  | s_dot(l :: Loc, obj :: Expr, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | s_get_bang(l :: Loc, obj :: Expr, field :: String)
  | s_bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s_colon(l :: Loc, obj :: Expr, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | s_colon_bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
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
        if PP.is-mt-doc(section): PP.mt-doc
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2*INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(fun(f): f.tosource() end))
      header = str-data + PP.str(self.name) + tys + str-colon
      _deriving =
        PP.surround-separate(INDENT, 0, PP.mt-doc, break-one + str-deriving, PP.commabreak, PP.mt-doc, self.mixins.map(fun(m): m.tosource() end))
      variants = PP.separate(break-one + str-pipespace,
        str-blank^list.link(self.variants.map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional_section(str-sharing,
        PP.separate(PP.commabreak, self.shared_members.map(fun(s): s.tosource() end)))
      _check = optional_section(str-where, self.check.tosource())
      footer = break-one + str-end
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s_datatype(
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      variants :: List<Variant>,
      check :: Expr
    ) with:
      label(self): "s_datatype" end,
    tosource(self):
      fun optional_section(lbl, section):
        if PP.is-empty(section): PP.empty
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2*INDENT, 0, PP.empty, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(fun(f): f.tosource() end))
      header = str-data + PP.str(self.name) + tys + str-colon
      variants = PP.separate(break-one + str-pipespace,
        str-blank^list.link(self.variants.map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      _check = optional_section(str-where, self.check.tosource())
      footer = break-one + str-end
      header + PP.group(PP.nest(INDENT, variants) + _check + footer)
    end
  | s_for(
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ) with:
    tosource(self):
      header = PP.group(str-for
          + self.iterator.tosource()
          + PP.surround-separate(2*INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
          self.bindings.map(fun(b): b.tosource() end))
          + PP.group(PP.nest(2*INDENT,
            break-one + str-arrow + break-one + self.ann.tosource() + str-colon)))
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s_check(
      l :: Loc,
      body :: Expr
    ) with:
    tosource(self):
      PP.surround(INDENT, 1, str-check, self.body.tosource(), str-end)
    end
end

data Bind:
  | s_bind(l :: Loc, id :: String, ann :: Ann) with:
    tosource(self):
      if is-a_blank(self.ann): PP.str(self.id)
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.id), self.ann.tosource())
      end
    end
end

data Member:
  | s_data_field(l :: Loc, name :: Expr, value :: Expr) with:
    tosource(self): PP.nest(INDENT, self.name.tosource() + str-colonspace + self.value.tosource()) end,
  | s_mutable_field(l :: Loc, name :: Expr, ann :: Ann, value :: Expr) with:
    tosource(self): PP.nest(INDENT, str-mutable + self.name.tosource() + str-coloncolon + self.ann.tosource() + str-colonspace + self.value.tosource()) end,
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
        | s_str(l, s) => PP.str(s)
        | else => self.name.tosource()
      end
      funlam_tosource(name-part,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self.check)
    end
end

data ForBind:
  | s_for_bind(l :: Loc, bind :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(self.bind.tosource() + break-one + str-from + break-one + self.value.tosource())
    end
end

data VariantMember:
  | s_variant_member(l :: Loc, member_type :: String, bind :: Bind) with:
    tosource(self):
      if self.member_type <> "normal":
        PP.str(self.member_type) + str-space + self.bind.tosource()
      else:
        self.bind.tosource()
      end
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
        + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
        self.members.map(fun(b): b.tosource() end))
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with_members.map(fun(m): m.tosource() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
  | s_singleton_variant(
      l :: Loc,
      name :: String,
      with_members :: List<Member>
    ) with:
    tosource(self):
      header-nowith = PP.str(self.name)
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with_members.map(fun(m): m.tosource() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
  | s_datatype_variant(
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ) with:
    label(self): "s_datatype_variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
  | s_datatype_singleton_variant(
      l :: Loc,
      name :: String,
      constructor :: Constructor
    ) with:
    label(self): "s_datatype_singleton_variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
end

data Constructor:
  | s_datatype_constructor(
      l :: Loc,
      self :: String,
      body :: Expr
      ) with:
    label(self): "s_datatype_constructor" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
end

data IfBranch:
  | s_if_branch(l :: Loc, test :: Expr, body :: Expr) with:
    tosource(self):
      str-if
        + PP.nest(2*INDENT, self.test.tosource()+ str-colon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
end

data CasesBranch:
  | s_cases_branch(l :: Loc, name :: String, args :: List<Bind>, body :: Expr) with:
    tosource(self):
      PP.group(PP.str("| " + self.name)
          + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
          self.args.map(fun(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
      self.body.tosource()
    end
end

data Ann:
  | a_blank with:
    tosource(self): str-any end
  | a_any with:
    tosource(self): str-any end
  | a_name(l :: Loc, id :: String) with:
    tosource(self): PP.str(self.id) end
  | a_arrow(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    tosource(self):
      PP.surround(INDENT, 1, PP.lparen,
        PP.separate(str-space,
          [PP.separate(PP.commabreak,
            self.args.map(fun(f): f.tosource() end))] + [str-arrow, self.ret.tosource()]), PP.rparen)
    end
  | a_method(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    tosource(self): PP.str("NYI: A_method") end
  | a_record(l :: Loc, fields :: List<AField>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.fields.map(fun(f): f.tosource() end))
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
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.ann.tosource())
      end
    end
end
