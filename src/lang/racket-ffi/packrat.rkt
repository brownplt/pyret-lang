#lang pyret

provide *

# Additive <- Multitive '+' Additive | Multitive
# Multitive <- Primary '*' Multitive | Primary
# Primary <- '(' Additive ')' | Decimal
# Decimal <- ['0' | ... | '9']+

data Result<T, R>:
  | parsed(v :: T, rest :: R)
  | no-parse
end

data AExp:
  | additive(left :: AExp, right :: AExp) with:
    _torepr(self): self.left._torepr() + "+" + self.right._torepr() end
  | multitive(left :: AExp, right :: AExp) with:
    _torepr(self): self.left._torepr() + "*" + self.right._torepr() end
  | primary(e :: AExp) with: _torepr(self): "(" + self.e._torepr() + ")" end
  | decimal(n :: Number) with: _torepr(self): torepr(self.n) end
end

# Initial naive algorithm, based on Ford's (POPL 2002) paper, without using derivatives

fun pChar(s :: String) -> Result<String, String>:
  if s == "": no-parse
  else: parsed(s.char-at(0), s.substring(1, s.length()))
  end
end

fun pAdditive(s :: String) -> Result<AExp, String>:
  fun alt1():
    cases(Result<AExp, String>) (pMultitive(s)):
      | parsed(left, s1) =>
        cases(Result<String, String>) (pChar(s1)):
          | parsed(c, s2) =>
            if c == "+":
              cases(Result<String, String>) (pAdditive(s2)):
                | parsed(right, s3) => parsed(additive(left, right), s3)
                | else => alt2()
              end
            else: alt2()
            end
          | else => alt2()
        end
      | else => alt2()
    end
  end
  fun alt2():
    cases(Result<AExp, String>) (pMultitive(s)):
      | parsed(m, s1) => parsed(m, s1)
      | else => no-parse
    end
  end
  alt1()
end

fun pMultitive(s :: String) -> Result<AExp, String>:
  fun alt1():
    cases(Result<AExp, String>) (pPrimary(s)):
      | parsed(left, s1) =>
        cases(Result<String, String>) (pChar(s1)):
          | parsed(c, s2) =>
            if c == "*":
              cases(Result<String, String>) (pMultitive(s2)):
                | parsed(right, s3) => parsed(multitive(left, right), s3)
                | else => alt2()
              end
            else: alt2()
            end
          | else => alt2()
        end
      | else => alt2()
    end
  end
  fun alt2():
    cases(Result<AExp, String>) (pPrimary(s)):
      | parsed(m, s1) => parsed(m, s1)
      | else => no-parse
    end
  end
  alt1()
end

fun pPrimary(s :: String) -> Result<AExp, String>:
  fun alt1():
    cases(Result<String, String>) (pChar(s)):
      | parsed(c1, s1) =>
        if c1 == "(":
          cases(Result<AExp, String>) (pAdditive(s1)):
            | parsed(e, s2) =>
              cases(Result<String, String>) (pChar(s2)):
                | parsed(c2, s3) =>
                  if c2 == ")": parsed(primary(e), s3)
                  else: alt2()
                  end
                | else => alt2()
              end
            | else => alt2()
          end
        else: alt2()
        end
      | else => alt2()
    end
  end
  fun alt2():
    cases(Result<AExp, String>) (pDecimal(s)):
      | parsed(d, s1) => parsed(d, s1)
      | else => no-parse
    end
  end
  alt1()
end

fun pDecimal(s :: String) -> Result<AExp, String>:
  cases(Result<String, String>) (pDigits(s)):
    | parsed(v, s1) => parsed(decimal(v.tonumber()), s1)
    | else => no-parse
  end
end

fun pDigits(s :: String) -> Result<String, String>:
  cases(Result<String, String>) (pChar(s)):
    | parsed(c, s1) =>
      if "1234567890".contains(c):
        cases(Result<String, String>) (pDigits(s1)):
          | parsed(n, s2) => parsed(c + n, s2)
          | else => parsed(c, s1)
        end
      else: no-parse
      end
    | else => no-parse
  end
end

check:
  parse1 = pAdditive("1+2*3*(4+5)")
  parse1 satisfies is-parsed
  parse1.rest is ""
  
  parse2 = pAdditive("123+*4")
  parse2 satisfies is-parsed
  parse2.rest is "+*4"
end

# Second algorithm, using derivatives, from Ford (POPL 2002)

data Derivs:
  | derivs(s :: String, mutable cache :: Object) with:
    _torepr(self): torepr(self!cache) + "@\"" + self.s + "\"" end
sharing:
  get(self, name :: String, parsers :: Object) -> Derivs:
    if builtins.has-field(self!cache, name): self!cache.[name]
    else:
      ans = parsers.[name](parsers, self)
      self!{cache: self!cache.{[name]: ans}}
      ans
    end
  end
end

fun<T> make-parser(parsers :: Object, start :: String) -> (String -> Result<T, Derivs>):
  fun(s :: String) -> Result<T, Derivs>:
    derivs(s, {}).get(start, parsers)
  end
end

default-parsers = {
  char(_, parser, d :: Derivs) -> Result<String, Derivs>:
    if d.s == "": no-parse
    else:
      parsed(d.s.char-at(0), derivs(d.s.substring(1, d.s.length()), {}))
    end
  end,
  null(_, parser, d :: Derivs): parsed(d.s, d) end,
  whitespace(_, parser, d :: Derivs) -> Result<String, Derivs>:
    cases(Result<String, Derivs>) d.get("char", parser):
      | parsed(c, d1) =>
        if " \t\r\n".contains(c):
          cases(Result<String, Derivs>) d1.get("whitespace", parser):
            | parsed(ws, d2) => parsed(c + ws, d2)
            | no-parse => parsed(c, d1)
          end
        else: no-parse
        end
      | else => no-parse
    end
  end
}

data Tree:
  | node(name, children :: List<Tree>) with:
    _torepr(self): self.name + "(" + self.children.join-str(", ") + ")" end,
    tostring(self): self._torepr() end
end

fun leaf(name): node(name, []) end

fun match-letter(c):
  fun(parser, d :: Derivs) -> Result<String, Derivs>:
    if d.s == "": no-parse
    else if d.s.char-at(0) == c:
      parsed(c, derivs(d.s.substring(1, d.s.length()), {}))
    else: no-parse
    end
  end
end

# fun<T> seq(p1, p2):
#   fun(parser, d :: Derivs):
#     cases(Result<T, Derivs>) p1(parser, d):
#       | no-parse => no-parse
#       | parsed(val, d1) => p2(val)(parser, d1)
#     end
#   end
# end

# fun<T> alt(p1, p2):
#   fun(parser, d :: Derivs):
#     ans1 = p1(parser, d)
#     cases(Result<T, Derivs>) ans1:
#       | no-parse => p2(parser, d)
#       | else => ans1
#     end
#   end
# end

data PEGRule:
  | peg-token(s :: String) with:
    _torepr(self): "'" + self.s + "'" end,
    to-parser(self):
      fun(parser, d :: Derivs):
        if d.s.substring(0, self.s.length()) == self.s:
          parsed(leaf(self.s), derivs(d.s.substring(self.s.length(), d.s.length()), {}))
        else: no-parse
        end
      end
    end
  | peg-charset(chars :: String) with:
    _torepr(self): "[" + self.s + "]" end,
    to-parser(self):
      fun(parser, d :: Derivs):
        ans = d.get("char", parser)
        cases(Result<String, Derivs>) ans:
          | parsed(c, new-d) => if self.chars.contains(c): parsed(c, new-d) else: no-parse end
          | else => no-parse
        end
      end
    end
  | peg-star(nonterm :: String) with:
    _torepr(self): self.nonterm + "*" end,
    to-parser(self):
      star-name = self.nonterm + "*"
      fun<T>(parser, d :: Derivs):
        cases(Result<T, Derivs>) d.get(self.nonterm, parser):
          | no-parse => parsed([], d)
          | parsed(p-ans, new-d) =>
            cases(Result<List<T>, Derivs>) new-d.get(star-name, parser):
              | no-parse => parsed([p-ans], new-d)
              | parsed(more-ps, new-d1) => parsed(p-ans^list.link(more-ps), new-d1)
            end
        end
      end
    end
  | peg-plus(nonterm :: String) with:
    _torepr(self): self.nonterm + "+" end,
    to-parser(self):
      star-name = self.nonterm + "+"
      fun<T>(parser, d :: Derivs):
        cases(Result<T, Derivs>) d.get(self.nonterm, parser):
          | no-parse => no-parse
          | parsed(p-ans, new-d) =>
            cases(Result<List<T>, Derivs>) new-d.get(star-name, parser):
              | no-parse => parsed([p-ans], new-d)
              | parsed(more-ps, new-d1) => parsed(p-ans^list.link(more-ps), new-d1)
            end
        end
      end
    end
  | peg-not(nonterm :: String) with:
    _torepr(self): "!" + self.nonterm end,
    to-parser(self):
      fun<T>(parser, d :: Derivs):
        cases(Result<T, Derivs>) d.get(self.nonterm, parser):
          | no-parse => parsed(nothing, d)
          | else => no-parse
        end
      end
    end
  | peg-and(p1 :: String, p2 :: String) with:
    _torepr(self): "&" + self.p1 + " " + self.p2 end,
    to-parser(self):
      fun<T>(parser, d :: Derivs):
        cases(Result<T, Derivs>) d.get(self.p1, parser):
          | no-parse => no-parse
          | parsed(_, new-d) => new-d.get(self.p1, parser)
        end
      end
    end
  | peg-nonterm(name :: String) with:
    _torepr(self): self.name end,
    to-parser(self):
      fun(parser, d :: Derivs):
        d.get(self.name, parser)
      end
    end
  | peg-seq(name :: String, parts :: List<PEGRule>) with:
    _torepr(self): self.parts.map(torepr).join-str(" ") end,
    to-parser(self):
      part-parsers = self.parts.map(fun(p): p.to-parser() end)
      fun(parser, d :: Derivs):
        ans = for list.fold(acc from parsed([], d), part from part-parsers):
          cases(Result<List<Tree>, Derivs>) acc:
            | no-parse => acc
            | parsed(rev-results, new-d) =>
              cases(Result<List<Tree>, Derivs>) part(parser, new-d):
                | no-parse => no-parse
                | parsed(res, new-d2) => parsed(res^list.link(rev-results), new-d2)
              end
          end
        end
        cases(Result<List<Tree>, Derivs>) ans:
          | no-parse => no-parse
          | parsed(rev-results, new-d) => parsed(node(self.name, rev-results.reverse()), new-d)
        end
      end
    end
  | peg-alt(choices :: List<PEGRule>) with:
    _torepr(self): self.choices.map(torepr).join-str(" | ") end,
    to-parser(self):
      part-parsers = self.choices.map(fun(p): p.to-parser() end)
      fun(parser, d :: Derivs):
        ans = for list.fold(acc from parsed(none, d), part from part-parsers):
          cases(Option<Tree>) acc.v:
            | none =>
              part-ans = part(parser, acc.rest)
              cases(Result<Option<Tree>, Derivs>) part-ans:
                | no-parse => acc
                | parsed(part-res, part-d) => parsed(some(part-res), part-d)
              end
            | else => acc
          end
        end
        cases(Result<Option<Tree>, Derivs>) ans:
          | no-parse => no-parse
          | parsed(res, new-d) =>
            cases(Option<Tree>) res:
              | none => no-parse
              | some(t) => parsed(t, new-d)
            end
        end
      end
    end
end

aexp-parsers = default-parsers.{
  pAdditive(_, parser, d :: Derivs) -> Result<AExp, Derivs>:
      fun alt1():
        cases(Result<AExp, Derivs>) d.get("pMultitive", parser):
          | parsed(left, d1) =>
            cases(Result<String, Derivs>) d1.get("char", parser):
              | parsed(c, d2) =>
                if c == "+":
                  cases(Result<String, Derivs>) d2.get("pAdditive", parser):
                    | parsed(right, d3) => parsed(additive(left, right), d3)
                    | else => alt2()
                  end
                else: alt2()
                end
              | else => alt2()
            end
          | else => alt2()
        end
      end
      fun alt2():
        cases(Result<AExp, Derivs>) d.get("pMultitive", parser):
          | parsed(m, d1) => parsed(m, d1)
          | else => no-parse
        end
      end
      alt1()
    end,

    pMultitive(_, parser, d :: Derivs) -> Result<AExp, Derivs>:
      fun alt1():
        cases(Result<AExp, Derivs>) d.get("pPrimary", parser):
          | parsed(left, d1) =>
            cases(Result<String, Derivs>) d1.get("char", parser):
              | parsed(c, d2) =>
                if c == "*":
                  cases(Result<String, Derivs>) d2.get("pMultitive", parser):
                    | parsed(right, d3) => parsed(multitive(left, right), d3)
                    | else => alt2()
                  end
                else: alt2()
                end
              | else => alt2()
            end
          | else => alt2()
        end
      end
      fun alt2():
        cases(Result<AExp, Derivs>) d.get("pPrimary", parser):
          | parsed(m, d1) => parsed(m, d1)
          | else => no-parse
        end
      end
      alt1()
    end,

    pPrimary(_, parser, d :: Derivs) -> Result<AExp, Derivs>:
      fun alt1():
        cases(Result<String, Derivs>) d.get("char", parser):
          | parsed(c1, d1) =>
            if c1 == "(":
              cases(Result<AExp, Derivs>) d1.get("pAdditive", parser):
                | parsed(e, d2) =>
                  cases(Result<String, Derivs>) d2.get("char", parser):
                    | parsed(c2, d3) =>
                      if c2 == ")": parsed(primary(e), d3)
                      else: alt2()
                      end
                    | else => alt2()
                  end
                | else => alt2()
              end
            else: alt2()
            end
          | else => alt2()
        end
      end
      fun alt2():
        cases(Result<AExp, Derivs>) d.get("pDecimal", parser):
          | parsed(n, d1) => parsed(n, d1)
          | else => no-parse
        end
      end
      alt1()
    end,

  pDecimal(_, parser, d :: Derivs) -> Result<AExp, Derivs>:
      cases(Result<String, Derivs>) d.get("pDigits", parser):
        | parsed(v, d1) => parsed(decimal(v.tonumber()), d1)
        | else => no-parse
      end
    end,

  pDigits(_, parser, d :: Derivs) -> Result<String, Derivs>:
      cases(Result<String, Derivs>) d.get("char", parser):
        | parsed(c, d1) =>
          if "1234567890".contains(c):
            cases(Result<String, Derivs>) d1.get("pDigits", parser):
              | parsed(n, d2) => parsed(c + n, d2)
              | else => parsed(c, d1)
            end
          else: no-parse
          end
        | else => no-parse
      end
    end
}

aexp-parse = make-parser(aexp-parsers, "pAdditive")

check:
  parse1 = aexp-parse("1+2*3*(4+5)")
  parse1 satisfies is-parsed
  print(parse1.v)
  parse1.rest.s is ""

  parse2 = aexp-parse("123+*4")
  parse2 satisfies is-parsed
  parse2.rest.s is "+*4"
end

aexp-peg = default-parsers.{
  pAdditive:
    peg-alt(
      [ peg-seq("SUM", [peg-nonterm("pMultitive"), peg-token("+"), peg-nonterm("pAdditive")]),
        peg-nonterm("pMultitive") ]).to-parser(),
  pMultitive:
    peg-alt(
      [ peg-seq("PROD", [peg-nonterm("pPrimary"), peg-token("*"), peg-nonterm("pMultitive")]),
        peg-nonterm("pPrimary") ]).to-parser(),
  pPrimary:
    peg-alt(
      [ peg-seq("PAREN", [peg-token("("), peg-nonterm("pAdditive"), peg-token(")")]),
        peg-nonterm("pDigit+") ]).to-parser(),
  ["pDigit+"]: peg-plus("pDigit").to-parser(),
  pDigit: peg-charset("1234567890").to-parser()
}

tree-parse = make-parser(aexp-peg, "pAdditive")

check:
  parse1 = tree-parse("1+2*3*(4+5)")
  parse1 satisfies is-parsed
  print(parse1.v)
  parse1.rest.s is ""

  parse2 = tree-parse("123+*4")
  parse2 satisfies is-parsed
  parse2.rest.s is "+*4"
end

