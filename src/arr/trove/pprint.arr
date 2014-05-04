#lang pyret

provide {
  PPrintDoc: PPrintDoc,
  mt-doc: mt-doc,
  is-mt-doc: is-mt-doc,
  str: str,
  number: number,
  hardline: hardline,
  blank: blank,
  break: break,
  concat: concat,
  nest: nest,
  if-flat: if-flat,
  group: group,
  flow: flow,
  flow-map: flow-map,
  vert: vert,
  parens: parens,
  dquote: dquote,
  squote: squote,
  align: align,
  hang: hang,
  prefix: prefix,
  infix: infix,
  infix-break: infix-break,
  separate: separate,
  surround: surround,
  soft-surround: soft-surround,
  surround-separate: surround-separate,
  label-align-surround: label-align-surround,
  lparen: lparen,
  rparen: rparen,
  lbrace: lbrace,
  rbrace: rbrace,
  lbrack: lbrack,
  rbrack: rbrack,
  langle: langle,
  rangle: rangle,
  comma: comma,
  commabreak: commabreak
} end

data PPrintDoc:
  | mt-doc(flat-width :: Number, has-hardline :: Bool)
  | str(s :: String, flat-width :: Number, has-hardline :: Bool)
  | hardline(flat-width :: Number, has-hardline :: Bool)
  | blank(n :: Number, flat-width :: Number, has-hardline :: Bool)
  | concat(fst :: PPrintDoc, snd :: PPrintDoc, flat-width :: Number, has-hardline :: Bool)
  | nest(indent :: Number, d :: PPrintDoc, flat-width :: Number, has-hardline :: Bool)
  | if-flat(flat :: PPrintDoc, vert :: PPrintDoc, flat-width :: Number, has-hardline :: Bool)
  | align(d :: PPrintDoc, flat-width :: Number, has-hardline :: Bool)
  | align-spaces(n :: Number, flat-width :: Number, has-hardline :: Bool)
  | group(d :: PPrintDoc, flat-width :: Number, has-hardline :: Bool)
sharing:
  _plus(self, other):
    if is-mt-doc(self): other
    else if is-mt-doc(other): self
    else if is-concat(self):
      self.fst + (self.snd + other)
    else:
      if self.has-hardline or other.has-hardline: concat(self, other, 0, true)
      else: concat(self, other, self.flat-width + other.flat-width, false)
      end
    end
  end,
  tostring(self):
    cases(PPrintDoc) self:
      | mt-doc(_, _) => "EmptyDoc"
      | str(s, _, _) => "Str('" + s + "')"
      | hardline(_, _) => "CRLF"
      | blank(n, _, _) => "Blank(" + tostring(n) + ")"
      | concat(fst, snd, _, _) => "Concat(" + tostring(fst) + ", " + tostring(snd) + ")"
      | nest(indent, d, _, _) => "Nest(" + tostring(indent) + ", " + tostring(d) + ")"
      | if-flat(flat, vert, _, _) => "IfFlat(" + tostring(flat) + ", " + tostring(vert) + ")"
      | group(d, _, _) => "Group(" + tostring(d) + ", " + ")"
      | align(d, _, _) => "Align(" + tostring(d) + ")"
      | align-spaces(n, _, _) => "AlignSpaces(" + tostring(n) + ")"
    end
  end,
  pretty(self, width):
    format(width, self)
  end
end
data Item:
  | item(indent :: Number, is-flat :: Boolean, d :: PPrintDoc)
end
# Not needed at the moment...
# fun fits(width :: Number, items :: List<Item>) -> Bool:
#   if width < 0: false
#   else if is-empty(items): true
#   else: 
#     first = items.first
#     i = first.indent
#     m = first.is-flat
#     cases(PPrintDoc) first.d:
#       | mt-doc(_, _) => fits(width, items.rest)
#       | str(s, flat-width, _) => fits(width - flat-width, items.rest)
#       | hardline(_, _) => true
#       | blank(n, _, _) => fits(width - n, items.rest)
#       | concat(fst, snd, _, _) => fits(width, [item(i, m, fst), item(i, m, snd)] + items.rest)
#       | nest(_, d, _, _) => fits(width, [item(i, m, d)] + items.rest)
#       | align(d, _, _) => fits(width, [item(i, m, d)] + items.rest)
#       | if-flat(flat, vert, _, _) =>
#         if m: fits(width, [item(i, m, flat)] + items.rest)
#         else: fits(width, [item(i, m, vert)] + items.rest)
#         end
#       | align-spaces(n, _, _) =>
#         if m: fits(width, items.rest)
#         else: fits(width - n, items.rest)
#         end
#       | group(d, flat-width, has-hardline) =>
#         if has-hardline: fits(width, [item(i, false, d)] + items.rest)
#         else: fits(width - flat-width, items.rest)
#         end
#     end
#   end
# end
fun format(width, doc :: PPrintDoc):
  var output = [[]]
  fun emit-text(s):
    output := [[s] + output.first] + output.rest
  end
  fun emit-spaces(n):
    emit-text(string-repeat(" ", n))
  end
  fun emit-newline(i):
    output := [[string-repeat(" ", i)]] + output
  end
  fun gen-output():
    for list.fold(lines from [], line from output):
      l = for list.fold(acc from "", piece from line):
        piece + acc
      end
      [l] + lines
    end
  end
  fun process(column :: Number, items :: List<Item>) -> Nothing:
    if is-empty(items): nothing
    else:
      first = items.first
      i = first.indent
      m = first.is-flat
      cases(PPrintDoc) first.d:
        | mt-doc(_, _) => process(column, items.rest)
        | concat(fst, snd, _, _) => process(column, [item(i, m, fst), item(i, m, snd)] + items.rest)
        | str(s, flat-width, _) =>
          emit-text(s)
          process(column + flat-width, items.rest)
        | blank(n, _, _) =>
          emit-spaces(n)
          process(column + n, items.rest)
        | align(d, _, _) => process(column, [item(column, m, d)] + items.rest)
        | nest(n, d, _, _) => process(column, [item(i + n, m, d)] + items.rest)
        | hardline(_, _) =>
          if m: raise("Impossible for HardLine to be flat")
          else:
            emit-newline(i)
            process(i, items.rest)
          end
        | if-flat(flat, vert, _, _) =>
          process(column, [item(i, m, if m: flat else: vert end)] + items.rest)
        | align-spaces(n, _, _) =>
          if m: process(column, items.rest)
          else:
            emit-spaces(n)
            process(column + n, items.rest)
          end
        | group(d, flat-width, has-hardline) =>
          if m: process(column, [item(i, true, d)] + items.rest)
          else if has-hardline: process(column, [item(i, false, d)] + items.rest)
          else if (width - column) >= flat-width:
            # This used to check whether items.rest fits into the remaining space,
            # but that precludes implementing "flowing" text, which is more important.
            # If we need both behaviors, I guess I can add a flow-group...
            process(column, [item(i, true, d)] + items.rest)
          else:
            process(column, [item(i, false, d)] + items.rest)
          end
      end
    end
  end
  process(0, [item(0, false, group(doc, doc.flat-width, doc.has-hardline))])
  gen-output()
end

shadow mt-doc = mt-doc(0, false)
shadow hardline = hardline(0, true)
shadow align = fun(d): align(d, d.flat-width, d.has-hardline) end
shadow group = fun(d): group(d, d.flat-width, d.has-hardline) end
shadow if-flat = fun(flat, vert): if-flat(flat, vert, flat.flat-width, flat.has-hardline) end
shadow nest = fun(n, d): nest(n, d, d.flat-width, d.has-hardline) end
shadow concat = fun(fst, snd): fst + snd end
shadow blank = fun(n): blank(n, n, false) end
shadow str = fun(s): str(s, string-length(s), false) end

fun number(n :: Number): str(tostring(n)) end
lparen = str("(")
rparen = str(")")
lbrace = str("{")
rbrace = str("}")
lbrack = str("[")
rbrack = str("]")
langle = str("<")
rangle = str(">")
comma = str(",")
fun break(n): if-flat(blank(n), hardline) end
commabreak = comma + break(1)

fun flow-map(sep, f, items):
  for list.fold(acc from mt-doc, shadow item from items):
    if is-mt-doc(acc): f(item)
    else: acc + group(sep + f(item))
    end
  end
end
fun flow(items): flow-map(break(1), fun(x): x end, items) end
fun vert(items): flow-map(hardline, fun(x): x end, items) end
fun parens(d): group(lparen + d + rparen) end
str-squote = str("'")
str-dquote = str('"')
fun dquote(s): group(str-dquote + s + str-dquote) end
fun squote(s): group(str-squote + s + str-squote) end

fun hang(i, d): align(nest(i, d)) end
fun prefix(n, b, x, y): group(x + nest(n, break(b) + y)) end
fun infix(n :: Number, b :: Number, op :: PPrintDoc, x :: PPrintDoc, y :: PPrintDoc):
  prefix(n, b, (x + blank(b) + op), y)
end
fun infix-break(n :: Number, b :: Number, op :: PPrintDoc, x :: PPrintDoc, y :: PPrintDoc):
  prefix(n, b, x, (op + blank(b) + y))
end
fun surround(n :: Number, b :: Number, open :: PPrintDoc, contents :: PPrintDoc, close :: PPrintDoc):
  if is-mt-doc(close): group(open + nest(n, break(b) + contents))
  else: group(open + nest(n, break(b) + contents) + break(b) + close)
  end
end
fun soft-surround(n :: Number, b :: Number, open :: PPrintDoc, contents :: PPrintDoc, close :: PPrintDoc):
  if is-mt-doc(close): group(open + nest(n, group(break(b) + contents)))
  else: group(open + nest(n, group(break(b) + contents)) + group(break(b) + close))
  end
end

fun separate(sep :: PPrintDoc, docs :: list.List):
  for list.fold(acc from mt-doc, d from docs):
    if is-mt-doc(d): acc
    else if is-mt-doc(acc): d
    else: acc + sep + d
    end
  end
end
fun surround-separate(n :: Number, b :: Number, void :: PPrintDoc, open :: PPrintDoc, sep :: PPrintDoc, close :: PPrintDoc, docs :: list.List):
  if list.is-empty(docs): void
  else: surround(n, b, open, separate(sep, docs), close)
  end
end

fun label-align-surround(label, open, sep, contents, close):
  group(label + align(open + align(separate(sep, contents)) + group(break(0) + close)))
end

check:
  test-words = ["This", "is", "a", "sentence", "with", "eight", "words"].map(str)
  test = flow(test-words)

  test.pretty(40) is ["This is a sentence with eight words"]
  test.pretty(30) is ["This is a sentence with eight", "words"]
  test.pretty(20) is ["This is a sentence", "with eight words"]
  test.pretty(10) is ["This is a", "sentence", "with eight", "words"]

  fun opt-break(x, y):
    if is-empty(x): y
    else if is-empty(y): x
    else: x + (break(1) + y)
    end
  end
  fun binop(left, op, right):
    group(nest(2,
        opt-break(group(opt-break(str(left), str(op))), str(right))))
  end
  fun ifthen(c, t, f):
    group(
      group(nest(2, opt-break(str("if"), c))) ^ opt-break(
        group(nest(2, opt-break(str("then"), t))) ^ opt-break(
          group(nest(2, opt-break(str("else"), f))))))
  end
  ifthenelse = ifthen(binop("a", "==", "b"), binop("a", "<<", "2"), binop("a", "+", "b"))
  ifthenelse.pretty(32) is ["if a == b then a << 2 else a + b"]
  ifthenelse.pretty(15) is ["if a == b", "then a << 2", "else a + b"]
  ifthenelse.pretty(10) is ["if a == b", "then", "  a << 2", "else a + b"]
  ifthenelse.pretty(8) is
  [ "if",
    "  a == b",
    "then",
    "  a << 2",
    "else",
    "  a + b" ]
  ifthenelse.pretty(7) is
  [ "if",
    "  a ==",
    "    b",
    "then",
    "  a <<",
    "    2",
    "else",
    "  a + b" ]
  ifthenelse.pretty(6) is
  [ "if",
    "  a ==",
    "    b",
    "then",
    "  a <<",
    "    2",
    "else",
    "  a +",
    "    b" ]
end
