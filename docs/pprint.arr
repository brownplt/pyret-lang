#lang pyret

provide {
  PPrintDoc: PPrintDoc,
  empty: empty,
  is-empty: is-empty,
  str: str,
  number: number,
  hardline: hardline,
  blank: blank,
  break: break,
  concat: concat,
  nest: nest,
  ifFlat: ifFlat,
  group: group,
  nesting: nesting,
  column: column,
  flow: flow,
  flow_map: flow_map,
  vert: vert,
  parens: parens,
  dquote: dquote,
  align: align,
  hang: hang,
  prefix: prefix,
  infix: infix,
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
  | empty
  | str(s :: String)
  | hardline
  | blank(n :: Number)
  | concat(fst :: PPrintDoc, snd :: PPrintDoc)
  | nest(indent :: Number, d :: PPrintDoc)
  | ifFlat(flat :: PPrintDoc, vert :: PPrintDoc)
  | column(func :: (Number -> PPrintDoc))
  | nesting(func :: (Number -> PPrintDoc))
  | group(d :: PPrintDoc)
sharing:
  _plus(self, other):
    if is-empty(self): other
    else if is-empty(other): self
    else: concat(self, other)
    end
  end,
  tostring(self):
    cases(PPrintDoc) self:
      | empty => "Empty"
      | string(s) => "Str(" + s + ")"
      | hardline => "CRLF"
      | blank(n) => "Blank(" + tostring(n) + ")"
      | concat(fst, snd) => "Concat(" + tostring(fst) + ", " + tostring(snd) + ")"
      | nest(indent, d) => "Nest(" + tostring(indent) + ", " + tostring(d) + ")"
      | ifFlat(flat, _vert) => "IfFlat(" + tostring(flat) + ", " + tostring(_vert) + ")"
      | group(d) => "Group(" + tostring(d) + ")"
      | column(_) => "Column(<func>)"
      | nesting(_) => "Nesting(<func>)"
    end
  end,
  #  tostring(self): self.pretty(100) end,
  pretty(self, width :: Number) -> List:
    var output = [[]]
    var indent = 0
    var curcol = 0
    var is-flat = true
    var in-group = false
    fun blanks(n :: Number): " ".repeat(n) end
    fun gen-output():
      (for list.map(lines from output):
          for list.fold(acc from "", piece from lines):
            piece + acc
          end
        end).reverse()
    end
    fun emit_string(s :: String, len :: Number):
      when (in-group or is-flat) and ((curcol + len) >= width):
        raise("String doesn't fit")
      end
      output := (s^list.link(output.first))^list.link(output.rest)
      curcol := curcol + len
      nothing
    end
    fun emit_blanks(n :: Number):
      emit_string(blanks(n), n)
    end
    fun emit_newline():
      output := []^list.link(output)
      curcol := 0
      emit_blanks(indent)
    end
    fun run(pdoc):
      if is-empty(pdoc): emit_string("", 0)
      else if is-str(pdoc): emit_string(pdoc.s, pdoc.s.length())
      else if is-hardline(pdoc):
        if is-flat: raise("Hardline isn't flat")
        else: emit_newline()
        end
      else if is-blank(pdoc): emit_blanks(pdoc.n)
      else if is-ifFlat(pdoc):
        if is-flat: run(pdoc.flat)
        else: run(pdoc.vert)
        end
      else if is-concat(pdoc):
        run(pdoc.fst)
        run(pdoc.snd)
      else if is-nest(pdoc):
        cur-indent = indent
        indent := indent + pdoc.indent
        run(pdoc.d)
        indent := cur-indent
      else if is-group(pdoc):
        if not in-group:
          cur-indent = indent
          cur-column = curcol
          cur-flat = is-flat
          cur-output = output
          in-group := true
          is-flat := true
          try:
            run(pdoc.d)
          except(_):
            indent := cur-indent
            curcol := cur-column
            output := cur-output
            is-flat := false
            in-group := false
            run(pdoc.d)
          end
          in-group := false
          is-flat := cur-flat
        else: run(pdoc.d)
        end
      else if is-column(pdoc): run(pdoc.func(curcol))
      else if is-nesting(pdoc): run(pdoc.func(indent))
      end
    end
    run(self)
    gen-output()
  end
end

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
fun break(n): ifFlat(blank(n), hardline) end
commabreak = str(",") + break(1)

fun flow_map(sep, f, items):
  for list.fold(acc from empty, item from items):
    if is-empty(acc): f(item)
    else: acc + group(sep + f(item))
    end
  end
end
fun flow(items): flow_map(break(1), fun(x): x end, items) end
fun vert(items): flow_map(hardline, fun(x): x end, items) end
fun parens(d): group(str("(") + d + str(")")) end
fun dquote(s): group(str("\"") + s + str("\"")) end

fun align(d):
  column(fun(col): nesting(fun(indent): nest(col - indent, d) end) end)
end

fun hang(i, d): align(nest(i, d)) end
fun prefix(n, b, x, y): group(x + nest(n, break(b) + y)) end
fun infix(n :: Number, b :: Number, op :: PPrintDoc, x :: PPrintDoc, y :: PPrintDoc):
  prefix(n, b, (x + blank(b) + op), y)
end
fun surround(n :: Number, b :: Number, open :: PPrintDoc, contents :: PPrintDoc, close :: PPrintDoc):
  if is-empty(close): group(open + nest(n, break(b) + contents))
  else: group(open + nest(n, break(b) + contents) + break(b) + close)
  end
end
fun soft-surround(n :: Number, b :: Number, open :: PPrintDoc, contents :: PPrintDoc, close :: PPrintDoc):
  if is-empty(close): group(open + nest(n, group(break(b) + contents)))
  else: group(open + nest(n, group(break(b) + contents)) + group(break(b) + close))
  end
end

fun separate(sep :: PPrintDoc, docs :: list.List):
  for list.fold(acc from empty, d from docs):
    if is-empty(d): acc
    else if is-empty(acc): d
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

# test-words = ["This", "is", "a", "sentence", "with", "eight", "words"].map(str)
# test = flow(test-words)
# print(tostring(test))
# print("")
# print("Width 40--------------------------------:")
# iter(print, test.pretty(40))
# print("")
# print("Width 30----------------------:")
# iter(print, test.pretty(30))
# print("")
# print("Width 20------------:")
# iter(print, test.pretty(20))
# print("")
# print("Width 10--:")
# iter(print, test.pretty(10))

