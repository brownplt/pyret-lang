provide {
    parse-pattern: parse-pattern,
    parse-ast: parse-ast,
    parse-ds-rules: parse-ds-rules,
    TOP-LOC: TOP-LOC
} end

import ast as AST
include either
include string-dict

include file("ds-structs.arr")

# Important! The parser must not backtrack too much, or else
# it will take exponential time, and the ellipsis counter will skip numbers.

################################################################################
#  Errors
#

fun parse-error(message :: String):
  raise({"Failed to parse sugar definitions file"; message})
end


################################################################################
#  Tokenization
#

WHITESPACE = [list: " ", "\t", "\n"]
SPECIAL-TOKENS = [list:
  "_{", ",", "|", ";", ":", "(", ")", "[", "]", "{", "}", "@", "=>",
  "<", ">", "...", "_"
]
MAGIC-LOC = "@"
TOP-LOC = "@toploc"

data Token:
  | t-str(tok :: String)
  | t-num(tok :: Number)
  | t-symbol(tok :: String)
  | t-name(tok :: String)
end

fun tokenize(input-str :: String) -> List<Token> block:
  var token :: String = ""
  var tokens :: List<Token> = [list:]
  var in-string :: Boolean = false
  var in-comment :: Boolean = false

  fun token-break():
    when token <> "" block:
      the-token =
        if string-char-at(token, 0) == '"':
          t-str(string-substring(token, 1, string-length(token) - 1))
        else if SPECIAL-TOKENS.member(token):
          t-symbol(token)
        else:
          cases (Option) string-to-number(token):
            | none      => t-name(token)
            | some(num) => t-num(num)
          end
        end
      tokens := link(the-token, tokens)
      token := ""
    end
  end

  var input = string-explode(input-str)

  # TODO: probably clean this up. Using external state is horrendously hideous
  var exact-result = ""

  fun take-safe(lst, len):
    if len == 0:
      empty
    else:
      cases (List) lst:
        | empty => empty
        | link(f, r) => link(f, take-safe(r, len - 1))
      end
    end
  end

  fun consumes-eof():
    input == empty
  end

  fun consumes-exact(s :: String):
    len = string-length(s)

    if take-safe(input, len).join-str("") == s block:
      exact-result := s
      input := input.drop(len)
      true
    else:
      false
    end
  end

  fun consumes-any(lst :: List<String>):
    lists.any(consumes-exact, lst)
  end

  fun consumes-one():
    cases (List) input block:
      | empty => raise("consumes-one from empty list")
      | link(f, r) =>
        input := r
        f
    end
  end

  fun loop():
    ask block:
      | consumes-eof() then: nothing
      | in-comment then:
        char = consumes-one()
        when char == "\n":
          in-comment := false
        end
        loop()
      | in-string then:
        char = consumes-one()
        when char == "\n":
          parse-error("Unterminated string.")
        end
        token := token + char
        when char == '"' block:
          token-break()
          in-string := false
        end
        loop()
      | consumes-any(WHITESPACE) then:
        token-break()
        loop()
      | consumes-any(SPECIAL-TOKENS) then:
        token-break()
        token := exact-result
        token-break()
        loop()
      | consumes-exact("#") then:
        token-break()
        in-comment := true
        loop()
      | consumes-exact('"') then:
        token-break()
        token := '"'
        in-string := true
        loop()
      | otherwise:
        token := token + consumes-one()
        loop()
    end
  end

  loop()

  when in-string:
    parse-error("Unterminated string.")
  end
  token-break()
  tokens.reverse()
end

check "tokenize":
  #init = time-now()
  #input = string-repeat("[(define-struct    name:Var\t fields:StructFields) @rest:SurfStmts]", 1000)
  #tokenize(input)
  #print(time-now() - init)
  shadow input = "[(define-struct    name:Var\t fields:StructFields ...l) @rest:SurfStmts]"
  tokenize(input)
    is [list: t-symbol("["), t-symbol("("), t-name("define-struct"), t-name("name"),
    t-symbol(":"), t-name("Var"), t-name("fields"), t-symbol(":"), t-name("StructFields"),
    t-symbol("..."), t-name("l"), t-symbol(")"), t-symbol("@"), t-name("rest"), t-symbol(":"),
    t-name("SurfStmts"), t-symbol("]")]
  shadow input = '{Lambda l 55/6(CONCAT "for-body \\n<" (\nFORMAT l false) ">")}'
  tokenize(input)
    is [list: t-symbol("{"), t-name("Lambda"), t-name("l"), t-num(55/6),
    t-symbol("("), t-name("CONCAT"), t-str("for-body \\n<"), t-symbol("("),
    t-name("FORMAT"), t-name("l"), t-name("false"), t-symbol(")"), t-str(">"),
    t-symbol(")"), t-symbol("}")]
end



################################################################################
#  Parsing
#

type Parser<A> = (List<Token> -> Option<{A; List<Token>}>)

fun run-parser<A>(parser :: (List<Token> -> Option<{A; List<Token>}>), input :: String) -> A:
  tokens = tokenize(input)
  cases (Option) parser(tokens):
    | none => parse-error("Failed to parse")
    | some({res; shadow tokens}) =>
      if tokens == empty:
        res
      else:
        parse-error("Expected end of file")
      end
  end
end

fun parser-empty(tokens :: List<Token>) -> Option<{Nothing; List<Token>}>:
  some({nothing; tokens})
end

fun parser-const<A>(expected :: Token, value :: A) -> (List<Token> -> Option<{A; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{A; List<Token>}>:
    cases (List) tokens:
      | link(token, rest) =>
        if token == expected:
          some({value; rest})
        else:
          none
        end
      | empty => none
    end
  end
end

fun parser-ignore(expected :: Token) -> (List<Token> -> Option<{Nothing; List<Token>}>):
  parser-const(expected, nothing)
end

fun parser-pred<A>(pred :: (Token -> Option<A>)) -> (List<Token> -> Option<{A; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{A; List<Token>}>:
    cases (List) tokens:
      | link(token, rest) =>
        cases (Option) pred(token):
          | none => none
          | some(a) => some({a; rest})
        end
      | empty => none
    end
  end
end

fun parser-choices<A>(choices :: List<(List<Token> -> Option<{A; List<Token>}>)>) -> (List<Token> -> Option<{A; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{A; List<Token>}>:
    cases (List) choices:
      | empty => none
      | link(choice, shadow choices) =>
        cases (Option) choice(tokens):
          | some(answer) => some(answer)
          | none => parser-choices(choices)(tokens)
        end
    end
  end
end

fun parser-chain<A, B>(
    f :: (A -> Parser<B>),
    parser :: Parser<A>)
  -> Parser<B>:
  lam(tokens :: List<Token>) -> Option<{B; List<Token>}>:
    cases (Option) parser(tokens):
      | none => none
      | some({a; shadow tokens}) => f(a)(tokens)
    end
  end
end

fun parser-1<A, B>(
    func :: (A -> B),
    parser :: (List<Token> -> Option<{A; List<Token>}>))
  -> (List<Token> -> Option<{B; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{B; List<Token>}>:
    cases (Option) parser(tokens):
      | none => none
      | some({a; shadow tokens}) =>
        some({func(a); tokens})
    end
  end
end

fun parser-2<A, B, C>(
    join :: (A, B -> C),
    first :: (List<Token> -> Option<{A; List<Token>}>),
    second :: (List<Token> -> Option<{B; List<Token>}>))
  -> (List<Token> -> Option<{C; List<Token>}>):
  for parser-chain(a from first):
    for parser-1(b from second):
      join(a, b)
    end
  end
end

fun parser-3<A, B, C, D>(
    join :: (A, B, C -> D),
    first :: (List<Token> -> Option<{A; List<Token>}>),
    second :: (List<Token> -> Option<{B; List<Token>}>),
    third :: (List<Token> -> Option<{C; List<Token>}>))
  -> (List<Token> -> Option<{D; List<Token>}>):
  for parser-chain(a from first):
    for parser-chain(b from second):
      for parser-1(c from third):
        join(a, b, c)
      end
    end
  end
end

fun parser-4<A, B, C, D, E>(
    join :: (A, B, C, D -> E),
    first :: (List<Token> -> Option<{A; List<Token>}>),
    second :: (List<Token> -> Option<{B; List<Token>}>),
    third :: (List<Token> -> Option<{C; List<Token>}>),
    fourth :: (List<Token> -> Option<{D; List<Token>}>))
  -> (List<Token> -> Option<{E; List<Token>}>):
  for parser-chain(a from first):
    for parser-chain(b from second):
      for parser-chain(c from third):
        for parser-1(d from fourth):
          join(a, b, c, d)
        end
      end
    end
  end
end

fun parser-5<A, B, C, D, E, F>(
    join :: (A, B, C, D, E -> F),
    first :: (List<Token> -> Option<{A; List<Token>}>),
    second :: (List<Token> -> Option<{B; List<Token>}>),
    third :: (List<Token> -> Option<{C; List<Token>}>),
    fourth :: (List<Token> -> Option<{D; List<Token>}>),
    fifth :: (List<Token> -> Option<{E; List<Token>}>))
  -> (List<Token> -> Option<{F; List<Token>}>):
  for parser-chain(a from first):
    for parser-chain(b from second):
      for parser-chain(c from third):
        for parser-chain(d from fourth):
          for parser-1(e from fifth):
            join(a, b, c, d, e)
          end
        end
      end
    end
  end
end

fun parser-seq<A>(parser :: (List<Token> -> Option<{A; List<Token>}>)) -> (List<Token> -> Option<{List<A>; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{A; List<Token>}>:
    cases (Option) parser(tokens):
      | none => some({empty; tokens})
      | some({res; shadow tokens}) =>
        cases (Option) parser-seq(parser)(tokens):
          | none => panic("parser-seq: recursive call should have succeeded")
          | some({lst; shadow tokens}) => some({link(res, lst); tokens})
        end
    end
  end
end

fun parser-left<A, B>(parser :: (List<Token> -> Option<{A; List<Token>}>)) -> (List<Token> -> Option<{Either<A, B>; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{Either<A, B>; List<Token>}>:
    cases (Option) parser(tokens):
      | none => none
      | some({res; shadow tokens}) =>  some({left(res); tokens})
    end
  end
end

fun parser-right<A, B>(parser :: (List<Token> -> Option<{B; List<Token>}>)) -> (List<Token> -> Option<{Either<A, B>; List<Token>}>):
  lam(tokens :: List<Token>) -> Option<{Either<A, B>; List<Token>}>:
    cases (Option) parser(tokens):
      | none => none
      | some({res; shadow tokens}) =>  some({right(res); tokens})
    end
  end
end

parser-name = parser-pred(lam(tok):
    cases (Token) tok:
      | t-name(name) => some(name)
      | else => none
    end
  end)

fun parser-fresh-item():
  rec-fresh-item = lam(toks): parser-fresh-item()(toks) end
  
  parser-choices([list:
      for parser-1(name from parser-name):
        fresh-name(name)
      end,
      for parser-5(
          _ from parser-ignore(t-symbol("[")),
          item from rec-fresh-item,
          _ from parser-ignore(t-symbol("...")),
          label from parser-name,
          _ from parser-ignore(t-symbol("]"))):
        fresh-ellipsis(item, label)
      end
    ])
end

parser-fresh-item-list =
  for parser-3(
      _ from parser-ignore(t-symbol("[")),
      items from parser-seq(parser-fresh-item()),
      _ from parser-ignore(t-symbol("]"))):
    items
  end

fun parser-pattern() -> (List<Token> -> Option<{Pattern; List<Token>}>):
  
  fun parser-var(vars :: Set<String>):
    parser-pred(lam(tok):
        cases (Token) tok:
          | t-name(name) =>
            if vars.member(name):
              some(p-var(name))
            else:
              none
            end
          | else => none
        end
      end)
  end

  parser-drop = parser-choices([list:
      for parser-3(
          _ from parser-ignore(t-symbol("_")),
          _ from parser-ignore(t-symbol(":")),
          ty from parser-name):
        p-drop(some(ty))
      end,
      parser-const(t-symbol("_"), p-drop(none))
    ])
  
  parser-pvar-name-and-label = parser-choices([list:
      for parser-4(
        name from parser-name,
        _ from parser-ignore(t-symbol("_{")),
        labels from parser-seq(parser-name),
        _ from parser-ignore(t-symbol("}"))
      ):
        {name; list-to-set(labels)}
      end,
      for parser-1(name from parser-name):
        {name; [set: ]}
      end
    ])

  parser-pvar = parser-choices([list:
      for parser-3(
          {pvar; labels} from parser-pvar-name-and-label,
          _ from parser-ignore(t-symbol(":")),
          typ from parser-name):
        p-pvar(pvar, labels, some(typ))
      end,
      for parser-1({pvar; labels} from parser-pvar-name-and-label):
        p-pvar(pvar, labels, none)
      end
    ])

  fun get-ploc(maybe-loc :: Option<Pattern>) -> Pattern:
    cases (Option) maybe-loc:
      | none          => p-pvar("@", [set:], none)
      | some(loc-pat) => loc-pat
    end
  end

  fun parser-patt(vars :: Set<String>):
    fun rec-pattern(toks): parser-patt(vars)(toks) end
    fun rec-list(toks): parser-list-body(vars)(toks) end

    parser-name-and-opt-loc = parser-choices([list:
        for parser-3(
            name from parser-name,
            _ from parser-ignore(t-symbol("@")),
            loc from rec-pattern):
          {name; some(loc)}
        end,
        for parser-1(name from parser-name):
          {name; none}
        end
      ])
      
    parser-choices([list:
        parser-const(t-name("none"), p-option(none)),
        parser-const(t-name("true"), p-prim(e-bool(true))),
        parser-const(t-name("false"), p-prim(e-bool(false))),
        # Number
        parser-pred(lam(tok):
            cases (Token) tok:
              | t-num(n) => some(p-prim(e-num(n)))
              | else => none
            end
          end),
        # String
        parser-pred(lam(tok): # string
            cases (Token) tok:
              | t-str(s) => some(p-prim(e-str(s)))
              | else => none
            end
          end),
        # Variable
        parser-var(vars),
        # Drop
        parser-drop,
        # Pattern var
        parser-pvar,
        # Some
        for parser-4(
            _ from parser-ignore(t-symbol("{")),
            _ from parser-ignore(t-name("some")),
            arg from rec-pattern,
            _ from parser-ignore(t-symbol("}"))):
          p-option(some(arg))
        end,
        # Fresh
        for parser-chain(_ from parser-ignore(t-symbol("("))):
          for parser-chain(_ from parser-ignore(t-name("fresh"))):
            for parser-chain(items from parser-fresh-item-list):
              shadow vars = vars.union(list-to-set(map(get-fresh-item-name, items)))
              for parser-chain(body from parser-patt(vars)):
                for parser-1(_ from parser-ignore(t-symbol(")"))):
                  p-fresh(items, body)
                end
              end
            end
          end
        end,
        # Capture
        for parser-chain(_ from parser-ignore(t-symbol("("))):
          for parser-chain(_ from parser-ignore(t-name("capture"))):
            for parser-chain(items from parser-fresh-item-list):
              shadow vars = vars.union(list-to-set(map(get-fresh-item-name, items)))
              for parser-chain(body from parser-patt(vars)):
                for parser-1(_ from parser-ignore(t-symbol(")"))):
                  p-capture(items, body)
                end
              end
            end
          end
        end,
        # Meta
        for parser-5(
            _ from parser-ignore(t-symbol("(")),
            _ from parser-ignore(t-name("meta")),
            name from parser-name,
            args from parser-seq(rec-pattern),
            _ from parser-ignore(t-symbol(")"))):
          p-meta(name, args)
        end,
        # Bijection
        for parser-5(
            _ from parser-ignore(t-symbol("(")),
            _ from parser-ignore(t-name("biject")),
            name from parser-name,
            p from rec-pattern,
            _ from parser-ignore(t-symbol(")"))):
          p-biject(name, p)
        end,
        # Aux
        for parser-4(
            _ from parser-ignore(t-symbol("{")),
            name from parser-name,
            args from parser-seq(rec-pattern),
            _ from parser-ignore(t-symbol("}"))):
          p-aux(name, args)
        end,
        # Core (with implicit loc)
        for parser-4(
            _ from parser-ignore(t-symbol("<")),
            {name; maybe-loc} from parser-name-and-opt-loc,
            args from parser-seq(rec-pattern),
            _ from parser-ignore(t-symbol(">"))):
          p-core(name, link(get-ploc(maybe-loc), args))
        end,
        # Surface (with implicit loc)
        for parser-4(
            _ from parser-ignore(t-symbol("(")),
            {name; maybe-loc} from parser-name-and-opt-loc,
            args from parser-seq(rec-pattern),
            _ from parser-ignore(t-symbol(")"))):
          p-surf(name, link(get-ploc(maybe-loc), args))
        end,
        # List
        for parser-3(
            _ from parser-ignore(t-symbol("[")),
            body from rec-list,
            _ from parser-ignore(t-symbol("]"))):
          p-list(body)
        end
      ])
  end

  fun parser-list-body(vars :: Set<String>):
    fun rec-pattern(toks): parser-patt(vars)(toks) end
    fun rec-list(toks): parser-list-body(vars)(toks) end
    
    parser-choices([list:
        # Ellipsis
        for parser-2(
            patt from rec-pattern,
            either-ellipsis-or-cons :: Either<Nothing, SeqPattern> from parser-choices([list: 
                for parser-2(
                  _ from parser-ignore(t-symbol("...")),
                  label from parser-left(parser-name)
                ):
                  label
                end,
                parser-right(rec-list)
              ])):
          cases (Either) either-ellipsis-or-cons:
            | left(label) => seq-ellipsis(patt, label)
            | right(body) => seq-cons(patt, body)
          end
        end,
        for parser-1(_ from parser-empty):
          seq-empty
        end
      ])
  end
  
  parser-patt([set:])
end

fun parse-pattern(input :: String) -> Pattern:
  run-parser(parser-pattern(), input)
end

fun parse-ast(input :: String) -> Term:
  pattern = parse-pattern(input)
  fun args-to-asts(shadow patterns :: List<Pattern>) -> List<Term>:
    link(term-dummy-loc, patterns.rest.map(pattern-to-ast))
  end
  fun pattern-to-ast(shadow pattern :: Pattern) -> Term:
    cases (Pattern) pattern:
      | p-prim(v) => g-prim(v)
      | p-pvar(name, _, _) => g-var(naked-var(name)) # Justin's fault
      | p-var(v) => panic("parse-ast - unexpected var: " + tostring(v))
      | p-core(name, args) => g-core(name, args-to-asts(args))
      | p-aux(name,  args) => g-aux(name,  args-to-asts(args))
      | p-surf(name, args) => g-surf(name, args-to-asts(args))
      | p-meta(name, args) => panic("parse-ast: unexpected meta")
      | p-list(seq) => g-list(list-to-ast(seq))
      | p-option(opt) => 
        cases (Option) opt:
          | none => none
          | some(p) => some(pattern-to-ast(p))
        end ^ g-option
      | p-tag(lhs, rhs, body) => g-tag(lhs, rhs, pattern-to-ast(body))
      | p-fresh(_, _) => panic("parse-ast: unexpected fresh")
      | p-capture(_, _) => panic("parse-ast: unexpected capture")
    end
  end
  fun list-to-ast(seq :: SeqPattern) -> List<Term>:
    cases (SeqPattern) seq:
      | seq-ellipsis(_, _) => panic("Unexpected `...` in Term")
      | seq-empty => empty
      | seq-cons(p, shadow seq) => link(pattern-to-ast(p), list-to-ast(seq))
      | seq-ellipsis-list(_, _) =>
        panic("Unexpected ellipsis list in Term")
    end
  end
  pattern-to-ast(pattern)
end

parser-ds-rule-case =
  for parser-chain(_ from parser-ignore(t-symbol("|"))):
    for parser-3(
        lhs from parser-pattern(),
        _ from parser-ignore(t-symbol("=>")),
        rhs from parser-pattern()):
      shadow lhs = rename-p-pvar(lhs, lam(loc, labels, typ):
          if loc == MAGIC-LOC: p-drop(typ) else: p-pvar(loc, labels, typ) end
        end)
      {shadow lhs; toploc-name} = cases (Pattern) lhs:
        | p-surf(op, args) =>
          cases (Pattern) args.get(0):
            | p-drop(_) => {p-surf(op, link(p-pvar(TOP-LOC, [set:], none), args.rest)); TOP-LOC}
            | p-pvar(loc-name, _, _) => {lhs; loc-name}
            | else => panic("First argument of LHS surface is not p-pvar: " + tostring(lhs))
          end
        | else => fail("LHS should be surface pattern")
      end
      shadow rhs = rename-p-pvar(rhs, lam(loc, labels, typ):
          pvar = if loc == MAGIC-LOC: toploc-name else: loc end
          p-pvar(pvar, labels, typ)
        end)
      ds-rule-case(lhs, rhs)
    end
  end

parser-ds-rule =
  for parser-5(
      _ from parser-ignore(t-name("sugar")),
      op from parser-name,
      _ from parser-ignore(t-symbol(":")),
      kases from parser-seq(parser-ds-rule-case),
      _ from parser-ignore(t-name("end"))):
    {op; kases}
  end

parser-ds-rules = parser-seq(parser-ds-rule)

fun parse-ds-rules(input :: String) -> DsRules:
  for fold(acc from [string-dict: ],
           {op; kases} from run-parser(parser-ds-rules, input)):
    acc.set(op, kases)
  end
end
