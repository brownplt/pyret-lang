provide *

import srcloc as SL
import global as _
import base as _
import file("lambda-ast.arr") as A

#|
   Grammar for Lambda Calculus:

   Term ::= <Var>
          | '(' ('lambda' | 'λ') '(' [<Var> ...] ')' <Term> ')'
          | '(' <Term> <Term> ')'

   Var ::= IDENTIFIER
|#

data TokenType:
  | tt-lparen
  | tt-rparen
  | tt-lambda
  | tt-identifier
end

data TokenizerState:
  | t-state(ref stack, source-name, ref start-line, ref start-col, ref start-char, ref end-line, ref end-col, ref end-char) with:
    method push(self, elt):
      self!{stack: link(elt, self!stack)}
    end,
    method dump(self) block:
      ret = if is-empty(self!stack) :
        none
      else:
        some(self!stack.reverse().join-str(""))
      end
      self!{stack: [list: ]}
      ret
    end,
    method inc-start-line(self):
    self!{start-line: self!start-line + 1, end-line: self!start-line + 2}
    end,
    method reset-start-col(self):
      self!{start-col: 0, end-col: 1}
    end,
    method inc-start-col(self):
      self!{start-col: self!start-col + 1, end-col: self!start-col + 2}
    end,
    method inc-start-char(self):
      self!{start-char: self!start-char + 1, end-char: self!start-char + 2}
    end,
    method inc-end-line(self):
      self!{end-line: self!end-line + 1}
    end,
    method reset-end-col(self):
      self!{end-col: 0}
    end,
    method inc-end-col(self):
      self!{end-col: self!end-col + 1}
    end,
    method inc-end-char(self):
      self!{end-char: self!end-char + 1}
    end,
    method skip-char(self, is-newline):
      if is-newline:
        self!{start-char: self!start-char + 1, start-line: self!start-line + 1, start-col: 0,
          end-char: self!start-char + 1, end-line: self!start-line + 1, end-col: 0}
      else:
        self!{start-char: self!start-char + 1, start-col: self!start-col + 1,
          end-char: self!start-char + 1, end-col: self!start-col + 1}
      end
    end,
    method rejoin-region(self):
      self!{start-char: self!end-char, start-line: self!end-line, start-col: self!end-col}
    end,
    method get-srcloc(self):
      SL.srcloc(self.source-name, self!start-line, self!start-col, self!start-char, self!end-line, self!end-col, self!end-char)
    end,
    method pop-token(self) block:
      sl = self.get-srcloc()
      self.rejoin-region()
      self.dump().and-then(lam(v):
          if string-equal(v, "lambda") or string-equal(v, "λ"):
            token(tt-lambda, v, sl)
          else:
            token(tt-identifier, v, sl)
          end
        end)
    end
end

data Token:
  | token(t :: TokenType, s :: String, loc :: SL.Srcloc) with:
    method format(self):
      "[Token<" + torepr(self.t) + ">\"" + self.s + "\"@" + self.loc.format(true) + "]"
    end
end

fun tokenize(src, s :: String):
  doc: "A hacked together s-exp tokenizer with srclocs."
  fun pop-and-add(state, acc):
    cases(Option) state.pop-token():
      | none => acc
      | some(tok) => link(tok, acc)
    end
  end
  fun get-tokens(state, chars, acc):
    cases(List) chars:
      | link(char, rest) =>
        if string-equal(char, "(") block:
          new-acc = pop-and-add(state, acc)
          state.inc-end-char()
          state.inc-end-col()
          sl = state.get-srcloc()
          state.rejoin-region()
          get-tokens(state, rest, link(token(tt-lparen, "(", sl), new-acc))
        else if string-equal(char, ")"):
          new-acc = pop-and-add(state, acc)
          state.inc-end-char()
          state.inc-end-col()
          sl = state.get-srcloc()
          state.rejoin-region()
          get-tokens(state, rest, link(token(tt-rparen, ")", sl), new-acc))
        else if string-equal(char, " ") or string-equal(char, "\t") or string-equal(char, "\r"):
          new-acc = pop-and-add(state, acc)
          state.skip-char(false)
          state.rejoin-region()
          get-tokens(state, rest, new-acc)
        else if string-equal(char, "\n"):
          new-acc = pop-and-add(state, acc)
          state.skip-char(true)
          get-tokens(state, rest, new-acc)
        else:
          state.push(char)
          state.inc-end-col()
          state.inc-end-char()
          get-tokens(state, rest, acc)
        end
      | empty => pop-and-add(state, acc).reverse()
    end
  end

  init-state = t-state([list: ], src, 1, 0, 0, 1, 0, 0)
  get-tokens(init-state, string-explode(s), [list: ])
end

data TokenStack:
  | t-stack(ref contents) with:
    method pop(self):
      cases(List) self!contents block:
        | link(hd, tl) =>
          self!{contents: tl}
          some(hd)
        | empty =>
          none
      end
    end,
    method push(self, tok):
      self!{contents: link(tok, self!contents)}
    end,
    method pop-expected(self, expected):
      cases(Option) self.pop():
        | some(tok) => tok
        | none => raise("Expected " + expected + ", but reached end of file.")
      end
    end,
    method pop-expected-t(self, expected, typ):
      tok = self.pop-expected(expected)
      if typ <> tok.t:
        raise("Expected " + expected + ", but got \""
            + tok.s + "\" at " + tok.loc.format(true))
      else:
        tok
      end
    end
end

fun parse(toks):
  fun expected-but-got(expected, tok):
    raise("Expected " + expected + ", but got \"" + tok.s
        + "\" at " + tok.loc.format(true))
  end
  fun expected-but-eof(expected):
    raise("Expected " + expected + ", but reached end of file")
  end
  fun unexpected(tok):
    raise("Unexpected \"" + tok.s + "\" at " + tok.loc.format(true))
  end
  fun parse-var(tok-stack):
    cases(Option) tok-stack.pop():
      | some(tok) =>
        if not(is-tt-identifier(tok.t)):
          expected-but-got("identifier", tok)
        else:
          A.lc-var-inner(tok.loc, tok.s)
        end
      | none =>
        expected-but-eof("identifier")
    end
  end
  fun parse-term-and-rparen(tok-stack):
    term = parse-term(tok-stack)
    cases(Option) tok-stack.pop():
      | some(tok) =>
        if not(is-tt-rparen(tok.t)):
          expected-but-got("closing parentheses", tok)
        else:
          {term.loc + tok.loc; term}
        end
      | none =>
        expected-but-eof("closing parentheses")
    end
  end
  fun parse-fun-args(tok-stack):
    lparen = tok-stack.pop-expected-t("opening parentheses", tt-lparen)
    var args = [list:]
    fun loop():
      next-tok = tok-stack.pop-expected("identifier or closing parentheses")
      cases(TokenType) next-tok.t block:
        | tt-rparen =>
          if is-empty(args):
            expected-but-got("identifier", next-tok)
          else:
            {lparen.loc + next-tok.loc; args}
          end
        | tt-identifier =>
          args := link(A.lc-var-inner(next-tok.loc, next-tok.s), args)
          loop()
        | else => expected-but-got("identifier or closing parentheses")
      end
    end
    loop()
  end
  fun parse-term(tok-stack):
    cases(Option) tok-stack.pop():
      | some(tok) =>
        cases(TokenType) tok.t:
          | tt-rparen => unexpected(tok)
          | tt-lambda => unexpected(tok)
          | tt-lparen =>
            first-inner-opt = tok-stack.pop()
            cases(Option) first-inner-opt:
              | some(first-inner) =>
                cases(TokenType) first-inner.t:
                  | tt-lambda =>
                    {param-loc; params} = parse-fun-args(tok-stack)
                    body = parse-term(tok-stack)
                    rparen = tok-stack.pop-expected-t("closing parentheses", tt-rparen)
                    A.lc-abs(tok.loc + rparen.loc, params, body)
                  | else =>
                    first-term = parse-term(tok-stack.push(first-inner))
                    second-term = parse-term(tok-stack)
                    A.lc-app(first-term.loc + second-term.loc, first-term, second-term)
                end
              | none => expected-but-eof("lambda or term")
            end
          | tt-identifier =>
            inner =  A.lc-var-inner(tok.loc, tok.s)
            A.lc-var(inner.loc, inner)
        end
      | none =>
        expected-but-eof("term")
    end
  end
  parse-term(t-stack(toks))
end
