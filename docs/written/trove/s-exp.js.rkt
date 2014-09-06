#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(define (sref s)
  (a-id s (xref "s-exp-structs" s)))
@docmodule["s-exp"]{
@ignore[(list "s-sym" "s-str" "s-num" "s-list")]
  @para{
  This module re-exports the constructors from @sref["S-Exp"],
  which defines the result of parsing an s-exp.
  }

  @function["read-s-exp" #:args '(("sexp-str" ""))
    #:contract (a-arrow (a-id "String" (xref "<global>" "String"))
                        (a-id "S-Exp" (xref "s-exp-structs" "S-Exp")))]{
    Reads an @emph{s-expression} as a string, and returns it as a Pyret value.

    An s-expression is a string that satisfies the following grammar:

    @verbatim{
s-exp = "(" s-exp ... ")"
      | <number>
      | <string>
      | <id>
    }

    The first form parses to a @sref["s-list"] containing the nested
    sub-expression results.  Numbers become @sref["s-num"]s, strings become
    @sref["s-str"]s, and all other names not inside quotes
    become @sref["s-sym"]s.

@examples{
import s-exp as S

p = S.read-s-exp
s-list = S.s-list
s-num = S.s-num
s-str = S.s-str
s-sym = S.s-sym

check:
  p("()") is s-list(empty)
  p("(5)") is s-list([list: s-num(5)])
  p("(5 4)") is s-list([list: s-num(5), s-num(4)])
  p("(a 5)") is s-list([list: s-sym("a"), s-num(5)])
  p("a") is s-sym("a")
  p("\"a\"") is s-str("a")
  p("(a (b c))") is
    s-list([list:
      s-sym("a"),
      s-list([list: s-sym("b"), s-sym("c")])
    ])
  p("(\"a\" (5 (4) ()) \"b\")") is
    s-list([list:
      s-str("a"),
      s-list([list:
        s-num(5),
        s-list([list: s-num(4)]),
        s-list(empty)
      ]),
      s-str("b")
    ])

  p("-5") is s-num(-5)
  p("-4.4") is s-num(-4.4)
  p("-3.") is s-num(-3.0)
  # Make sure bignums parse correctly
  p(num-tostring(num-expt(100, 100))) is
    s-num(num-expt(100, 100))
  p("-abc3.3") is s-sym("-abc3.3")

  p("())") raises "Invalid"
  p("('a' 5)") raises "'quote'"
  p("(a") raises "Invalid"
  p(")") raises "Invalid"
  p("('a)") raises "Invalid"
  p("(()") raises "Invalid"
  p("(a')") raises "Invalid"

end
}
  }
}
