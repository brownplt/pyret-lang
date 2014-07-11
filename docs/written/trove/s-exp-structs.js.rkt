
#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(define S-Exp (a-id "S-Exp" (xref "s-exp-structs" "S-Exp")))
@docmodule["s-exp-structs"]{
  @section[#:tag "S-Exp_DataTypes"]{The S-Exp Datatype}
  @ignore[(list "is-s-list" "is-s-num" "is-s-str" "is-s-sym")]
  @data-spec["S-Exp"]{
  @para{
    This datatype defines the result of parsing s-expressions.  See
    @a-id["read-s-exp" @xref["s-exp" "read-s-exp"]] for more details and examples.
  }
    @verbatim{
  data S-Exp:
    | s-list(exps :: List<S-Exp>)
    | s-num(n :: Number)
    | s-str(s :: String)
    | s-sym(s :: String)
  end
    }
    @constr-spec["s-list"]{
      @members{@member-spec["exps" #:contract (L-of S-Exp)]}
    }
    @constr-spec["s-num"]{
      @members{@member-spec["n"]}
    }
    @constr-spec["s-str"]{
      @members{@member-spec["s"]}
    }
    @constr-spec["s-sym"]{
      @members{@member-spec["s"]}
    }
  }
}
