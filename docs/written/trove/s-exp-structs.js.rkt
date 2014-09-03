
#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(define S-Exp (a-id "S-Exp" (xref "s-exp-structs" "S-Exp")))

@(define l-of-S-Exp (a-app (a-id "List" (xref "lists" "List")) (a-id "S-Exp" (xref "s-exp-structs" "S-Exp"))))
@(define boolean (a-id "Boolean" (xref "<global>" "Boolean")))
@(define number (a-id "Number" (xref "<global>" "Number")))
@(define string (a-id "String" (xref "<global>" "String")))

@(define s-list-args (list `("exps" ("type" "normal") ("contract" ,l-of-S-Exp))))
@(define s-num-args (list `("n" ("type" "normal") ("contract" ,number))))
@(define s-str-args (list `("s" ("type" "normal") ("contract" ,string))))
@(define s-sym-args (list `("s" ("type" "normal") ("contract" ,string))))

@docmodule["s-exp-structs"]{
  @section[#:tag "S-Exp_DataTypes"]{The S-Exp Datatype}
  @ignore[(list "is-s-list" "is-s-num" "is-s-str" "is-s-sym")]
  @para{
    This datatype defines the result of parsing s-expressions.  See
    @a-id["read-s-exp" @xref["s-exp" "read-s-exp"]] for more details and examples.
  }

  @data-spec2["S-Exp" (list) (list
    @constructor-spec["S-Exp" "s-list" s-list-args]
    @constructor-spec["S-Exp" "s-num" s-num-args]
    @constructor-spec["S-Exp" "s-str" s-str-args]
    @constructor-spec["S-Exp" "s-sym" s-sym-args]
  )]

  @nested[#:style 'inset]{
  @constructor-doc["S-Exp" "s-list" s-list-args S-Exp]
  @constructor-doc["S-Exp" "s-num" s-num-args S-Exp]
  @constructor-doc["S-Exp" "s-str" s-str-args S-Exp]
  @constructor-doc["S-Exp" "s-sym" s-sym-args S-Exp]

  @function["is-s-list" #:alt-docstrings ""]
  @function["is-s-num" #:alt-docstrings ""]
  @function["is-s-str" #:alt-docstrings ""]
  @function["is-s-sym" #:alt-docstrings ""]
  }
}
