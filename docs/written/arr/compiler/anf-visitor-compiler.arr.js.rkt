#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/anf-visitor-compiler.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-concat-empty"
      "is-concat-singleton"
      "is-concat-append"
      "is-concat-cons"
      "is-concat-snoc")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "get-field-loc"
      "throw-uninitialized"
      "source-name"
      "undefined"
      "compiler-visitor"
      "remove-useless-if-visitor")
  ]
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_ReExports"]{
    Re-exported values
  }
  @re-export["j-fun" (from (xref "\"compiler/js-ast.arr\"" "j-fun"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-fun"]}
  }
  @re-export["j-var" (from (xref "\"compiler/js-ast.arr\"" "j-var"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-var"]}
  }
  @re-export["j-id" (from (xref "\"compiler/js-ast.arr\"" "j-id"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-id"]}
  }
  @re-export["j-method" (from (xref "\"compiler/js-ast.arr\"" "j-method"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-method"]}
  }
  @re-export["j-block" (from (xref "\"compiler/js-ast.arr\"" "j-block"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-block"]}
  }
  @re-export["j-true" (from (xref "\"compiler/js-ast.arr\"" "j-true"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-true"]}
  }
  @re-export["j-false" (from (xref "\"compiler/js-ast.arr\"" "j-false"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-false"]}
  }
  @re-export["j-num" (from (xref "\"compiler/js-ast.arr\"" "j-num"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-num"]}
  }
  @re-export["j-str" (from (xref "\"compiler/js-ast.arr\"" "j-str"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-str"]}
  }
  @re-export["j-return" (from (xref "\"compiler/js-ast.arr\"" "j-return"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-return"]}
  }
  @re-export["j-assign" (from (xref "\"compiler/js-ast.arr\"" "j-assign"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-assign"]}
  }
  @re-export["j-if" (from (xref "\"compiler/js-ast.arr\"" "j-if"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-if"]}
  }
  @re-export["j-if1" (from (xref "\"compiler/js-ast.arr\"" "j-if1"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-if1"]}
  }
  @re-export["j-app" (from (xref "\"compiler/js-ast.arr\"" "j-app"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-app"]}
  }
  @re-export["j-list" (from (xref "\"compiler/js-ast.arr\"" "j-list"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-list"]}
  }
  @re-export["j-obj" (from (xref "\"compiler/js-ast.arr\"" "j-obj"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-obj"]}
  }
  @re-export["j-dot" (from (xref "\"compiler/js-ast.arr\"" "j-dot"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-dot"]}
  }
  @re-export["j-bracket" (from (xref "\"compiler/js-ast.arr\"" "j-bracket"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-bracket"]}
  }
  @re-export["j-field" (from (xref "\"compiler/js-ast.arr\"" "j-field"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-field"]}
  }
  @re-export[
    "j-dot-assign"
    (from (xref "\"compiler/js-ast.arr\"" "j-dot-assign"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-dot-assign"]}
  }
  @re-export[
    "j-bracket-assign"
    (from (xref "\"compiler/js-ast.arr\"" "j-bracket-assign"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-bracket-assign"]}
  }
  @re-export[
    "j-try-catch"
    (from (xref "\"compiler/js-ast.arr\"" "j-try-catch"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-try-catch"]}
  }
  @re-export["j-throw" (from (xref "\"compiler/js-ast.arr\"" "j-throw"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-throw"]}
  }
  @re-export["j-expr" (from (xref "\"compiler/js-ast.arr\"" "j-expr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-expr"]}
  }
  @re-export["j-binop" (from (xref "\"compiler/js-ast.arr\"" "j-binop"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-binop"]}
  }
  @re-export["j-eq" (from (xref "\"compiler/js-ast.arr\"" "j-eq"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-eq"]}
  }
  @re-export["j-neq" (from (xref "\"compiler/js-ast.arr\"" "j-neq"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-neq"]}
  }
  @re-export["j-unop" (from (xref "\"compiler/js-ast.arr\"" "j-unop"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-unop"]}
  }
  @re-export["j-decr" (from (xref "\"compiler/js-ast.arr\"" "j-decr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-decr"]}
  }
  @re-export["j-incr" (from (xref "\"compiler/js-ast.arr\"" "j-incr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-incr"]}
  }
  @re-export["j-ternary" (from (xref "\"compiler/js-ast.arr\"" "j-ternary"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-ternary"]}
  }
  @re-export["j-null" (from (xref "\"compiler/js-ast.arr\"" "j-null"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-null"]}
  }
  @re-export["j-parens" (from (xref "\"compiler/js-ast.arr\"" "j-parens"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-parens"]}
  }
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_DataTypes"]{Data types}
  @data-spec["ConcatList" #:params (list "a")]{
    @variants{
      @singleton-spec["concat-empty"]{
        @with-members{
          @method-spec[
            "to-list-acc"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
        }
      }
      @constr-spec["concat-singleton"]{
        @members{@member-spec["element"]}
        @with-members{
          @method-spec[
            "to-list-acc"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
        }
      }
      @constr-spec["concat-append"]{
        @members{@member-spec["left"] @member-spec["right"]}
        @with-members{
          @method-spec[
            "to-list-acc"
            ;; N.B. Pyret contract: (ConcatList, List27 -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
        }
      }
      @constr-spec["concat-cons"]{
        @members{@member-spec["first"] @member-spec["rest"]}
        @with-members{
          @method-spec[
            "to-list-acc"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
        }
      }
      @constr-spec["concat-snoc"]{
        @members{@member-spec["head"] @member-spec["last"]}
        @with-members{
          @method-spec[
            "to-list-acc"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (ConcatList, Any -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_plus"
        ;; N.B. Pyret contract: (ConcatList, ConcatList76 -> Any)
        
      ]
      @method-spec[
        "to-list"
        ;; N.B. Pyret contract: (ConcatList -> Any)
        
      ]
    }
  }
  
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_Functions"]{Functions}
  @function["type-name"]
  @function["js-id-of"]
  @function["compiler-name"]
  @function["obj-of-loc"]
  @function["get-field"]
  @function["raise-id-exn"]
  @function["add-stack-frame"]
  @function["rt-field"]
  @function["rt-method"]
  @function["app"]
  @function["thunk-app"]
  @function["thunk-app-stmt"]
  @function["helper-name"]
  @function["compile-helper"]
  @function["compile-tail-app"]
  @function["compile-split-app"]
  @function["compile-ann"]
  @function["arity-check"]
  @function["contract-checks"]
  @function["mk-abbrevs"]
  @function["compile-program"]
  @function["splitting-compiler"]
  @function["non-splitting-compiler"]
}