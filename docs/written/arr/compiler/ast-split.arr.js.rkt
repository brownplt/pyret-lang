#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-split.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-concat-empty"
      "is-concat-singleton"
      "is-concat-append"
      "is-helper"
      "is-split-result"
      "is-split-result-int-e"
      "is-split-result-int-l")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "INDENT")]
  @section[#:tag "\"compiler/ast-split.arr\"_ReExports"]{Re-exported values}
  @re-export["names" (from (xref "ast" "global-names"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["ast" "global-names"]}
  }
  @section[#:tag "\"compiler/ast-split.arr\"_DataTypes"]{Data types}
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
            ;; N.B. Pyret contract: (ConcatList, List17 -> Any)
            
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
        ;; N.B. Pyret contract: (ConcatList, ConcatList66 -> Any)
        
      ]
      @method-spec[
        "to-list"
        ;; N.B. Pyret contract: (ConcatList -> Any)
        
      ]
    }
  }
  
  @data-spec["Helper"]{
    @variants{
      @constr-spec["helper"]{
        @members{@member-spec["name"] @member-spec["args"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Helper -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["SplitResult"]{
    @variants{
      @constr-spec["split-result"]{
        @members{
          @member-spec["helpers"]
          @member-spec["body"]
          @member-spec["freevars"]
        }
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (SplitResult -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["SplitResultInt"]{
    @variants{
      @constr-spec["split-result-int-e"]{
        @members{
          @member-spec["helpers"]
          @member-spec["body"]
          @member-spec["freevars"]
        }
        @with-members{}
      }
      @constr-spec["split-result-int-l"]{
        @members{
          @member-spec["helpers"]
          @member-spec["body"]
          @member-spec["freevars"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/ast-split.arr\"_Functions"]{Functions}
  @function["freevars-helper"]
  @function["freevars-split-result"]
  @function["unions" #:params (list "a")]
  @function["ast-split"]
  @function["ast-split-expr"]
  @function["ast-split-lettable"]
  @function["param"]
}