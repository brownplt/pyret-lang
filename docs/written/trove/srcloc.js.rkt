#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["srcloc"]{
  @; Ignored type testers
  @ignore[(list "is-builtin" "is-srcloc")]
  @section[#:tag "srcloc_DataTypes"]{Data types}
  @data-spec["Srcloc"]{
    @variants{
      @constr-spec["builtin"]{
        @members{@member-spec["module-name"]}
        @with-members{
          @method-spec[
            "format"
            ;; N.B. Pyret contract: (Srcloc, Any -> Any)
            
          ]
          @method-spec[
            "same-file"
            ;; N.B. Pyret contract: (Srcloc, Any -> Any)
            
          ]
          @method-spec[
            "before"
            ;; N.B. Pyret contract: (Srcloc, Any -> Any)
            
          ]
        }
      }
      @constr-spec["srcloc"]{
        @members{
          @member-spec["source"]
          @member-spec["start-line"]
          @member-spec["start-column"]
          @member-spec["start-char"]
          @member-spec["end-line"]
          @member-spec["end-column"]
          @member-spec["end-char"]
        }
        @with-members{
          @method-spec[
            "format"
            ;; N.B. Pyret contract: (Srcloc, Any -> Any)
            
          ]
          @method-spec[
            "same-file"
            ;; N.B. Pyret contract: (Srcloc, Srcloc60 -> Any)
            
          ]
          @method-spec[
            "before"
            ;; N.B. Pyret contract: (Srcloc, Srcloc60 -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_output"
        ;; N.B. Pyret contract: (Srcloc -> Any)
        
      ]
      @method-spec[
        "after"
        ;; N.B. Pyret contract: (Srcloc, Any -> Any)
        
      ]
    }
  }
  
  @section[#:tag "srcloc_Functions"]{Functions}
}
