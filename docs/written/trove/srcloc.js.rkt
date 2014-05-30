#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["srcloc"]{
  @section[#:tag "srcloc_ReExports"]{Re-exported values}
  @section[#:tag "srcloc_DataTypes"]{Data types}
  @data-spec["Srcloc"]{
    @variants{
      @constr-spec["builtin"]{
        @members{@member-spec["module-name" #:contract "Any"]}
        @with-members{
          @method-spec[
            "format"
            #:contract
            (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any")
          ]
          @method-spec[
            "same-file"
            #:contract
            (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any")
          ]
          @method-spec[
            "before"
            #:contract
            (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any")
          ]
        }
      }
      @constr-spec["srcloc"]{
        @members{
          @member-spec[
            "source"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "start-line"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "start-column"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "start-char"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "end-line"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "end-column"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "end-char"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{
          @method-spec[
            "format"
            #:contract
            (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any")
          ]
          @method-spec[
            "same-file"
            #:contract
            (a-arrow
              (a-id "Srcloc" (xref "srcloc" "Srcloc"))
              (a-id "Srcloc" (xref "srcloc" "Srcloc"))
              "Any")
          ]
          @method-spec[
            "before"
            #:contract
            (a-arrow
              (a-id "Srcloc" (xref "srcloc" "Srcloc"))
              (a-id "Srcloc" (xref "srcloc" "Srcloc"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "tostring"
        #:contract (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any")
      ]
      @method-spec[
        "after"
        #:contract
        (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any")
      ]
    }
  }
  @section[#:tag "srcloc_Functions"]{Functions}
  @function[
    "is-builtin"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-srcloc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "old-srcloc"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any" "Any" "Any")
  ]
}