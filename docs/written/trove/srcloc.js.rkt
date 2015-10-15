#lang scribble/base
@(require "../../scribble-api.rkt")

@(append-gen-docs
'(module
  "srcloc"
  (path "src/arr/trove/srcloc.arr")
  (data-spec
    (name "Srcloc")
    (type-vars ())
    (variants ("builtin" "srcloc"))
    (shared
      ((method-spec
        (name "_output")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any")))
      (method-spec
        (name "after")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow (a-id "Srcloc" (xref "srcloc" "Srcloc")) "Any" "Any"))))))
  
  (constr-spec
    (name "builtin")
    (members (("module-name" (type normal) (contract "Any"))))
    (with-members
      ((method-spec
        (name "format")
        (arity 2)
        (params ())
        (args ("self" "show-file"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any" "Any")))
      (method-spec
        (name "key")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any")))
      (method-spec
        (name "same-file")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any" "Any")))
      (method-spec
        (name "before")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any" "Any")))
      (method-spec
        (name "is-builtin")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any"))))))
  (fun-spec
    (name "is-builtin")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a builtin"))
  (constr-spec
    (name "srcloc")
    (members
      (("source"
        (type normal)
        (contract (a-id "String" (xref "<global>" "String"))))
      ("start-line"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("start-column"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("start-char"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("end-line"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("end-column"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("end-char"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))))
    (with-members
      ((method-spec
        (name "format")
        (arity 2)
        (params ())
        (args ("self" "show-file"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any" "Any"))
        (doc
          "Returns either 'file: line, col' or just 'line, col', depending on the show-file flag"))
      (method-spec
        (name "key")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any")))
      (method-spec
        (name "same-file")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow
            (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc"))
            (a-id "Srcloc" (xref "srcloc" "Srcloc"))
            "Any")))
      (method-spec
        (name "before")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow
            (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc"))
            (a-id "Srcloc" (xref "srcloc" "Srcloc"))
            "Any"))
        (doc
          "Returns true if this location comes before the other one, assuming they come from the same file"))
      (method-spec
        (name "at-start")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any")))
      (method-spec
        (name "at-end")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any")))
      (method-spec
        (name "_plus")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow
            (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc"))
            "Srcloc11(~is-srcloc16)"
            "Any")))
      (method-spec
        (name "is-builtin")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "is-Srcloc" (xref "srcloc" "is-Srcloc")) "Any"))))))
  (fun-spec
    (name "is-srcloc")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a srcloc"))))

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
        "after"
        ;; N.B. Pyret contract: (Srcloc, Any -> Any)
        
      ]
    }
  }
  
  @section[#:tag "srcloc_Functions"]{Functions}
}
