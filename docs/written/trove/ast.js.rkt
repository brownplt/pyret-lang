#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["ast"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "dummy-loc"
      "INDENT"
      "break-one"
      "str-any"
      "str-arrow"
      "str-arrowspace"
      "str-as"
      "str-blank"
      "str-let"
      "str-letrec"
      "str-block"
      "str-brackets"
      "str-cases"
      "str-caret"
      "str-checkcolon"
      "str-examplescolon"
      "str-colon"
      "str-coloncolon"
      "str-colonspace"
      "str-comment"
      "str-constructor"
      "str-data"
      "str-data-expr"
      "str-datatype"
      "str-deriving"
      "str-doc"
      "str-elsebranch"
      "str-elsecolon"
      "str-otherwisecolon"
      "str-elsespace"
      "str-end"
      "str-except"
      "str-for"
      "str-from"
      "str-fun"
      "str-lam"
      "str-graph"
      "str-if"
      "str-askcolon"
      "str-import"
      "str-method"
      "str-mutable"
      "str-period"
      "str-bang"
      "str-pipespace"
      "str-provide"
      "str-provide-star"
      "str-sharing"
      "str-space"
      "str-spacecolonequal"
      "str-spaceequal"
      "str-thencolon"
      "str-thickarrow"
      "str-try"
      "str-use-loc"
      "str-var"
      "str-val"
      "str-when"
      "str-where"
      "str-with"
      "global-names"
      "default-map-visitor"
      "default-iter-visitor"
      "dummy-loc-visitor")
  ]
  @section[#:tag "ast_ReExports"]{Re-exported values}
  @re-export["Loc" (from (xref "srcloc" "Srcloc"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["srcloc" "Srcloc"]}
  }
  @re-export["loc" (from (xref "srcloc" "srcloc"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["srcloc" "srcloc"]}
  }
  @section[#:tag "ast_DataTypes"]{Data types}
  @data-spec["Name"]{
    @variants{
      @constr-spec["s-underscore"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "to-compiled-source"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "to-compiled"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "toname"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "key"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
        }
      }
      @constr-spec["s-name"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "s"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "to-compiled-source"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "to-compiled"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "toname"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "key"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
        }
      }
      @constr-spec["s-global"]{
        @members{
          @member-spec[
            "s"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "to-compiled-source"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "to-compiled"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "toname"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "key"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
        }
      }
      @constr-spec["s-atom"]{
        @members{
          @member-spec[
            "base"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "serial"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{
          @method-spec[
            "to-compiled-source"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "to-compiled"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "toname"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
          @method-spec[
            "key"
            #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_lessthan"
        #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any" "Any")
      ]
      @method-spec[
        "_lessequal"
        #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any" "Any")
      ]
      @method-spec[
        "_greaterthan"
        #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any" "Any")
      ]
      @method-spec[
        "_greaterequal"
        #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any" "Any")
      ]
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Name" (xref "ast" "Name")) "Any" "Any")
      ]
    }
  }
  @data-spec["Program"]{
    @variants{
      @constr-spec["s-program"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "_provide"
            #:contract (a-id "Provide" (xref "ast" "Provide"))
          ]
          @member-spec[
            "imports"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Import" (xref "ast" "Import")))
          ]
          @member-spec["block" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Program" (xref "ast" "Program")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Program" (xref "ast" "Program")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Program" (xref "ast" "Program")) "Any" "Any")
      ]
    }
  }
  @data-spec["Import"]{
    @variants{
      @constr-spec["s-import"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "file"
            #:contract (a-id "ImportType" (xref "ast" "ImportType"))
          ]
          @member-spec["name" #:contract (a-id "Name" (xref "ast" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Import" (xref "ast" "Import")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Import" (xref "ast" "Import")) "Any")
          ]
        }
      }
      @constr-spec["s-import-fields"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Name" (xref "ast" "Name")))
          ]
          @member-spec[
            "file"
            #:contract (a-id "ImportType" (xref "ast" "ImportType"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Import" (xref "ast" "Import")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Import" (xref "ast" "Import")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Import" (xref "ast" "Import")) "Any" "Any")
      ]
    }
  }
  @data-spec["Provide"]{
    @variants{
      @constr-spec["s-provide"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["block" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
        }
      }
      @constr-spec["s-provide-all"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
        }
      }
      @constr-spec["s-provide-none"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Provide" (xref "ast" "Provide")) "Any" "Any")
      ]
    }
  }
  @data-spec["ImportType"]{
    @variants{
      @constr-spec["s-file-import"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "file"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "ImportType" (xref "ast" "ImportType")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "ImportType" (xref "ast" "ImportType")) "Any")
          ]
        }
      }
      @constr-spec["s-const-import"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "mod"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "ImportType" (xref "ast" "ImportType")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "ImportType" (xref "ast" "ImportType")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "ImportType" (xref "ast" "ImportType")) "Any" "Any")
      ]
    }
  }
  @data-spec["Hint"]{
    @variants{
      @constr-spec["h-use-loc"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Hint" (xref "ast" "Hint")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Hint" (xref "ast" "Hint")) "Any" "Any")
      ]
    }
  }
  @data-spec["LetBind"]{
    @variants{
      @constr-spec["s-let-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["b" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "LetBind" (xref "ast" "LetBind")) "Any")
          ]
        }
      }
      @constr-spec["s-var-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["b" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "LetBind" (xref "ast" "LetBind")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "LetBind" (xref "ast" "LetBind")) "Any" "Any")
      ]
    }
  }
  @data-spec["LetrecBind"]{
    @variants{
      @constr-spec["s-letrec-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["b" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "LetrecBind" (xref "ast" "LetrecBind")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "LetrecBind" (xref "ast" "LetrecBind")) "Any" "Any")
      ]
    }
  }
  @data-spec["Expr"]{
    @variants{
      @constr-spec["s-let-expr"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "binds"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "LetBind" (xref "ast" "LetBind")))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-letrec"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "binds"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "LetrecBind" (xref "ast" "LetrecBind")))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-hint-exp"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "hints"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Hint" (xref "ast" "Hint")))
          ]
          @member-spec["exp" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-instantiate"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["expr" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "params"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Ann" (xref "ast" "Ann")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-block"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "stmts"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-user-block"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-fun"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "params"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Bind" (xref "ast" "Bind")))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "doc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-var"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-let"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "keyword-val"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-graph"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "bindings"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "is-s-let" (xref "ast" "is-s-let")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-contract"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Name" (xref "ast" "Name"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-when"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["test" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["block" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-assign"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-if-pipe"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-if-pipe-else"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch")))
          ]
          @member-spec["_else" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-if"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "IfBranch" (xref "ast" "IfBranch")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-if-else"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "IfBranch" (xref "ast" "IfBranch")))
          ]
          @member-spec["_else" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-cases"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["typ" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["val" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "CasesBranch" (xref "ast" "CasesBranch")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-cases-else"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["typ" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["val" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "branches"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "CasesBranch" (xref "ast" "CasesBranch")))
          ]
          @member-spec["_else" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-try"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["id" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["_except" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-op"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "op"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["left" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["right" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-check-test"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "op"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["left" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["right" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-paren"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["expr" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-lam"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "params"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Bind" (xref "ast" "Bind")))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "doc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-method"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Bind" (xref "ast" "Bind")))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "doc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-extend"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["supe" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-update"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["supe" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-obj"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-array"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "values"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-construct"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "modifier"
            #:contract
            (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
          ]
          @member-spec[
            "constructor"
            #:contract (a-id "Expr" (xref "ast" "Expr"))
          ]
          @member-spec[
            "values"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["_fun" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-prim-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "_fun"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-prim-val"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-id"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-id-var"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-id-letrec"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
          @member-spec[
            "safe"
            #:contract (a-id "Boolean" (xref "<global>" "Boolean"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-undefined"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-srcloc"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-num"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "n"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-frac"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "num"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "den"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-bool"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["b" #:contract (a-id "Bool" (xref "<global>" "Bool"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-str"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "s"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-dot"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["obj" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-get-bang"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["obj" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-bracket"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["obj" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["field" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-data"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "params"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "mixins"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
          @member-spec[
            "variants"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Variant" (xref "ast" "Variant")))
          ]
          @member-spec[
            "shared-members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-data-expr"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "params"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "mixins"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
          @member-spec[
            "variants"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Variant" (xref "ast" "Variant")))
          ]
          @member-spec[
            "shared-members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-for"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["iterator" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "bindings"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "ForBind" (xref "ast" "ForBind")))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
      @constr-spec["s-check"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "keyword-check"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Expr" (xref "ast" "Expr")) "Any" "Any")
      ]
    }
  }
  @data-spec["ConstructModifier"]{
    @variants{
      @singleton-spec["s-construct-normal"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
              "Any")
          ]
        }
      }
      @singleton-spec["s-construct-lazy"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow
          (a-id "ConstructModifier" (xref "ast" "ConstructModifier"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["Bind"]{
    @variants{
      @constr-spec["s-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "shadows"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Bind" (xref "ast" "Bind")) "Any")
          ]
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Bind" (xref "ast" "Bind")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Bind" (xref "ast" "Bind")) "Any" "Any")
      ]
    }
  }
  @data-spec["Member"]{
    @variants{
      @constr-spec["s-data-field"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
        }
      }
      @constr-spec["s-mutable-field"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
        }
      }
      @constr-spec["s-once-field"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
        }
      }
      @constr-spec["s-method-field"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["name" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Bind" (xref "ast" "Bind")))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "doc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec[
            "_check"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "Expr" (xref "ast" "Expr")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Member" (xref "ast" "Member")) "Any" "Any")
      ]
    }
  }
  @data-spec["ForBind"]{
    @variants{
      @constr-spec["s-for-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["bind" #:contract (a-id "Bind" (xref "ast" "Bind"))]
          @member-spec["value" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "ForBind" (xref "ast" "ForBind")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "ForBind" (xref "ast" "ForBind")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "ForBind" (xref "ast" "ForBind")) "Any" "Any")
      ]
    }
  }
  @data-spec["VariantMemberType"]{
    @variants{
      @singleton-spec["s-normal"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
        }
      }
      @singleton-spec["s-cyclic"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
        }
      }
      @singleton-spec["s-mutable"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow
          (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["VariantMember"]{
    @variants{
      @constr-spec["s-variant-member"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "member-type"
            #:contract
            (a-id "VariantMemberType" (xref "ast" "VariantMemberType"))
          ]
          @member-spec["bind" #:contract (a-id "Bind" (xref "ast" "Bind"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "VariantMember" (xref "ast" "VariantMember")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "VariantMember" (xref "ast" "VariantMember")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow
          (a-id "VariantMember" (xref "ast" "VariantMember"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["Variant"]{
    @variants{
      @constr-spec["s-variant"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "constr-loc"
            #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))
          ]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "VariantMember" (xref "ast" "VariantMember")))
          ]
          @member-spec[
            "with-members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Variant" (xref "ast" "Variant")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Variant" (xref "ast" "Variant")) "Any")
          ]
        }
      }
      @constr-spec["s-singleton-variant"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "with-members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Member" (xref "ast" "Member")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Variant" (xref "ast" "Variant")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Variant" (xref "ast" "Variant")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Variant" (xref "ast" "Variant")) "Any" "Any")
      ]
    }
  }
  @data-spec["DatatypeVariant"]{
    @variants{
      @constr-spec["s-datatype-variant"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "VariantMember" (xref "ast" "VariantMember")))
          ]
          @member-spec[
            "constructor"
            #:contract (a-id "Constructor" (xref "ast" "Constructor"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
              "Any")
          ]
        }
      }
      @constr-spec["s-datatype-singleton-variant"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "constructor"
            #:contract (a-id "Constructor" (xref "ast" "Constructor"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow
          (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["Constructor"]{
    @variants{
      @constr-spec["s-datatype-constructor"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "self"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "Constructor" (xref "ast" "Constructor")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "Constructor" (xref "ast" "Constructor")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "Constructor" (xref "ast" "Constructor")) "Any" "Any")
      ]
    }
  }
  @data-spec["IfBranch"]{
    @variants{
      @constr-spec["s-if-branch"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["test" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "IfBranch" (xref "ast" "IfBranch")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "IfBranch" (xref "ast" "IfBranch")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "IfBranch" (xref "ast" "IfBranch")) "Any" "Any")
      ]
    }
  }
  @data-spec["IfPipeBranch"]{
    @variants{
      @constr-spec["s-if-pipe-branch"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["test" #:contract (a-id "Expr" (xref "ast" "Expr"))]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch")) "Any" "Any")
      ]
    }
  }
  @data-spec["CasesBranch"]{
    @variants{
      @constr-spec["s-cases-branch"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Bind" (xref "ast" "Bind")))
          ]
          @member-spec["body" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow (a-id "CasesBranch" (xref "ast" "CasesBranch")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow (a-id "CasesBranch" (xref "ast" "CasesBranch")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract
        (a-arrow (a-id "CasesBranch" (xref "ast" "CasesBranch")) "Any" "Any")
      ]
    }
  }
  @data-spec["Ann"]{
    @variants{
      @singleton-spec["a-blank"]{
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @singleton-spec["a-any"]{
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-name"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "ast" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-arrow"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Ann" (xref "ast" "Ann")))
          ]
          @member-spec["ret" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "use-parens"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-method"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Ann" (xref "ast" "Ann")))
          ]
          @member-spec["ret" #:contract (a-id "Ann" (xref "ast" "Ann"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-record"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "ast" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Ann" (xref "ast" "Ann")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-pred"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
          @member-spec["exp" #:contract (a-id "Expr" (xref "ast" "Expr"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
      @constr-spec["a-dot"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["obj" #:contract (a-id "Name" (xref "ast" "Name"))]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "Ann" (xref "ast" "Ann")) "Any" "Any")
      ]
    }
  }
  @data-spec["AField"]{
    @variants{
      @constr-spec["a-field"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["ann" #:contract (a-id "Ann" (xref "ast" "Ann"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract (a-arrow (a-id "AField" (xref "ast" "AField")) "Any")
          ]
          @method-spec[
            "tosource"
            #:contract (a-arrow (a-id "AField" (xref "ast" "AField")) "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        #:contract (a-arrow (a-id "AField" (xref "ast" "AField")) "Any" "Any")
      ]
    }
  }
  @data-spec["Pair"]{
    @variants{
      @constr-spec["pair"]{
        @members{
          @member-spec["l" #:contract "Any"]
          @member-spec["r" #:contract "Any"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "ast_Functions"]{Functions}
  @function[
    "is-s-underscore"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-name"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-global"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-atom"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["MakeName" #:contract (a-arrow "Any" "Any")]
  @function[
    "funlam-tosource"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Bind" (xref "ast" "Bind")))
      (a-id "Ann" (xref "ast" "Ann"))
      (a-id "String" (xref "<global>" "String"))
      (a-id "Expr" (xref "ast" "Expr"))
      (a-app
        (a-id "Option" (xref "option" "Option"))
        (a-id "Expr" (xref "ast" "Expr")))
      (a-compound (a-dot "PP" "PPrintDoc") (xref "pprint" "PPrintDoc")))
  ]
  @function[
    "is-s-program"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-import"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-import-fields"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-provide"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-provide-all"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-provide-none"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-file-import"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-const-import"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-h-use-loc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-let-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-var-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-letrec-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-let-expr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-letrec"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-hint-exp"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-instantiate"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-block"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-user-block"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-fun"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-let"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-graph"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-contract"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-when"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-assign"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if-pipe"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if-pipe-else"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if-else"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-cases"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-cases-else"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-try"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-op"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-check-test"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-paren"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-lam"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-method"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-extend"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-update"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-obj"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-array"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-construct"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-prim-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-prim-val"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-id-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-id-letrec"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-undefined"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-srcloc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-num"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-frac"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-bool"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-str"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-dot"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-get-bang"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-bracket"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-data"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-data-expr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-for"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-check"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-construct-normal"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-construct-lazy"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-data-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-mutable-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-once-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-method-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-for-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-normal"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-cyclic"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-mutable"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-variant-member"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-singleton-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-datatype-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-datatype-singleton-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-datatype-constructor"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if-branch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-if-pipe-branch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-s-cases-branch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-blank"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-any"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-name"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-arrow"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-method"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-record"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-pred"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-dot"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["make-checker-name" #:contract (a-arrow "Any" "Any")]
  @function[
    "flatten"
    #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "binding-ids"
    #:contract
    (a-arrow
      "Any"
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Name" (xref "ast" "Name"))))
  ]
  @function[
    "block-ids"
    #:contract
    (a-arrow
      (a-id "is-s-block" (xref "ast" "is-s-block"))
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Name" (xref "ast" "Name"))))
  ]
  @function[
    "toplevel-ids"
    #:contract
    (a-arrow
      (a-id "Program" (xref "ast" "Program"))
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Name" (xref "ast" "Name"))))
  ]
  @function[
    "is-pair"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["length-andmap" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function[
    "equiv-ast-prog"
    #:contract
    (a-arrow
      (a-id "Program" (xref "ast" "Program"))
      (a-id "Program" (xref "ast" "Program"))
      "Any")
  ]
  @function[
    "equiv-name"
    #:contract
    (a-arrow
      (a-id "Name" (xref "ast" "Name"))
      (a-id "Name" (xref "ast" "Name"))
      "Any")
  ]
  @function[
    "equiv-ast-member"
    #:contract
    (a-arrow
      (a-id "Member" (xref "ast" "Member"))
      (a-id "Member" (xref "ast" "Member"))
      "Any")
  ]
  @function[
    "equiv-ast-bind"
    #:contract
    (a-arrow
      (a-id "Bind" (xref "ast" "Bind"))
      (a-id "Bind" (xref "ast" "Bind"))
      "Any")
  ]
  @function[
    "equiv-ast-for-binding"
    #:contract
    (a-arrow
      (a-id "ForBind" (xref "ast" "ForBind"))
      (a-id "ForBind" (xref "ast" "ForBind"))
      "Any")
  ]
  @function[
    "equiv-ast-if-branch"
    #:contract
    (a-arrow
      (a-id "IfBranch" (xref "ast" "IfBranch"))
      (a-id "IfBranch" (xref "ast" "IfBranch"))
      "Any")
  ]
  @function[
    "equiv-ast-if-pipe-branch"
    #:contract
    (a-arrow
      (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch"))
      (a-id "IfPipeBranch" (xref "ast" "IfPipeBranch"))
      "Any")
  ]
  @function[
    "equiv-ast-cases-branch"
    #:contract
    (a-arrow
      (a-id "CasesBranch" (xref "ast" "CasesBranch"))
      (a-id "CasesBranch" (xref "ast" "CasesBranch"))
      "Any")
  ]
  @function[
    "equiv-ast-variant-member"
    #:contract
    (a-arrow
      (a-id "VariantMember" (xref "ast" "VariantMember"))
      (a-id "VariantMember" (xref "ast" "VariantMember"))
      "Any")
  ]
  @function[
    "equiv-ast-variant"
    #:contract
    (a-arrow
      (a-id "Variant" (xref "ast" "Variant"))
      (a-id "Variant" (xref "ast" "Variant"))
      "Any")
  ]
  @function[
    "equiv-ast-datatype-variant"
    #:contract
    (a-arrow
      (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
      (a-id "DatatypeVariant" (xref "ast" "DatatypeVariant"))
      "Any")
  ]
  @function[
    "equiv-ast-constructor"
    #:contract
    (a-arrow
      (a-id "Constructor" (xref "ast" "Constructor"))
      (a-id "Constructor" (xref "ast" "Constructor"))
      "Any")
  ]
  @function["equiv-ast-ann" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "equiv-ast-fun"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any")
  ]
  @function[
    "equiv-ast-provide"
    #:contract
    (a-arrow
      (a-id "Provide" (xref "ast" "Provide"))
      (a-id "Provide" (xref "ast" "Provide"))
      "Any")
  ]
  @function["equiv-import-type" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "equiv-ast-import"
    #:contract
    (a-arrow
      (a-id "Import" (xref "ast" "Import"))
      (a-id "Import" (xref "ast" "Import"))
      "Any")
  ]
  @function[
    "equiv-ast-let-bind"
    #:contract
    (a-arrow
      (a-id "LetBind" (xref "ast" "LetBind"))
      (a-id "LetBind" (xref "ast" "LetBind"))
      "Any")
  ]
  @function[
    "equiv-ast-letrec-bind"
    #:contract
    (a-arrow
      (a-id "LetrecBind" (xref "ast" "LetrecBind"))
      (a-id "LetrecBind" (xref "ast" "LetrecBind"))
      "Any")
  ]
  @function[
    "equiv-opt"
    #:contract
    (a-arrow
      (a-app
        (a-id "Option" (xref "option" "Option"))
        (a-id "Expr" (xref "ast" "Expr")))
      (a-app
        (a-id "Option" (xref "option" "Option"))
        (a-id "Expr" (xref "ast" "Expr")))
      "Any")
  ]
  @function[
    "equiv-ast"
    #:contract
    (a-arrow
      (a-id "Expr" (xref "ast" "Expr"))
      (a-id "Expr" (xref "ast" "Expr"))
      "Any")
  ]
}