#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-anf.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "INDENT"
      "break-one"
      "str-method"
      "str-letrec"
      "str-period"
      "str-bang"
      "str-brackets"
      "str-colon"
      "str-coloncolon"
      "str-colonspace"
      "str-end"
      "str-let"
      "str-var"
      "str-if"
      "str-elsecolon"
      "str-try"
      "str-except"
      "str-spacecolonequal"
      "str-spaceequal"
      "str-import"
      "str-provide"
      "str-as"
      "dummy-loc"
      "default-map-visitor")
  ]
  @section[#:tag "\"compiler/ast-anf.arr\"_ReExports"]{Re-exported values}
  @re-export["Loc" (from (xref "srcloc" "Srcloc"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["srcloc" "Srcloc"]}
  }
  @section[#:tag "\"compiler/ast-anf.arr\"_DataTypes"]{Data types}
  @data-spec["AProg"]{
    @variants{
      @constr-spec["a-program"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "imports"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AHeader" (xref "<global>" "AHeader")))
          ]
          @member-spec[
            "body"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AProg" (xref "\"compiler/ast-anf.arr\"" "AProg"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AProg" (xref "\"compiler/ast-anf.arr\"" "AProg"))
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
          (a-id "AProg" (xref "\"compiler/ast-anf.arr\"" "AProg"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["AImport"]{
    @variants{
      @constr-spec["a-import-file"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "file"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["name" #:contract (a-id "Name" (xref "<global>" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AImport" (xref "\"compiler/ast-anf.arr\"" "AImport"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AImport" (xref "\"compiler/ast-anf.arr\"" "AImport"))
              "Any")
          ]
        }
      }
      @constr-spec["a-import-builtin"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "lib"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["name" #:contract (a-id "Name" (xref "<global>" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AImport" (xref "\"compiler/ast-anf.arr\"" "AImport"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AImport" (xref "\"compiler/ast-anf.arr\"" "AImport"))
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
          (a-id "AImport" (xref "\"compiler/ast-anf.arr\"" "AImport"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["AExpr"]{
    @variants{
      @constr-spec["a-let"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "bind"
            #:contract (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
          ]
          @member-spec[
            "e"
            #:contract
            (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          ]
          @member-spec[
            "body"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-var"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "bind"
            #:contract (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
          ]
          @member-spec[
            "e"
            #:contract
            (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          ]
          @member-spec[
            "body"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-seq"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "e1"
            #:contract
            (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          ]
          @member-spec[
            "e2"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-tail-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "f"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-split-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "is-var"
            #:contract (a-id "Boolean" (xref "<global>" "Boolean"))
          ]
          @member-spec[
            "f"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
          @member-spec[
            "helper"
            #:contract (a-id "Name" (xref "<global>" "Name"))
          ]
          @member-spec[
            "helper-args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-if"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "c"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "t"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
          @member-spec[
            "e"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["a-lettable"]{
        @members{
          @member-spec[
            "e"
            #:contract
            (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
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
          (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["ABind"]{
    @variants{
      @constr-spec["a-bind"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "<global>" "Name"))]
          @member-spec[
            "ann"
            #:contract (a-compound (a-dot "A" "Ann") (xref "ast" "Ann"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
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
          (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["AVariant"]{
    @variants{
      @constr-spec["a-variant"]{
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
              (a-id
                "AVariantMember"
                (xref "\"compiler/ast-anf.arr\"" "AVariantMember")))
          ]
          @member-spec[
            "with-members"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
              "Any")
          ]
        }
      }
      @constr-spec["a-singleton-variant"]{
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
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
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
          (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["AMemberType"]{
    @variants{
      @singleton-spec["a-normal"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
        }
      }
      @singleton-spec["a-cyclic"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
        }
      }
      @singleton-spec["a-mutable"]{
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id
                "AMemberType"
                (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["AVariantMember"]{
    @variants{
      @constr-spec["a-variant-member"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "member-type"
            #:contract
            (a-id "AMemberType" (xref "\"compiler/ast-anf.arr\"" "AMemberType"))
          ]
          @member-spec[
            "bind"
            #:contract (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id
                "AVariantMember"
                (xref "\"compiler/ast-anf.arr\"" "AVariantMember"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id
                "AVariantMember"
                (xref "\"compiler/ast-anf.arr\"" "AVariantMember"))
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
          (a-id
            "AVariantMember"
            (xref "\"compiler/ast-anf.arr\"" "AVariantMember"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["ALettable"]{
    @variants{
      @constr-spec["a-data-expr"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "variants"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant")))
          ]
          @member-spec[
            "shared"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-assign"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "<global>" "Name"))]
          @member-spec[
            "value"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "_fun"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-prim-app"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "f"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-obj"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-update"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "supe"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-extend"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "supe"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-dot"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "obj"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-colon"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "obj"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-get-bang"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "obj"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-lam"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind")))
          ]
          @member-spec[
            "body"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
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
              (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind")))
          ]
          @member-spec[
            "body"
            #:contract (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["a-val"]{
        @members{
          @member-spec[
            "v"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
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
          (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          "Any"
          "Any")
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
          @member-spec[
            "value"
            #:contract (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField"))
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
          (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField"))
          "Any"
          "Any")
      ]
    }
  }
  @data-spec["AVal"]{
    @variants{
      @constr-spec["a-srcloc"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-num"]{
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
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-str"]{
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
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-bool"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["b" #:contract (a-id "Bool" (xref "<global>" "Bool"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-array"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "values"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-undefined"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-id"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "<global>" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-id-var"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "<global>" "Name"))]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
        }
      }
      @constr-spec["a-id-letrec"]{
        @members{
          @member-spec["l" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec["id" #:contract (a-id "Name" (xref "<global>" "Name"))]
          @member-spec[
            "safe"
            #:contract (a-id "Boolean" (xref "<global>" "Boolean"))
          ]
        }
        @with-members{
          @method-spec[
            "label"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
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
          (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
          "Any"
          "Any")
      ]
    }
  }
  @section[#:tag "\"compiler/ast-anf.arr\"_Functions"]{Functions}
  @function[
    "is-a-program"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-import-file"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-import-builtin"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-let"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-seq"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-tail-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-split-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-if"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-lettable"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-singleton-variant"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-normal"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-cyclic"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-mutable"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-variant-member"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-data-expr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-assign"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-prim-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-obj"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-update"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-extend"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-dot"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-colon"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-get-bang"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-lam"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-method"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-val"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["fun-method-pretty" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function[
    "is-a-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-srcloc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-num"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-str"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-bool"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-array"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-undefined"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-id-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-a-id-letrec"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "strip-loc-prog"
    #:contract
    (a-arrow (a-id "AProg" (xref "\"compiler/ast-anf.arr\"" "AProg")) "Any")
  ]
  @function[
    "strip-loc-header"
    #:contract (a-arrow (a-id "AHeader" (xref "<global>" "AHeader")) "Any")
  ]
  @function[
    "strip-loc-expr"
    #:contract
    (a-arrow (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr")) "Any")
  ]
  @function[
    "strip-loc-bind"
    #:contract
    (a-arrow (a-id "ABind" (xref "\"compiler/ast-anf.arr\"" "ABind")) "Any")
  ]
  @function[
    "strip-loc-lettable"
    #:contract
    (a-arrow
      (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
      "Any")
  ]
  @function[
    "strip-loc-field"
    #:contract
    (a-arrow (a-id "AField" (xref "\"compiler/ast-anf.arr\"" "AField")) "Any")
  ]
  @function[
    "strip-loc-val"
    #:contract
    (a-arrow (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal")) "Any")
  ]
  @function[
    "freevars-e-acc"
    #:contract
    (a-arrow
      (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name")))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "freevars-e"
    #:contract
    (a-arrow
      (a-id "AExpr" (xref "\"compiler/ast-anf.arr\"" "AExpr"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]{
    @; let  d = dummy-loc:
    @;   freevars-e(a-let(d,
    @;       a-bind(d, "x", A.a-blank),
    @;       a-val(a-num(d, 4)),
    @;       a-lettable(a-val(a-id(d, "y")))))
    @;     .to-list() is
    @;     [list: "y"]
    @; end
    
  }
  @function[
    "freevars-variant-acc"
    #:contract
    (a-arrow
      (a-id "AVariant" (xref "\"compiler/ast-anf.arr\"" "AVariant"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name")))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "freevars-l-acc"
    #:contract
    (a-arrow
      (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name")))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "freevars-l"
    #:contract
    (a-arrow
      (a-id "ALettable" (xref "\"compiler/ast-anf.arr\"" "ALettable"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "freevars-v-acc"
    #:contract
    (a-arrow
      (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name")))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "freevars-v"
    #:contract
    (a-arrow
      (a-id "AVal" (xref "\"compiler/ast-anf.arr\"" "AVal"))
      (a-app
        (a-id "Set" (xref "<global>" "Set"))
        (a-id "Name" (xref "<global>" "Name"))))
  ]
  @function[
    "unions"
    #:params (list "a")
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-app (a-id "Set" (xref "<global>" "Set")) "a"))
      (a-app (a-id "Set" (xref "<global>" "Set")) "a"))
  ]
}