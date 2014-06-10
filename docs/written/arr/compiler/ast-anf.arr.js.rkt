#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-anf.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-a-program"
      "is-a-import-builtin"
      "is-a-import-file"
      "is-a-import"
      "is-a-import-types"
      "is-a-type-bind"
      "is-a-newtype-bind"
      "is-a-type-let"
      "is-a-let"
      "is-a-var"
      "is-a-seq"
      "is-a-tail-app"
      "is-a-split-app"
      "is-a-if"
      "is-a-lettable"
      "is-a-bind"
      "is-a-variant"
      "is-a-singleton-variant"
      "is-a-normal"
      "is-a-cyclic"
      "is-a-mutable"
      "is-a-variant-member"
      "is-a-module"
      "is-a-data-expr"
      "is-a-assign"
      "is-a-app"
      "is-a-prim-app"
      "is-a-obj"
      "is-a-update"
      "is-a-extend"
      "is-a-dot"
      "is-a-colon"
      "is-a-get-bang"
      "is-a-lam"
      "is-a-method"
      "is-a-val"
      "is-a-field"
      "is-a-srcloc"
      "is-a-num"
      "is-a-str"
      "is-a-bool"
      "is-a-array"
      "is-a-undefined"
      "is-a-id"
      "is-a-id-var"
      "is-a-id-letrec")
  ]
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
      "str-type-let"
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
      "str-newtype"
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
        @members{@member-spec["l"] @member-spec["imports"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AProg -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AProg -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AProg, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AImportType"]{
    @variants{
      @constr-spec["a-import-builtin"]{
        @members{@member-spec["l"] @member-spec["lib"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AImportType -> Any)
            
          ]
        }
      }
      @constr-spec["a-import-file"]{
        @members{@member-spec["l"] @member-spec["file"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AImportType -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["AImport"]{
    @variants{
      @constr-spec["a-import"]{
        @members{
          @member-spec["l"]
          @member-spec["import-type"]
          @member-spec["name"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AImport -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AImport -> Any)
            
          ]
        }
      }
      @constr-spec["a-import-types"]{
        @members{
          @member-spec["l"]
          @member-spec["import-type"]
          @member-spec["name"]
          @member-spec["types"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AImport -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AImport -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AImport, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ATypeBind"]{
    @variants{
      @constr-spec["a-type-bind"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ATypeBind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ATypeBind -> Any)
            
          ]
        }
      }
      @constr-spec["a-newtype-bind"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["namet"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ATypeBind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ATypeBind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ATypeBind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AExpr"]{
    @variants{
      @constr-spec["a-type-let"]{
        @members{@member-spec["l"] @member-spec["bind"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-let"]{
        @members{
          @member-spec["l"]
          @member-spec["bind"]
          @member-spec["e"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-var"]{
        @members{
          @member-spec["l"]
          @member-spec["bind"]
          @member-spec["e"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-seq"]{
        @members{@member-spec["l"] @member-spec["e1"] @member-spec["e2"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-tail-app"]{
        @members{@member-spec["l"] @member-spec["f"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-split-app"]{
        @members{
          @member-spec["l"]
          @member-spec["is-var"]
          @member-spec["f"]
          @member-spec["args"]
          @member-spec["helper"]
          @member-spec["helper-args"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-if"]{
        @members{
          @member-spec["l"]
          @member-spec["c"]
          @member-spec["t"]
          @member-spec["e"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
      @constr-spec["a-lettable"]{
        @members{@member-spec["e"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AExpr -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AExpr, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ABind"]{
    @variants{
      @constr-spec["a-bind"]{
        @members{@member-spec["l"] @member-spec["id"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ABind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ABind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ABind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AVariant"]{
    @variants{
      @constr-spec["a-variant"]{
        @members{
          @member-spec["l"]
          @member-spec["constr-loc"]
          @member-spec["name"]
          @member-spec["members"]
          @member-spec["with-members"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVariant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVariant -> Any)
            
          ]
        }
      }
      @constr-spec["a-singleton-variant"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["with-members"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVariant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVariant -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AVariant, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AMemberType"]{
    @variants{
      @singleton-spec["a-normal"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
          ]
        }
      }
      @singleton-spec["a-cyclic"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
          ]
        }
      }
      @singleton-spec["a-mutable"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AMemberType -> Any)
            
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
          @member-spec["l"]
          @member-spec["member-type"]
          @member-spec["bind"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVariantMember -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVariantMember -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AVariantMember, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ALettable"]{
    @variants{
      @constr-spec["a-module"]{
        @members{
          @member-spec["l"]
          @member-spec["answer"]
          @member-spec["provides"]
          @member-spec["types"]
          @member-spec["checks"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-data-expr"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["namet"]
          @member-spec["variants"]
          @member-spec["shared"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-assign"]{
        @members{@member-spec["l"] @member-spec["id"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-app"]{
        @members{@member-spec["l"] @member-spec["_fun"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-prim-app"]{
        @members{@member-spec["l"] @member-spec["f"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-obj"]{
        @members{@member-spec["l"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-update"]{
        @members{@member-spec["l"] @member-spec["supe"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-extend"]{
        @members{@member-spec["l"] @member-spec["supe"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-dot"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-colon"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-get-bang"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-lam"]{
        @members{
          @member-spec["l"]
          @member-spec["args"]
          @member-spec["ret"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-method"]{
        @members{
          @member-spec["l"]
          @member-spec["args"]
          @member-spec["ret"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["a-val"]{
        @members{@member-spec["v"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ALettable -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ALettable, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AField"]{
    @variants{
      @constr-spec["a-field"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AField -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AField -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AField, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AVal"]{
    @variants{
      @constr-spec["a-srcloc"]{
        @members{@member-spec["l"] @member-spec["loc"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-num"]{
        @members{@member-spec["l"] @member-spec["n"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-str"]{
        @members{@member-spec["l"] @member-spec["s"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-bool"]{
        @members{@member-spec["l"] @member-spec["b"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-array"]{
        @members{@member-spec["l"] @member-spec["values"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-undefined"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-id"]{
        @members{@member-spec["l"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-id-var"]{
        @members{@member-spec["l"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
      @constr-spec["a-id-letrec"]{
        @members{@member-spec["l"] @member-spec["id"] @member-spec["safe"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (AVal -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (AVal, Any -> Any)
        
      ]
    }
  }
  
  @section[#:tag "\"compiler/ast-anf.arr\"_Functions"]{Functions}
  @function["fun-method-pretty"]
  @function["strip-loc-prog"]
  @function["strip-loc-import"]
  @function["strip-loc-import-type"]
  @function["strip-loc-expr"]
  @function["strip-loc-bind"]
  @function["strip-loc-lettable"]
  @function["strip-loc-field"]
  @function["strip-loc-val"]
  @function["freevars-list-acc"]
  @function["freevars-ann-acc"]
  @function["freevars-e-acc"]
  @function[
    "freevars-e"
    #:examples
    '@{
      @; let  d = dummy-loc, n = A.global-names.make-atom, x = n("x"), y = n("y"):
      @;   freevars-e(a-let(d,
      @;       a-bind(d, x, A.a-blank),
      @;       a-val(a-num(d, 4)),
      @;       a-lettable(a-val(a-id(d, y)))))
      @;     .to-list() is
      @;     [list: y]
      @; end
      
    }
  ]
  @function["freevars-variant-acc"]
  @function["freevars-l-acc"]
  @function["freevars-l"]
  @function["freevars-v-acc"]
  @function["freevars-v"]
  @function["unions" #:params (list "a")]
}