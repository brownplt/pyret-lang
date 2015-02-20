#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["ast"]{
  @; Ignored type testers
  @ignore[
    (list
      "is-s-underscore"
      "is-s-name"
      "is-s-global"
      "is-s-type-global"
      "is-s-atom"
      "is-s-program"
      "is-s-import"
      "is-s-import-types"
      "is-s-import-fields"
      "is-s-provide"
      "is-s-provide-all"
      "is-s-provide-none"
      "is-s-provide-types"
      "is-s-provide-types-all"
      "is-s-provide-types-none"
      "is-s-file-import"
      "is-s-const-import"
      "is-h-use-loc"
      "is-s-let-bind"
      "is-s-var-bind"
      "is-s-letrec-bind"
      "is-s-type-bind"
      "is-s-newtype-bind"
      "is-s-module"
      "is-s-type-let-expr"
      "is-s-let-expr"
      "is-s-letrec"
      "is-s-hint-exp"
      "is-s-instantiate"
      "is-s-block"
      "is-s-user-block"
      "is-s-fun"
      "is-s-type"
      "is-s-newtype"
      "is-s-var"
      "is-s-let"
      "is-s-graph"
      "is-s-contract"
      "is-s-when"
      "is-s-assign"
      "is-s-if-pipe"
      "is-s-if-pipe-else"
      "is-s-if"
      "is-s-if-else"
      "is-s-cases"
      "is-s-cases-else"
      "is-s-try"
      "is-s-op"
      "is-s-check-test"
      "is-s-paren"
      "is-s-lam"
      "is-s-method"
      "is-s-extend"
      "is-s-update"
      "is-s-obj"
      "is-s-array"
      "is-s-construct"
      "is-s-confirm"
      "is-s-bless"
      "is-s-app"
      "is-s-prim-app"
      "is-s-prim-val"
      "is-s-id"
      "is-s-id-var"
      "is-s-id-letrec"
      "is-s-undefined"
      "is-s-srcloc"
      "is-s-num"
      "is-s-frac"
      "is-s-bool"
      "is-s-str"
      "is-s-dot"
      "is-s-get-bang"
      "is-s-bracket"
      "is-s-data"
      "is-s-data-expr"
      "is-s-for"
      "is-s-check"
      "is-s-construct-normal"
      "is-s-construct-lazy"
      "is-s-bind"
      "is-s-data-field"
      "is-s-mutable-field"
      "is-s-once-field"
      "is-s-method-field"
      "is-s-for-bind"
      "is-s-normal"
      "is-s-cyclic"
      "is-s-mutable"
      "is-s-variant-member"
      "is-s-variant"
      "is-s-singleton-variant"
      "is-s-datatype-variant"
      "is-s-datatype-singleton-variant"
      "is-s-datatype-constructor"
      "is-s-if-branch"
      "is-s-if-pipe-branch"
      "is-s-cases-branch"
      "is-a-blank"
      "is-a-any"
      "is-a-name"
      "is-a-arrow"
      "is-a-method"
      "is-a-record"
      "is-a-app"
      "is-a-pred"
      "is-a-dot"
      "is-a-field")
  ]
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
      "str-type-let"
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
      "str-provide-types"
      "str-provide-star"
      "str-provide-types-star"
      "str-sharing"
      "str-space"
      "str-spacecolonequal"
      "str-spaceequal"
      "str-thencolon"
      "str-thickarrow"
      "str-try"
      "str-use-loc"
      "str-var"
      "str-newtype"
      "str-type"
      "str-bless"
      "str-confirm"
      "str-val"
      "str-when"
      "str-where"
      "str-with"
      "global-names"
      "default-map-visitor"
      "default-iter-visitor"
      "dummy-loc-visitor")
  ]
  @section[#:tag "ast_DataTypes"]{Data types}
  @data-spec["Name"]{
    @variants{
      @constr-spec["s-underscore"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "to-compiled-source"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "to-compiled"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "toname"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "key"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
        }
      }
      @constr-spec["s-name"]{
        @members{@member-spec["l"] @member-spec["s"]}
        @with-members{
          @method-spec[
            "to-compiled-source"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "to-compiled"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "toname"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "key"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
        }
      }
      @constr-spec["s-global"]{
        @members{@member-spec["s"]}
        @with-members{
          @method-spec[
            "to-compiled-source"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "to-compiled"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "toname"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "key"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
        }
      }
      @constr-spec["s-type-global"]{
        @members{@member-spec["s"]}
        @with-members{
          @method-spec[
            "to-compiled-source"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "to-compiled"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "toname"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "key"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
        }
      }
      @constr-spec["s-atom"]{
        @members{@member-spec["base"] @member-spec["serial"]}
        @with-members{
          @method-spec[
            "to-compiled-source"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "to-compiled"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "toname"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
          @method-spec[
            "key"
            ;; N.B. Pyret contract: (Name -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_lessthan"
        ;; N.B. Pyret contract: (Name, Any -> Any)
        
      ]
      @method-spec[
        "_lessequal"
        ;; N.B. Pyret contract: (Name, Any -> Any)
        
      ]
      @method-spec[
        "_greaterthan"
        ;; N.B. Pyret contract: (Name, Any -> Any)
        
      ]
      @method-spec[
        "_greaterequal"
        ;; N.B. Pyret contract: (Name, Any -> Any)
        
      ]
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Name, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Program"]{
    @variants{
      @constr-spec["s-program"]{
        @members{
          @member-spec["l"]
          @member-spec["_provide"]
          @member-spec["provided-types"]
          @member-spec["imports"]
          @member-spec["block"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Program -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Program -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Program, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Import"]{
    @variants{
      @constr-spec["s-import"]{
        @members{@member-spec["l"] @member-spec["file"] @member-spec["name"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
        }
      }
      @constr-spec["s-import-types"]{
        @members{
          @member-spec["l"]
          @member-spec["file"]
          @member-spec["name"]
          @member-spec["types"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
        }
      }
      @constr-spec["s-import-fields"]{
        @members{@member-spec["l"] @member-spec["fields"] @member-spec["file"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Import -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Import, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Provide"]{
    @variants{
      @constr-spec["s-provide"]{
        @members{@member-spec["l"] @member-spec["block"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
        }
      }
      @constr-spec["s-provide-all"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
        }
      }
      @constr-spec["s-provide-none"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Provide -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Provide, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ProvideTypes"]{
    @variants{
      @constr-spec["s-provide-types"]{
        @members{@member-spec["l"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
        }
      }
      @constr-spec["s-provide-types-all"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
        }
      }
      @constr-spec["s-provide-types-none"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ProvideTypes -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ProvideTypes, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ImportType"]{
    @variants{
      @constr-spec["s-file-import"]{
        @members{@member-spec["l"] @member-spec["file"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ImportType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ImportType -> Any)
            
          ]
        }
      }
      @constr-spec["s-const-import"]{
        @members{@member-spec["l"] @member-spec["mod"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ImportType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ImportType -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ImportType, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Hint"]{
    @variants{
      @constr-spec["h-use-loc"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Hint -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Hint, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["LetBind"]{
    @variants{
      @constr-spec["s-let-bind"]{
        @members{@member-spec["l"] @member-spec["b"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (LetBind -> Any)
            
          ]
        }
      }
      @constr-spec["s-var-bind"]{
        @members{@member-spec["l"] @member-spec["b"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (LetBind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (LetBind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["LetrecBind"]{
    @variants{
      @constr-spec["s-letrec-bind"]{
        @members{@member-spec["l"] @member-spec["b"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (LetrecBind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (LetrecBind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["TypeLetBind"]{
    @variants{
      @constr-spec["s-type-bind"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (TypeLetBind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (TypeLetBind -> Any)
            
          ]
        }
      }
      @constr-spec["s-newtype-bind"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["namet"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (TypeLetBind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (TypeLetBind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (TypeLetBind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Expr"]{
    @variants{
      @constr-spec["s-module"]{
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
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-type-let-expr"]{
        @members{@member-spec["l"] @member-spec["binds"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-let-expr"]{
        @members{@member-spec["l"] @member-spec["binds"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-letrec"]{
        @members{@member-spec["l"] @member-spec["binds"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-hint-exp"]{
        @members{@member-spec["l"] @member-spec["hints"] @member-spec["exp"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-instantiate"]{
        @members{@member-spec["l"] @member-spec["expr"] @member-spec["params"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-block"]{
        @members{@member-spec["l"] @member-spec["stmts"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-user-block"]{
        @members{@member-spec["l"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-fun"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["params"]
          @member-spec["args"]
          @member-spec["ann"]
          @member-spec["doc"]
          @member-spec["body"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-type"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-newtype"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["namet"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-var"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-let"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["value"]
          @member-spec["keyword-val"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-graph"]{
        @members{@member-spec["l"] @member-spec["bindings"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-contract"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["ann"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-when"]{
        @members{@member-spec["l"] @member-spec["test"] @member-spec["block"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-assign"]{
        @members{@member-spec["l"] @member-spec["id"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-if-pipe"]{
        @members{@member-spec["l"] @member-spec["branches"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-if-pipe-else"]{
        @members{
          @member-spec["l"]
          @member-spec["branches"]
          @member-spec["_else"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-if"]{
        @members{@member-spec["l"] @member-spec["branches"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-if-else"]{
        @members{
          @member-spec["l"]
          @member-spec["branches"]
          @member-spec["_else"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-cases"]{
        @members{
          @member-spec["l"]
          @member-spec["typ"]
          @member-spec["val"]
          @member-spec["branches"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-cases-else"]{
        @members{
          @member-spec["l"]
          @member-spec["typ"]
          @member-spec["val"]
          @member-spec["branches"]
          @member-spec["_else"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-try"]{
        @members{
          @member-spec["l"]
          @member-spec["body"]
          @member-spec["id"]
          @member-spec["_except"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-op"]{
        @members{
          @member-spec["l"]
          @member-spec["op"]
          @member-spec["left"]
          @member-spec["right"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-check-test"]{
        @members{
          @member-spec["l"]
          @member-spec["op"]
          @member-spec["left"]
          @member-spec["right"]
        }
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-paren"]{
        @members{@member-spec["l"] @member-spec["expr"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-lam"]{
        @members{
          @member-spec["l"]
          @member-spec["params"]
          @member-spec["args"]
          @member-spec["ann"]
          @member-spec["doc"]
          @member-spec["body"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-method"]{
        @members{
          @member-spec["l"]
          @member-spec["args"]
          @member-spec["ann"]
          @member-spec["doc"]
          @member-spec["body"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-extend"]{
        @members{@member-spec["l"] @member-spec["supe"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-update"]{
        @members{@member-spec["l"] @member-spec["supe"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-obj"]{
        @members{@member-spec["l"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-array"]{
        @members{@member-spec["l"] @member-spec["values"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-construct"]{
        @members{
          @member-spec["l"]
          @member-spec["modifier"]
          @member-spec["constructor"]
          @member-spec["values"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-confirm"]{
        @members{@member-spec["l"] @member-spec["expr"] @member-spec["typ"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-bless"]{
        @members{@member-spec["l"] @member-spec["expr"] @member-spec["typ"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-app"]{
        @members{@member-spec["l"] @member-spec["_fun"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-prim-app"]{
        @members{@member-spec["l"] @member-spec["_fun"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-prim-val"]{
        @members{@member-spec["l"] @member-spec["name"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-id"]{
        @members{@member-spec["l"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-id-var"]{
        @members{@member-spec["l"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-id-letrec"]{
        @members{@member-spec["l"] @member-spec["id"] @member-spec["safe"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-undefined"]{
        @members{@member-spec["l"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-srcloc"]{
        @members{@member-spec["l"] @member-spec["loc"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-num"]{
        @members{@member-spec["l"] @member-spec["n"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-frac"]{
        @members{@member-spec["l"] @member-spec["num"] @member-spec["den"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-bool"]{
        @members{@member-spec["l"] @member-spec["b"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-str"]{
        @members{@member-spec["l"] @member-spec["s"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-dot"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-get-bang"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-bracket"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-data"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["params"]
          @member-spec["mixins"]
          @member-spec["variants"]
          @member-spec["shared-members"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-data-expr"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["namet"]
          @member-spec["params"]
          @member-spec["mixins"]
          @member-spec["variants"]
          @member-spec["shared-members"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-for"]{
        @members{
          @member-spec["l"]
          @member-spec["iterator"]
          @member-spec["bindings"]
          @member-spec["ann"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
      @constr-spec["s-check"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["body"]
          @member-spec["keyword-check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Expr -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Expr, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ConstructModifier"]{
    @variants{
      @singleton-spec["s-construct-normal"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ConstructModifier -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ConstructModifier -> Any)
            
          ]
        }
      }
      @singleton-spec["s-construct-lazy"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ConstructModifier -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ConstructModifier -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ConstructModifier, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Bind"]{
    @variants{
      @constr-spec["s-bind"]{
        @members{
          @member-spec["l"]
          @member-spec["shadows"]
          @member-spec["id"]
          @member-spec["ann"]
        }
        @with-members{
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Bind -> Any)
            
          ]
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Bind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Bind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Member"]{
    @variants{
      @constr-spec["s-data-field"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
        }
      }
      @constr-spec["s-mutable-field"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["ann"]
          @member-spec["value"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
        }
      }
      @constr-spec["s-once-field"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["ann"]
          @member-spec["value"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
        }
      }
      @constr-spec["s-method-field"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["args"]
          @member-spec["ann"]
          @member-spec["doc"]
          @member-spec["body"]
          @member-spec["_check"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Member -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Member, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["ForBind"]{
    @variants{
      @constr-spec["s-for-bind"]{
        @members{@member-spec["l"] @member-spec["bind"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (ForBind -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (ForBind -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (ForBind, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["VariantMemberType"]{
    @variants{
      @singleton-spec["s-normal"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
        }
      }
      @singleton-spec["s-cyclic"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
        }
      }
      @singleton-spec["s-mutable"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (VariantMemberType -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (VariantMemberType, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["VariantMember"]{
    @variants{
      @constr-spec["s-variant-member"]{
        @members{
          @member-spec["l"]
          @member-spec["member-type"]
          @member-spec["bind"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (VariantMember -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (VariantMember -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (VariantMember, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Variant"]{
    @variants{
      @constr-spec["s-variant"]{
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
            ;; N.B. Pyret contract: (Variant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Variant -> Any)
            
          ]
        }
      }
      @constr-spec["s-singleton-variant"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["with-members"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Variant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Variant -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Variant, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["DatatypeVariant"]{
    @variants{
      @constr-spec["s-datatype-variant"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["members"]
          @member-spec["constructor"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (DatatypeVariant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (DatatypeVariant -> Any)
            
          ]
        }
      }
      @constr-spec["s-datatype-singleton-variant"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["constructor"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (DatatypeVariant -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (DatatypeVariant -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (DatatypeVariant, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Constructor"]{
    @variants{
      @constr-spec["s-datatype-constructor"]{
        @members{@member-spec["l"] @member-spec["self"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Constructor -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Constructor -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Constructor, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["IfBranch"]{
    @variants{
      @constr-spec["s-if-branch"]{
        @members{@member-spec["l"] @member-spec["test"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (IfBranch -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (IfBranch -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (IfBranch, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["IfPipeBranch"]{
    @variants{
      @constr-spec["s-if-pipe-branch"]{
        @members{@member-spec["l"] @member-spec["test"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (IfPipeBranch -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (IfPipeBranch -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (IfPipeBranch, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["CasesBranch"]{
    @variants{
      @constr-spec["s-cases-branch"]{
        @members{
          @member-spec["l"]
          @member-spec["name"]
          @member-spec["args"]
          @member-spec["body"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (CasesBranch -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (CasesBranch -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (CasesBranch, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["Ann"]{
    @variants{
      @singleton-spec["a-blank"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @singleton-spec["a-any"]{
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-name"]{
        @members{@member-spec["l"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-arrow"]{
        @members{
          @member-spec["l"]
          @member-spec["args"]
          @member-spec["ret"]
          @member-spec["use-parens"]
        }
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-method"]{
        @members{@member-spec["l"] @member-spec["args"] @member-spec["ret"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-record"]{
        @members{@member-spec["l"] @member-spec["fields"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-app"]{
        @members{@member-spec["l"] @member-spec["ann"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-pred"]{
        @members{@member-spec["l"] @member-spec["ann"] @member-spec["exp"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
      @constr-spec["a-dot"]{
        @members{@member-spec["l"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "label"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (Ann -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "visit"
        ;; N.B. Pyret contract: (Ann, Any -> Any)
        
      ]
    }
  }
  
  @data-spec["AField"]{
    @variants{
      @constr-spec["a-field"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["ann"]}
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
  
  @section[#:tag "ast_Functions"]{Functions}
  @function["MakeName"]
  @function["funlam-tosource"]
  @function["make-checker-name"]
  @function["flatten"]
  @function["binding-type-ids"]
  @function["block-type-ids"]
  @function["binding-ids"]
  @function["block-ids"]
  @function["toplevel-ids"]
}
