#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/js-ast.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-j-block"
      "is-j-var"
      "is-j-if1"
      "is-j-if"
      "is-j-return"
      "is-j-try-catch"
      "is-j-throw"
      "is-j-expr"
      "is-j-plus"
      "is-j-minus"
      "is-j-times"
      "is-j-divide"
      "is-j-and"
      "is-j-or"
      "is-j-lt"
      "is-j-leq"
      "is-j-gt"
      "is-j-geq"
      "is-j-eq"
      "is-j-equals"
      "is-j-neq"
      "is-j-nequals"
      "is-j-incr"
      "is-j-decr"
      "is-j-postincr"
      "is-j-postdecr"
      "is-j-parens"
      "is-j-unop"
      "is-j-binop"
      "is-j-fun"
      "is-j-app"
      "is-j-method"
      "is-j-ternary"
      "is-j-assign"
      "is-j-bracket-assign"
      "is-j-dot-assign"
      "is-j-dot"
      "is-j-bracket"
      "is-j-list"
      "is-j-obj"
      "is-j-id"
      "is-j-str"
      "is-j-num"
      "is-j-true"
      "is-j-false"
      "is-j-null"
      "is-j-undefined"
      "is-j-raw"
      "is-j-raw-holes"
      "is-j-field")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "INDENT" "break-one" "blank-one")]
  @section[#:tag "\"compiler/js-ast.arr\"_ReExports"]{Re-exported values}
  @re-export["format" (from (xref "format" "format"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["format" "format"]}
  }
  @section[#:tag "\"compiler/js-ast.arr\"_DataTypes"]{Data types}
  @data-spec["JBlock"]{
    @variants{
      @constr-spec["j-block"]{
        @members{@member-spec["stmts"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JBlock, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JBlock -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        ;; N.B. Pyret contract: (JBlock -> Any)
        
      ]
    }
  }
  
  @data-spec["JStmt"]{
    @variants{
      @constr-spec["j-var"]{
        @members{@member-spec["name"] @member-spec["rhs"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-if1"]{
        @members{@member-spec["cond"] @member-spec["consq"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-if"]{
        @members{@member-spec["cond"] @member-spec["consq"] @member-spec["alt"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-return"]{
        @members{@member-spec["expr"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-try-catch"]{
        @members{@member-spec["body"] @member-spec["exn"] @member-spec["catch"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-throw"]{
        @members{@member-spec["exp"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
      @constr-spec["j-expr"]{
        @members{@member-spec["expr"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JStmt, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JStmt -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        ;; N.B. Pyret contract: (JStmt -> Any)
        
      ]
    }
  }
  
  @data-spec["JBinop"]{
    @variants{
      @singleton-spec["j-plus"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-minus"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-times"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-divide"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-and"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-or"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-lt"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-leq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-gt"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-geq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-eq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-equals"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-neq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-nequals"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JBinop -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "print-ugly-source"
        ;; N.B. Pyret contract: (JBinop, Any -> Any)
        
      ]
      @method-spec[
        "tosource"
        ;; N.B. Pyret contract: (JBinop -> Any)
        
      ]
    }
  }
  
  @data-spec["JUnop"]{
    @variants{
      @singleton-spec["j-incr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JUnop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-decr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JUnop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-postincr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JUnop -> Any)
            
          ]
        }
      }
      @singleton-spec["j-postdecr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            ;; N.B. Pyret contract: (JUnop -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "print-ugly-source"
        ;; N.B. Pyret contract: (JUnop, Any -> Any)
        
      ]
      @method-spec[
        "tosource"
        ;; N.B. Pyret contract: (JUnop -> Any)
        
      ]
    }
  }
  
  @data-spec["JExpr"]{
    @variants{
      @constr-spec["j-parens"]{
        @members{@member-spec["exp"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-unop"]{
        @members{@member-spec["exp"] @member-spec["op"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-binop"]{
        @members{@member-spec["left"] @member-spec["op"] @member-spec["right"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-fun"]{
        @members{@member-spec["args"] @member-spec["body"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-app"]{
        @members{@member-spec["func"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-method"]{
        @members{@member-spec["obj"] @member-spec["meth"] @member-spec["args"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-ternary"]{
        @members{
          @member-spec["test"]
          @member-spec["consq"]
          @member-spec["altern"]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-assign"]{
        @members{@member-spec["name"] @member-spec["rhs"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-bracket-assign"]{
        @members{@member-spec["obj"] @member-spec["field"] @member-spec["rhs"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-dot-assign"]{
        @members{@member-spec["obj"] @member-spec["name"] @member-spec["rhs"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-dot"]{
        @members{@member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-bracket"]{
        @members{@member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-list"]{
        @members{@member-spec["multi-line"] @member-spec["elts"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-obj"]{
        @members{@member-spec["fields"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-id"]{
        @members{@member-spec["id"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-str"]{
        @members{@member-spec["s"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-num"]{
        @members{@member-spec["n"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @singleton-spec["j-true"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @singleton-spec["j-false"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @singleton-spec["j-null"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @singleton-spec["j-undefined"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-raw"]{
        @members{@member-spec["raw-js"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
      @constr-spec["j-raw-holes"]{
        @members{
          @member-spec["raw-js"]
          @member-spec["fills"]
          @member-spec["width-tolerance"]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JExpr, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JExpr -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        ;; N.B. Pyret contract: (JExpr -> Any)
        
      ]
    }
  }
  
  @data-spec["JField"]{
    @variants{
      @constr-spec["j-field"]{
        @members{@member-spec["name"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "print-ugly-source"
            ;; N.B. Pyret contract: (JField, Any -> Any)
            
          ]
          @method-spec[
            "tosource"
            ;; N.B. Pyret contract: (JField -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        ;; N.B. Pyret contract: (JField -> Any)
        
      ]
    }
  }
  
  @section[#:tag "\"compiler/js-ast.arr\"_Functions"]{Functions}
  @function["string-printer"]
}