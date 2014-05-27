#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/js-ast.arr\""]{
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
        @members{
          @member-spec[
            "stmts"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt")))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        #:contract
        (a-arrow
          (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          "Any")
      ]
    }
  }
  @data-spec["JStmt"]{
    @variants{
      @constr-spec["j-var"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "rhs"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-if1"]{
        @members{
          @member-spec[
            "cond"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "consq"
            #:contract (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-if"]{
        @members{
          @member-spec[
            "cond"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "consq"
            #:contract (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          ]
          @member-spec[
            "alt"
            #:contract (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-return"]{
        @members{
          @member-spec[
            "expr"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-try-catch"]{
        @members{
          @member-spec[
            "body"
            #:contract (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
          ]
          @member-spec[
            "exn"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "catch"
            #:contract (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-throw"]{
        @members{
          @member-spec[
            "exp"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
      @constr-spec["j-expr"]{
        @members{
          @member-spec[
            "expr"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        #:contract
        (a-arrow (a-id "JStmt" (xref "\"compiler/js-ast.arr\"" "JStmt")) "Any")
      ]
    }
  }
  @data-spec["JBinop"]{
    @variants{
      @singleton-spec["j-plus"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-minus"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-times"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-divide"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-and"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-or"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-lt"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-leq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-gt"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-geq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-eq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-equals"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-neq"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-nequals"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "print-ugly-source"
        #:contract
        (a-arrow
          (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
          "Any"
          "Any")
      ]
      @method-spec[
        "tosource"
        #:contract
        (a-arrow
          (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
          "Any")
      ]
    }
  }
  @data-spec["JUnop"]{
    @variants{
      @singleton-spec["j-incr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-decr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-postincr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-postdecr"]{
        @with-members{
          @method-spec[
            "to-ugly-source"
            #:contract
            (a-arrow
              (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "print-ugly-source"
        #:contract
        (a-arrow
          (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
          "Any"
          "Any")
      ]
      @method-spec[
        "tosource"
        #:contract
        (a-arrow (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop")) "Any")
      ]
    }
  }
  @data-spec["JExpr"]{
    @variants{
      @constr-spec["j-parens"]{
        @members{
          @member-spec[
            "exp"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-unop"]{
        @members{
          @member-spec[
            "exp"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "op"
            #:contract (a-id "JUnop" (xref "\"compiler/js-ast.arr\"" "JUnop"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-binop"]{
        @members{
          @member-spec[
            "left"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "op"
            #:contract (a-id "JBinop" (xref "\"compiler/js-ast.arr\"" "JBinop"))
          ]
          @member-spec[
            "right"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-fun"]{
        @members{
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "body"
            #:contract (a-id "JBlock" (xref "\"compiler/js-ast.arr\"" "JBlock"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-app"]{
        @members{
          @member-spec[
            "func"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr")))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-method"]{
        @members{
          @member-spec[
            "obj"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "meth"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr")))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-ternary"]{
        @members{
          @member-spec[
            "test"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "consq"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "altern"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-assign"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "rhs"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-bracket-assign"]{
        @members{
          @member-spec[
            "obj"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "rhs"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-dot-assign"]{
        @members{
          @member-spec[
            "obj"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "rhs"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-dot"]{
        @members{
          @member-spec[
            "obj"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-bracket"]{
        @members{
          @member-spec[
            "obj"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
          @member-spec[
            "field"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-list"]{
        @members{
          @member-spec[
            "multi-line"
            #:contract (a-id "Boolean" (xref "<global>" "Boolean"))
          ]
          @member-spec[
            "elts"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr")))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-obj"]{
        @members{
          @member-spec[
            "fields"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JField" (xref "\"compiler/js-ast.arr\"" "JField")))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-id"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-str"]{
        @members{
          @member-spec[
            "s"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-num"]{
        @members{
          @member-spec[
            "n"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-true"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-false"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-null"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @singleton-spec["j-undefined"]{
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-raw"]{
        @members{
          @member-spec[
            "raw-js"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
      @constr-spec["j-raw-holes"]{
        @members{
          @member-spec[
            "raw-js"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "fills"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr")))
          ]
          @member-spec["width-tolerance" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        #:contract
        (a-arrow (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr")) "Any")
      ]
    }
  }
  @data-spec["JField"]{
    @variants{
      @constr-spec["j-field"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "value"
            #:contract (a-id "JExpr" (xref "\"compiler/js-ast.arr\"" "JExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "print-ugly-source"
            #:contract
            (a-arrow
              (a-id "JField" (xref "\"compiler/js-ast.arr\"" "JField"))
              "Any"
              "Any")
          ]
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "JField" (xref "\"compiler/js-ast.arr\"" "JField"))
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "to-ugly-source"
        #:contract
        (a-arrow
          (a-id "JField" (xref "\"compiler/js-ast.arr\"" "JField"))
          "Any")
      ]
    }
  }
  @section[#:tag "\"compiler/js-ast.arr\"_Functions"]{Functions}
  @function["string-printer" #:contract (a-arrow "Any")]
  @function[
    "is-j-block"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-if1"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-if"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-return"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-try-catch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-throw"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-expr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-plus"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-minus"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-times"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-divide"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-and"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-or"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-lt"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-leq"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-gt"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-geq"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-eq"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-equals"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-neq"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-nequals"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-incr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-decr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-postincr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-postdecr"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-parens"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-unop"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-binop"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-fun"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-method"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-ternary"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-assign"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-bracket-assign"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-dot-assign"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-dot"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-bracket"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-list"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-obj"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-str"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-num"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-true"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-false"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-null"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-undefined"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-raw"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-raw-holes"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-j-field"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}