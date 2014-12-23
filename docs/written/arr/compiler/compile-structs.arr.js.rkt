#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/compile-structs.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-Pyret"
      "is-Bootstrap"
      "is-compile-env"
      "is-ok"
      "is-err"
      "is-wf-err"
      "is-wf-err-split"
      "is-reserved-name"
      "is-zero-fraction"
      "is-unbound-id"
      "is-unbound-var"
      "is-unbound-type-id"
      "is-unexpected-type-var"
      "is-pointless-var"
      "is-pointless-shadow"
      "is-bad-assignment"
      "is-mixed-id-var"
      "is-shadow-id"
      "is-duplicate-id"
      "is-type-id"
      "is-type-module-bindings"
      "is-builtin-id"
      "is-module-bindings")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "runtime-types"
      "standard-types"
      "runtime-builtins"
      "no-builtins"
      "minimal-builtins"
      "bootstrap-builtins"
      "standard-builtins")
  ]
  @section[#:tag "\"compiler/compile-structs.arr\"_DataTypes"]{Data types}
  @data-spec["PyretDialect"]{
    @variants{
      @singleton-spec["Pyret"]{@with-members{}}
      @singleton-spec["Bootstrap"]{@with-members{}}
    }
    @shared{}
  }
  
  @data-spec["CompileEnvironment"]{
    @variants{
      @constr-spec["compile-env"]{
        @members{@member-spec["bindings"] @member-spec["types"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["CompileResult" #:params (list "C")]{
    @variants{
      @constr-spec["ok"]{@members{@member-spec["code"]} @with-members{}}
      @constr-spec["err"]{@members{@member-spec["problems"]} @with-members{}}
    }
    @shared{}
  }
  
  @data-spec["CompileError"]{
    @variants{
      @constr-spec["wf-err"]{
        @members{@member-spec["msg"] @member-spec["loc"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["wf-err-split"]{
        @members{@member-spec["msg"] @member-spec["loc"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["reserved-name"]{
        @members{@member-spec["loc"] @member-spec["id"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["zero-fraction"]{
        @members{@member-spec["loc"] @member-spec["numerator"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["unbound-id"]{
        @members{@member-spec["id"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["unbound-var"]{
        @members{@member-spec["id"] @member-spec["loc"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["unbound-type-id"]{
        @members{@member-spec["ann"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["unexpected-type-var"]{
        @members{@member-spec["loc"] @member-spec["name"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["pointless-var"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["pointless-shadow"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["bad-assignment"]{
        @members{
          @member-spec["id"]
          @member-spec["loc"]
          @member-spec["prev-loc"]
        }
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["mixed-id-var"]{
        @members{
          @member-spec["id"]
          @member-spec["var-loc"]
          @member-spec["id-loc"]
        }
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["shadow-id"]{
        @members{
          @member-spec["id"]
          @member-spec["new-loc"]
          @member-spec["old-loc"]
        }
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
      @constr-spec["duplicate-id"]{
        @members{
          @member-spec["id"]
          @member-spec["new-loc"]
          @member-spec["old-loc"]
        }
        @with-members{
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (CompileError -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["CompileTypeBinding"]{
    @variants{
      @constr-spec["type-id"]{@members{@member-spec["id"]} @with-members{}}
      @constr-spec["type-module-bindings"]{
        @members{@member-spec["name"] @member-spec["bindings"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["CompileBinding"]{
    @variants{
      @constr-spec["builtin-id"]{@members{@member-spec["id"]} @with-members{}}
      @constr-spec["module-bindings"]{
        @members{@member-spec["name"] @member-spec["bindings"]}
        @with-members{}
      }
    }
    @shared{}
  }
}
