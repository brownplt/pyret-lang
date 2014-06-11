#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-util.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-b-prim"
      "is-b-dict"
      "is-b-exp"
      "is-b-dot"
      "is-b-typ"
      "is-b-unknown"
      "is-e-bind")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "flatten-single-blocks"
      "merge-nested-blocks"
      "binding-handlers"
      "inline-lams"
      "letrec-visitor")
  ]
  @section[#:tag "\"compiler/ast-util.arr\"_DataTypes"]{Data types}
  @data-spec["BindingInfo"]{
    @variants{
      @constr-spec["b-prim"]{@members{@member-spec["name"]} @with-members{}}
      @constr-spec["b-dict"]{@members{@member-spec["dict"]} @with-members{}}
      @constr-spec["b-exp"]{@members{@member-spec["exp"]} @with-members{}}
      @constr-spec["b-dot"]{
        @members{@member-spec["base"] @member-spec["name"]}
        @with-members{}
      }
      @singleton-spec["b-typ"]{@with-members{}}
      @singleton-spec["b-unknown"]{@with-members{}}
    }
    @shared{}
  }
  
  @data-spec["Binding"]{
    @variants{
      @constr-spec["e-bind"]{
        @members{@member-spec["loc"] @member-spec["mut"] @member-spec["info"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/ast-util.arr\"_Functions"]{Functions}
  @function["ok-last"]
  @function["checkers"]
  @function["append-nothing-if-necessary"]
  @function["count-apps"]
  @function["bind-exp"]
  @function["bind-or-unknown"]
  @function["binding-type-env-from-env"]
  @function["binding-env-from-env"]
  @function["default-env-map-visitor" #:params (list "a" "c")]
  @function["default-env-iter-visitor" #:params (list "a" "c")]
  @function["binding-env-map-visitor"]
  @function["binding-env-iter-visitor"]
  @function["link-list-visitor"]
  @function["bad-assignments"]
  @function[
    "check-unbound"
    #:examples
    '@{
      @; let 
      @;     p = PP.surface-parse(_, "test"),
      @;     unbound1 = check-unbound(CS.no-builtins, p("x")):
      @;   unbound1.length() is 1
      @; end
      
    }
  ]
  @function["value-delays-exec-of"]
}