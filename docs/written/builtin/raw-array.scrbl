
#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "raw-arrays"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "RawArray")
      (variants)
      (shared))
    (fun-spec
      (name "raw-array-of")
      (arity 2)
      (args ("value" "count"))
      (doc ""))
    (fun-spec
      (name "raw-array-get")
      (arity 2)
      (args ("array" "index"))
      (doc ""))
    (fun-spec
      (name "raw-array-set")
      (arity 3)
      (args ("array" "index" "new-value"))
      (doc ""))
    (fun-spec
      (name "raw-array-length")
      (arity 1)
      (args ("array"))
      (doc ""))
    (fun-spec
      (name "raw-array-to-list")
      (arity 1)
      (args ("array"))
      (doc ""))
    (fun-spec
      (name "raw-array-fold")
      (arity 4)
      (args ("f" "init" "array" "start-index"))
      (doc ""))
))


@docmodule["raw-arrays" #:noimport #t]{
   @type-spec["RawArray" (list)]

   The type of raw array values.  Raw arrays are a primitive datastructure
   that allows for lookup and (mutable) update by non-negative integers.

   Raw arrays are used in the interface to constructor functions like
   @pyret{[constr: e1, e2, ...]}, as the way of bundling up the values from
   @pyret{e1}, @pyret{e2}, etc. to pass them to @pyret{constr.make}.

     @section{RawArray Functions}

  @function["raw-array-of" #:contract (a-arrow "a" N (RA-of "a"))]{

  }
  @function["raw-array-get" #:contract (a-arrow (RA-of "a") N "a")]{

  }
  @function["raw-array-set" #:contract (a-arrow (RA-of "a") N "a" (RA-of "a"))]{

  }
  @function["raw-array-length" #:contract (a-arrow (RA-of "a") N)]{

  }
  @function["raw-array-to-list" #:contract (a-arrow (RA-of "a") (L-of "a"))]{

  }
  @function["raw-array-fold" #:contract (a-arrow (a-arrow "b" "a" N) "b" (RA-of "a") N "b")]{

  }

}
