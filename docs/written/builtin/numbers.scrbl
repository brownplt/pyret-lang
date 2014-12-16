#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "numbers"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Number")
      (variants)
      (shared))
  (fun-spec
    (name "num-equal")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-max")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-min")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-abs")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-cos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-asin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-acos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-atan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-modulo")
    (arity 2)
    (args ("n" "divisor"))
    (doc ""))
  (fun-spec
    (name "num-truncate")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqrt")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqr")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-ceiling")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-floor")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-round")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-log")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-exp")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-expt")
    (arity 2)
    (args ("base" "exponent"))
    (doc ""))
  (fun-spec
    (name "num-exact")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-integer")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-fixnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tostring")
    (arity 1)
    (args ("n"))
    (doc ""))
))

@docmodule["numbers" #:noimport #t #:friendly-title "Numbers"]{
   @type-spec["Number" (list)]
     @para{
       The type of number values
     }
     @section{Number Functions}
  @function["num-equal" #:contract (a-arrow N N B)]{

  }
  @function["num-max" #:contract (a-arrow N N N)]{

  }
  @function["num-min" #:contract (a-arrow N N N)]{

  }
  @function["num-abs" #:contract (a-arrow N N)]{

  }
  @function["num-sin" #:contract (a-arrow N N)]{

  }
  @function["num-cos" #:contract (a-arrow N N)]{

  }
  @function["num-tan" #:contract (a-arrow N N)]{

  }
  @function["num-asin" #:contract (a-arrow N N)]{

  }
  @function["num-acos" #:contract (a-arrow N N)]{

  }
  @function["num-atan" #:contract (a-arrow N N)]{

  }
  @function["num-modulo" #:contract (a-arrow N N N)]{

  }
  @function["num-truncate" #:contract (a-arrow N N)]{

  }
  @function["num-sqrt" #:contract (a-arrow N N)]{

  }
  @function["num-sqr" #:contract (a-arrow N N)]{

  }
  @function["num-ceiling" #:contract (a-arrow N N)]{

  }
  @function["num-floor" #:contract (a-arrow N N)]{

  }
  @function["num-round" #:contract (a-arrow N N)]{

  }
  @function["num-log" #:contract (a-arrow N N)]{

  }
  @function["num-exp" #:contract (a-arrow N N)]{

  }
  @function["num-expt" #:contract (a-arrow N N N)]{

  }
  @function["num-exact" #:contract (a-arrow N N)]{

  }
  @function["num-is-integer" #:contract (a-arrow N B)]{

  }
  @function["num-is-fixnum" #:contract (a-arrow N B)]{

  }
  @function["num-tostring" #:contract (a-arrow N S)]{

  }

}
