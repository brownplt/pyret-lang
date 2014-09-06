#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "strings"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "String")
      (variants)
      (shared))
    (fun-spec
      (name "strings-equal")
      (arity 2)
      (args ("s1" "s2"))
      (doc ""))
    (fun-spec
      (name "string-contains")
      (arity 2)
      (args ("string-to-search" "string-to-find"))
      (doc ""))
    (fun-spec
      (name "string-append")
      (arity 2)
      (args ("beginning" "end"))
      (doc ""))
    (fun-spec
      (name "string-length")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-tonumber")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-repeat")
      (arity 2)
      (args ("s" "n"))
      (doc ""))
    (fun-spec
      (name "string-substring")
      (arity 3)
      (args ("s" "start" "end"))
      (doc ""))
    (fun-spec
      (name "string-replace")
      (arity 3)
      (args ("original-string" "string-to-find" "replacement-string"))
      (doc ""))
    (fun-spec
      (name "string-split")
      (arity 2)
      (args ("original-string" "string-to-split-on"))
      (doc ""))
    (fun-spec
      (name "string-char-at")
      (arity 2)
      (args ("s" "n"))
      (doc ""))
    (fun-spec
      (name "string-toupper")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-tolower")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-explode")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-index-of")
      (arity 2)
      (args ("original-string" "string-to-find"))
      (doc ""))))


@docmodule["strings" #:noimport #t #:friendly-title "Strings"]{

@type-spec["String" (list)]

The type of string values

@section{String Functions}

  @function["strings-equal" #:contract (a-arrow S S B)]{

  }
  @function["string-contains" #:contract (a-arrow S S B)]{

  }
  @function["string-append" #:contract (a-arrow S S S)]{

  }
  @function["string-length" #:contract (a-arrow S N)]{

  }
  @function["string-tonumber" #:contract (a-arrow S N)]{

  }
  @function["string-repeat" #:contract (a-arrow S N S)]{

  }
  @function["string-substring" #:contract (a-arrow S N N S)]{

  }
  @function["string-replace" #:contract (a-arrow S S S S)]{

  }
  @function["string-split" #:contract (a-arrow S S)]{

  }
  @function["string-char-at" #:contract (a-arrow S N S)]{

  }
  @function["string-toupper" #:contract (a-arrow S S)]{

  }
  @function["string-tolower" #:contract (a-arrow S S)]{

  }
  @function["string-explode" #:contract (a-arrow S (L-of S))]{

  }
  @function["string-index-of" #:contract (a-arrow S S N)]{

  }

}
