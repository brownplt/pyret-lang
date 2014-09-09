#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt" scriblib/footnote (only-in scribble/manual link))

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
      (doc ""))
    (fun-spec
      (name "string-to-code-point")
      (arity 1)
      (args ("s"))
      (doc ""))
    (fun-spec
      (name "string-from-code-point")
      (arity 1)
      (args ("code"))
      (doc ""))
    (fun-spec
      (name "string-to-code-points")
      (arity 1)
      (args ("codes"))
      (doc ""))
    (fun-spec
      (name "string-from-code-points")
      (arity 1)
      (args ("codes"))
      (doc ""))
      ))


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
  @function["string-to-code-point" #:contract (a-arrow S N)]

  @note{For strings
  that contain a single character whose code point is greater than
  @pyret{65535}, this function raises an error.
  To get multiple codes at once for a longer string (or a string with larger code points), use
  @pyret-id{string-to-code-points}.}

  Converts @pyret{s}, which must be a single-character string, to a character
  code -- a number corresponding to its unicode code point
  (@url["http://en.wikipedia.org/wiki/Code_point"]).
  

  @examples{
check:
  string-to-code-point("a") is 97
  string-to-code-point("\n") is 10
  string-to-code-point("λ") is 955
end
  }

  @function["string-to-code-points" #:contract (a-arrow S (L-of N))]

  Converts the string (of any length) to a list of code points.  Note that
  strings are encoded in such a way that some characters correspond to two code
  points (see the note in @pyret-id{string-to-code-point}).

@examples{
check:
  string-to-code-points("") is [list:]
  string-to-code-points("abc") is [list: 97, 98, 99]
  string-to-code-points("𝄞") is [list: 55348, 56606] 
end
}

  @function["string-from-code-point" #:contract (a-arrow N S)]

  @note{Code points greater than 65535 are not supported.  You must encode
  higher code points with a @link["http://en.wikipedia.org/wiki/UTF-16"
  "surrogate pair"] in combination with
  @pyret-id{string-from-code-points} and @pyret-id{string-to-code-points}.}

  Converts the code point @pyret{code} to a Pyret string.

@examples{
check:
  string-from-code-point(97) is "a"
  string-from-code-point(10) is "\n"
  string-from-code-point(955) is "λ"
end
}


  @function["string-from-code-points" #:contract (a-arrow (L-of N) S)]

  Converts from a list of code points to a Pyret string.

@examples{
check:
  string-from-code-points([list:]) is ""
  string-from-code-points([list: 97, 98, 99]) is "abc"
  string-from-code-points([list: 55348, 56606]) is "𝄞"
end
}

}
