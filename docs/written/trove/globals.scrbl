#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@docmodule["<global>" #:noimport #t]{
  @section[#:tag "globals_DataTypes"]{Built-in Types}
   @data-spec["Boolean"]{
     @nested{
       The type of the values @tt{true} and @tt{false}.  For example:

       @pyret-block{
         x :: Boolean = true
         x :: Boolean = 52 # an error

         fun f(x) -> Boolean:
           if x: 1
           else: 0
           end
         end
         f(2) # also an error, because f does not evaluate to a Boolean
       }
     }
   }
   @data-spec["String"]{
     @para{
       The type of string values
     }

     @subsection{String Functions}

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
   @data-spec["Number"]{
     @para{
       The type of number values
     }
     @subsection{Number Functions}
  @function["nums-equal" #:contract (a-arrow N N B)]{

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
   @data-spec["Nothing"]{
     @para{
       The type of the special value @tt{nothing}
     }
   }
   @data-spec["Function"]{
     @para{
       The type of all function values
     }
   }
   @data-spec["Method"]{
     @para{
       The type of all method values
     }
   }
   @data-spec["RawArray"]{
     @para{
       The type of raw arrays: can be parameterized by type (as in @tt{RawArray<String>})
     }
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
   @data-spec["Object"]{
     @para{
       The type of all objects
     }
   }

  @section[#:tag "globals_Functions"]{Functions}

   @function["is-nothing" #:contract (a-arrow "Any" (a-id "Booleanean" (xref "<global>" "Boolean")))]{
      The type of the value @tt{nothing}
   }
   @function["is-boolean" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]{
      Returns @tt{true} for @tt{true} and @tt{false}, and @tt{false} for all other values.
   }
   @function["is-string" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]{
      @para{Returns true for strings, false for non-strings.  Strings can be written @tt{@literal{"}text@literal{"}} or @tt{@literal{'}text@literal{'}}, 
      and may not span multiple lines.  Allowed escapes are @tt{\n} (newline), 
      @tt{\r} (carriage return), @tt{\t} (tab), @tt{\[0-8]{1,3}} for octal escapes, 
      @tt{\x[0-9a-fA-F]{1,2}} for single-byte hexadecimal escapes, or @tt{\u[0-9a-fA-F]{1,4}} 
      for double-byte Unicode escapes.  Additionally, @tt{@literal{\"}} escapes a double-quote within a 
      double-quoted string, and @tt{@literal{\'}} escapes a single quote within a single-quoted string.}
      
      @para{Multi-line string literals may be written @tt{@literal{```} text @literal{```}}.  The same escape sequences
      are valid as for single-line strings.  Leading and trailing whitespace of the string are
      trimmed.}
   }
   @function["is-number" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]{
      Returns true for numbers, false for non-numbers.  Numbers are @itemlist[
         @item{Integers, e.g. @tt{345} or @tt{-321}}
         @item{Rationals, e.g. @tt{355/113} or @tt{-321/6789}}
         @item{Inexact numbers, e.g. @tt{123.4567} or @tt{-0.987}}
         @item{Complex numbers, e.g. @tt{1+2i}, where the real and imaginary components may be integers, rationals or inexact numbers}
      ]
   }  
   @function["is-function" #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))]{
      Returns true for functions, false for non-functions
      ]
   }  


}
