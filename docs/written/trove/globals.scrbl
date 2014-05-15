#lang scribble/base
@(require "../../scribble-api.rkt")

@docmodule["<global>"]{
   @function["Nothing" #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))]{
      The type of the value @tt{nothing}
   }
   @function["Bool" #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))]{
      The type of the values @tt{true} and @tt{false}
   }
   @function["String" #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))]{
      @para{The type of string values.  Strings can be written @tt{@literal{"}text@literal{"}} or @tt{@literal{'}text@literal{'}}, 
      and may not span multiple lines.  Allowed escapes are @tt{\n} (newline), 
      @tt{\r} (carriage return), @tt{\t} (tab), @tt{\[0-8]{1,3}} for octal escapes, 
      @tt{\x[0-9a-fA-F]{1,2}} for single-byte hexadecimal escapes, or @tt{\u[0-9a-fA-F]{1,4}} 
      for double-byte Unicode escapes.  Additionally, @tt{@literal{\"}} escapes a double-quote within a 
      double-quoted string, and @tt{@literal{\'}} escapes a single quote within a single-quoted string.}
      
      @para{Multi-line string literals may be written @tt{@literal{```} text @literal{```}}.  The same escape sequences
      are valid as for single-line strings.  Leading and trailing whitespace of the string are
      trimmed.}
   }
   @function["Number" #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))]{
      The type of numbers.  Numbers are @itemlist[
         @item{Integers, e.g. @tt{345} or @tt{-321}}
         @item{Rationals, e.g. @tt{355/113} or @tt{-321/6789}}
         @item{Inexact numbers, e.g. @tt{123.4567} or @tt{-0.987}}
         @item{Complex numbers, e.g. @tt{1+2i}, where the real and imaginary components may be integers, rationals or inexact numbers}
      ]
   }  
}