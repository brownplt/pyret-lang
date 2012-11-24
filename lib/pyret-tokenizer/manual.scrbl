#lang scribble/manual
@(require planet/scribble
          scribble/eval
          (for-label (this-package-in main)
                     racket/base
                     racket/sequence))

@(define myeval (make-base-eval))
@(myeval '(require racket/sequence))
@(myeval '(require racket/list))

@title{pyret-tokenizer: a translation of Python's @tt{tokenize.py}
library for Racket, forked and hacked to provide support for Pyret}
@author+email["Joe Gibbs Politz" "joe@cs.brown.edu"]

This is a fairly close translation of the
@link["http://hg.python.org/cpython/file/2.7/Lib/tokenize.py"]{@tt{tokenize.py}}
library from @link["http://python.org"]{Python}.

The main function, @racket[generate-tokens], consumes an input port
and produces a sequence of tokens.

For example:

@interaction[#:eval myeval
(require (planet jpolitz/pyret-tokenizer))
(define sample-input (open-input-string "def d22(a, b, c=2, d=2, *k): pass"))
(define tokens 
  (generate-tokens sample-input))
(for ([t tokens])
  (printf "~s ~s ~s ~s\n" (first t) (second t) (third t) (fourth t)))
]

@section{API}
@defmodule/this-package[main]

@defproc[(generate-tokens [inp input-port]) (sequenceof (list/c symbol? string? (list/c number? number?) (list/c number? number?) string?))]{
Consumes an input port and produces a sequence of tokens.

Each token is a 5-tuple consisting of:
@itemize[#:style 'ordered

@item{token-type: one of the following symbols: 
@racket['NAME], @racket['NUMBER], @racket['STRING],
@racket['OP], @racket['COMMENT], @racket['NL],
@racket['NEWLINE], @racket['DEDENT], @racket['INDENT],
@racket['ERRORTOKEN], or @racket['ENDMARKER].  The only difference between
@racket['NEWLINE] and @racket['NL] is that @racket['NEWLINE] will only occurs
if the indentation level is at @racket[0].}

@item{text: the string content of the token.}

@item{start-pos: the line and column as a list of two numbers}

@item{end-pos: the line and column as a list of two numbers}

@item{current-line: the current line that the tokenizer is on}
]

The last token produced, under normal circumstances, will be
@racket['ENDMARKER].


If a recoverable error occurs, @racket[generate-tokens] will produce
single-character tokens with the @racket['ERRORTOKEN] type until it
can recover.

Unrecoverable errors occur when the tokenizer encounters @racket[eof]
in the middle of a multi-line string or statement, or if an
indentation level is inconsistent.  On an unrecoverable error,
@racket[generate-tokesn] will raise an @racket[exn:fail:token] or
@racket[exn:fail:indentation] error.


@defstruct[(exn:fail:token exn:fail) ([loc (list/c number number)])]{
Raised when @racket[eof] is unexpectedly encounted.
@racket[exn:fail:token-loc] holds the start position.
}

@defstruct[(exn:fail:indentation exn:fail) ([loc (list/c number number)])]{
Raised when the indentation is inconsistent.
@racket[exn:fail:indentation-loc] holds the start position.
}

}







@section{Translator Comments}

The translation is a fairly direct one; I wrote an
@link["https://github.com/dyoo/while-loop"]{auxiliary package} to deal
with the @racket[while] loops, which proved invaluable during the
translation of the code.  It may be instructive to compare the
@link["https://github.com/dyoo/python-tokenizer/blob/master/python-tokenizer.rkt"]{source}
here to that of
@link["http://hg.python.org/cpython/file/2.7/Lib/tokenize.py"]{tokenize.py}.


Here are some points I observed while doing the translation:

@itemize[

@item{Mutation pervades the entirety of the tokenizer's main loop.
The main reason is because @racket[while] has no return type and
doesn't carry variables around; the @racket[while] loop communicates
values from one part of the code to others through mutation, often in
wildly distant locations.}


@item{Racket makes a syntactic distinction between variable definition
(@racket[define]) and mutation (@racket[set!]).  I've had to deduce
which variables were intended to be temporaries, and hopefully I
haven't induced any errors along the way.}

@item{In some cases, Racket has finer-grained type distinctions than
Python.  Python does not use a separate type to represent individual
characters, and instead uses a length-1 string.  In this translation,
I've used characters where I think they're appropriate.}

@item{Most uses of raw strings in Python can be translated to
uses of the
@link["http://docs.racket-lang.org/scribble/reader-internals.html#(mod-path._at-exp)"]{at-exp}
reader.}


@item{Generators in Racket and in Python are pretty similar, though
the Racket documentation can do a better job in documenting them.

When dealing with generators in Racket, what one really wants to
usually produce is a generic sequence.  For that reason, the
Racket documentation really needs to place more emphasis in 
@racket[in-generator], not the raw @racket[generator] form.}


@item{Python heavily overloads the @tt{in} operator.  Its expressivity
makes it easy to write code with it.  On the flip side, its
flexibility makes it a little harder to know what it actually means.}

@item{Regular expressions, on the whole, match
@; Yeah, that's a pun.  I had to get that in somewhere... :)
well between the two
languages.  Minor differences in the syntax are potholes: Racket's
regular expression matcher does not have an implicit @emph{begin}
anchor, and Racket's regexps are more sensitive to escape characters.

Python's regexp engine returns a single match object that can support
different operators.  Racket, on the other hand, requires the user to
select between getting the position of the match, with
@racket[regexp-match-positions], or getting the textual content with
@racket[regexp-match].}
]
