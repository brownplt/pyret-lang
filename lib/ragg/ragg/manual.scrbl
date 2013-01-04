#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     ragg/support
                     ragg/examples/nested-word-list))


@title{ragg: a Racket AST Generator Generator}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@section{Informal quickstart}

@(define informal-eval (make-base-eval))
@(informal-eval '(require ragg/examples/nested-word-list 
                          racket/list
                          racket/match))

Salutations!  Let's consider the following scenario: say that we're given the
following string:
@racketblock["(radiant (humble))"]


@margin-note{(... and pretend that we don't already know about the built-in
@racket[read] function.)}  How do we go about turn this kind of string into a
structured value?  That is, how would we @emph{parse} it?

We need to first consider the shape of the things we'd like to parse.  The
string above looks like a deeply nested list of words.  How might we describe
this formally?  A convenient notation to describe the shape of these things is
@link["http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form"]{Backus-Naur
Form} (BNF).  So let's try to notate the structure of nested word lists in BNF.


@nested[#:style 'code-inset]{
@verbatim{
nested-word-list: WORD
                | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

What we intend by this notation is this: @racket[nested-word-list] is either an
atomic @racket[WORD], or a parenthesized list of any number of
@racket[nested-word-list]s.  We use the character @litchar{*} to represent zero
or more repetitions of the previous thing, and we treat the uppercased
@racket[LEFT-PAREN], @racket[RIGHT-PAREN], and @racket[WORD] as placeholders
for atomic @emph{tokens}.

Here are a few examples of tokens:
@interaction[#:eval informal-eval
(require ragg/support)
(token 'LEFT-PAREN)
(token 'WORD "crunchy" #:span 7)
(token 'RIGHT-PAREN)]


Have we made progress?  At this point, we only have a BNF description in hand,
but we're still missing a @emph{parser}, something to take that description and
use it to make structures out of a sequence of tokens.


It's clear that we don't yet have a program because there's no @litchar{#lang}
line.  We should add one.  Put @litchar{#lang ragg} at the top of the BNF
description, and save it as a file called @filepath{nested-word-list.rkt}.

@filebox["nested-word-list.rkt"]{
@verbatim{
#lang ragg
nested-word-list: WORD
                | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

Now it is a proper program.  But what does it do?

@interaction[#:eval informal-eval
@eval:alts[(require "nested-word-list.rkt") (void)]
parse
]

It gives us a @racket[parse] function.  Let's investigate what @racket[parse]
does for us.  What happens if we pass it a sequence of tokens?

@interaction[#:eval informal-eval
             (define a-parsed-value
               (parse (list (token 'LEFT-PAREN "(")
                            (token 'WORD "some")
                            (token 'LEFT-PAREN "[") 
                            (token 'WORD "pig")
                            (token 'RIGHT-PAREN "]") 
                            (token 'RIGHT-PAREN ")"))))
             a-parsed-value]

Wait... that looks suspiciously like a syntax object!
@interaction[#:eval informal-eval
(syntax->datum a-parsed-value)
]


That's @racket[(some [pig])], essentially.

What happens if we pass it a more substantial source of tokens?
@interaction[#:eval informal-eval
@code:comment{tokenize: string -> (sequenceof token-struct?)}
@code:comment{Generate tokens from a string:}
(define (tokenize s)
  (for/list ([str (regexp-match* #px"\\(|\\)|\\w+" s)])
    (match str
      ["("
       (token 'LEFT-PAREN str)]
      [")"
       (token 'RIGHT-PAREN str)]
      [else
       (token 'WORD str)])))

@code:comment{For example:}
(define token-source (tokenize "(welcome (to (((ragg)) ())))"))
(define v (parse token-source))
(syntax->datum v)
]

Welcome to @tt{ragg}.







@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Introduction}

@tt{ragg} is a parsing framework with the design goal to be easy
to use.  It includes the following features:
@itemize[

@item{It provides a @litchar{#lang} for writing extended BNF grammars.
A module written in @litchar{#lang ragg} automatically generates a
parser.  The output of this parser tries to follow
@link["http://en.wikipedia.org/wiki/How_to_Design_Programs"]{HTDP}
doctrine; the structure of the grammar informs the structure of the
Racket syntax objects it generates.}

@item{The language uses a few conventions to simplify the expression of
grammars.  The first rule in the grammar is automatically assumed to be the
starting production.  Identifiers in uppercase are assumed to represent
terminal tokens, and are otherwise the names of nonterminals.}

@item{Tokenizers can be developed completely independently of parsers.
@tt{ragg} takes a liberal view on tokens: they can be strings,
symbols, or instances constructed with @racket[token].  Furthermore,
tokens can optionally provide location: if tokens provide location, the
generated syntax objects will as well.}

@item{The underlying parser should be able to handle ambiguous grammars.}
]


FILL ME IN: I NEED A LONG-RUNNING EXAMPLE OF A DSL HERE.  THE GOAL
IS TO SHOW HOW RAGG HELPS US TO MAKE OTHER #LANG'S REALLY QUICKLY.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{The language}

@subsection{Syntax and terminology}
A program in the @tt{ragg} language consists of the language line
@litchar{#lang ragg}, followed by a collection of @tech{rule}s and
@tech{line comment}s.

A @deftech{rule} is a sequence consisting of: a @tech{rule identifier}, a colon
@litchar{":"}, and a @tech{pattern}.

A @deftech{rule identifier} is an @tech{identifier} that is not in upper case.

A @deftech{token identifier} is an @tech{identifier} that is in upper case.

An @deftech{identifier} is a character sequence of letters, numbers, and
characters in @racket["-.!$%&/<=>?^_~@"].  It must not contain
@litchar{*} or @litchar{+}, as those characters are used to denote
quantification.


A @deftech{pattern} is one of the following:
@itemize[
@item{an implicit sequence of @tech{pattern}s separated by whitespace}
@item{a terminal: either a literal string or a @tech{token identifier}}
@item{a @tech{rule identifier}}
@item{a @deftech{choice pattern}: a sequence of @tech{pattern}s delimited with @litchar{|} characters.}
@item{a @deftech{quantifed pattern}: a @tech{pattern} followed by either @litchar{*} (``zero or more'') or @litchar{+} (``one or more'')}
@item{an @deftech{optional pattern}: a @tech{pattern} surrounded by @litchar{[} and @litchar{]}}
@item{an explicit sequence: a @tech{pattern} surrounded by @litchar{(} and @litchar{)}}]

A @deftech{line comment} begins with either @litchar{#} or @litchar{;} and
continues till the end of the line.


For example, in the following program:
@nested[#:style 'inset
@verbatim|{
#lang ragg
;; A parser for a silly language
sentence: verb optional-adjective object
verb: greeting
optional-adjective: ["happy" | "frumpy"]
greeting: "hello" | "hola" | "aloha"
object: "world" | WORLD
}|]

the elements @tt{sentence}, @tt{verb}, @tt{greeting}, and @tt{object} are rule
identifiers.  The third line, @litchar{sentence: verb optional-adjective
object}, is a rule whose right side is an implicit pattern sequence of three
sub-patterns.  The uppercased @tt{WORLD} is a token identifier.  The last rule in the program associates @tt{greeting} with a @tech{choice pattern}.



More examples:
@itemize[

@item{A
@link["http://hashcollision.org/ragg/examples/01-equal.rkt"]{BNF} for binary
strings that contain an equal number of zeros and ones.
@verbatim|{
#lang ragg
equal: [zero one | one zero]   ;; equal number of "0"s and "1"s.
zero: "0" equal | equal "0"    ;; has an extra "0" in it.
one: "1" equal | equal "1"     ;; has an extra "1" in it.
}|
}

@item{A @link["http://hashcollision.org/ragg/examples/baby-json.rkt"]{BNF} for
@link["http://www.json.org/"]{JSON}-like structures.
@verbatim|{
#lang ragg
json: number | string
    | array  | object
number: NUMBER
string: STRING
array: "[" [json ("," json)*] "]"
object: "{" [kvpair ("," kvpair)*] "}"
kvpair: ID ":" json
}|
}
]

The @link["https://github.com/dyoo/ragg"]{ragg github source repository}
includes
@link["https://github.com/dyoo/ragg/tree/master/ragg/examples"]{several more
examples}.



@subsection{Syntax errors}

Besides the basic syntax errors that can occur with a malformed grammar, there
are a few other classes of situations that @litchar{#lang ragg} will consider
as syntax errors.

@tt{ragg} will raise a syntax error if the grammar:
@itemize[
@item{doesn't have any rules.}

@item{has a rule with the same left hand side as any other rule.}

@item{refers to rules that have not been defined.  e.g. the
following program:
@nested[#:style 'code-inset
@verbatim|{
#lang ragg
foo: [bar]
}|
]
should raise an error because @tt{bar} has not been defined, even though
@tt{foo} refers to it in an @tech{optional pattern}.}


@item{contains a rule that has no finite derivation.  e.g. the following
program:
@nested[#:style 'code-inset
@verbatim|{
#lang ragg
infinite-a: "a" infinite-a
}|
]
should raise an error because no finite sequence of tokens will satisfy
@tt{infinite-a}.}

]

Otherwise, @tt{ragg} should be fairly tolerant and permit even ambiguous
grammars.

@subsection{Semantics}
@declare-exporting[ragg/examples/nested-word-list]

A program written in @litchar{#lang ragg} produces a module that provides a few
bindings.  The most important of these is @racket[parse]:

@defproc[(parse [source any/c #f] 
                [token-source sequence?])
         syntax?]{

Parses the sequence of tokens according to the rules in the grammar, using the
first rule as the start production.  The parse must completely consume
@racket[token-source].  See @secref{token-sources} for more details on what
kind of values are considered to be tokens.

If it succeeds, @racket[parse] will return a syntax object.
The structure of the syntax object follows the overall structure of the rules in
the BNF.  For each rule @racket[r] and its associated pattern @racket[p],
@racket[parse] generates a syntax object @racket[#'(r p-value)] where
@racket[p-value]'s structure follows a case analysis on @racket[p]:

@itemize[
@item{For implicit and explicit sequences of @tech{pattern}s @racket[p1],
      @racket[p2], ..., the corresponding values, spliced into the
      structure.}
@item{For terminals, the value associated to the token.}
@item{For @tech{rule identifier}s: the associated parse value for the rule.}
@item{For @tech{choice pattern}s: the associated parse value for one of the matching subpatterns.}
@item{For @tech{quantifed pattern}s and @tech{optional pattern}s: the corresponding values, spliced into the structure.}
]

Consequently, it's only the presence of @tech{rule identifier}s in a rule's
pattern that informs the parser to introduces nested structure into the syntax
object.


If the parse cannot be performed successfully, an instance of @racket[exn:fail:parsing] is raised.
}



It's often convenient to extract a parser for other non-terminal rules in the
grammar, and not just for the first rule.  A @tt{ragg}-generated module also
provides a form called @racket[make-rule-parser] to extract a parser for the
other non-terminals:

@defform[#:id make-rule-parser
         (make-rule-parser name)]{
Constructs a parser for the @racket[name] of one of the non-terminals
in the grammar.

For example, given the @tt{ragg} program
@filepath{simple-arithmetic-grammar.rkt}:
@filebox["simple-arithmetic-grammar.rkt"]{
@verbatim|{
#lang ragg
expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT
}|
}
the following interaction shows how to extract a parser for @racket[term]s.
@interaction[#:eval informal-eval
@eval:alts[(require "simple-arithmetic-grammar.rkt") 
                    (require ragg/examples/simple-arithmetic-grammar)]
(define term-parse (make-rule-parser term))
(define tokens (list (token 'INT 3) 
                     "*" 
                     (token 'INT 4)))
(syntax->datum (parse tokens))
(syntax->datum (term-parse tokens))

(define another-token-sequence
  (list (token 'INT 1) "+" (token 'INT 2)
        "*" (token 'INT 3)))
(syntax->datum (parse another-token-sequence))
@code:comment{Note that term-parse will break on another-token-sequence}
@code:comment{as it does not know what to do with the "+"}
(term-parse another-token-sequence)
]

}



@subsection[#:tag "token-sources"]{Token sources}

FILL ME IN PLEASE





@section{Support API}

@defmodule[ragg/support]

The @racketmodname[ragg/support] module provides functions to interact with
@tt{ragg} programs.  The most useful is the @racket[token] function, which
produces tokens to be parsed.

@defproc[(token [type (or/c string? symbol?)]
                [val any/c #f]
                [#:line line (or/c number? #f) #f]
                [#:column column (or/c number? #f) #f]
                [#:offset offset (or/c number? #f) #f]
                [#:span span (or/c number? #f) #f]
                [#:whitespace? whitespace boolean? #f]
                )
         token-struct?]{
Creates instances of @racket[token-struct]s.

The syntax objects produced by a parse will inject the value @racket[val] in
place of the token name in the grammar.

If @racket[#:whitespace?] is true, then the parser will skip over it during a
parse.}


@defstruct[token-struct ([type symbol?]
                         [val any/c]
                         [offset (or/c number? #f)]
                         [line (or/c number? #f)]
                         [column (or/c number? #f)]
                         [span (or/c number? #f)]
                         [whitespace? boolean?])
                        #:transparent]{
The token structure type.

Rather than directly using the @racket[token-struct] constructor, please use
the helper function @racket[token] to construct instances.
}




@defstruct[(exn:fail:parsing exn:fail) 
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [srclocs (listof srcloc?)])]{
The exception raised when parsing fails.
}



@defthing[current-source (parameterof any/c)]{
blah blah}

@defthing[current-parser-error-handler (parameterof any/c)]{
blah blah blah
}

@defthing[current-tokenizer-error-handler (parameterof any/c)]{
blah!
}





@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Bugs and caveats and TODOs}

[Missing test for grammars with undefined rules.]

[Missing test for grammars with repeated rules.]

[Missing explanation for ambiguous parses]

[Handle specials with distinguished SPECIAL token name]

[Larger, more comprehensive test suite]

[Missing convenient syntax for simple lexers]

[Symbols can be tokens?]

[Strings can be tokens?]


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Miscellaneous and thanks}

Thanks to Joe Politz for advice and feedback.  Also, he suggested the name
``ragg''.  Other alternatives I'd been considering were
``autogrammar'' or ``chompy''.  Thankfully, he is a better Namer than me.
Daniel Patterson provided feedback that led to @racket[make-rule-parser].


@close-eval[informal-eval]