#lang scribble/manual

@(require
  racket/list
  racket/file
  (only-in racket/string string-join))


@title{Pyret Language Reference}

This page has detailed information on the Pyret grammar and the behavior of its
expression forms.

@section{Statements and Expressions}

@subsection{Blocks and Binding}

@subsubsection{Blocks}

Blocks are a fundamental concept in Pyret.  They act as both a construct for
sequencing statements, and as a unit of nesting in lexical scope.


@subsubsection{Let Expressions}

Let expressions look like

@code{
x = expression
}

A 

@section{Grammar}

@(define (pycode . stx)
  (nested #:style 'code-inset
   (verbatim (string-join stx "\n"))))

@(apply pycode (rest (file->lines "../src/lang/grammar.rkt")))


