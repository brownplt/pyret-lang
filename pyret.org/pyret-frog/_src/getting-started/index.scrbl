#lang scribble/manual

@; This is a Scribble comment.

@(require "../../lib/bootscrbl.rkt")

@title{Getting Started}

Pyret runs on top of Racket, so you'll first need to install Racket from:

@url{http://download.racket-lang.org/}

Then, choose how you'd like to proceed:

@seclink["s:ide"]{
I want an IDE and a REPL and click-to-install.
}

@seclink["s:cli"]{
I want to use my favorite editor and the command line.
}

@section[#:tag "s:ide"]{Getting Started with an IDE}

Open DrRacket, which comes with the Racket installation.  It has an icon that
looks like this:

@image["img/plt-logo-red-shiny.png"]

Then, use File -> Install .plt File, select the Web tab, copy paste the URL
below into the space, and click "OK"

@url{http://pyret.org/download/pyret.plt}

@image["img/install-via-url.png"]


@section[#:tag "s:cli"]{Getting Started at the Command Line}

