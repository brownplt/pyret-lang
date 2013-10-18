#lang scribble/manual

@(require
  racket/list
  racket/file
  "common.rkt")

@title[#:tag "s:installing"]{Installing Pyret}

There are several options for installing Pyret.

@section[#:tag "s:requirements"]{Requirements}

You need to have installed at least Racket 5.3.4 (the current version, 5.3.6,
works great).  You can get Racket at @url{http://download.racket-lang.org/}.

@section[#:tag "s:url"]{Via DrRacket Automatic Download}

Open a new definition in DrRacket, and using File -> Install .plt File, select
the URL tab. Copy and paste the URL below into the space, and click "Okay"

@url{http://cs.brown.edu/~joe/public/pyret/pyret-current.plt}

@section[#:tag "s:plt-file"]{Via a .plt File}

Download this .plt file:

@url{http://cs.brown.edu/~joe/public/pyret/pyret-current.plt}

Then, you can install by either (your choice, don't do both):

@itemlist[
  @item{Opening DrRacket, and using File -> Install .plt File..., then selecting the file you downloaded}
  @item{Running @tt{raco setup -A pyret-current.plt} from the command line}
]

@section[#:tag "s:github"]{Via Github}

Clone this repository: @url{https://github.com/brownplt/pyret-lang}.  For
example:

@verbatim{
$ git clone https://github.com/brownplt/pyret-lang
}

Then build Pyret:

@verbatim{
$ cd pyret-lang
$ make dep
$ make
}


