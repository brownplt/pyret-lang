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

Once this process has completed, you can run Pyret programs in DrRacket.

To run a Pyret program, change the top line to say @tt{#lang
pyret/check}, and write your program below it.  For example, you might
put this into the definitions window and run it:

@pre[
"#lang pyret/check

fun square(n):
  n * n
where:
  square(2) is 4
  square(4) is 16
  square(8) is 64
end"
]

Run this with "Ctrl-R" or by pressing the green arrow "Run" button.  You should
see, in the interactions window, a success message like

@pre[
"Looks shipshape, all 2 tests passed, mate!"
]

Once you've gotten this far, you can jump ahead to a @seclink["s:tour" "tour of
Pyret's features"].

@section[#:tag "s:cli"]{Getting Started at the Command Line}

First, make sure that the Racket binaries are in your path.  Then, check
out the @link["http://github.com/brownplt/pyret-lang" "Pyret repo"]:

@pre[
"$ git clone https://github.com/brownplt/pyret-lang.git"
]

Then, run two commands to build Pyret:

@pre[
"$ make dep
$ make"
]

Now, you can open any editor and write a Pyret program by starting it
with @tt{#lang pyret/check} as the first line.  For example, copy this
into your favorite editor, and save it in a file called “square.arr”:

@pre[
"#lang pyret/check

fun square(n):
  n * n
where:
  square(2) is 4
  square(4) is 16
  square(8) is 64
end"
]

Then, from the command line, run:

@pre[
"$ raco pyret square.arr
Looks shipshape, all 3 tests passed, mate!"
]

Pyret currently supports a few
@link["/docs/s_running.html#%28part._s~3aeditors%29" "editor modes"],
more will be added as time goes on.

Once you see this program working, move on to the @seclink["s:tour" "tour
of Pyret's features"].

@section[#:tag "s:tour"]{A Tour of Programming in Pyret}

