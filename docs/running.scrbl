#lang scribble/manual

@(require
  racket/list
  racket/file
  "common.rkt")

@title[#:tag "s:running"]{Editing and Running Pyret}

There are several options for running Pyret.  For coursework, you should start
your Pyret file with the line:

@verbatim{
#lang pyret/check
}

This tells Pyret that the file is a Pyret program, and you want to run all the
check blocks in it when it is run.

@section[#:tag "s:drracket"]{DrRacket}

You can edit Pyret files directly in DrRacket.  Make sure under Language ->
Choose Language..., "The Racket Language" is selected.  At the bottom left of
DrRacket, it should say ``Determine language from source.''  You should make
sure the top line says @tt{#lang pyret/check}, not @tt{#lang racket} or
anything else.  @bold{Important}: This is a change from previous instructions
which said to use @tt{#lang pyret}.  Using @tt{#lang pyret} won't run
@tt{check:} and @tt{where:} blocks in DrRacket.

You can run Pyret programs in DrRacket with the "Run" button (green arrow), or
with Ctrl-R.  You can interact with functions you've defined in the
interactions prompt.

@section[#:tag "s:command-line"]{Text Editor and Command Line}

You can edit Pyret files using any editor you like, and run them from the
command line.

@subsection[#:tag "s:cl-run"]{Running from the Command Line}

You can run Pyret files with:

@verbatim{
$ raco pyret <pyret-file-here>
}

This will run all the check blocks in the file you provide.  If the Pyret
program in that file imports any other files, check blocks in those files will
@emph{not} be run.

You can run @tt{raco help pyret} to see more options.

If you get an error like @tt{-bash: raco: command not found}, you probably need
to add the Racket binaries to your path.  The easiest way to do this is usually
to edit (or create) a file called @tt{.bashrc} in your home or user directory.
First, find where Racket is installed (possibly in Applications/ in OSX, or in
your home directory if you installed via the Racket installer), and copy that
path; for example:

@verbatim{
/home/joe/racket/bin/
}

Then, you should add a line to the end of @tt{.bashrc} like:

@verbatim{
PATH=${PATH}:/home/joe/racket/bin/
}

This will cause the @emph{environment variable} called @tt{$PATH} to include
the path to Racket on your machine, and it will be set this way each time you
open a new terminal.  (Your terminal uses the value of @tt{$PATH} to decide
which directories to look in for commands to run.)

@subsection[#:tag "s:editors"]{Editors}

Pyret currently has modes written for Emacs and Vim.

@subsubsection[#:tag "s:emacs"]{Emacs}

The Emacs mode for Pyret is the most complete, supporting syntax highlighting
and automatic indentation.  You can get it at
@url{https://raw.github.com/brownplt/pyret-lang/master/src/emacs/pyret.el}.

To have it load when you run Emacs, save that file somewhere (the below assumes
you saved it in a directory called @tt{/path/to/pyret/emacs/}), and add the
following to your @tt{~/.emacs}:

@verbatim{
;; Pyret
(ignore-errors
  (add-to-list 'load-path (expand-file-name "/path/to/pyret/emacs/"))
  (require 'pyret)
  (add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
  (add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8)))
}

@subsubsection[#:tag "s:vim"]{Vim}

The Vim mode is more spartan than the Emacs mode, only supporting syntax
highlighting, and no automatic indentation.  You can get it at
@url{https://raw.github.com/brownplt/pyret-lang/master/src/vim/pyret.vim}; save
a copy of it in your @tt{.vim/syntax/} directory.  Then add a line like:

@verbatim{
au BufReadPost *.arr set filetype=pyret
}

to your @tt{~/.vimrc}.


