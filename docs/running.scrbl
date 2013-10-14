#lang scribble/manual

@(require
  racket/list
  racket/file
  "common.rkt")

@title[#:tag "s:running"]{Editing and Running Pyret}

There are several options for running Pyret.  In all cases, you should start
your Pyret file with the line:

@verbatim{
#lang pyret
}

This tells Pyret that the file is a Pyret program.

@section[#:tag "s:drracket"]{DrRacket}

You can edit Pyret files directly in DrRacket.  Make sure under Language ->
Choose Language..., "The Racket Language" is selected.  At the bottom left of
DrRacket, it should say ``Determine language from source.''  You should make
sure the top line says @tt{#lang pyret}, not @tt{#lang racket} or anything
else.

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

@subsection[#:tag "s:editors"]{Editors}

Pyret currently has modes written for Emacs and Vim.

@subsubsection[#:tag "s:emacs"]{Emacs}

The Emacs mode for Pyret is the most complete, supporting syntax highlighting
and automatic indentation.  You can get it at
@url{https://raw.github.com/brownplt/pyret-lang/master/src/emacs/pyret.el}.

To have it load when you run Emacs, save that file somewhere on your system,
and add:

@verbatim{
(if (file-exists-p "path-to-your-copy-of/pyret.el")
    (load-file "path-to-your-copy-of/pyret.el"))
}

to your @tt{~/.emacs}

@subsubsection[#:tag "s:vim"]{Vim}

The Vim mode is more spartan than the Emacs mode, only supporting syntax
highlighting, and no automatic indentation.  You can get it at
@url{https://raw.github.com/brownplt/pyret-lang/master/src/vim/pyret.vim}; save
a copy of it in your @tt{.vim/syntax/} directory.  Then add a line like:

@verbatim{
au BufReadPost *.arr set filetype=pyret
}

to your @tt{~/.vimrc}.


