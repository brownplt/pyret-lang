#lang scribble/base

@(require (only-in scribble/manual link))

@title[#:style '(toc)]{Getting Started}

First, the easiest place to run Pyret is @url["https://code.pyret.org"], which
runs Pyret entirely within your browser.

If you're interested in a textbook, you can try out
@link["http://papl.cs.brown.edu/2014/" "Programming and Programming Languages
(PAPL)"], which uses Pyret for most of its programming examples.

You can follow a short tour of Pyret's features to get a feel for the language,
and for a more in-depth introduction, this documentation comes with a tutorial
that you can follow to build up a simple animated game:

@(table-of-contents)

@include-section["tour.scrbl"]
@include-section["Pyret-Tutorial/lander.scrbl"]
