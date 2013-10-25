#lang scribble/manual

@(require racket/date scribble/core scribble/html-properties scribble/text)
@(require "../../lib/bootscrbl.rkt")

@(define (two-langs header words other-lang pyret-code other-code)
  (define other-block
    @col[4]{
      @bold[other-lang]
      @pre[other-code]
    })
  (define pyret-block
    @col[4]{
      @bold{Pyret}
      @pre[pyret-code]
    })
  (define description-block
    @col[4]{
      @bold[header]
      @words
    })

  @container{
    @row{
      @description-block
      @other-block
      @pyret-block
    }
  })

@title{Pyret's Design Principles}

@section{Pyret is For Programming Education}

Though we are making progress along multiple dimensions, creating a
pleasant and pedagogically sound language will always take priority in
what we make public. Therefore, Pyret users in education never need
worry that the language will gradually grow more and more warts to the
point of unusability.

Pyret is a @emph{lightweight} language. Its syntax is
lightweight---there are no class declarations, access control
specifications, or type definitions that get in the way of you writing
your first program.

Pyret recognizes that students must learn to add @emph{annotations} to
their programs. However, opinions differ on when this should
begin. Pyret takes a @emph{gradual} philosophy, accommodating a
variety of learning and teaching styles. Our continuing work on
specifications (@(seclink "s:specs" "Specification-Enriched")) will let you grow
the richness of annotations on demand.

@(two-langs "Numbers" @list{Pyret has @emph{numbers}} "Java"
"# this is true
((1 / 3) * 3) == 1"
"// this is not true
((1 / 3) * 3) == 1"
)


@(two-langs "Structured Data"
@list{
Being able to describe @emph{data} well is central to designing and
structuring programs. Pyret offers elegant mechanisms for writing data
definitions without the overhead of classes.
}
"Python"
"data BinTree:
  | leaf
  | node(v, l, r)
end"
"blurg")

@emph{Testing} is a central component of all modern programming. Pyret
integrates convenient syntactic support for writing tests so that
testing becomes a natural part of software development.

@(two-langs "Testing" "" "Python"
"fun square(x):
  x * x
where:
  square(10) is 100
  square(-1) is 1
end"
"def square(x):
  return x * x
...")

Any modern programming language needs to run seamlessly on the
@emph{Web}. One of Pyret's native execution environments is the
browser, though you can also run it on a desktop.

@emph{Indentation} will matter, but not necessarily in the way you
think. We believe well-indented programs are vital to
readability. However, rather than allowing indentation to determine
meaning---e.g., if you indent a line a few more spaces, it suddenly
becomes part of a loop rather than a statement beyond the end of
it---in Pyret, meaning will determine indentation: that is, there will
be a canonical indentation scheme. This, for instance, means you can
safely paste code from the Web, or from email, into the desired place,
and let the language's reindentation tool indent that fragment
correctly.

Inspired by Racket, Pyret will have @emph{language levels} or
@emph{dialects}. The student levels will be feature-rich and very
stable; we will also offer capability-safe and experimental dialects.

Pyret will come with reasonable @emph{batteries included}. We know, we
like to write programs, too. Indeed, we've been doing all our
programming in Pyret since it's been ready to run.

A great deal of motivational content for students depends on being
able to quickly and easily write @emph{interactive}
programs. Indeed, curricula such as 
@(hyperlink "http://www.bootstrapworld.org/" "Bootstrap")
crucially depend on this. Therefore, Pyret offers convenient,
lightweight support for writing interactive programs that does not
require students to learn complex APIs or control
operators. Furthermore, our style of interactive programming lends
itself naturally to testing.

@(two-langs "" "" "Java"
"big-bang(\"Press a key!\", {
  on-key: fun(w, k): k end,
  to-draw: fun(w):
    text(\"You last pressed \" + w, 12, \"blue\")
  end
})"
"...")

@emph{Functional programming} is no longer an exotic evolutionary
spur; rather, it is now a central component in a programmer's
toolset. Furthermore, programming functionally relates well to
specification and improving performance. Pyret is therefore designed
to support functional programming well from the start.

@(two-langs "" "" "Python"
"fun square(n):
  n * n
end"
"def square(n):
  return n * n")

Simple @emph{objects} are now a well-understood and beneficial program
structuring mechanism. Pyret therefore supports objects without
imposing religious beliefs about classes, inheritance, and so on.

Pyret's knowledge is @emph{portable}.  When you transition to other
languages, most of your knowledge of programming will come along with
you. Of course, you may need to work harder to write certain parts of
your programs in subsequent languages (e.g., data definitions in
Python, test cases in OCaml, types in Racket)---if any other language
were just as convenient, Pyret wouldn't need to exist at
all! Nevertheless, after programming in Pyret, you will feel frissons
of familiarity in every part of the linguistic menagerie, from
serpents to dromedaries.


@section[#:tag "Specification-Enriched"]{Pyret Will be Specification-Enriched}

One of our secret goals is to bring the joyous world of rich program
specification to the power of not only scripting programmers but even
to middle-school children. After all, the mark of a great programmer
is not just how much they program, but also how much they can say
@emph{about} their programs. As a starting point, therefore, Pyret
will have (gradual) typing with a conventional polymorphic type
systems with local inference. We are also interested in adding effect
annotations to better support capability-style reasoning.

Rather than simply slapping on refinement types or dependent types,
however, we are currently at work designing specification mechanisms
that have good @emph{ergonomics}; in particular, mechanisms that
should feel familiar and comfortable to even a scripting programmer
untrained in the delights of mathematical logic. We are looking, for
instance, at ways in which testing oracles can be harnessed for this
purpose. More as it happens.

@section[#:tag "Scripting-Rebooted"]{Pyret is Scripting Rebooted}

Over the past few years, we've spent a 
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/gsk-essence-javascript/"
            "lot")
of time (
and
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/pclpk-s5-semantics/"
            "then some"),
and
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/pmmwplck-python-full-monty/"
            "even more"))
investigating the deep structure of scripting languages. What we've
learned is that the power of scripting comes at a very, very deep
cost: defining a correct version of even simple variable renaming is
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/pmmwplck-python-full-monty/"
            "comically hard") (see appendix 2 on the last page),
and building a type checker is a
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/gsk-flow-typing-theory/"
            "multi-year"),
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/pgk-sem-type-fc-member-name/"
            "multi-part"),
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/lpgk-tejas-type-sys-js/"
            "non-trivial task").

Therefore, it's no surprise that programming @emph{environments} can
offer so much more to static languages like Java than to dynamic ones
like Python. At the same time, we've been finding interesting patterns
that exploit the innate reflective power of objects-as-dictionaries to
define several interesting libraries in Python, Ruby, JavaScript, and
so on. So the problem is a classic design trade-off between
flexibility and sanity.

In Pyret, we are trying to find a pleasant middle-ground that
harnesses most of the power of scripting languages without sacrificing
even simple, evidently desirable properties such as static scoping. In
particular, observe that treating objects as dictionaries is harmless
and easy; it is treating dictionaries as objects that leads to
difficulties, because the static (programming) environment is now at
the mercy of dynamic operations that it cannot properly
anticipate. Therefore, Pyret will offer ceremonies to promote
dictionaries to objects, and programming environments and other
programmer tools can exploit these ceremonies to provide reliable
information to the developer.

@section[#:tag "Other-Novelties"]{Other Novelties}

In addition to reexamining the foundations of scripting,
we are looking to grow the language
in other interesting ways. View this mostly as a wish-list, and also
an invitation for outsiders to contribute! Some of the design
areas in which we are wrestling with options include:

@itemlist[

  @item{@emph{Truly great error messages.} Comprehensible, actionable error
  messages are crucial for beginners, and this is a topic we
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/mfk-measur-effect-error-msg-novice-sigcse/"
            "care about")
@(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/mfk-mind-lang-novice-inter-error-msg/"
            "a great deal").  As we move forward, each new feature needs to
  make this a priority.}


  @item{@emph{Defining lightweight processes.} Building complex systems such as
  interactive evaluators, programming environments, load-balancing Web
  servers, and so on requires
  @(hyperlink  "http://cs.brown.edu/~sk/Publications/Papers/Published/ffkf-mred/"
	       "lightweight, process-like abstractions")
  in the language.}

  @item{@emph{Replacing exceptions with messaging.} Once we have
  processes, we hope to explore different ways of handling failure
  behavior in programs.}

  @item{@emph{Linguistic support for debugging.} Debugging is rarely a
  first-class concern in programs; therefore, debugging ends up
  introducing a separation between the language and some ill-defined
  environment that surrounds it. Ironically, much effort then goes
  into (poorly) restoring lost information from outside the
  program. Can we usefully alter this balance?}

  @item{@emph{Avoiding callbacks in reactive systems.} Numerous people
  have noted that callbacks lead to poor program structure, and some
  have built entire languages around the problem: for instance,
  @(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/ck-frtime/"
	      "FrTime") 
  and
  @(hyperlink "http://cs.brown.edu/~sk/Publications/Papers/Published/mgbcgbk-flapjax/"
	      "Flapjax"). However, our experience with these
  languages suggests that living entirely in the reactive world is
  both difficult and undesirable. We are therefore hoping to find
  harmonious ways of combining reactive with proactive evaluation.}

  @item{@emph{Reconciling the benefits and costs of continuations.}
  Yes, continuations. Programmers reinvent them all the
  time. Therefore, they're very dear to our heart. We have them
  breakfast every day. But, having had two decades of experience with
  them, we also recognize the problems they create. We think we can do
  better.}

]
