#lang scribble/manual

@(require
  racket/list
  "common.rkt")

@title[#:tag "s:timing"]{Timing Pyret Programs}

Pyret has some limited support for timing and profiling.  It is only available
offline; to use it, include

@(justcode "import profile as P")

at the top of your source file.  It has two functions:

@(label "profile.time")

@justcode{
fun <a> time(f :: ( -> a)) -> a
}

Takes a function of no arguments and runs it, printing information about the
time it took to run on standard output.  For example:

@justcode{
#lang pyret

import profile as P

nums = P.time(fun():
  for list.map(i from list.range(0, 100)):
    i + i
  end
end)

nums
}

@(label "profile.profile")

@justcode{
fun <a> profile(f :: ( -> a)) -> a
}

Takes a function of no arguments and runs it, monitoring it periodically and
reporting information about which calls are visited most.  The output is
heavily reliant on the underlying Racket system, and isn't always in terms of
only Pyret functions that are defined in your program.  Better profiling of
Pyret programs is a work in progress.  The output format is described at
@url{http://docs.racket-lang.org/profile/index.html#%28mod-path._profile%2Frender-text%29}.
This has the same interface as @code{time}:

@justcode{
#lang pyret

import profile as P

nums = P.profile(fun():
  for list.map(i from list.range(0, 100)):
    i + i
  end
end)

nums
}

