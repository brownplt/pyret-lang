
![Yarr](https://raw.github.com/brownplt/pyret-lang/master/img/pyret-banner.png)

[![Build Status](https://travis-ci.org/brownplt/pyret-lang.png)](https://travis-ci.org/brownplt/pyret-lang)

A scripting language.

The use of vocabulary from
http://reservationsbvi.com/thisoldpirate/glossary.html is recommended when
commenting and reporting issues.

All Aboard
----------

First, make sure ye've installed [Node >= 0.10](http://nodejs.org).  Then run:

    $ make install && make && make test

It'll build the Pyret compiler, run the tests, and hoist the anchor.

When yer ready to brave the sea, visit [the tour](http://pyret.org/tour/).


Notes for new-world
-------------------

This branch is a brand-new implementation of Pyret, so some things are
different (in yet-to-be-documented ways) from the master branch.

The easiest way to *run* a Pyret program in this branch is:

    $ node build/phaseX/main-wrapper.js <path-to-pyret-program-here>

Where X is a number from 0-3.

You can also see our prototype REPL by opening the file

    ./src/web/repl.html

in a browser; this only depends on `make phase1`.

Phases
------

Pyret is a self-hosted compiler, which means that building requires some
thought.  If you're going to get into the guts of the compiler, a brief
overview is worth reading.  The `build` directory is separated into four
distinct *phases*.

1.  Phase 0 is a standalone JavaScript file that contains an entire compiled
Pyret compiler and runtime.  This large blob gets committed into version
control whenever we add a nontrivial feature to the language.  It is large and
somewhat slow to load, but whatever is in build/phase0/pyret.js is currently
the canonical new-world implementation of Pyret.

2.  Phase 1 is set up to be populated with built versions of all the files for
the compiler, built by the phase 0 compiler.  Phase 1 is what most of the
testing scripts run against, since it is the easiest to create, and after it is
built it is your development snapshot of the compiler you're working on.
However, just building phase1 is not enough to fully re-boostrap the compiler,
because it contains a mix of old-compiler output and any new changes that were
made to runtime files.

3.  Phase 2 is set up to be populated with built versions of all the files for
the compiler, built by the phase 1 compiler.  This version does represent a
fully-bootstrapped compiler.  If you run `make standalone2`, you get a new
standalone compiler in the same format as `build/phase0/pyret.js`.

4.  Phase 3 builds from phase 2.  One major use of phase 3 is to check the
bootstrapped compiler from phase 2.  Before committing a new standalone in
phase 0, build both standalone2 and standalone3, and check:
    
        $ diff build/phase2/pyret.js build/phase3/pyret.js

    And it should be empty, which indicates that the bootstrapped compiler is
at
    least correct enough to recompile itself without error.

    To rebuild the compiler and get a new `phase0`, run

        $ make new-bootstrap

    which will build the phase2 and phase3 standalones, check the diff, and
    copy to phase0 if the diff is empty.

