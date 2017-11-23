
![Yarr](https://raw.github.com/brownplt/pyret-lang/master/img/pyret-banner.png)

[![Build Status](https://travis-ci.org/brownplt/pyret-lang.svg)](https://travis-ci.org/brownplt/pyret-lang)

A scripting language.

To learn about the language, visit [pyret.org](http://pyret.org).

To read an introduction of the language, visit [the tour](http://www.pyret.org/docs/latest/A_Tour_of_Pyret.html).

To read the documentation, visit [pyret.org/docs/](http://pyret.org/docs/).

There are two main ways to use Pyret:

1.  If all you want to do is program in Pyret, there is a web-based environment
at [code.pyret.org](https://code.pyret.org) that lets you run and save programs
that should be all you need.  If you're a student using Pyret, this is probably
where you will do your assignments, for example.

2.  If you want to develop Pyret, or install it for use at the command line,
the README starting below is for you.

The use of vocabulary from
http://reservationsbvi.com/thisoldpirate/glossary.html is recommended when
commenting and reporting issues.


Installing
----------

First, make sure you've installed [Node >= 6](http://nodejs.org).  Then run:

    $ npm install
    $ make
    $ make test

It'll build the Pyret compiler and run the tests.

Running Pyret
-------------

If you just want to run Pyret, visit [the online
environment](https://code.pyret.org) and work from there.  If you're interested
in Pyret development, read on:

The easiest way to *run* a Pyret program from the command-line is:

    $ ./src/scripts/phaseX <path-to-pyret-program-here> [command-line-args...]

Where `X` is `0`, `A`, `B`, or `C`, indicating a phase (described below). For
example:

    $ ./src/scripts/phaseA src/scripts/show-compilation.arr examples/ahoy-world.arr

Alternatively, you can compile and run a standalone JavaScript file via:

    $ node build/phaseX/pyret.jarr \
        --build-runnable <path-to-pyret-program-here> \
        --outfile <path-for-standalone-js> \
        --builtin-js-dir src/js/trove/ \
        --builtin-arr-dir src/arr/trove \
        --require-config src/scripts/standalone-configA.json
    $ node <path-for-standalone-js>

Phases
------

Pyret is a self-hosted compiler, which means that building requires some
thought.  If you're going to get into the guts of the compiler, a brief
overview is worth reading.  The `build` directory is separated into four
distinct *phases*.

1.  Phase 0 is a standalone JavaScript file that contains an entire compiled
Pyret compiler and runtime.  This large blob gets committed into version
control whenever we add a nontrivial feature to the language.  It is large and
somewhat slow to load, but whatever is in build/phase0/pyret.jarr is currently
the canonical new-world implementation of Pyret.

2.  Phase A is set up to be populated with built versions of all the files for
the compiler, built by the phase 0 compiler.  Phase A is what most of the
testing scripts run against, since it is the easiest to create, and after it is
built it is your development snapshot of the compiler you're working on.
However, just building phase1 is not enough to fully re-boostrap the compiler,
because it contains a mix of old-compiler output and any new changes that were
made to runtime files.

3.  Phase B is set up to be populated with built versions of all the files for
the compiler, built by the phase A compiler.  This version does represent a
fully-bootstrapped compiler.  If you run `make phaseB`, you get a new
standalone compiler in the same format as `build/phase0/pyret.js`.

4.  Phase C builds from phase B.  One major use of phase C is to check the
bootstrapped compiler from phase B.  Before committing a new standalone in
phase 0, build both phaseB and phaseC, and check:

        $ diff build/phaseB/pyret.jarr build/phaseC/pyret.jarr

    And it should be empty, which indicates that the bootstrapped compiler is
at
    least correct enough to recompile itself without error.

    To rebuild the compiler and get a new `phase0`, run

        $ make new-bootstrap

    which will build the phaseB and phaseC standalones, check the diff, and
    copy to phase0 if the diff is empty.


Pyret + Stopify
---------------

This is currently experimental.

#### Setting up Stopify

1. Clone the Stopify repo.
2. Navigate to `Stopify/stopify` and run the following commands:
   ```
   Stopify/stopify $ yarn
   Stopify/stopify $ yarn run link:global
   ```
   The first line builds stopify while the second line creates a global node
   installation for the `Stopify` module.
3. Navigate to the pyret-compiler and run `npm link Stopify`. This makes the
   global installation of stopify to pyret.
4. Build the stack saving portion of the runtime by running `make -B
   stopify-build`. **NOTE**: This must be run everytime the stopify's stack
   saving techique is changed.
5. Build the pyret compiler using `make`.

#### Building

1. To build straight-line code without stopify, run `make <filename>.v.jarr` in
the repository's top-level.

2. To build a pyret file with stopify, simply run `make <filename>.vs.jarr` in
   the repository's top-level. The `%.v.jarr` rule in the Makefile uses the
   following specific options to build the file with pyret:

```
		--standalone-file "src/js/base/handalone.stop.js" \
		-straight-line \
		--require-config src/scripts/standalone-configV.json
```

Stopify's options for pyret are configured in require-node-dependencies.js. For
more information about the options, take a look at the stopify README.
