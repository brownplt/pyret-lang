# Desugaring Design Decisions

This document is a short, technical overview of design decisions made
for Pyret's new desugaring system.

## OI vs. IO

Should sugars expand from the outside-in (OI), or from the inside-out
(IO)? We chose IO, primarily because it makes desugaring rules behave
similarly to pattern-matching functions. In OI, you cannot use "helper
functions". For example if (A x) desugars to (B (C x)), then B will
expand before C, and recieve (C x) as an argument, which is
unintuitive if you think of sugars as functions over syntax.

## Explicit subscripts

For the moment, we explicitly mark ellipses and repeated variables
with subscripts. For example, this desugaring rule:

    sugar transpose:
    | (transpose [[x_{ij} ...i] ...j]) =>
      [[x_{ij} ...j] ...i]
    end

performs a matrix transposition. In contrast, this rule:

    sugar transpose:
    | (transpose [[x ...i] ...j]) =>
      [[x ...j] ...i]
    end

also performs a transposition, but it requires all of the elements of
the matrix to be the same.

We can either leave the rules explicit like this, or have the rules be
_written_ without underscores, but then infer them when parsing the
rules. Doing so would slightly weaken the power of the desugaring
language: for instance, these transposition examples would be inferred
to be identity functions instead.

## Surface, Core, and Auxilliary

The desugaring system syntactically distinguishes between three kinds
of terms:

* Surface terms are the sugars of the surface language. Right after
  parsing, all of Pyret is a surface term. They are written in
  parentheses, like s-expressions `(op e ... e)` (where `op` is the
  constructor name, and `e` is a term).
* Core terms are the terms after desugaring. They are written in angle
  brackets `<op e ... e>`. It is useful to distinguish surface from
  core terms, because surface terms will be recursively expanded while
  core terms will not. For example, this `if` rule:

        sugar s-if-else:
          | (s-if-else [] else blocky) => else
          | (s-if-else [branch rest_{x} ...x] else blocky) =>
            <s-if-else [branch] (s-if-else [rest_{x} ...x] else blocky) blocky>
        end

  desugars into a core term for its base case; otherwise it would
  run forever.
* Auxilliary "terms" aren't quite terms; they're really data
  structures. Sometimes a sugar would like to return a piece of data
  other than syntax; this is what auxilliary terms are for. Auxilliary
  terms are written in curly braces `{op e ... e}`.

We also require all terms to have fixed arity (i.e., a fixed number of
children). This will make various checks on desugaring rules easier to
perform, and give better error messages if you write down a malformed
desugaring rule. Lists are written in square brackets `[e ... e]`.

## Metafunctions and Bijections

Sometimes it is difficult or inefficient to write a desugaring rule
using only pattern-based rules. We therefore support _bijections_ and
_metafunctions_, which are defined by Pyret code, and can be used on
the right-hand-side of a desugaring rule. A bijection is a _pair of
Pyret functions_, one to use when desugaring, and one to use when
resugaring. These functions must be inverses (or else resugaring won't
work properly).

A metafunction, on the other hand, is a _single Pyret function_. It
does not need to have an inverse, but in return it should only
be used to construct static data that won't change during evaluation
(or else resugaring won't work properly).

Metafunctions are called with `(meta f e ...)`---where `f` is
the name of the metafunction---and bijections are called with `(biject
f e ...)`.

## Source Locations

By default, desugaring rules omit source locations, and desugaring
will use the sugar's source location to automatically fill in the
source locations of the pattern it desugars into. For example, in this
sugar:

    sugar A:
    | (A x) => <B <C x>>
    end

A's source location will be propogated to B and C. Sometimes you need
more advanced behavior, though, so we support explicitly matching
source locations with `@`. For example, in this sugar:

    sugar A:
    | (A @l x) => <B @l <C @(meta get-loc-of x) x>>
    end

B is still given A's srcloc, but C is given `x`'s srcloc via the
bulitin metafunction `get-loc-of`.

## Fresh Variables

Fresh variables can be introduced with `(fresh [x] e)`, where `x` is
the variable name and `e` is the term. Right now, if you use a
variable without `fresh`, it will be unhygienic. A better design would
be to have both `fresh` and `capture`, and have all variables be
unbound otherwise.

(Implementation note: this "better design" would also simplify
parsing, which right now has to be clever about distinguishing
variables from pattern variables.)

## Error Collection (NYI)

If a sugar produces an auxilliary of the form `{error [msg ...]}`,
where `msg` is one more more (but typically one) Pyret error message,
then desugaring will fail and raise those messages. If multiple errors
are produced in different parts of the AST, they will all be collected
and raised together.

## Tag Style

In order to allow for resugaring, terms need to be tagged by the sugar
they came from during desugaring. There are two approaches to this in
Justin's PLDI'14 and ICFP'15 resugaring papers.  For technical
reasons, we use the ICFP'15 approach, where if a desugaring rule
rewrites pattern p to p', then it's given a tag containing p and p'.

(Justin's note to self: this is due to the interaction of auxilliary
terms, tags, and IO evaluation order.)

## Resugaring Implementation (NYI)

Getting a core-language stepper for Pyret involves rewriting a Pyret
AST into another AST that behaves the same way as the first, but emits
its steps. We plan to do this using desugaring rules.
