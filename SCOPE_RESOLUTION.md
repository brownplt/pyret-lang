# Scope Resolution

This branch (resugar-opt) is in a good position to implement scope
resolution for Pyret very clearly using declarative rules, and a
generic resolution agorithm written in bucklescript that traverses the
generic ast representation. This file is an overview of a resolution
algorithm that I (Justin) expect to work well for Pyret. It's loosely
based on my
[scope inference](http://cs.brown.edu/research/plt/dl/icfp2017/)
paper, but you don't need to understand all of the complicated
inference things, because Pyret doesn't need scope inference, just
scope resolution. So, here's the algorithm:

## Scope rules

Every syntactic construct in Pyret would gain a set of _scope rules_
that describes its scope. Each rule either acts on a syntactic
construct of fixed arity, e.g. `(Lambda params body)`, or on a
syntactic construct that always appears in a list, in which case the
rest of the list will be important and is also named, e.g.
`[(Param param) ...params]`.

After the syntactic construct, there can be two kinds of declarations:

- `bind i in j`: Make child `i`'s declarations available in child `j`.
- `export j`: Export child `j`'s declarations.

For example, this scope rule:
  
    (Lambda params body) {
      bind params in body;
    }

means that a lambda's parameter list is bound in its body. Thus
variable references in `body` may refer to variable declarations in
`params`. Likewise, this scope rule:
  
    [(Param param) ...params] {
      export param;
      export params;
    }

says that a function's parameter list exports both `param`, its first
parameter, and `params`, the rest of the parameter list. Here are some
additional scope rules for Pyret:
  
    [(LetBind name value) ...binds] {
      export binds;
      bind name in binds;
    }
  
    (LetExpr bindings body) {
      bind bindings in body;
    }
  
    [(Newtype name namet) ...rest] {
      export name;
      export namet;
    }

## Scope Resolution Algorithm

The scope resolution algorithm would proceed in three parts:

1. When rules are declared, do some pre-computations.
2. Resolve the scope to figure out what's bound where.
3. Check for errors, like unbound variables

The resolution algorithm will rely on all variables having unique ids,
so that it can distinguish between two occurrences of the same
variable.

### 1. Pre-computations

When a scope rule is declared, two things should be computed as part
of its construction:

- It's "binds" and "exports" should actually obey two transitivity
  rules. If there is `bind i in j` and `bind j in k`, then `bind i in
  k` should also be included. Likewise, if there is `bind i in j` and
  `export j`, then `export i` should be included.
- To make the upcoming step easier, a scope rule should do a toposort
  on its "binds", to find an order of its children such that if `i`
  comes before `j`, then there is no `bind j in i`.

### 2. Resolve Scope

Scope resolution will recursively compute for each node in the ast two
sets:

- A _require_ set of unbound (or at least not yet bound) variable
  references.
- A _provide_ set of variable declarations that have been `export`ed
  from this ast.

As it goes, it will also add to a global dictionary of all bindings
that it has found, and shadowings that it has found.

There are essentially two base cases and one recursive case:

- **variable reference** a variable reference (e.g. `x` in expression
  position) _requires_ itself, and _provides_ nothing.
- **variable declaration** a variable declaration (e.g. `x` in
  function parameter position) _provides_ itself and _requires_
  nothing.
- **ast node** an ast node N with scope rule R obeys these rules:
    - If R contains `bind i in j`, and child `i` _provides_ a variable
      "x", and child `j` _requires_ "x", then `j`'s "x" is bound by
      `i`'s "x".
    - Likewise, if R contains `bind i in j`, and both child `i` and
      `j` _provide_ a variable "x", then `j`'s "x" _shadows_ `i`'s
      "x".
    - If a child `i` _requires_ a variable "x", and "x" is not bound
      by the above rule, then N _requires_ "x".
    - If R contains `export i`, and child `i` _provides_ a variable
      "x", and "x" is not shadowed by the above rule, then N
      _provides_ "x".

### 3. Error Checking

There are a few errors to check for:

- Unbound variables. These are variables that are _required_ by the
  root of the ast.
- Shadowed variables that aren't marked with `shadow`. These are
  variables that appear in the global "shadow" dictionary but weren't
  marked with the `shadow` keyword.
- A variable that is bound by more than one declaration. (This can
  happen in general, and should be considered an error. I'm not sure
  if it can happen for Pyret in particular.)


## Tricky Things

- One feature I didn't describe above, but that is important, is to be
  able to namespace bindings. For example, Pyret has both value and
  type bindings, and so it would be useful to say `export i.type` and
  `bind i.type in j` (or something like that) where "type" is an
  arbitrary label.
- I gave base cases for variable references and declarations, but how
  do you know which is which? The scope rules could contain this info.
  Maybe you say this:
  
        [(Param param:decl) ...params] {
          export param;
          export params;
        }
- Pyret has modules, and both "import" and "include". I don't know
  enough about how these work to describe an algorithm for it, but
  it's important to support.
