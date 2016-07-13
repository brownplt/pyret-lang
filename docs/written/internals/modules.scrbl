#lang scribble/base

@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@title{Modules}

@section[#:tag "s:module-rep"]{Representation}

@subsection[#:tag "s:single-module"]{Single Modules}

Modules are represented as JavaScript object literals.  Aside from the
@tt{theModule} field, which contains the compiled code of the module, they
follow a JSON-structured schema.  The format has several design goals:

@itemlist[

@item{It should not require any free or global JavaScript identifiers beyond
chapter 15 of the ES5 spec, allowing the compiler and runtime system to
parameterize the module by its context.@margin-note{Some modules, like images
and the world library, use global variables like @tt{document}, though we'd
like to refactor them so they don't need to.}}

@item{It should be reasonable for a developer to write by hand, so that modules
in pure JavaScript can seamlessly co-exist with compiled Pyret modules.}

@item{It should be simple for the compiler to produce and consume.}

@item{It should contain the information necessary for its dependents to be
statically checked, without consulting the original source program.}

]

A module, whether compiled or handwritten, has the following form:

@verbatim{
module := {
  "requires": [<require>, ...],
  "provides": <provides>,
  "nativeRequires": [<nativeRequire>, ...],
  "theModule": <moduleFunction>
}

require :=
  | { "import-type": "builtin", "name": <string> }
  | { "import-type": "dependency", "protocol": <string>, "args": [<string>, ...] }

nativeRequire :=
  | <string>

provides :=
  {
    aliases: { <name>: <type>, ... },
    values: { <name>: <type>, ... },
    datatypes: { <name>: <type>, ... }
    // <type>s in shorthands cannot use shorthands as types
    // (described below)
    shorthands: { <name>: <type>, ... },
  }

prim-type :=
  | "tany" | "Number" | "String" | "Boolean" | "Any" | "Nothing"


type :=
  | <prim-type>
  | <type-full>
  | <type-array>
  | <string-defined-in-shorthands>

type-full :=
  | { tag: "any" }
  | { tag: "name", origin: <require>, name: <string> }
  | { tag: "forall", args: [<string>, ...], onto: <type> }
  | { tag: "arrow", args: [<type>, ...], ret: <type> }
  | { tag: "tyapp", onto: <type>, args: [<type>, ...] }
  | { tag: "tyvar", name: <string> }
  | { tag: "record", fields: { <name>: <type> }, ... }
  | { tag: "data",
      name: <string>,
      params: [<string>, ...],
      variants: [<variant-full>, ...],
      methods: { <name>: <type>, ... }
    }

variant-full :=
  | { tag: "variant",
      name: <string>,
      vmembers: [<vmember-full>, ...]
    }
  | { tag: "singleton-variant", name: <string> }

vmember-full :=
  | { tag: "variant-member", name: <string>, kind: <variant-kind>, typ: <type> }

variant-kind :=
  | "normal" | "ref"

type-array :=
  | ["Array", <type>]
  | ["RawArray", <type>]
  | ["Option", <type>]
  | ["List", <type>]
    # type of args, resulting constructed type
  | ["Maker", <type>, <type>]
  | ["arrow", [<type>, ...], <type>]
  | ["data",
      <string>,
      [<string>, ...],
      [<variant-array>, ...],
      { <name>: <type>, ... }
    ]
  | ["tid", <string>]
  | ["forall", [<string>, ...], <type>]
  | ["local", <string>]
  | ["record", { <name>: <type>, ... }]
  | ["tyapp", <type>, [<type>, ...]]

variant-array :=
  | [<string>]
  | [<string>, [<vmember-array>, ...]]

vmember-array :=
  | [<string>, <type>]
  | ["ref", <string>, <type>]

moduleFunction :=
  | function(runtime, namespace, uri, <id>, ..., <id>, ...) {
      // compiled or handwritten JavaScript code
    }

}

The first three fields—@tt{requires}, @tt{provides}, and
@tt{nativeRequires}—hold static information about the modules dependencies and
exports.

@subsubsub*section{@bold{requires}}

The @tt{requires} field holds the compiled equivalent of an @tt{import} line.
This includes the kind of import, and any parameters that are part of the
import statement.  For example, the import line @tt{import
file("./lib/helpers.arr") as H} would show up in the compiled code as

@verbatim{
{ "import-type": "dependency",
  "protocol": "file",
  "args": ["./lib/helpers.arr"] }
}

Builtin imports, like @tt{lists} and @tt{sets}, have an @tt{import-type} of
@tt{"builtin"}:

@verbatim{
{ "import-type": "builtin", name: "lists" }
}

Note that the @tt{require} can be generated from an import line without any
special context information.  For example, in the example above, the path is
not resolved to an absolute path.  This happens later in [REF].  This decision
in large part supports the goal of handwritten modules, where it would be
onerous to fill in absolute paths and keep track of them.

@subsubsub*section{@bold{provides}}


@tt{provides} describe the types exported from a module.  This includes:

@itemlist[
  @item{The types of exported @tt{values}.  So, for example, a program that
  defines

@verbatim{
x :: Number = 22
}

  would have the following in its in its compiled @tt{provides.values}:

@verbatim{
x: "Number"
}

}


  @item{The types of exported @tt{aliases}.  For example, a program that
  defines

@verbatim{
type Point = { x :: Number, y :: Number }
}

  would have the following in its compiled @tt{provides.aliases}:

@verbatim{
Point: {
  tag: "record",
  fields: { x: "Number", y: "Number" }
}
}

}
  
  @item{Any exported @tt{datatypes}.  For example, a program that defines

@verbatim{
data Point:
  | point(x :: Number, y :: Number)
end
}

  would have the following in its compiled @tt{provides.datatypes}:

@verbatim{
Point: {
  tag: "data",
  name: "Point",
  params: [],
  variants: [
    {
      tag: "variant",
      name: "point",
      vmembers: [
        {
          tag: "variant-member",
          kind: "normal",
          name: "x"
          typ: "Number"
        },
        {
          tag: "variant-member",
          kind: "normal",
          name: "y"
          typ: "Number"
        }
      ]
    }
  ],
  methods: {}
}
}
}
  
  
]

Writing out all of the types fully, with @tt{tag} and so on, is quite a bit of
typing for handwritten modules.  So these types can also be specified in an
array notation, where the first element of the array is typically a string
indicating the tag, and the rest of the array describes the type positionally.
So, for example, the datatype for @tt{Point} could be written:

@verbatim{
Point: ["data",
         "Point",
         [],
         [
           ["variant", "point", [["x", "Number"], ["y", "Number"]]]
         ],
         {}
       ]
}

Both styles are fully supported and can be interchanged.

Since often modules refer to the same type many times, it can be painful to
write out the same type over and over in a handwritten specification.  The
@tt{provides} declaration also allows specification of @tt{shorthands}, which
are not new types exported by the module, but rather shortcuts for writing out
its types.  For example, a module that implements dictionaries from a key type
@tt{K} to a value type @tt{V} will likely use a type like this repeatedly:

@verbatim{
["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]
}

That is, the locally-defined (within this module) type @tt{Dict}, parameterized
by two type variables.  Instead of writing:

@verbatim{
{
  values: {
    "new-dict": ["forall", ["K", "V"], ["arrow", [], 
      ["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]]],
    "set": ["forall", ["K", "V"], ["arrow",
        [
          ["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]
          ["tid", "K"], 
          ["tid", "V"]
        ],
        ["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]]],
    "get": ["forall", ["K", "V"], ["arrow",
        [
          ["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]
          ["tid", "K"], 
        ],
        ["Option", "V"]]]
  }
  // aliases and types and so on
}
}

It's easier to define:

@verbatim{
{
  shorthands: {
    dOfKV: ["tyapp", ["local", "Dict"], [["tid", "K"], ["tid", "V"]]]
  },
  values: {
    "new-dict": ["forall", ["K", "V"], ["arrow", [], dofKV]],
    "set": ["forall", ["K", "V"], ["arrow",
        [ dOfKV, ["tid", "K"], ["tid", "V"] ],
        dOfKV]],
    "get": ["forall", ["K", "V"], ["arrow",
        [ dOfKV, ["tid", "K"], ],
        ["Option", "V"]]]
  }
  // aliases and types and so on
}
}

There are several examples of the uses of these declarations in [REF].

Some "shorthands with options" are predefined, namely @tt{Option}, @tt{Array},
@tt{RawArray}, @tt{List}, and @tt{Maker}.  The first four of these are
straightforward, single-argument type constructors.  The last one describes the
type of @tt{list} in @tt{[list: ...]}, namely, the type of the object whose
fields allow for the construction of composite values.  Makers accept two type
arguments: the type of the @tt{...} arguments in the constructor notation, and
the resulting type of the constructed value.


@subsubsub*section{@bold{nativeRequires}}

Describe dependencies of the module that are not Pyret-based.  The strings in
@tt{nativeRequires} are processed not by Pyret's module loading system, but by
a (configurable) use of RequireJS.  This is discussed later in [REF].

Pyret distinguishes @tt{nativeRequires} for several reasons.  In some contexts,
like running on Node, there needs to be some mechanism for accessing system
libraries like @tt{fs} for filesystem access.  In addition, there are numerous
JavaScript libraries implemented in RequireJS format, and it's useful to have a
way for handwritten Pyret modules to import and use them directly.  To avoid
using global scope or other mechanisms, the runtime uses RequireJS as a
standard way to locate and load these modules.

@margin-note{Of course, this also assumes that code is run within a sandbox so
it cannot simply @tt{eval} its way to arbitrary behavior.  While Pyret doesn't
currently run within something like Caja [REF] when evaled, it is a long-term
goal.} In addition, with the assumption that modules do not rely on globals,
this makes the task of auditing modules for their use of special, non-language
behavior easier, since such an audit can start from the @tt{nativeRequires}
specifications across all modules.

@subsubsub*section{@bold{theModule}}

The final field, @tt{theModule}, holds a function that implements the module's
behavior, and constructs the values that it provides.  Its arguments have a
particular shape:

@itemlist[

@item{@bold{runtime} – the first argument is the current runtime (as described
in @secref["s:runtime-object"]).  This is the entrypoint for most interesting
built-in Pyret behavior, and used pervasively in compiled and handwritten
modules alike.}

@item{@bold{namespace} – the second argument is a dictionary object, called a
namespace [REF], that holds the mappings for global identifiers and types
available in the module.  This is seldom useful in handwritten code; its main
use is in modules at the REPL that have an interesting and changing set of
globally-available names.}

@item{@bold{uri} – the third argument is the URI of the module, as a string.
Since the same code could be loaded for different purposes (e.g. a Google Drive
module loaded both from a shared import and a path import), a module does not
store its URI at compile time.  The URI is provided when the module is
instantiated, which can be used for logging, reporting error messages, and
other unique module identification purposes.}

@item{@bold{requires} ids – After @tt{uri}, there should be a number of
identifiers equal to the number of @tt{require}s listed.  These will hold the
module objects [REF] for the specified dependencies when the module is loaded.}

@item{@bold{nativeRequires} ids – After the @tt{requires} ids, there should be
a number of identifiers equal to the number of @tt{nativeRequires} listed.
These will hold the values returned from using RequireJS on the native
dependencies when the module is loaded.}

]

@subsection[#:tag "s:complete"]{Complete Programs}

Modules as described in @secref["s:single-module"] lack the necessary
information and context to run – their dependencies must still be provided,
most crucially, and the runtime needs to know in which order to run them.

To this end, Pyret also specifies a format for complete programs, which
contains all the information needed to run a program, given a runtime and an
implementation of RequireJS.  Running such a complete program, which can be
done in several ways, is discussed in [REF].  This section lays out and
motivates its structure.  This structure is not intended to be written by hand.

@verbatim{
program := {
  staticModules: <staticModules>,
  depMap: <depmap>,
  toLoad: [<uri>, ...],
}

depmap := { <uri>: { <dependency> : <uri>, ... }, ... }

staticModules := { <uri>: <module>, ... }

dependency := string encoding of <require>

module := as above
}

The dictionary of @tt{staticModules} maps from uri to module structures as
described in @secref["s:single-module"].  This includes all the Pyret-based
modules and code that the program will use.  It's worth noting that the
information in the @tt{provides} block is (potentially) extraneous if the only
goal is to run the program.  However, if compiled modules are to provide enough
information to e.g. type-check code that is linked against them in the future,
it's worth keeping this static information around.

The @tt{depmap} indicates, for each listed @tt{require} dependency, which
module should be used to satisfy it.  This is indicated by mapping from a
string representation of the @tt{require} to the URI for the appropriate
module.  The string encoding is straightforward, and creates a string that
looks much like the original import line.  For example, a @tt{require} like:

@verbatim{
{ "import-type": "dependency", "protocol": "file", "args": ["./lib/helpers.arr"] }
}

would appear encoded as

@verbatim{
file(./lib/helpers.arr)
}

The @tt{toLoad} list indicates the order in which the modules should be loaded.
It should always be a valid topological sort of the graph implicit in
@tt{depmap}.  In that sense, it's not strictly necessary information, but it
makes running a generated program much more straightforward, since its clear in
which order to instantiate modules.  This also makes it easy to determine the
main entrypoint for the program, which is the @emph{last} module indicated in
the @tt{toLoad} list.  That is, the modules leading up to the last one are
exactly its (transitive) dependencies, and run in order to create their
exports, which will be used later in the @tt{toLoad} list to instantiate
further modules.

Concretely, the first few modules in the @tt{toLoad} list are typically
builtins, like @tt{lists} and @tt{error}, required for just about every program.
Increasing indices in the @tt{toLoad} list tend towards user-implemented code
until finally reaching the main module that the user requested be compiled.



