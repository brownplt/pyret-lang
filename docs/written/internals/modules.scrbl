#lang scribble/base

@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@title{Modules}

@section[#:tag "s:module-rep"]{Representation}

Modules are represented as JavaScript object literals.  Aside from the
@tt{theModule} field, which contains the compiled code of the module, they
follow a JSON-structured schema.  The format has several design goals:

@itemlist[

@item{It should not require any free or global JavaScript identifiers beyond
chapter 15 of the ES5 spec, allowing the compiler and runtime system to
parameterize the module by its context.@margin-note{Some modules, like images
and the world library, use global variables like @tt{document} for historical
reasons, but it is not imperative to how they work that they do so.}}

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
  ... (being finalized) ...

moduleFunction :=
  | function(runtime, namespace, uri, <id>, ..., <id>, ...) {
      // compiled or handwritten JavaScript code
    }

}

The first three fields—@tt{requires}, @tt{provides}, and
@tt{nativeRequires}—hold static information about the modules dependencies and
exports.

@itemlist[

@item{@bold{requires} holds the compiled equivalent of an @tt{import} line.
This includes the kind of import, and any parameters that are part of the
import statement.  For example, the import line @tt{import
file("./lib/helpers.arr") as H} would show up in the compiled code as

@verbatim{
{ "import-type": "dependency", "protocol": "file", "args": ["./lib/helpers.arr"] }
}

Note that the @tt{require} can be generated from an import line without any
special context information.  For example, in the example above, the path is
not resolved to an absolute path.  This happens later in REF.  This decision in
large part supports the goal of handwritten modules, where it would be onerous
to fill in absolute paths and keep track of them.

}

@item{@bold{provides} describe the values and types exported from a module.  FILL}

@item{@bold{nativeRequires} describe dependencies of the module that are not
Pyret-based.  The strings in @tt{nativeRequires} are processed not by Pyret's
module loading system, but by a (configurable) use of RequireJS.  This is
discussed later in [REF].

Pyret distinguishes @tt{nativeRequires} for several reasons.  In some contexts,
like running on Node, there needs to be some mechanism for accessing system
libraries like @tt{fs} for filesystem access.  In addition, there are numerous
JavaScript libraries implemented in RequireJS format, and it's useful to have a
way for handwritten Pyret modules to import and use them directly.  To avoid
using global scope or other mechanisms, the runtime uses RequireJS as a
standard way to locate and load these modules.
}

]

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

