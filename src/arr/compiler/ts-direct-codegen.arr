import file("ast.arr") as A
import file("js-ast.arr") as J
import js-file("ts-direct-codegen-impl") as TDC
import file("provide-serialization.arr") as PSE
import file("concat-lists.arr") as CL
import file("compile-structs.arr") as CS
import string-dict as D
import pathlib as P
include from J:
  *,
  type *
end
provide *

# TODO(Ben/Joe): get rid of this and translate to TypeScript

type CompileMode = CS.CompileMode

cl-empty = CL.concat-empty
cl-sing = CL.concat-singleton
cl-append = CL.concat-append
cl-cons = CL.concat-cons
cl-snoc = CL.concat-snoc

fun starts-with(s, prefix):
  string-index-of(s, prefix) == 0
end


fun serialize-requires(env :: CS.CompileEnvironment, options) -> CList<JExpr>:
  # NOTE(alex): current implementation includes the entire dependency subgraph that
  #   was present while compiling the current module, not just the dependency subgrpah
  #   reachable from the current module
  #
  # For example: A depends on B, A depends on C
  #    B will still show up in the requires of C
  #    and vice versa if the compiler visits dependency C first
  result = env.all-modules.keys-list-now().foldl(lam(uri-key :: String, acc :: CList<JExpr>):
    # TODO(alex): would be nice if CompileEnvironment stored the dependencies as an actual
    #  compile-structs:Dependency so we didn't have to parse the all-modules keys

    serialize-result = ask:
      | starts-with(uri-key, "builtin://") then:
        name = P.basename(uri-key, ".arr")
        serialize-builtin-requires(name, options)
      | starts-with(uri-key, "jsfile://")  then:
        serialize-file-requires(uri-key, "js-file", options)
      | starts-with(uri-key, "file://") then:
        serialize-file-requires(uri-key, "file", options)
      | otherwise: raise("Unknown uri kind:" + uri-key)
    end

    cl-cons(serialize-result, acc)

    # cl-cons(j-str(uri-key), acc)
    # cl-cons(j-str(P.basename(uri-key, ".arr")), acc)

  end, cl-empty)

  #_ = print("\nSerialized:")
  #_ = print(result)
  #_ = print("\n\n")
  result
end

fun serialize-builtin-requires(name, options) -> JExpr:
  j-obj([clist:
    j-field("import-type", j-str("builtin")),
    j-field("name", j-str(name))]
  )
end

fun serialize-file-requires(uri-key, protocol, options) -> JExpr:
  name = P.basename(uri-key, ".arr")

  # NOTE(alex): In cm-builtin-stage-1 and cm-builtin-general, treat ALL
  #  dependencies as builtin modules
  cases(CompileMode) options.compile-mode:
    | cm-normal =>
      j-obj([clist:
        j-field("import-type", j-str("dependency")),
        # NOTE(alex): protocol comes from cli-module-loader.arr
        j-field("protocol", j-str(protocol)),
        j-field("args", j-list(true, cl-sing(j-str(uri-key))))])
    | cm-builtin-stage-1 => serialize-builtin-requires(name, options)
    | cm-builtin-general => serialize-builtin-requires(name, options)
  end
end


fun compile-program(prog :: A.Program, uri, env, post-env, provides, options) block:
  result = TDC.compile-program(prog, uri, env, post-env, provides, options)

  # TODO(Ben/Joe): convert this to TypeScript
  # Note(alex): Necessary to change URIs for builtin-stage-1 and builtin-general
  #    modules to be used in "include from" syntax with values
  serialized-provides = cases(CompileMode) options.compile-mode:
    | cm-normal => PSE.compile-provides(provides)
    | cm-builtin-general => PSE.compile-provides-override-uri(provides, true)
    | cm-builtin-stage-1 => PSE.compile-provides-override-uri(provides, true)
  end

  [D.string-dict:
    "requires", j-list(true, serialize-requires(env, options)), # TODO: ->TS
    "provides", serialized-provides,                            # TODO: ->TS
    "nativeRequires", j-list(true, [clist:]),
    "theModule", J.j-raw-code(result.theModule),
    "theMap", J.j-str(result.theMap)
    ]
end
