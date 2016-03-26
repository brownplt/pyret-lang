provide {
  name-locs: name-locs
} end

import ast as A
import string-dict as SD
import srcloc as S
import "compiler/js-of-pyret.arr" as P
import "compiler/compile-structs.arr" as C
import "compiler/well-formed.arr" as W
import "compiler/ast-util.arr" as U
import "compiler/resolve-scope.arr" as R
import "compiler/desugar.arr" as D
import "compiler/desugar-post-tc.arr" as DP
import "compiler/type-check.arr" as T
import "compiler/desugar-check.arr" as CH

import parse-pyret as PP
import file as F
import cmdline as CL

var loc-to-names = [SD.mutable-string-dict: ]
var name-to-locs = [SD.mutable-string-dict: ]

fun add-use(loc, name):
  # map from the starting character of the usage to the (atom) Name
  cases(S.Srcloc) loc:
    | builtin(_) => nothing
    | srcloc(_, _, _, start-char, _, _, _) =>
      s-c = tostring(start-char)
      names = if loc-to-names.has-key-now(s-c): loc-to-names.get-value-now(s-c) else: [set: ] end
      loc-to-names.set-now(s-c, names.add(name))
      # map from a Name to all the locations of usage
      n-key = name.key()
      locs = if name-to-locs.has-key-now(n-key): name-to-locs.get-value-now(n-key) else: [set: ] end
      name-to-locs.set-now(n-key, locs.add(loc))
  end
  nothing
end

find-uses = A.default-iter-visitor.{
  s-id(self, l, id):
    add-use(l, id)
    true
  end,
  s-id-var(self, l, id):
    add-use(l, id)
    true
  end,
  s-id-letrec(self, l, id, _):
    add-use(l, id)
    true
  end,
  a-name(self, l, id):
    add-use(l, id)
    true
  end,
  a-type-var(self, l, id):
    add-use(l, id)
    true
  end
  # s-defined-value(self, _, _): true end,
  # s-defined-type(self, _, _): true end
}

fun name-locs(ast, env, libs, options):
  loc-to-names := [SD.mutable-string-dict: ]
  name-to-locs := [SD.mutable-string-dict: ]
  
  var ast-ended = U.append-nothing-if-necessary(ast)
  checker = if options.check-mode: CH.desugar-check else: CH.desugar-no-checks end
  var checked = checker(ast-ended.or-else(ast))
  ast-ended := nothing
  var imported = U.wrap-extra-imports(checked, libs)
  checked := nothing
  var scoped = R.desugar-scope(imported, env)
  imported := nothing
  named = R.resolve-names(scoped, env)
  scoped := nothing

  # print(torepr(named.ast))
  
  # datatypes = named.datatypes
  # for each(namet from datatypes.keys-list-now()):
  #   locs = if datatypes.has-key-now(namet.key()): datatypes.get-now(namet.key()) else: empty end

  for each(bkey from named.bindings.keys-list-now()):
    b = named.bindings.get-value-now(bkey)
    add-use(b.loc, b.atom)
  end

  for each(tkey from named.type-bindings.keys-list-now()):
    t = named.type-bindings.get-value-now(tkey)
    add-use(t.loc, t.atom)
  end

  named.ast.visit(find-uses)

  { loc-to-names: loc-to-names.freeze(), name-to-locs: name-to-locs.freeze() }
end


fun all-locs(file):
  str = F.file-to-string(file)
  ast = PP.surface-parse(str, file)
  options = { check-mode: false }
  locs = name-locs(ast, C.standard-builtins, C.standard-imports, options)
  print("Starting locations of names:")
  for each(start from locs.loc-to-names.keys-list()):
    print(start + " ==> "
        + tostring(for sets.fold(acc from [set:], loc from locs.loc-to-names.get-value(start)):
          acc.add(loc.key())
        end))
  end
  print("Usage locations for each name:")
  for each(name from locs.name-to-locs.keys-list()):
    print(name + " ==> " + tostring(locs.name-to-locs.get-value(name)))
  end
end

all-locs(CL.args.first)
