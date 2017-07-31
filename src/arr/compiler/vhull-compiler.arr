provide *
import ast as A
import file("ast-anf.arr") as N
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as CL
import file("js-dag-utils.arr") as DAG
import file("ast-util.arr") as AU
import file("type-structs.arr") as T
import string-dict as D
import srcloc as SL
import sets as S
import sha as sha

string-dict = D.string-dict
mutable-string-dict = D.mutable-string-dict

type Loc = SL.Srcloc
type CList = CL.ConcatList
clist = CL.clist

compiler-visitor = {
  method a-module(self, l, answer, dvs, dts, provides, types, checks):
    raise("a-module not implemented")
  end,
  method a-type-let(self, l, bind, body):
    raise("a-type-let not implemented")
  end,
  method a-let(self, _, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    raise("a-let not implemented")
  end,
  method a-arr-let(self, _, b :: N.ABind, idx :: Number, e :: N.ALettable, body :: N.AExpr):
    raise("a-arr-let not implemented")
  end,
  method a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    raise("a-var not implemented")
  end,
  method a-seq(self, _, e1, e2):
    raise("a-seq not implemented")
  end,
  method a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr, alt :: N.AExpr):
    raise("a-if not implemented")
  end,
  method a-cases(self, l :: Loc, typ :: A.Ann, val :: N.AVal, branches :: List<N.ACasesBranch>, _else :: N.AExpr):
    raise("a-cases not implemented")
  end,
  method a-update(self, l, obj, fields):
    raise("a-update not implemented")
  end,
  method a-lettable(self, _, e :: N.ALettable):
    raise("a-lettable not implemented")
  end,
  method a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    raise("a-assign not implemented")
  end,
  method a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    raise("a-app not implemented")
  end,
  method a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    raise("a-prim-app not implemented")
  end,
  method a-ref(self, l, maybe-ann):
    raise("a-ref not implemented")
  end,
  method a-obj(self, l :: Loc, fields :: List<N.AField>):
    raise("a-obj not implemented")
  end,
  method a-get-bang(self, l :: Loc, obj :: N.AVal, field :: String):
    raise("a-get-bang not implemented")
  end,
  method a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    raise("a-extend not implemented")
  end,
  method a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    raise("a-dot not implemented")
  end,
  method a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    raise("a-colon not implemented")
  end,
  method a-method(self, l :: Loc, name :: String, args :: List<N.ABind>, ret :: A.Ann, body :: N.AExpr):
    raise("a-method not implemented")
  end,
  method a-val(self, l :: Loc, v :: N.AVal):
    raise("a-val not implemented")
  end,
  method a-field(self, l :: Loc, name :: String, value :: N.AVal):
    raise("a-field not implemented")
  end,
  method a-tuple(self, l, values):
    raise("a-tuple not implemented")
  end,
  method a-tuple-get(self, l, tup, index):
    raise("a-tuple-get not implemented")
  end,
  method a-array(self, l, values):
    raise("a-array not implemented")
  end,
  method a-srcloc(self, l, loc):
    raise("a-srcloc not implemented")
  end,
  method a-num(self, l :: Loc, n :: Number):
    raise("a-num not implemented")
  end,
  method a-str(self, l :: Loc, s :: String):
    raise("a-str not implemented")
  end,
  method a-bool(self, l :: Loc, b :: Boolean):
    raise("a-bool not implemented")
  end,
  method a-undefined(self, l :: Loc):
    raise("a-undefined not implemented")
  end,
  method a-id(self, l :: Loc, id :: A.Name):
    raise("a-id not implemented")
  end,
  method a-id-var(self, l :: Loc, id :: A.Name):
    raise("self not implemented")
  end,
  method a-id-safe-letrec(self, l :: Loc, id :: A.Name):
    raise("a-id-safe-letrec not implemented")
  end,
  method a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    raise("a-id-letrec not implemented")
  end,
  method a-data-expr(self, l, name, namet, variants, shared):
    raise("a-data-expr not implemented")
  end,
  method a-program(self, l, _, imports, body) block:
    raise("a-program not implemented")
  end
}

fun vhull-compiler(env, add-phase, flatness-env, provides, options):
  compiler-visitor
end
