
# Record types are unordered: two record annotations that list the same fields
# in different orders denote the SAME type. This exercises the type checker's
# TRecord.key() (src/ts-compiler/src/type-structs.ts), which is used to key the
# type identity of a record and must therefore be field-order independent, to
# agree with structural equality. See the port review note on JS Map insertion
# order silently replacing Pyret StringDict hash order (cross-cutting #1).

data Box<C>:
  | box(v :: C)
end

# The record type argument is written { a; b } here and { b; a } below; both
# name the same type, so the single `val` below must satisfy both annotations.
fun wants-ab(bx :: Box<{ a :: Number, b :: Number }>) -> Number:
  bx.v.a
end

fun wants-ba(bx :: Box<{ b :: Number, a :: Number }>) -> Number:
  bx.v.b
end

val = box({ b : 2, a : 1 })
ab :: Number = wants-ab(val)
ba :: Number = wants-ba(val)

# An if whose two branches build the same record type in different field orders:
# the inferred branch type must unify to a single record type (the meet keys the
# branch types by TRecord.key()).
picked = if true: { a : 10, b : 20 } else: { b : 40, a : 30 } end
picked-a :: Number = picked.a
picked-b :: Number = picked.b
