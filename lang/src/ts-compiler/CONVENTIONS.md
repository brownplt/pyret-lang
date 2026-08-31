# Pyret-to-TypeScript Porting Conventions

This directory contains a TypeScript port of the Pyret compiler that lives in
`src/arr/compiler`. The port is **strictly additive**: nothing under
`src/arr/`, `src/js/`, or the existing Makefile targets may change. The TS
compiler must produce compiled modules that run on the *unchanged*
`src/js/base/runtime.js` (same value representations, stack management,
module format).

## Layout

- One `.ts` file per ported `.arr` file, same base name:
  `src/arr/compiler/resolve-scope.arr` → `src/ts-compiler/src/resolve-scope.ts`.
- Compiler-support trove modules that the compiler itself uses as *data*
  (not runtime libraries) are also ported: `ast.arr`, `srcloc.arr`,
  `error-display.arr`, `pprint.arr` → `ast.ts`, `srcloc.ts`,
  `error-display.ts`, `pprint.ts`. The runtime trove (`src/arr/trove/*`)
  stays in Pyret and is still compiled BY this compiler — never ported.
- JS infrastructure that is runtime-independent is *reused, not ported*:
  `pyret-tokenizer.js`, `pyret-parser.js` (generated), `lib/jglr/rnglr.js`,
  `js-numbers.js`. They are AMD modules; load them through
  `src/interop/amd.ts`.
- Build output goes to `build/ts-compiler/` via `tsc -p .`.

## Naming

- kebab-case → camelCase for functions, methods, fields, locals
  (`check-well-formed` → `checkWellFormed`).
- Data variants become PascalCase classes (`s-block` → `SBlock`); the
  original Pyret tag is kept in a readonly `$name` field (`'s-block'`).
- Union type per Pyret `data` declaration: `type Expr = SBlock | SUserBlock | ...`.
- Type guards mirror Pyret `is-` functions: `isSBlock(x)`.
- File-level export style: plain ES module exports, no default exports.

## Data definitions

Pyret:
```pyret
data Expr:
  | s-block(l :: Loc, stmts :: List<Expr>) with:
    method label(self): "s-block" end
sharing:
  method visit(self, visitor): ... end
end
```

TypeScript:
```ts
export abstract class ExprBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;   // dispatches to visitor[camel($name)](this)
  // `sharing` methods go here
}
export class SBlock extends ExprBase {
  get $name(): 's-block' { return 's-block'; }
  constructor(public l: Loc, public stmts: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sBlock(this); }
  label(): string { return 's-block'; }
}
export type Expr = SBlock | SUserBlock | /* ... */;
export function isSBlock(x: any): x is SBlock { return x instanceof SBlock; }
```

**Visitor convention difference from Pyret:** Pyret visitors receive the
node's fields as separate arguments. In the TS port a visitor method
receives **the node itself**: `visitor.sBlock(node)`. All ported passes
must follow this convention. Default visitors are classes
(`DefaultMapVisitor`, `DefaultIterVisitor`) that passes `extend` and
selectively override.

- `default-map-visitor` method `s-block(self, l, stmts): s-block(l, stmts.map(_.visit(self)))`
  becomes `sBlock(node: SBlock): Expr { return new SBlock(node.l, node.stmts.map(s => s.visit(this))); }`
- `default-iter-visitor` methods return `boolean`.
- `dummy-loc-visitor` replaces every loc with `dummyLoc`.

## Core type mappings

| Pyret | TypeScript |
|---|---|
| `List<T>` | `T[]` (immutable by convention — never mutate a list you didn't create) |
| `empty` / `link(h, t)` | `[]` / `[h, ...t]` (prefer building with push on fresh arrays) |
| `Option<T>` | `T \| undefined` (`none` → `undefined`, `some(v)` → `v`) |
| `Either<L,R>` | `{ $name: 'left', v: L } \| { $name: 'right', v: R }` (helpers in `shared.ts`) |
| `StringDict<T>` (immutable) | `Map<string, T>` **plus copy discipline** (see below) |
| `MutableStringDict<T>` | `Map<string, T>` |
| `Set<String>` | `Set<string>` |
| sets of `Name` | `Map<string, Name>` keyed by `name.key()` |
| `Number` used as count/index/position | `number` |
| Pyret numeric *literal values* (e.g. `s-num.n`) | opaque `PyretNumber` (js-numbers value; see `interop/js-numbers.ts`) |
| `String` | `string` |
| `raise(<string>)` | `throw new InternalCompilerError(msg)` (in `shared.ts`) |
| `tostring(x)` | explicit `String(...)` or `.toString()` — **not** `jsnums.toString`, which does not exist (calling it silently yields `"[object Object]"`; see the NOTE in `interop/js-numbers.ts`) |

**Persistent-dict discipline:** Pyret `StringDict` is persistent —
`d.set(k, v)` returns a NEW dict. Where ported code extends a dict and
*also* keeps using the original (common in scope-resolution environments),
you must copy: `const d2 = new Map(d); d2.set(k, v);`. Where the original
is dead after the set, an in-place `d.set(k, v)` is fine. Read each call
site carefully; this is the #1 source of porting bugs. For env-threading
code, prefer the helper `mapSet(d, k, v)` from `shared.ts` which copies.

**Equality discipline:** Pyret `==` is structural. Never translate it to
`===` for compound values. Names compare with `a.key() === b.key()`,
srclocs with `a.key() === b.key()` or dedicated methods (`.same(other)`),
lists of names by element. When Pyret code uses values as `string-dict`
keys via `tostring`/`.key()`, do the same. **Locator identity** is the
`uri()` string; Pyret's `Locator._equals` is not ported (and locators must be
plain object literals, not classes — see the note at the `Locator` interface in
`compile-lib.ts`).

- Pyret string `+` on doc/pretty-printing → `.append()` for `PPrintDoc`,
  plain `+` for strings.
- `for fold(...)`/`for map(...)`/`for each(...)` → loops or
  `reduce`/`map`/`forEach`, preserving evaluation ORDER (gensym calls are
  order-sensitive; generated names appear in output and parity tests
  compare output).
- `cases(T) x: | variant(a, b) => ...` → `switch (x.$name) { case 'variant': ... }`
  or `if (x instanceof Variant)`. Use `switch` on `$name` with an
  exhaustiveness `default: throw` when the Pyret cases had no `else`.

## Gensym / name counters

`gensym.arr` and `ast.arr`'s `global-names = MakeName(0)` are global,
resettable counters; `compile-module` resets both before each module.
Preserve exact call order so generated names match the Pyret compiler's
output (`$underscore4`, `tail$5`, etc.) — byte-level parity of emitted JS
is the goal wherever feasible.

Byte-parity is **cutover scaffolding**, not a permanent law: it exists to prove
the TS compiler reproduces the Pyret one so we can switch over. Once the Pyret
compiler is retired, TS is the source of truth and the parity-specific
constraints below (matching `StringDict` iteration order, the two-sided
co-modification invariant) stop being live invariants. Until then, honor them.

## Determinism and iteration order

`Map` stands in for `StringDict`, and their iteration orders can differ — but
less often than it first looks. A `StringDict` is **insertion-ordered exactly
while it stays an `ArrayMapNode`**: it has never held more than
`MAX_ARRAY_MAP_SIZE` = 8 entries (`SHIFT=5`, `SIZE=32`, `SIZE/4`), **and** no key
was ever removed. Overwriting an existing key keeps its slot; but removal is a
swap-with-last, which reorders, and growing past 8 converts the node to a
hash-trie that iterates in **content-deterministic hash order** from then on.
`Map` is always insertion order. So:

- **A small, build-once, bounded dict (≤8 keys, no removals) needs no sorting** —
  `Map` insertion order already matches `StringDict`. This is exactly what makes
  the `clMapSd` exception below safe.
- **A dict that can exceed 8 entries, or that ever removes a key, must be sorted
  wherever its key order is observable** — where the order feeds
  gensyms/existentials, a `.key()` identity (e.g. `TRecord.key()`), serialized
  output, or a user-visible message. Insertion order is otherwise acceptable only
  for pure lookups whose iteration never escapes. This substitution is the single
  biggest semantic gap in the port; when unsure, sort.
- When you add a canonicalizing sort for parity, it usually must be **mirrored
  in the Pyret compiler too** (the reference output was produced by a sort added
  on *both* sides). See the header of `anf-loop-compiler.ts` — it names every
  co-located sort site in both compilers and says to keep them in lockstep.
  Changing one side alone silently breaks byte-parity.

**Sanctioned byte-parity exceptions.** Byte-parity of emitted JS is the goal,
but a few places knowingly rely on insertion order because it is provably
equivalent. These are the only blessed exceptions; each is justified at its
site, and new ones need the same treatment:

- `js-of-pyret.ts` `clMapSd` — the top-level module object has 5 fixed keys set
  in the same order by both compilers (and a `StringDict` of ≤8 keys is itself
  insertion-ordered), so `Map` order matches. See the comment there.

## Srcloc

`Srcloc = Builtin(moduleName) | Srcloc(source, startLine, startColumn, startChar, endLine, endColumn, endChar)`.
Port all methods (`format`, `key`, `before`, `after`, `same-file`, `upto`,
`upto-end`, ...). `dummyLoc = new Builtin('dummy location')`.

## Numbers in the AST and codegen

Numeric literals must keep exact rational semantics end to end:
`parse-pyret` constructs them with js-numbers `fromString`; `js-ast.ts`'s
printer must serialize them exactly the way the Pyret compiler does (see
`j-num` handling in `js-ast.arr` / `anf-loop-compiler.arr`). Use the
shared js-numbers library (`interop/js-numbers.ts` wraps
`src/js/base/js-numbers.js`) — do NOT use JS floats for Pyret numbers.

## Output contract (must match the existing compiler)

A compiled module on disk is
`({ theMap, theModule?, nativeRequires, provides, requires })` —
see `cli-module-loader.arr` + `anf-loop-compiler.arr`. The standalone
`.jarr` bundles `{staticModules, depMap, toLoad, uris, runtimeOptions}`
through `js/trove/make-standalone.js` + requirejs exactly like the
current pipeline. Any deviation breaks `runtime.js`/`handalone.js`
loading — when in doubt, diff against output of `build/phaseA/pyret.jarr`.

## Fidelity rules

1. **Port logic line by line; do not "improve" algorithms or traversal order.**
   Two carve-outs the port relies on hundreds of times:
   - *User-facing strings exact; internal strings best-effort.* Every
     render-reason / check-result string a user can see must match byte-for-byte
     (parity tests compare renderings — verified in well-formed, compile-errors).
     Internal `InternalCompilerError` / `TODOError` messages, `PyretParseError`
     text, and local `torepr`/`tostring` used only inside raise messages are
     best-effort; their drift is sanctioned.
   - *Stack-safety rewrites are allowed, with a parity comment.* Linear
     per-statement recursion may be converted to iteration / trampoline /
     work-list (resolve-scope, anf hole-patching, anf-loop-compiler,
     `computeLiveVars`, pprint) when the site carries a comment arguing
     effect-order parity. What must **never** be silently converted: anything
     that reorders gensym / `global-names` effects — generated names appear in
     the output.
2. Keep one TS function per Pyret function, same name (camelCased), same
   argument order.
3. **Comments: no narration in mechanical ports — but the runtime boundary is
   the exception.** Carry over Pyret `why` comments; don't add narration to a
   straight port. DO add architecture headers and `DEVIATION:` / parity-argument
   comments at the runtime boundary (cli-module-loader, server, pyret, url,
   builtin-modules, the interop layer). That is where the port's load-bearing
   documentation is *supposed* to live.
4. **Not-ported taxonomy — pick the right marker; silent omission is never
   allowed.**
   - `throw new TODOError("...")` (from `shared.ts`) — *intended to be ported,
     not done yet.*
   - explanatory `throw new InternalCompilerError("dead: ...")` — *dead by
     construction*: a Pyret path the port's architecture deliberately replaces
     (e.g. in-process `run` / `propagate-exit`, superseded by the child-process
     standalone). Do **not** use `TODOError` for these — it invites a port that
     will never come.
   - Silently dropping a method or branch is **not allowed.** A dropped method
     (`_equals`, `pyretToJsPretty`, …) must throw explicitly or carry a NOTE
     explaining why it is unreachable.
5. **Latent Pyret bugs.** Mirror non-crashing quirks bit-for-bit; fix a bug only
   when it would crash the port (or the reference, but not both). Leave a
   `// NOTE:` at the site either way. Never silently "fix" a quirk the reference
   compiler still exhibits — that breaks parity.
6. `provide *` ⇒ export everything the Pyret module exported.
