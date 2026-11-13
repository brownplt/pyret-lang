# js-numbers oracle test suite

## Maintainers say

This directory has an oracle-based suite for the js-numbers library. The oracles
are Racket, Julia, and CPython, each with their own number library. The goal is
to, for every test, one of:

- Make sure js-numbers agrees with the majority vote of the oracles
- If there is disagreement between all 3, we picked the answer we want on purpose
- If we want to disagree with the oracles, we picked the answer we want on purpose

All the infrastructure exists to support that.

Tests are written in JSON format in `cases/*.json`. A test looks like this:

```
    {"id":"asin-boundary-0003","family":"boundary","args":["-1"],"expect":{"rough":"bff921fb54442d18"},"mode":"ulp","agreed":["julia","python","racket"]},
```

This describes a test for `asin(-1)`, and gives the expected answer as the
64-bit value of the double that should be produced (in Pyret we call these
Roughnums, hence `"rough"`). All three oracles agree on the result, which is
exactly those bits. This particular operation allows agreement within an ulp
tolerance specified in `policy.js`. More on that in the agent-generated details
below.

Here's an example where there is *dis*agreement:

```
    {"id":"divide-boundary-0190","family":"boundary","args":["0","~-0.0"],"expect":{"error":"div-by-zero"},"mode":"exact","agreed":["julia","python","racket"],"policy":["zero-divisor"],"raw":{"racket":"exact:0","julia":"rough:fff8000000000000","python":"div-by-zero"}},
```

Note that here there are `"raw"` results for the three other systems. Here we
had to make a *policy decision* for our library. We decided that dividing by 0
should always be an error, and this division by approximate zero “counts” as a
zero. We happen to agree with Python in this case, but also it's useful to have
a mechanism to say “we don't care what all the other tools do, here's our
result”.

There are some other formats worth mentioning, because many of the interesting
parts are around exact rational results. For example:

```
    {"id":"divide-boundary-0319","family":"boundary","args":["67108865","1267650600228229401496703205375"],"expect":{"exact":"13421773/253530120045645880299340641075"},"mode":"exact","agreed":["julia","python","racket"]},
```

In this case, there's no 64-bit values and no error. This just says that all of
these systems produce the same rational number, written as `n/d` in reduced
form.

Overall, most cases should (and do) not need any mention of a "policy"
– js-numbers just agrees on exactly the same output as the other libraries. The
detailed notes below show how a few different decisions propagate through the
library.

Contributing new tests means adding new cases, which requires installing and
running on all the oracles to get their values. New tests should mostly get
added to `cases.json` with no policy, just an indication of agreement.

If you want to write new tests just of Pyret numbers, adding them in
lang/tests/pyret/tests/test-numbers.arr is a great thing to do. This directory
is mainly for detailed numeric testing of the js-numbers library that needs
assurances of numerical precision.

---

## Details (Claude, agent-written)

Everything below this line was written by the agent that built this
suite.

### What this is

`cases/*.json` holds test cases for `js-numbers.js`, one
file per exported operation. Every frozen case's expected value was
independently computed by Racket, Julia, and Python/mpmath and all three
agreed (after the policy adjustments in `policy.js` — see below). The
point of going through three unrelated numeric towers is that a fix and a
test written by the same hand can share a wrong assumption and pass
together; an external oracle can't share that assumption.

`make jsnums-test` runs the frozen cases against the library in plain
Node, no build, in well under a minute, and needs nothing beyond the
repo. `make jsnums-oracle` is the only target that needs Racket, Julia,
and Python installed; it only reruns when case files or harness code
change, and only touches cases that aren't frozen yet.

### Running it

```
make jsnums-test                      # node --test, ~15s, no external tools
JSNUMS_PATH=/path/to/js-numbers.js make jsnums-test   # test a different copy
JSNUMS_FILTER=divide-F26 make jsnums-test             # subset by case id substring
JSNUMS_TIMEOUT=30000 make jsnums-test                 # per-case watchdog, ms (default 10000)

make jsnums-oracle                    # needs racket, julia, python3+mpmath
node tests/jsnums-test/oracle/freeze.js --refreeze    # regenerate every case from scratch
node tests/jsnums-test/gen/generate.js [op ...]       # add generator inputs as pending

node tests/jsnums-test/compare.js A.js B.js --details                 # tabulate by case family
node tests/jsnums-test/sensitivity.js --base <ref> --fixed <ref|path>  # revert a hunk, see what fails
```

`JSNUMS_PATH`/`JSNUMS_FILTER`/`JSNUMS_TIMEOUT` are read by `runner.test.js`
and `watchdog.js`. The watchdog exists because a buggy library can hang
forever on some input (e.g. `remainder` by zero on an unguarded
implementation); each case runs in a worker thread and a hang becomes a
timed-out failure instead of blocking the whole run.

The runner evaluates every case under three jsbn digit configurations
(26-, 28-, 30-bit, by injecting a fake `navigator.appName`), because jsbn
picks its digit width from `navigator.appName` and that's a real
reachability axis (Node ≥ 21 defines a `navigator` global with no string
`appName`, which is how a division bug in the 26-bit configuration became
reachable in the first place).

### Case file shape

```jsonc
{
  "op": "divide",
  "kinds": ["num", "num"],          // argument kinds: num | double | str | bool
  "generated": { "generator": "gen/generate.js", "seed": 20260904 },
  "oracles": { "racket": "8.10 [cs]", "julia": "1.12.7", "python": "Python 3.12.3 mpmath 1.4.1" },
  "cases": [
    { "id": "divide-boundary-0006", "family": "boundary", "args": ["5", "0"],
      "expect": { "error": "div-by-zero" }, "mode": "exact",
      "agreed": ["julia", "python", "racket"] }
  ],
  "disputed": [],   // oracles disagreed; raw outputs kept; the runner ignores these
  "pending": []      // no oracle has run on these yet
}
```

Argument and expected-value literals use one canonical syntax (see
`numfmt.js`): an integer or `n/d` fraction for exact values (no exponent
notation — write out the digits), and a tilde-prefixed JS-float literal
for roughnums (`~1.5`, `~1e300`, `~-0.0`, `~inf`, `~nan`). `expect` is one
of an exact fraction (`{exact: "n/d"}`), the raw 64 bits of a double
(`{rough: "<16 hex>"}` or `{double: "..."}` for `toFixnum`'s raw-JS-number
result), an error class (`{error: "div-by-zero"}` or `{error: "error"}`),
a boolean, a string, or `{none: true}` for `fromString`'s `false`. `mode`
is `"exact"` unless it's `"ulp"` (rough transcendental results — see
`ULP_LIMIT` in `policy.js`), `"decimal-string"` (`toStringDigits`), or
`"repeating"` (`toRepeatingDecimal`).

The runner holds every result to one universal rule instead of a
per-case kind assertion: the representation must be canonical for the
value — small integers unboxed, larger ones `BigInteger`, a reduced
`Rational` only for non-integers, `Roughnum` only when the value is
rough. `makeBignum` is the one exception (it's the `BigInteger`
constructor; internal callers rely on getting a `BigInteger` back even
for small values), marked with `repr: 'bigint'` in `jsops.js`.

### Adding a case to an existing operation

Add an entry to that op's `pending` array by hand, or add an input family
to `gen/generate.js`'s `GEN[op]` function and run
`node gen/generate.js <op>`, then `make jsnums-oracle` to freeze it (or
`--refreeze <op>.json` to recompute everything for that file from
scratch, which is useful after a `policy.js` change). Freezing is
deterministic and idempotent: running it twice produces byte-identical
files.

### Adding a new operation

1. Add a row to `jsops.js`: argument `kinds`, `result` type, and the call
   into the library.
2. Add a matching row to each of the three oracle tables
   (`oracle/racket.rkt`, `oracle/julia.jl`, `oracle/python.py`) —
   same operation name as the key, computing the same mathematical
   function. These tables are the human-reviewable core of the suite;
   keep each row to a line or two, and push anything nontrivial into a
   named helper (see `pyret_expt`/`pyret_atan2`/etc. in each file) rather
   than inlining it.
3. If the result is a rough transcendental value, add an entry to
   `ULP_LIMIT` in `policy.js`.
4. Add input cases (by hand or via `gen/generate.js`) and run
   `make jsnums-oracle`.
5. Check `oracle/disagreements.md` (git-ignored, regenerated each run) —
   anything there means the three oracles didn't agree and needs either a
   policy rule or a row fix, not a forced value.

### Why js-numbers departs from the oracles (`policy.js`)

Every departure from raw oracle consensus is a named rule, stamped onto
the frozen cases it applies to, alongside the oracles' raw, unmodified
outputs (`"raw": {...}`), so a reviewer can always see what the oracles
actually said before a rule touched it.

Most of these rules only reclassify or reshape a result that the oracles
(or that specific oracle) already agreed was unusual in some way, so the
case still carries a real `"agreed": [...]` list alongside `"policy":
[...]`. A few rules — `zero-divisor`, `rough-zero-exponent-ambiguous`,
`trig-overflow-is-error`, `expt-exponent-limit`, `min-value-tolerance-error`
(listed in `OVERRIDE_RULES` in `policy.js`) — trigger on the arguments
alone and overwrite every oracle's result without ever looking at it, so
"all three oracles agreed" would be true by construction and isn't real
evidence. Those cases carry `"policy-override": [...]` instead of
`"policy"`/`"agreed"`, to make clear there's no agreement being reported,
only an override and the (still-attached) raw outputs it discarded. See
`divide-boundary-0190` in `cases/divide.json` for a case where this
matters: Racket, Julia, and Python computed three different things for
`divide(0, ~-0.0)` (an exact-zero short-circuit, `NaN`, and a native
`ZeroDivisionError`, respectively) and all three get overwritten to the
same `div-by-zero` error regardless.

| rule | what it does |
|---|---|
| `mixed-compare-via-double` | comparing an exact value against a roughnum converts the exact side to the nearest double first, matching js-numbers' own comparison semantics rather than an exact-vs-exact comparison |
| `no-nonfinite` | an infinite or NaN oracle result is a js-numbers error ("roughnum overflow"), except `toFixnum`, which returns the raw double |
| `rough-equals-raises` | `equals` with any rough argument raises |
| `rough-numden` | `numerator`/`denominator` of a roughnum are roughnums |
| `zero-divisor` | `divide`, `remainder`, `makeRational` with a zero divisor, and `expt(0, negative)`, are division-by-zero errors regardless of what the raw oracle computed (some give infinity, NaN, or their own divide-by-zero exception) |
| `exact-at-identity` | the identities `sin/tan/asin/atan(0)`, `cos/exp(0)`, `log/acos(1)`, `atan2(0, positive)`, `expt(x, 0)`, `expt(1, y)`, `expt(0, positive)` stay exact even if the oracle's raw answer happens to look like a rounded double |
| `modulo-zero-is-domain-error` | `modulo` by zero is a domain error, not a division error |
| `exact-zero-annihilates` | exact 0 times, or divided by, a roughnum is exact 0 |
| `exact-zero-add-identity` | adding exact 0 returns the other operand completely unchanged (so `0 + ~-0.0` stays `~-0.0`) |
| `trig-overflow-is-error` | `sin/cos/tan` of an exact value beyond the double range raise, rather than the silent overflow some oracles produce |
| `expt-exponent-limit` | an integer exponent beyond the fixnum range (for a base other than 0 or 1) is an error, mirroring jsbn's own `2^32-1` limit on bignum exponentiation |
| `min-value-tolerance-error` | `roughlyEquals` with a tolerance of exactly `Number.MIN_VALUE` raises when the difference is also exactly `Number.MIN_VALUE` (no meaningful comparison is possible at that resolution) |
| `rough-zero-exponent-ambiguous` | `expt(0, y)` with `y` a rough zero raises, rather than taking the oracles' `pow(0,0)=1` IEEE convention. This one is worth spelling out: for a nonzero base, a rough exponent near 0 is a rounding error away from the exact-0 answer (continuity). For a base of exactly 0, the three neighborhoods of a rough zero exponent give three *unrelated* answers — `0` for a positive epsilon, `1` at exactly zero, division-by-zero for a negative epsilon — so there's no single value to round to, and the oracles' raw `1.0` doesn't see that because plain IEEE float `pow` doesn't model the ambiguity at all. |

`ULP_LIMIT` in `policy.js` sets the tolerance for the `"ulp"` comparison
mode (`sin cos tan asin acos atan exp log expt atan2`, plus `sqrt` at 0
ulp since IEEE guarantees it's exact): 1 ulp except `atan2`, `log`, and
`expt` at 2. That's what this Node's own libm actually produces with no
slack to spare — don't tighten it without checking `compare.js` doesn't
start flagging real libm rounding as a regression.

### Out of scope, on purpose

- `fromSchemeString` — a legacy WeScheme-syntax reader that nothing in
  `runtime.js` or any trove module calls. Julia and Python have no
  equivalent reader, so it can never reach two-oracle consensus; it has
  no rows in the oracle tables.
- `_innards`, `FloatPoint`, `Complex` (a FIXME-marked alias of
  `Roughnum`) — internal/legacy exports, not real operations.
- `MIN_FIXNUM`/`MAX_FIXNUM` and the `BigInteger`/`Rational`/`Roughnum`
  classes — checked directly by `policy.test.js`, not via the case files.

`policy.test.js` is also where to look for anything that genuinely can't
be expressed as an oracle case: the digit-configuration default, the
module's export surface, and resource guards (e.g. that `expt` refuses an
exponent search that would otherwise run forever) that need a worker
thread to test safely rather than an oracle answer.
