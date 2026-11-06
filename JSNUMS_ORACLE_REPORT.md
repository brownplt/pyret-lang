# js-numbers oracle suite: report

Author: Claude (agent), 2026-09-04. Everything in this file is agent-written.
It describes the two branches `jsnums-oracle-suite` and `jsnums-hardening-rebased`
built against goal file `~/work/JSNUMS-ORACLE-GOAL.md`. Nothing was pushed.

## 1. What was delivered

`jsnums-oracle-suite` (off `drydock`, no change to `js-numbers.js`):

| path | role | lines |
|---|---|---:|
| `lang/tests/jsnums-test/cases/*.json` | 59 case files, 21,915 frozen cases, provenance per case | data |
| `lang/tests/jsnums-test/oracle/racket.rkt` | Racket harness + op table | 188 |
| `lang/tests/jsnums-test/oracle/python.py` | Python harness + op table | 227 |
| `lang/tests/jsnums-test/oracle/julia.jl` | Julia harness + op table | 257 |
| `lang/tests/jsnums-test/policy.js` | policy module (12 named rules) | 174 |
| `lang/tests/jsnums-test/jsops.js` | JS op table (one row per operation) | 70 |
| `lang/tests/jsnums-test/oracle/freeze.js` | consensus and freeze tool (`make jsnums-oracle`) | 246 |
| `lang/tests/jsnums-test/suite.js`, `watchdog.js`, `runner.test.js` | plain-Node runner (`make jsnums-test`) | 410 |
| `lang/tests/jsnums-test/policy.test.js` | 8 hand-written checks no oracle can express | 70 |
| `lang/tests/jsnums-test/gen/generate.js` | seeded input generator (seed 20260904) | 276 |
| `lang/tests/jsnums-test/compare.js`, `sensitivity.js`, `sensitivity-groups.json` | drydock-vs-branch table, hunk reversion | 166 |
| `lang/tests/jsnums-test/numfmt.js`, `load.js` | canonical syntax, bits, loader with fake `navigator` | 204 |

`jsnums-hardening-rebased` (stacked on the above): one commit with the
branch's `js-numbers.js` diff squashed onto drydock, then ten correction
commits (section 6).

The jasmine file `lang/tests/jsnums-test/jsnums-test.js` (needed a phaseA
build) is removed; its three assertions live on in `policy.test.js`.
`lang/Makefile` gains `jsnums-oracle` and `jsnums-sensitivity`, and
`jsnums-test` no longer depends on `phaseA`. `lang/.gitignore` gains the
freeze stamp file. Both are system prose-free edits; flagging them here.

### How to run

```
cd lang
make jsnums-test                 # node --test, no build, ~16 s on the fixed library
make jsnums-oracle               # needs racket, julia, python3+mpmath; reruns only when inputs change
node tests/jsnums-test/oracle/freeze.js --refreeze   # regenerate every frozen case from scratch
node tests/jsnums-test/gen/generate.js               # add any new generator inputs as pending
node tests/jsnums-test/compare.js A.js B.js --details
make jsnums-sensitivity JSNUMS_BASE=drydock JSNUMS_FIXED=lang/src/js/base/js-numbers.js JSNUMS_GROUPS=tests/jsnums-test/sensitivity-groups.json
JSNUMS_PATH=/other/js-numbers.js make jsnums-test    # test another copy of the library
JSNUMS_FILTER=divide-F26 make jsnums-test            # subset by case id
```

Oracle versions recorded in every case file: Racket 8.10 [cs], Julia 1.12.7,
Python 3.12.3 with mpmath 1.4.1. Node 24.20.0 ran the suite.

## 2. Design as built, and where it deviates from the goal file

Case files hold `cases` (frozen, with `agreed` listing the oracles and
`policy` listing the rules applied), `disputed` (oracles disagreed; raw
outputs kept; runner ignores them) and `pending`. One case per line so
diffs review. Expected values are an exact reduced fraction, the raw 64
bits of a double, an error class (`div-by-zero` or `error`), a boolean, or
`false` for `fromString`. Rough results of the transcendental operations
carry `mode: "ulp"`.

Harness protocol is TSV in, TSV out, one line per case; the JS driver
never interprets oracle output beyond the seven result forms. A case is
frozen only when every oracle that ran agrees after policy normalization
(section 4) and at least two ran; every frozen case was agreed by all
three.

Deviations, each with the reason:

- **Roughnum-to-exact conversion lives in the op tables, not the policy
  module.** js-numbers reads a roughnum as the decimal it prints as
  (`Roughnum.toRational` goes through `fromString(n.toString())`), so
  `toRational(~0.1)` is `1/10`. The goal file put this in the policy
  module. It also governs intermediate values (`toStringDigits(~1e21, 2)`
  rounds the double 1e23 as the decimal `1e23`), which a pre-transform of
  the arguments cannot express. Each harness has a one-line `float_exact`
  helper (Racket `number->string` then `decimal-as-exact`; Python
  `Fraction(repr(x))`; Julia `parse_decimal(string(x))`) and the rows for
  `toRational`, `toExact`, `numerator`, `denominator`, `fromFixnum`,
  `floor`, `ceiling`, `round`, `roundEven`, `roughlyEquals`,
  `roughlyEqualsRel` and `toStringDigits` use it. The three printers agree
  on shortest round-trip digits (checked for subnormals, 1e21, 1e23,
  `-0.0`).
- **Operation preconditions are in the rows.** Pyret's integer-only
  operations (`modulo`, `quotient`, `gcd`, `lcm`, `integerSqrt`,
  `toRepeatingDecimal`), its "exact root only for denominators up to 8"
  rule for `expt`, its real odd roots of negative bases, and its
  `[0, 2pi)` range for `atan2` are written into each language's row. The
  alternative was a policy rule that overrides consensus for every such
  case, which would have made the override list thousands long. The rows
  are the reviewable artifact; the three implementations are independent.
- **Transcendental references are taken at the double nearest the input**
  (`via1`/`via2` helpers), because that is what js-numbers does; the
  exact value is used only when rounding to a double would lose the
  magnitude (over- or underflow), which is where js-numbers switches to
  its digit-count algorithms. Bit-exact comparison against a 200-bit
  MPFR/mpmath reference then rounded is what "correctly rounded" means
  here; all three oracles agreed bit-for-bit on every such case.
- **A worker-thread watchdog.** drydock's `remainder(1/2, 0)` and
  `expt(3/2, 2^32)` never return, so the runner evaluates cases in a
  worker and reports a case that exceeds `JSNUMS_TIMEOUT` (default 10 s)
  as a failure. Without it `make jsnums-test` on drydock hangs.
- **`makeBignum` is exempt from the canonical-representation rule.** It
  is the BigInteger constructor; internal callers rely on getting a
  BigInteger back (`makeBignum(x).add(makeBignum(y))`). The runner checks
  it returns a BigInteger of the right value. Every other operation is
  held to: small integers unboxed, larger ones `BigInteger`, reduced
  `Rational` only for non-integers, `Roughnum` only when rough.
- **Fixnum negative zero is accepted.** `ceiling(~-0.5)`, `round(~-0.4)`
  and `multiply(-1, 0)` return the JS number `-0`. It is invisible to
  Pyret (`num-to-string` prints `0`, comparisons and division by zero
  behave). 149 frozen cases produce it on the fixed library; the runner
  counts them. Rejecting it would be a one-line change in
  `suite.js` (`negZero`) if a human prefers.
- **`fromSchemeString` is out of scope** (section 8).

## 3. The fix list, derived from the diff

`git diff drydock...js-numbers-hardening -- lang/src/js/base/js-numbers.js`
(348 lines changed) decomposes into these distinct fixes. Hunk numbers
refer to `git diff -U0` between drydock and the rebased branch, as
printed by `sensitivity.js --list`.

| id | hunks | what the diff does | verdict from the suite |
|---|---|---|---|
| F01 | 1 2 26 27 33 | `normalizeInteger`: BigInteger results that fit a fixnum are unboxed (binops, integer ops, `Rational` parts) | real; the universal representation rule fails 8811 case-configs on drydock |
| F02 | 3 | `fromFixnum(NaN/Infinity)` throws instead of returning `false` | real |
| F03 | 4 | `divide` with a rough zero numerator returns the rough zero, not exact 0 | real (oracles: `0.0/5 = 0.0`) |
| F04 | 5 | `equalsAnyZero` on a BigInteger zero | real |
| F05 | 6 | `eqv` on two roughnums compares the doubles instead of raising | real |
| F06 | 11 12 | fixnum `expt` by exact square-and-multiply instead of `Math.pow` | no observable effect: a search of every integer base up to 10^7 and every exponent whose result is within the fixnum range (10.2 million powers) found no inexact `Math.pow` on Node 24; the hunk is harmless and exact by construction, but the claimed bug is not demonstrable here |
| F07 | 13 | `expt(0, ~0)`: branch returned `~1`; drydock returns `0` | branch's value contradicts the spec; corrected to exact 1 (C01), see section 9 |
| F08 | 15 | `modulo` fixnum path requires both operands unboxed | real: `modulo(-3, 10^16)` was the string `"-310000000000000000"` |
| F09 | 16 | fixnum `sqrt` exact only if `r*r === n` | real near 2^52 |
| F10 | 21 | fixnum `integerSqrt` adjusts by one near squares | real |
| F11 | 22 | `lcm` takes `abs` of the second argument | real: `lcm(4, -6)` was `-12` |
| F12 | 23 24 | `quotient`/`remainder` by zero raise | real: drydock returns NaN, or loops forever for a rational dividend |
| F13 | 25 | `isOverflow(NaN)` is true | real (drydock lets NaN through as a fixnum) |
| F14 | 28 29 31 67 | correctly rounded integer/integer to double (`bigIntegerQuotientToDouble`) for `toFixnum` | real: about 20% of bignum/rational conversions were 1 ulp off, ties at the subnormal boundary wrong |
| F15 | 32 | `makeRational` with zero denominator raises | real |
| F16 | 34 35 | `Rational.sqrt` rewrite | real (drydock: `sqrt(2/9)` went through `sqrt(2)/sqrt(9)` with rounding) |
| F17 | 37 | `Rational.expt` refuses integer exponents above 2^32-1 | real: drydock's `expt(3/2, 2^32)` runs out of memory |
| F18 | 40 41 | `Roughnum.numerator/denominator` via `toRational` | real: string hack broke on `1e21`-style doubles |
| F19 | 42 43 44 | `Roughnum` floor/ceiling/round beyond 9e15 return bignums | real (drydock returned unboxed doubles above the fixnum bound) |
| F20 | 45 | `Roughnum.roundEven` sign | real: `roundEven(~-2.5)` was `2` |
| F21 | 46 | `Roughnum.expt` NaN becomes a domain error | real (message only; drydock threw "roughnum overflow") |
| F22 | 49 50 | `fromString("1/0")` returns `false` | real; Racket also reads `1/0` as not-a-number |
| F23 | 51-58 | `fromSchemeString` family | out of scope, unverified (section 8) |
| F24 | 59 60 | `navigator.appName` guard for Node 21+ | real; `policy.test.js` checks the default is 28-bit digits |
| F25 | 61 | `bnpFromInt`: `x+DV` to `x+this.DV` | unreachable: `fromInt` is only ever called with 0, 1 or a small prime |
| F26 | 62 | jsbn `divRemTo` quotient-digit repair | real, 26-bit configuration only: 284 of 200,000 random quotients wrong on drydock; 0 in 28- and 30-bit |
| F27 | 63 64 | `bnMod` with a negative modulus | real: `modulo(-10^16, -3)` was `-4` |
| F28 | 66 | `makeBignum` rejects non-integer strings | real, incomplete: `"1.5e0"` still returned `1` (C08) |
| F29 | 69 70 | `BigInteger.sqrt` of a huge non-square | wrong fix: replaced a false exact with a "roughnum overflow" error although `~1e200` is representable (C07) |
| F30 | 73 | `toStringDigits` expansion limit `max(512, d+10)` | real |
| F31 | 74 75 | wrapped module exports `makeRational`, `makeRoughnum`, the classes, bounds, `_innards` | real: the wrapped surface was missing names the runtime uses via `MakeNumberLibrary` |

Baseline: the branch's own suite (`node --test`, 121 tests) passes 121 in
9.2 s on its own library.

## 4. Policy overrides

Every departure from the oracles is a named rule in `policy.js`, stamped
on the frozen case (with the raw oracle outputs kept beside it). Counts
are frozen cases carrying the rule.

| rule | cases | one sentence |
|---|---:|---|
| `mixed-compare-via-double` | 1613 | comparing a rough with an exact converts the exact side to the nearest double first (Racket, Julia and Python compare exactly); the converted argument is recorded as `oracle_args` |
| `no-nonfinite` | 558 | an infinite or NaN oracle result is a js-numbers error ("roughnum overflow"), except for `toFixnum`, which returns the raw double |
| `rough-equals-raises` | 557 | `equals` with a rough argument raises |
| `rough-numden` | 194 | `numerator`/`denominator` of a roughnum are roughnums |
| `zero-divisor` | 91 | `divide`, `remainder`, `makeRational` with a zero divisor, and `expt(0, negative)`, are division-by-zero errors whatever the oracle did (IEEE infinity, NaN, Python `ZeroDivisionError`, Racket's exact `0` for `(/ 0 -0.0)`) |
| `exact-at-identity` | 37 | `sin/tan/asin/atan(0)`, `cos/exp(0)`, `log/acos(1)`, `atan2(0, positive)`, `expt(x, 0)`, `expt(1, y)`, `expt(0, positive)` and `expt(0, ~0)` are exact with an exact zero or one argument (numbers.scrbl) |
| `modulo-zero-is-domain-error` | 34 | `modulo` by zero is reported as a domain error, not a division error |
| `exact-zero-annihilates` | 8 | exact 0 times, or divided by, a roughnum is exact 0 (Racket agrees) |
| `exact-zero-add-identity` | 3 | adding exact 0 returns the other operand unchanged, so `0 + ~-0.0` is `~-0.0` (Racket agrees; IEEE gives `+0.0`) |
| `trig-overflow-is-error` | 3 | `sin/cos/tan` of an exact beyond the double range raise (test-numbers.arr requires it) |
| `expt-exponent-limit` | 1 | an integer exponent beyond the fixnum range with a base other than 0 or 1 is an error (jsbn's 2^32 guard), so `(-1)^(10^21+1)` raises rather than giving -1 |
| `min-value-tolerance-error` | 1 | `roughlyEquals` with tolerance `~5e-324` raises when the difference is exactly `5e-324` |

The `ulp` comparison mode applies to `sin cos tan asin acos atan atan2 exp
log expt` (and `sqrt` with limit 0). Limits in `policy.js`: 1 ulp for
everything except `atan2` (2), `log` (2) and `expt` (2). Observed maxima on
the fixed library: `expt` 2 (one case, `expt(416, 5/6)`, the nth-root path
raises `416^5` to `1/6` in doubles), everything else 1. On drydock and on
the unmodified branch `log` reached 837,091 ulp (section 6, C05) and `expt`
28 ulp (C09). The threshold is a human decision; the numbers above are
what this Node (V8's own libm port) produces.

## 5. Oracle disagreements

Final state: 0 disputed cases across 21,915. Every disagreement met on the
way is listed here with its classification, because each one changed a row
or a rule and a reviewer should be able to disagree with the classification.

| inputs | Racket | Julia | Python | classification and resolution |
|---|---|---|---|---|
| `1/3 / ~0.5` | 0.666… double | 200-bit BigFloat | double | oracle quirk: Julia promotes Rational{BigInt} with Float64 to BigFloat; rows use `mixed()` to convert to doubles first |
| `0 / ~2.5` | exact 0 | 0.0 | 0.0 | Pyret policy `exact-zero-annihilates` (matches Racket) |
| `1 / ~-0.0`, `~1/~0` | -inf | -Inf | ZeroDivisionError | policy `zero-divisor` |
| `0 / ~-0.0` | exact 0 | NaN | ZeroDivisionError | policy `zero-divisor` (Racket's exact-zero shortcut wins even over a zero divisor) |
| `toFixnum(10^400)` | +inf | Inf | OverflowError | oracle quirk: Python's float(Fraction) raises on overflow; the row uses an IEEE-overflow helper |
| `0 + ~-0.0` | -0.0 | 0.0 | 0.0 | Pyret policy `exact-zero-add-identity` (matches Racket) |
| `atan(~-0.0)`, `sin`, `tan`, `sqrt`, `atan2(~-0.0, x>0)` | -0.0 | -0.0 | +0.0 | oracle quirk: mpmath has no signed zero; the row restores the sign for odd functions and sqrt |
| `integerSqrt(-1)` | complex | DivideError | ValueError | oracle quirk: GMP raises a division error for the square root of a negative; row guards |
| `fromString("~1/0")` | #f | none | ZeroDivisionError | oracle quirk: Python's `Fraction("1/0")`; row checks the denominator first |
| `fromString("١")` (Arabic digit) | #f | error | exact 1 | oracle quirk: Python's `\d` matches Unicode digits, JS's does not; all three gates use `[0-9]` |
| `log(10^400)` | 921.03 | 921.03 | error | oracle quirk: CPython's 4300-digit int-to-str limit; harness lifts it |
| `roughlyEqualsRel(0, 1, 2, false)` | div-by-zero | false | div-by-zero | oracle quirk: Julia's `1//0` is a legal value; row raises (js-numbers divides and raises) |
| `toStringDigits(~2.5, 0)` etc. | exact | MethodError | exact | harness bug: Julia product promoted to BigFloat; row uses `mixed(*)` |
| `toStringDigits(~0.1, 400)` | error | DivideError | error | oracle quirk: Julia's `Rational(Inf)` raises DivideError; row checks finiteness |
| `fromString("~1e-400")` | 0.0 | parse error | 0.0 | oracle quirk: Julia's `parse(Float64)` rejects underflow; row parses the exact decimal then converts |
| `remainder(-big, ~1.86e156)` | error | DivideError | error | same Julia quirk; row checks finiteness |
| `remainder(~5.5, 3/5)` | | | | my row bug: js-numbers converts both operands to doubles before `%`; rows now do the same, then exact fmod |
| `remainder(~-4.9e-185, -10^300…)` | error | error | error | my row bug: IEEE `fmod(x, ±inf) = x`, which is what JS `%` gives; rows now mirror fmod, including the sign of a zero result |
| `expt(-1, 2/3)`, `expt(-2, 2/3)` | | | | my row bug in all three tables: the sign of a negative base with an odd denominator is `(-1)^p`, not `-1`; shows the shared-assumption risk the goal warns about, caught only because js-numbers disagreed |
| `fromString("~-0")` | 0.0 | 0.0 | 0.0 | rows read `~` literals via exact then converted, losing the sign; JS `Number("-0")` is `-0`; rows now parse the float directly |
| `makeBignum("2.0")` | 2 | 2 | 2 | my gate was wider than js-numbers' grammar (`digits[.digits]e+digits` or `digits`); tightened |
| `sqrt(10^400+1)` | 1e200 | Inf | OverflowError | Racket is right; `via1` now evaluates at the exact value when the double overflows, and C07 makes js-numbers agree |
| `log(1/9^7813)` | | | | `via1` initially used the double-rounded input, which underflows to 0; now uses the exact value when the double would be 0; js-numbers computes this one from digit counts and is 1 ulp from the reference |

## 6. drydock versus branch versus corrected

Three runs of the frozen suite (21,915 cases, three digit configurations
each, 65,745 evaluations): drydock's `js-numbers.js` (A), the hardening
branch's (B), and the rebased branch with corrections (C).

Summary (file-config groups, `make jsnums-test` plus `policy.test.js`):

| library | runner groups passing | policy checks passing | wall time |
|---|---:|---:|---:|
| drydock | 66 / 177 | 4 / 8 | 3 min 30 s (`make jsnums-test`; watchdog timeouts on hanging cases) |
| branch 6c558ba68 | 144 / 177 | 7 / 8 | 1 min 15 s |
| rebased + corrections | 177 / 177 | 8 / 8 | 16 s (`make jsnums-test`, runner plus policy checks) |

Per case family, drydock (A) against the branch (B):

| op | family | cases x configs | fail A, pass B | fail both | pass A, fail B |
|---|---|---:|---:|---:|---:|
| abs | rand | 402 | 30 | 0 | 0 |
| acos | boundary | 57 | 0 | 3 | 0 |
| add | boundary | 1128 | 72 | 0 | 0 |
| add | rand | 894 | 87 | 0 | 0 |
| asin | boundary | 57 | 0 | 3 | 0 |
| atan2 | boundary | 507 | 48 | 114 | 0 |
| atan2 | rand | 600 | 0 | 6 | 0 |
| ceiling | boundary | 210 | 39 | 0 | 0 |
| ceiling | F19 | 24 | 21 | 0 | 0 |
| ceiling | rand | 519 | 84 | 0 | 0 |
| denominator | boundary | 93 | 15 | 0 | 0 |
| denominator | F18 | 21 | 12 | 0 | 0 |
| denominator | rand | 447 | 69 | 0 | 0 |
| divide | boundary | 1248 | 303 | 0 | 0 |
| divide | F26 | 477 | 147 | 0 | 0 |
| divide | rand | 897 | 231 | 0 | 0 |
| eqv | boundary | 1875 | 363 | 0 | 0 |
| eqv | rand | 771 | 138 | 0 | 0 |
| expt | boundary | 1230 | 60 | 6 | 0 |
| expt | rand | 1182 | 165 | 24 | 0 |
| floor | boundary | 210 | 39 | 0 | 0 |
| floor | F19 | 24 | 21 | 0 | 0 |
| floor | rand | 504 | 87 | 0 | 0 |
| fromFixnum | boundary | 168 | 87 | 0 | 0 |
| fromFixnum | F02 | 9 | 9 | 0 | 0 |
| fromFixnum | rand | 333 | 243 | 0 | 0 |
| fromString | boundary | 375 | 81 | 0 | 0 |
| fromString | F22 | 18 | 18 | 0 | 0 |
| fromString | rand | 711 | 96 | 0 | 0 |
| gcd | boundary | 1812 | 879 | 0 | 0 |
| gcd | rand | 600 | 399 | 0 | 0 |
| greaterThan | rand | 774 | 0 | 6 | 0 |
| greaterThanOrEqual | rand | 762 | 0 | 6 | 0 |
| integerSqrt | boundary | 231 | 102 | 0 | 0 |
| integerSqrt | F10 | 180 | 102 | 0 | 0 |
| integerSqrt | rand | 639 | 114 | 0 | 0 |
| lcm | boundary | 1815 | 591 | 0 | 0 |
| lcm | F11 | 18 | 6 | 0 | 0 |
| lcm | rand | 591 | 258 | 0 | 0 |
| lessThan | rand | 768 | 0 | 9 | 0 |
| log | boundary | 165 | 0 | 9 | 0 |
| log | rand | 489 | 0 | 3 | 0 |
| makeBignum | boundary | 96 | 33 | 3 | 0 |
| makeBignum | F28 | 21 | 18 | 3 | 0 |
| makeRational | boundary | 249 | 114 | 3 | 0 |
| makeRational | F15 | 12 | 12 | 0 | 0 |
| makeRational | rand | 447 | 276 | 0 | 0 |
| modulo | boundary | 1797 | 765 | 0 | 0 |
| modulo | F26 | 27 | 27 | 0 | 0 |
| modulo | rand | 837 | 351 | 0 | 0 |
| multiply | boundary | 1128 | 18 | 0 | 0 |
| multiply | rand | 900 | 138 | 0 | 0 |
| numerator | boundary | 93 | 18 | 0 | 0 |
| numerator | F18 | 21 | 12 | 0 | 0 |
| numerator | rand | 414 | 75 | 0 | 0 |
| quotient | boundary | 1797 | 975 | 0 | 0 |
| quotient | F26 | 327 | 54 | 0 | 0 |
| quotient | rand | 834 | 381 | 0 | 0 |
| remainder | boundary | 1941 | 1092 | 0 | 0 |
| remainder | F26 | 327 | 267 | 0 | 0 |
| remainder | rand | 897 | 411 | 0 | 0 |
| round | boundary | 210 | 39 | 0 | 0 |
| round | F19 | 24 | 21 | 0 | 0 |
| round | rand | 507 | 81 | 0 | 0 |
| roundEven | boundary | 210 | 57 | 0 | 0 |
| roundEven | F19 | 24 | 24 | 0 | 0 |
| roundEven | F20 | 21 | 18 | 0 | 0 |
| roundEven | rand | 507 | 180 | 0 | 0 |
| sqr | rand | 549 | 15 | 0 | 0 |
| sqrt | boundary | 291 | 63 | 3 | 0 |
| sqrt | F09 | 180 | 48 | 0 | 0 |
| sqrt | rand | 936 | 141 | 0 | 0 |
| subtract | boundary | 1128 | 78 | 0 | 0 |
| subtract | rand | 900 | 144 | 0 | 0 |
| toExact | boundary | 93 | 36 | 0 | 0 |
| toExact | rand | 504 | 222 | 0 | 0 |
| toFixnum | boundary | 135 | 18 | 0 | 0 |
| toFixnum | F14 | 123 | 18 | 0 | 0 |
| toFixnum | rand | 1080 | 309 | 0 | 0 |
| toRational | boundary | 93 | 36 | 0 | 0 |
| toRational | rand | 483 | 216 | 0 | 0 |
| toRoughnum | boundary | 135 | 18 | 0 | 0 |
| toRoughnum | rand | 1083 | 279 | 0 | 0 |
| toStringDigits | boundary | 123 | 6 | 0 | 0 |
| toStringDigits | F30 | 9 | 9 | 0 | 0 |
| **total** | | 65745 | 12129 | 201 | 0 |

Details (first entries per family):

#### fail on both: acos / boundary
- acos-boundary-0008@28: A: got 0 [fixnum] | B: got 0 [fixnum]
- acos-boundary-0008@30: A: got 0 [fixnum] | B: got 0 [fixnum]
- acos-boundary-0008@26: A: got 0 [fixnum] | B: got 0 [fixnum]

#### fail on both: asin / boundary
- asin-boundary-0010@28: A: got 0 [fixnum] | B: got 0 [fixnum]
- asin-boundary-0010@30: A: got 0 [fixnum] | B: got 0 [fixnum]
- asin-boundary-0010@26: A: got 0 [fixnum] | B: got 0 [fixnum]

#### fail on both: atan2 / boundary
- atan2-boundary-0004@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 0 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 0 ~0
- atan2-boundary-0005@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 0 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 0 ~0
- atan2-boundary-0017@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 1 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 1 ~0
- atan2-boundary-0018@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 1 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, 1 ~0
- atan2-boundary-0026@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-boundary-0030@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, -1 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, -1 ~0
- atan2-boundary-0031@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, -1 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, -1 ~0
- atan2-boundary-0039@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-boundary-0040@28: A: expected error, got ~4.71238898038469 [4012d97c7f3321d2] | B: expected error, got ~4.71238898038469 [4012d97c7f3321d2]
- atan2-boundary-0043@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, ~0 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, ~0 ~0
- atan2-boundary-0044@28: A: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, ~0 ~0 | B: got error(div-by-zero): js-numbers div-by-zero: /: division by zero, ~0 ~0
- atan2-boundary-0053@28: A: expected error, got ~4.71238898038469 [4012d97c7f3321d2] | B: expected error, got ~4.71238898038469 [4012d97c7f3321d2]
- ... 102 more

#### fail on both: atan2 / rand
- atan2-rand-0082@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-rand-0108@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-rand-0082@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-rand-0108@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-rand-0082@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- atan2-rand-0108@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error

#### fail on both: expt / boundary
- expt-boundary-0014@28: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]
- expt-boundary-0015@28: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]
- expt-boundary-0014@30: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]
- expt-boundary-0015@30: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]
- expt-boundary-0014@26: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]
- expt-boundary-0015@26: A: got 0 [fixnum] | B: got ~1.0 [3ff0000000000000]

#### fail on both: expt / rand
- expt-rand-0299@28: A: off by 24 ulp (limit 2): got ~2.1270726335865246e+202 [69f15e29354509b5] | B: off by 24 ulp (limit 2): got ~2.1270726335865246e+202 [69f15e29354509b5]
- expt-rand-0323@28: A: off by 28 ulp (limit 2): got ~1.5250985811147657e+183 [65f6f895a77e3359] | B: off by 28 ulp (limit 2): got ~1.5250985811147657e+183 [65f6f895a77e3359]
- expt-rand-0342@28: A: off by 5 ulp (limit 2): got ~2.4860190222943427e-17 [3c7ca96edd74ee32] | B: off by 5 ulp (limit 2): got ~2.4860190222943427e-17 [3c7ca96edd74ee32]
- expt-rand-0344@28: A: off by 5 ulp (limit 2): got ~1.1064925534770745e+22 [4482bea5826b167b] | B: off by 5 ulp (limit 2): got ~1.1064925534770745e+22 [4482bea5826b167b]
- expt-rand-0353@28: A: off by 3 ulp (limit 2): got ~336017293895812250000.0 [4432372d3c8097ec] | B: off by 3 ulp (limit 2): got ~336017293895812250000.0 [4432372d3c8097ec]
- expt-rand-0362@28: A: off by 4 ulp (limit 2): got ~-114316811608443.8 [c2d9fe1d3ca85ef3] | B: off by 4 ulp (limit 2): got ~-114316811608443.8 [c2d9fe1d3ca85ef3]
- expt-rand-0363@28: A: off by 6 ulp (limit 2): got ~-6.598286410428221e-31 [b9aac40d6778f967] | B: off by 6 ulp (limit 2): got ~-6.598286410428221e-31 [b9aac40d6778f967]
- expt-rand-0387@28: A: off by 14 ulp (limit 2): got ~4.346841741163224e-46 [3683da554342558c] | B: off by 14 ulp (limit 2): got ~4.346841741163224e-46 [3683da554342558c]
- expt-rand-0299@30: A: off by 24 ulp (limit 2): got ~2.1270726335865246e+202 [69f15e29354509b5] | B: off by 24 ulp (limit 2): got ~2.1270726335865246e+202 [69f15e29354509b5]
- expt-rand-0323@30: A: off by 28 ulp (limit 2): got ~1.5250985811147657e+183 [65f6f895a77e3359] | B: off by 28 ulp (limit 2): got ~1.5250985811147657e+183 [65f6f895a77e3359]
- expt-rand-0342@30: A: off by 5 ulp (limit 2): got ~2.4860190222943427e-17 [3c7ca96edd74ee32] | B: off by 5 ulp (limit 2): got ~2.4860190222943427e-17 [3c7ca96edd74ee32]
- expt-rand-0344@30: A: off by 5 ulp (limit 2): got ~1.1064925534770745e+22 [4482bea5826b167b] | B: off by 5 ulp (limit 2): got ~1.1064925534770745e+22 [4482bea5826b167b]
- ... 12 more

#### fail on both: greaterThan / rand
- greaterThan-rand-0030@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThan-rand-0192@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThan-rand-0030@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThan-rand-0192@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThan-rand-0030@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThan-rand-0192@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error

#### fail on both: greaterThanOrEqual / rand
- greaterThanOrEqual-rand-0072@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThanOrEqual-rand-0106@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThanOrEqual-rand-0072@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThanOrEqual-rand-0106@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThanOrEqual-rand-0072@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- greaterThanOrEqual-rand-0106@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error

#### fail on both: lessThan / rand
- lessThan-rand-0060@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0186@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0189@28: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0060@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0186@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0189@30: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0060@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0186@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error
- lessThan-rand-0189@26: A: got error(error): js-numbers domain-error: roughnum overflow error | B: got error(error): js-numbers domain-error: roughnum overflow error

#### fail on both: log / boundary
- log-boundary-0021@28: A: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a] | B: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a]
- log-boundary-0022@28: A: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000] | B: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000]
- log-boundary-0023@28: A: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000] | B: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000]
- log-boundary-0021@30: A: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a] | B: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a]
- log-boundary-0022@30: A: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000] | B: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000]
- log-boundary-0023@30: A: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000] | B: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000]
- log-boundary-0021@26: A: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a] | B: off by 3 ulp (limit 2): got ~-0.4054651081081643 [bfd9f323ecbf984a]
- log-boundary-0022@26: A: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000] | B: off by 837091 ulp (limit 2): got ~9.999995000953277e-7 [3eb0c6f714000000]
- log-boundary-0023@26: A: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000] | B: off by 732624 ulp (limit 2): got ~-0.0000010000005001842283 [beb0c6f82d800000]

#### fail on both: log / rand
- log-rand-0042@28: A: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478] | B: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478]
- log-rand-0042@30: A: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478] | B: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478]
- log-rand-0042@26: A: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478] | B: off by 4 ulp (limit 2): got ~-0.13353139262452252 [bfc1178e8227e478]

#### fail on both: makeBignum / boundary
- makeBignum-boundary-0013@28: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]
- makeBignum-boundary-0013@30: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]
- makeBignum-boundary-0013@26: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]

#### fail on both: makeBignum / F28
- makeBignum-F28-0007@28: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]
- makeBignum-F28-0007@30: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]
- makeBignum-F28-0007@26: A: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum] | B: expected error, got 1 [bigint, NON-CANONICAL: BigInteger that fits a fixnum]

#### fail on both: makeRational / boundary
- makeRational-boundary-0081@28: A: expected error, got 1/3 [rational] | B: expected error, got 1/3 [rational]
- makeRational-boundary-0081@30: A: expected error, got 1/3 [rational] | B: expected error, got 1/3 [rational]
- makeRational-boundary-0081@26: A: expected error, got 1/3 [rational] | B: expected error, got 1/3 [rational]

#### fail on both: sqrt / boundary
- sqrt-boundary-0031@28: A: got 316227766016837933199889354443271853371955513932521682685750485279259443863923822134424810837930029518734728415284005514854885603045388001469051959670015390334492165717925994065915015347411333948412408 [bigint] | B: got error(error): js-numbers domain-error: roughnum overflow error
- sqrt-boundary-0031@30: A: got 316227766016837933199889354443271853371955513932521682685750485279259443863923822134424810837930029518734728415284005514854885603045388001469051959670015390334492165717925994065915015347411333948412408 [bigint] | B: got error(error): js-numbers domain-error: roughnum overflow error
- sqrt-boundary-0031@26: A: got 316227766016837933199889354443271853371955513932521682685750485279259443863923822134424810837930029518734728415284005514854885603045388001469051959670015390334492165717925994065915015347411333948412408 [bigint] | B: got error(error): js-numbers domain-error: roughnum overflow error

Per case family, the branch (A) against the corrected library (B):

| op | family | cases x configs | fail A, pass B | fail both | pass A, fail B |
|---|---|---:|---:|---:|---:|
| acos | boundary | 57 | 3 | 0 | 0 |
| asin | boundary | 57 | 3 | 0 | 0 |
| atan2 | boundary | 507 | 114 | 0 | 0 |
| atan2 | rand | 600 | 6 | 0 | 0 |
| expt | boundary | 1230 | 6 | 0 | 0 |
| expt | rand | 1182 | 24 | 0 | 0 |
| greaterThan | rand | 774 | 6 | 0 | 0 |
| greaterThanOrEqual | rand | 762 | 6 | 0 | 0 |
| lessThan | rand | 768 | 9 | 0 | 0 |
| log | boundary | 165 | 9 | 0 | 0 |
| log | rand | 489 | 3 | 0 | 0 |
| makeBignum | boundary | 96 | 3 | 0 | 0 |
| makeBignum | F28 | 21 | 3 | 0 | 0 |
| makeRational | boundary | 249 | 3 | 0 | 0 |
| sqrt | boundary | 291 | 3 | 0 | 0 |
| **total** | | 65745 | 201 | 0 | 0 |

Details:



Per case family, drydock (A) against the corrected library (B):

| op | family | cases x configs | fail A, pass B | fail both | pass A, fail B |
|---|---|---:|---:|---:|---:|
| abs | rand | 402 | 30 | 0 | 0 |
| acos | boundary | 57 | 3 | 0 | 0 |
| add | boundary | 1128 | 72 | 0 | 0 |
| add | rand | 894 | 87 | 0 | 0 |
| asin | boundary | 57 | 3 | 0 | 0 |
| atan2 | boundary | 507 | 162 | 0 | 0 |
| atan2 | rand | 600 | 6 | 0 | 0 |
| ceiling | boundary | 210 | 39 | 0 | 0 |
| ceiling | F19 | 24 | 21 | 0 | 0 |
| ceiling | rand | 519 | 84 | 0 | 0 |
| denominator | boundary | 93 | 15 | 0 | 0 |
| denominator | F18 | 21 | 12 | 0 | 0 |
| denominator | rand | 447 | 69 | 0 | 0 |
| divide | boundary | 1248 | 303 | 0 | 0 |
| divide | F26 | 477 | 147 | 0 | 0 |
| divide | rand | 897 | 231 | 0 | 0 |
| eqv | boundary | 1875 | 363 | 0 | 0 |
| eqv | rand | 771 | 138 | 0 | 0 |
| expt | boundary | 1230 | 66 | 0 | 0 |
| expt | rand | 1182 | 189 | 0 | 0 |
| floor | boundary | 210 | 39 | 0 | 0 |
| floor | F19 | 24 | 21 | 0 | 0 |
| floor | rand | 504 | 87 | 0 | 0 |
| fromFixnum | boundary | 168 | 87 | 0 | 0 |
| fromFixnum | F02 | 9 | 9 | 0 | 0 |
| fromFixnum | rand | 333 | 243 | 0 | 0 |
| fromString | boundary | 375 | 81 | 0 | 0 |
| fromString | F22 | 18 | 18 | 0 | 0 |
| fromString | rand | 711 | 96 | 0 | 0 |
| gcd | boundary | 1812 | 879 | 0 | 0 |
| gcd | rand | 600 | 399 | 0 | 0 |
| greaterThan | rand | 774 | 6 | 0 | 0 |
| greaterThanOrEqual | rand | 762 | 6 | 0 | 0 |
| integerSqrt | boundary | 231 | 102 | 0 | 0 |
| integerSqrt | F10 | 180 | 102 | 0 | 0 |
| integerSqrt | rand | 639 | 114 | 0 | 0 |
| lcm | boundary | 1815 | 591 | 0 | 0 |
| lcm | F11 | 18 | 6 | 0 | 0 |
| lcm | rand | 591 | 258 | 0 | 0 |
| lessThan | rand | 768 | 9 | 0 | 0 |
| log | boundary | 165 | 9 | 0 | 0 |
| log | rand | 489 | 3 | 0 | 0 |
| makeBignum | boundary | 96 | 36 | 0 | 0 |
| makeBignum | F28 | 21 | 21 | 0 | 0 |
| makeRational | boundary | 249 | 117 | 0 | 0 |
| makeRational | F15 | 12 | 12 | 0 | 0 |
| makeRational | rand | 447 | 276 | 0 | 0 |
| modulo | boundary | 1797 | 765 | 0 | 0 |
| modulo | F26 | 27 | 27 | 0 | 0 |
| modulo | rand | 837 | 351 | 0 | 0 |
| multiply | boundary | 1128 | 18 | 0 | 0 |
| multiply | rand | 900 | 138 | 0 | 0 |
| numerator | boundary | 93 | 18 | 0 | 0 |
| numerator | F18 | 21 | 12 | 0 | 0 |
| numerator | rand | 414 | 75 | 0 | 0 |
| quotient | boundary | 1797 | 975 | 0 | 0 |
| quotient | F26 | 327 | 54 | 0 | 0 |
| quotient | rand | 834 | 381 | 0 | 0 |
| remainder | boundary | 1941 | 1092 | 0 | 0 |
| remainder | F26 | 327 | 267 | 0 | 0 |
| remainder | rand | 897 | 411 | 0 | 0 |
| round | boundary | 210 | 39 | 0 | 0 |
| round | F19 | 24 | 21 | 0 | 0 |
| round | rand | 507 | 81 | 0 | 0 |
| roundEven | boundary | 210 | 57 | 0 | 0 |
| roundEven | F19 | 24 | 24 | 0 | 0 |
| roundEven | F20 | 21 | 18 | 0 | 0 |
| roundEven | rand | 507 | 180 | 0 | 0 |
| sqr | rand | 549 | 15 | 0 | 0 |
| sqrt | boundary | 291 | 66 | 0 | 0 |
| sqrt | F09 | 180 | 48 | 0 | 0 |
| sqrt | rand | 936 | 141 | 0 | 0 |
| subtract | boundary | 1128 | 78 | 0 | 0 |
| subtract | rand | 900 | 144 | 0 | 0 |
| toExact | boundary | 93 | 36 | 0 | 0 |
| toExact | rand | 504 | 222 | 0 | 0 |
| toFixnum | boundary | 135 | 18 | 0 | 0 |
| toFixnum | F14 | 123 | 18 | 0 | 0 |
| toFixnum | rand | 1080 | 309 | 0 | 0 |
| toRational | boundary | 93 | 36 | 0 | 0 |
| toRational | rand | 483 | 216 | 0 | 0 |
| toRoughnum | boundary | 135 | 18 | 0 | 0 |
| toRoughnum | rand | 1083 | 279 | 0 | 0 |
| toStringDigits | boundary | 123 | 6 | 0 | 0 |
| toStringDigits | F30 | 9 | 9 | 0 | 0 |
| **total** | | 65745 | 12330 | 0 | 0 |

### The corrections (each its own commit on `jsnums-hardening-rebased`)

| id | commit | what and why | evidence |
|---|---|---|---|
| C01 | expt(0, ~0) is exact 1 | numbers.scrbl: an Exactnum 0 first argument gives an Exactnum; branch gave `~1`, drydock `0` | `expt-boundary-0014/0015` |
| C02 | asin/acos of a roughnum stay rough | `Roughnum.acos` re-entered `acos` with a raw JS number, so `acos(~1)` was exact 0 (drydock and branch) | `acos-boundary-0008`, `asin-boundary-0010` |
| C03 | atan2 rewrite | `atan2(1, ~0)` raised division by zero, `atan2(~0, 0)` returned `3pi/2`, `atan2(1, ~5e-324)` overflowed; now `Math.atan2` on the doubles, exact args beyond the double range scaled first, exact 0 result kept for `atan2(0, positive)` | 44 `atan2-boundary` cases |
| C04 | mixed comparison beyond the double range | `10^400 > ~1` raised "roughnum overflow" because the exact side was coerced with `toRoughnum` | `greaterThan-rand-0030`, `lessThan-rand-0060`, … |
| C05 | log of a non-integer rational via its double | `log(p/q)` was `log(p) - log(q)` in doubles: 837,091 ulp off at `log(1000001/1000000)`, 3–4 ulp off at `log(2/3)`; test-numbers.arr's `num-log(1 / num-expt(9, num-expt(5, 7)))` still goes through the digit-count path (the double underflows) | `log-boundary-0021..0023`, `log-rand-0042` |
| C06 | makeRational requires integer parts | `Rational.makeInstance(1/2, 3)` silently built `1/3` | `makeRational-boundary-0081` |
| C07 | sqrt of an exact non-square beyond the double range | `~1e200` is representable; new `bigIntegerSqrtToDouble` rounds `isqrt(n·4^k)/2^k` with the remainder as sticky bit, sharing the rounding tail of `bigIntegerQuotientToDouble` (refactored into `roundScaledToDouble`) | 7 `sqrt-boundary`/`sqrt-F09`/`sqrt-rand` cases |
| C08 | makeBignum rejects "1.5e0" | `expandExponent` dropped fractional digits the exponent could not absorb | `makeBignum-boundary-0013`, `makeBignum-F28-0007` |
| C09 | rough expt with a negative exponent uses `Math.pow` directly | `expt(x, y<0)` was `expt(1/x, -y)`; for roughnums the rounded reciprocal's error grows with `|y|` (5 ulp at `expt(~9.4784, -17)`, 28 ulp for rough^rough) | `expt-rand-0299/0323/0342/0344/0353/0362` |
| C10 | integer root searches start from a power of two | performance only: Newton from the radicand itself took one iteration per bit; `sqrt(10^400)` took 0.5 s. Cut the suite from 75 s to 12 s | timing |

Bugs found in drydock that the branch did not claim, all fixed above:
C02, C03, C04, C05 (accuracy), C06, C08, C09 (accuracy). Plus the drydock
hang in `remainder(rational, 0)` (NaN reaches the fixnum gcd loop; covered
by F12 and C06).

## 7. Sensitivity

`node sensitivity.js --base drydock --fixed <rebased js-numbers.js> --groups sensitivity-groups.json`
reverts each group of zero-context hunks alone against the corrected file
and counts frozen cases (times three configurations) that newly fail.

hunks in diff: 75
fixed file: 0 failures out of 65745

| group | hunks | new failures | first failing cases |
|---|---|---:|---|
| F01 normalizeInteger (canonical fixnums) | 1 2 26 27 33 | 8811 | abs-rand-0012@28, abs-rand-0017@28, abs-rand-0024@28, abs-rand-0043@28, ... |
| F02 fromFixnum(NaN/Infinity) error | 3 | 18 | fromFixnum-boundary-0053@28, fromFixnum-boundary-0054@28, fromFixnum-boundary-0055@28, fromFixnum-F02-0001@28, ... |
| F03 divide: rough zero numerator stays rough | 4 | 27 | divide-boundary-0193@28, divide-boundary-0194@28, divide-boundary-0195@28, divide-boundary-0198@28, ... |
| F04 equalsAnyZero on BigInteger | 5 | 0 |  |
| F05 eqv on two roughnums | 6 | 501 | eqv-boundary-0313@28, eqv-boundary-0314@28, eqv-boundary-0315@28, eqv-boundary-0316@28, ... |
| F06 expt fixnum path exact square-and-multiply | 11 12 | 0 |  |
| F07 expt(0, rough 0) (with C01) | 13 | 6 | expt-boundary-0014@28, expt-boundary-0015@28, expt-boundary-0014@30, expt-boundary-0015@30, ... |
| F08 modulo fixnum path needs both fixnums | 15 | 276 | modulo-boundary-0016@28, modulo-boundary-0018@28, modulo-boundary-0020@28, modulo-boundary-0023@28, ... |
| F09 sqrt fixnum exactness check | 16 | 18 | sqrt-boundary-0027@28, sqrt-boundary-0060@28, sqrt-boundary-0062@28, sqrt-F09-0021@28, ... |
| F10 integerSqrt fixnum adjustment | 21 | 6 | integerSqrt-boundary-0039@28, integerSqrt-F10-0022@28, integerSqrt-boundary-0039@30, integerSqrt-F10-0022@30, ... |
| F11 lcm non-negative | 22 | 804 | lcm-boundary-0009@28, lcm-boundary-0013@28, lcm-boundary-0016@28, lcm-boundary-0018@28, ... |
| F12 quotient/remainder by zero | 23 24 | 225 | quotient-boundary-0005@28, quotient-boundary-0029@28, quotient-boundary-0053@28, quotient-boundary-0077@28, ... |
| F13 isOverflow(NaN) | 25 | 0 |  |
| F14 correctly rounded toFixnum | 28 29 31 67 | 864 | add-rand-0008@28, add-rand-0028@28, add-rand-0115@28, add-rand-0182@28, ... |
| F15+C06 makeRational zero denominator and integer parts | 32 | 39 | makeRational-boundary-0001@28, makeRational-boundary-0011@28, makeRational-boundary-0021@28, makeRational-boundary-0031@28, ... |
| F16 Rational.sqrt rewrite | 34 35 | 15 | sqrt-boundary-0017@28, sqrt-boundary-0021@28, sqrt-rand-0157@28, sqrt-rand-0162@28, ... |
| F17 expt exponent guard in Rational.expt | 37 | 0 |  |
| F18 Roughnum numerator/denominator via toRational | 40 41 | 201 | denominator-boundary-0020@28, denominator-boundary-0025@28, denominator-boundary-0026@28, denominator-boundary-0029@28, ... |
| F19 Roughnum floor/ceiling/round beyond 9e15 | 42 43 44 | 315 | ceiling-boundary-0052@28, ceiling-boundary-0053@28, ceiling-boundary-0055@28, ceiling-boundary-0056@28, ... |
| F20 Roughnum.roundEven sign (with F19 roundEven overflow) | 45 | 225 | roundEven-boundary-0037@28, roundEven-boundary-0039@28, roundEven-boundary-0041@28, roundEven-boundary-0043@28, ... |
| F21 Roughnum.expt NaN error | 46 | 0 |  |
| F22 fromString zero denominator | 49 50 | 27 | fromString-boundary-0017@28, fromString-boundary-0018@28, fromString-boundary-0019@28, fromString-F22-0001@28, ... |
| F23 fromSchemeString (legacy, out of scope) | 51 52 53 54 55 56 57 58 | 0 |  |
| F24 navigator.appName guard | 59 60 | 0 |  |
| F25 bnpFromInt DV (unreachable) | 61 | 0 |  |
| F26 bnpDivRemTo remainder repair | 62 | 28 | modulo-F26-0001@26, modulo-F26-0002@26, modulo-F26-0003@26, modulo-F26-0004@26, ... |
| F27 bnMod negative modulus | 63 64 | 294 | modulo-boundary-0040@28, modulo-boundary-0042@28, modulo-boundary-0044@28, modulo-boundary-0047@28, ... |
| F28 makeBignum rejects non-integer strings | 66 | 51 | makeBignum-boundary-0011@28, makeBignum-boundary-0014@28, makeBignum-boundary-0015@28, makeBignum-boundary-0017@28, ... |
| F29 BigInteger.sqrt huge non-square (with C07) | 69 70 | 3 | sqrt-boundary-0031@28, sqrt-boundary-0031@30, sqrt-boundary-0031@26 |
| F30 toStringDigits expansion limit | 73 | 15 | toStringDigits-boundary-0013@28, toStringDigits-boundary-0014@28, toStringDigits-F30-0001@28, toStringDigits-F30-0002@28, ... |
| F31 module exports | 74 75 | 0 |  |
| C02 asin/acos roughness | 38 39 47 48 71 72 | 6 | acos-boundary-0008@28, asin-boundary-0010@28, acos-boundary-0008@30, asin-boundary-0010@30, ... |
| C03 atan2 rewrite | 18 19 20 | 120 | atan2-boundary-0004@28, atan2-boundary-0005@28, atan2-boundary-0017@28, atan2-boundary-0018@28, ... |
| C04 mixed comparison beyond double range | 7 8 9 10 | 21 | greaterThan-rand-0030@28, greaterThan-rand-0192@28, greaterThanOrEqual-rand-0072@28, greaterThanOrEqual-rand-0106@28, ... |
| C05 log of rational via double | 17 | 12 | log-boundary-0021@28, log-boundary-0022@28, log-boundary-0023@28, log-rand-0042@28, ... |
| C07 bigIntegerSqrtToDouble | 30 | 3 | sqrt-boundary-0031@28, sqrt-boundary-0031@30, sqrt-boundary-0031@26 |
| C08 makeBignum fractional mantissa | 65 | 6 | makeBignum-boundary-0013@28, makeBignum-F28-0007@28, makeBignum-boundary-0013@30, makeBignum-F28-0007@30, ... |
| C09 rough expt negative exponent | 14 | 24 | expt-rand-0299@28, expt-rand-0323@28, expt-rand-0342@28, expt-rand-0344@28, ... |
| C10 integer root initial guess (performance) | 36 68 | 0 |  |

Reading the table. Every group with zero new failures is explained:

- F04 (`equalsAnyZero` on a BigInteger): with F01 in place a BigInteger
  that is zero never exists, so the hunk is dead unless something calls
  `makeBignum("0")` and hands the result to `divide`; the canonical input
  syntax cannot express "a BigInteger holding 0", so no case does.
- F06 (fixnum `expt` without `Math.pow`): no inexact `Math.pow` exists in
  the fixnum range on this Node (section 3).
- F13 (`isOverflow(NaN)`): NaN no longer reaches `isOverflow` once F12 and
  C06 stop it at the source (`remainder` by zero, `makeRational`).
- F17 (`expt` exponent guard in `Rational.expt`): the guard fires only for
  exponents above 2^32-1, whose results no oracle can compute; it is
  covered by `policy.test.js` (which runs those calls in a worker and
  fails on drydock by timeout).
- F21 (`Roughnum.expt` NaN): changes the error message from "roughnum
  overflow" to a domain error; both are the `error` class.
- F23 (`fromSchemeString`): out of scope, no cases.
- F24 (`navigator.appName`): the runner injects a fake `navigator` for all
  three configurations, so the default is observable only by
  `policy.test.js`, which fails on drydock (26-bit digits).
- F25: unreachable code.
- F31 (module exports): not a numeric result; `policy.test.js` fails on
  drydock (10 names missing).
- C10: performance only.

A group whose reversion removes a definition still used elsewhere (F14's
`bigIntegerQuotientToDouble`, on which C07's `roundScaledToDouble` also
depends; F01's `normalizeInteger`) fails through a `ReferenceError` at the
call; the count is real but not a measure of the numeric effect, which is
why those hunks are grouped together. F26 fails only in the 26-bit
configuration, as the fuzz predicted.

## 8. Coverage of the exported surface

`MakeNumberLibrary(errbacks)` (what `runtime.js:38` uses) exports 68
names. 59 are rows in the op tables (every name in `jsops.js`, the same 59
rows in each oracle table). The rest:

| export | status |
|---|---|
| `fromSchemeString` | out of scope: legacy WeScheme reader, not called by `runtime.js` or any trove module; Racket could serve as its only oracle but Julia and Python cannot, so it would never reach two-oracle consensus. The branch's F23 hunks there are unverified. |
| `BigInteger`, `Rational`, `Roughnum` | classes, used by the runner to classify results and build inputs |
| `FloatPoint`, `Complex` | FIXME-marked aliases of `Roughnum`; excluded from the export-parity check |
| `MIN_FIXNUM`, `MAX_FIXNUM` | checked by `policy.test.js` (±9e15, and that 9e15+1 is boxed) |
| `_innards` | internals for tests; out of scope |
| module-level `MakeNumberLibrary` | the loader's entry point |

The wrapped module-level exports (`fromFixnum`, `add`, … with an errbacks
argument) are the same functions; `policy.test.js` checks the two surfaces
name the same set.

## 9. Decisions a human should re-decide

1. **`expt(0, ~0)`.** Spec says Exactnum (so 1). Racket gives `1.0`,
   drydock gives `0`, the branch gave `~1`. I followed the spec (C01).
2. **Mixed comparisons.** `1/3 < ~0.3333333333333333` is `false` in
   js-numbers (the exact side is rounded to a double first) and in
   Racket, Julia and Python it is the exact comparison (`1/3 >
   0.333…31483`, so `>` is true and `<` is false; but `~0.1 > 1/10` is
   true in Racket and false in js-numbers). The goal file ranks Racket
   above the current runtime. I kept the runtime's behavior as policy
   `mixed-compare-via-double` (1613 cases) rather than change comparison
   semantics; C04 only removes the overflow error.
3. **Rounding a roughnum beyond 2^53.** `floor(~1e300)` is `10^300` on the
   rebased branch (it goes through `toRational`, i.e. the printed decimal),
   not the binary value 1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386…
   that Racket's `inexact->exact` gives. This is consistent with
   `num-exact(~1e300)`; the rows encode it. drydock returned the unboxed
   double.
4. **ulp thresholds** (section 4).
5. **Negative-zero fixnums** (section 2).
6. **`fromString("~-0")`** is `~-0.0` (JS `Number("-0")`), while the three
   oracles' integer readers give `+0.0`; the rows now read the float
   directly. Whether the literal `~-0` should be negative zero is a
   language question.
7. **`atan2(0, ~x)` with x positive is exact 0.** It falls out of
   js-numbers' own rules (exact 0 over a roughnum is exact 0, `atan` of
   exact 0 is exact 0) and numbers.scrbl only speaks of `num-atan2(0, 1)`.
8. **`expt` exponent limit** for bases other than 0 and 1 (policy
   `expt-exponent-limit`): `(-1)^(10^21+1)` raises "too large" rather than
   returning -1. Cheap to special-case ±1 if wanted.
9. **`log` accuracy fix (C05)** changes results by a few ulp for rationals
   that previously went through the numerator/denominator split; any
   Pyret test that pins such digits would move. `make pyret-test` did not
   notice (section 10).
10. **`makeRational` validation (C06)** adds an `isInteger` check to every
    `Rational` construction (hot path). It is cheap, but it is a hot path.
11. **`fromSchemeString`** left unverified (section 8).

## 10. Pyret-level regression

`make pyret-test` in a worktree of `jsnums-hardening-rebased` (phaseA build
from scratch, then `tests/pyret/main2.jarr`): all 12,236 tests passed,
9 min 23 s wall including the build. The main2 suite therefore compiles and
runs on the patched library, including test-numbers.arr, test-roughnum.arr
and test-rounding.arr. It was not run on drydock separately; drydock's
own CI covers that.

## 11. Reproducibility

- `node oracle/freeze.js --refreeze` regenerates every case file
  byte-identically (checked with md5 before and after).
- `gen/generate.js` is deterministic in `SEED = 20260904`, one PRNG per
  operation, so adding an operation does not disturb the others. IDs are
  `<op>-<family>-<n>`; regression families are named after the fix ids
  (F02 F06 F09 F10 F11 F14 F15 F18 F19 F20 F22 F26 F28 F30) so `compare.js`
  tabulates them.
- The one-off differential fuzz of jsbn division (200,000 random quotients
  per digit configuration against JS BigInt) is not checked in; its nine
  triggers are frozen as `divide-F26-*`, `quotient-F26-*`,
  `remainder-F26-*`, `modulo-F26-*`.
- Inputs mined from Pyret's own tests: the `p/q` pair from
  test-numbers.arr's "non-erroring roughnum coercion" check (a 1 ulp
  `toFixnum` failure on drydock), the `123123123123123123123123123.5`
  rounding family, the `~1/2` rough-fraction literals, `num-expt(125, 1/3)`
  and friends, `num-log(1e309)`, `num-exp(710)`.

## 12. Things flagged for review that are not numbers

- `lang/.gitignore` gained one line (freeze stamp).
- `lang/Makefile`: `jsnums-test` no longer depends on `phaseA`; `test`
  therefore no longer builds phaseA for this target.
- Code comments in the suite are terse pointers, not documentation;
  `gen/generate.js` has none beyond its header.
- The oracle harnesses require `racket` (with `math` lib), `julia` (found
  on PATH, `$JULIA`, or `~/.juliaup/bin/julia`) and `python3` with
  `mpmath`; `freeze.js` refuses to run with fewer than two.

## 13. Addendum: human decisions, 2026-11-06

The eleven items in section 9 were put to Joe Politz on 2026-11-06. All
but one confirmed the choice already shipped; prose below is agent-written
(Claude), the decisions themselves are Joe's.

| # | item | decision |
|---|---|---|
| 1 | `expt(0, ~0)` | **Changed.** Rough `~1`, not exact `1`. C01 is superseded by a new commit on `jsnums-hardening-rebased` that restores the branch's original `(y instanceof Roughnum) ? Roughnum.makeInstance(1) : 1`; `policy.js`'s `identityValue` no longer forces this case to exact. `expt-boundary-0014` and `expt-boundary-0015` in `cases/expt.json` were refrozen accordingly (now `rough: 3ff0000000000000`, no `exact-at-identity` stamp); every other frozen case is unchanged (`--refreeze` is still byte-identical). |
| 2 | mixed exact/rough comparisons | Kept: round-to-double, as shipped. |
| 3 | rounding a roughnum beyond 2^53 | Kept: decimal-print convention, as shipped. |
| 4 | ulp thresholds | Kept: 1 ulp general, 2 ulp for `atan2`/`log`/`expt`, as shipped. |
| 5 | fixnum negative zero | Kept: accepted as harmless, as shipped. |
| 6 | `fromString("~-0")` | Kept: preserves the sign (`~-0.0`), as shipped. |
| 7 | `atan2(0, ~x)` with x positive | Kept: exact `0`, as shipped. |
| 8 | `expt` exponent limit on bases other than 0/1 | Kept: no special case for ±1, as shipped. |
| 9 | `log` accuracy fix (C05) | Kept, as shipped. |
| 10 | `makeRational` validation (C06) | Kept, as shipped. |
| 11 | `fromSchemeString` | Kept: unverified fixes left in, as shipped. |

Only item 1 changed a shipped artifact. The tables in sections 3, 5, 6 and
7 were produced before this decision and are not regenerated here: the
`exact-at-identity` count in section 4 is now 35 rather than 37 (the two
`expt(0, ~0)` cases lost the stamp), and the "F07" row of section 3 and
the "C01" row of section 6 describe the state before this addendum rather
than the final one. `make jsnums-test` against the corrected library
(`jsnums-hardening-rebased`, tip after this addendum's commit) passes all
185 groups in 18 s; a full `--refreeze` of all 21,915 cases is
byte-identical except for the two cases named above.
