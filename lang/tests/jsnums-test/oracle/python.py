#!/usr/bin/env python3
# Oracle harness: reads TSV lines "id \t op \t arg..." on stdin, prints
# "id \t result" per line. Same result vocabulary as racket.rkt.
import sys, re, math, struct
from fractions import Fraction as Fr
import mpmath
mpmath.mp.prec = 200
sys.set_int_max_str_digits(0)

class Domain(Exception):
    pass

LIT = re.compile(r'^~?[+-]?[0-9]+(?:/[0-9]+|(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)$')
BIGNUM_LIT = re.compile(r'^[+-]?[0-9]+(?:(?:\.[0-9]+)?[eE]\+?[0-9]+)?$')
EXACT = re.compile(r'^[+-]?[0-9]+(?:/[0-9]+)?$')

def parse_num(s):
    if s.startswith('~'):
        r = s[1:]
        if r == 'inf': return math.inf
        if r == '-inf': return -math.inf
        if r == 'nan': return math.nan
        return float(r)
    if not EXACT.match(s): raise ValueError('bad exact ' + s)
    return Fr(s)

def fmt(v):
    if isinstance(v, bool): return 'bool:true' if v else 'bool:false'
    if isinstance(v, str): return 'string:' + v
    if v is None: return 'none'
    if isinstance(v, (int, Fr)):
        v = Fr(v)
        return 'exact:%d' % v.numerator if v.denominator == 1 else 'exact:%d/%d' % (v.numerator, v.denominator)
    if isinstance(v, float): return 'rough:' + struct.pack('>d', v).hex()
    if isinstance(v, (complex, mpmath.mpc)): return 'domain:complex'
    return 'harness-error:unexpected %r' % (v,)

# helpers
def int_only(*xs):
    for x in xs:
        if not (isinstance(x, Fr) and x.denominator == 1): raise Domain('not an exact integer')
def ieee_float(x):
    if isinstance(x, float): return x
    try: return float(x)
    except OverflowError: return math.inf if x > 0 else -math.inf
# a double read back as the decimal it prints as (js-numbers' Roughnum.toRational)
def float_exact(x): return Fr(repr(x))
def to_exact(x): return x if isinstance(x, Fr) else float_exact(x)
# evaluate at the double nearest x unless that loses the magnitude
def to_mp(x):
    d = ieee_float(x)
    if math.isfinite(d) and (d != 0 or x == 0): return mpmath.mpf(d)
    return mpmath.mpf(x.numerator) / mpmath.mpf(x.denominator)
def to_float(v):
    if isinstance(v, mpmath.mpc): raise Domain('complex')
    return float(v)
def via1(f, x): return to_float(f(to_mp(x)))
# mpmath drops the sign of zero; odd functions and sqrt keep it in IEEE
def via1_signed(f, x):
    r = via1(f, x)
    return math.copysign(0.0, ieee_float(x)) if r == 0 and ieee_float(x) == 0 else r
def via2(f, x, y): return to_float(f(to_mp(x), to_mp(y)))
def round_away(x):
    e = to_exact(x)
    r = math.floor(abs(e) + Fr(1, 2))
    return -r if e < 0 else r
def iroot(n, k):
    if n < 2: return n
    x = 1 << ((n.bit_length() + k - 1) // k + 1)
    while True:
        y = ((k - 1) * x + n // x ** (k - 1)) // k
        if y >= x: return x
        x = y
def exact_root(n, k):
    r = iroot(n, k)
    return r if r ** k == n else None
def pyret_sqrt(x):
    if isinstance(x, Fr) and x >= 0:
        rn, rd = exact_root(x.numerator, 2), exact_root(x.denominator, 2)
        if rn is not None and rd is not None: return Fr(rn, rd)
    return via1_signed(mpmath.sqrt, x)
def pyret_expt(x, y):
    if isinstance(x, Fr) and isinstance(y, Fr) and y.denominator == 1:
        return x ** y.numerator
    if isinstance(x, Fr) and isinstance(y, Fr) and y.denominator <= 8:
        p, q, neg = y.numerator, y.denominator, x < 0
        if neg and q % 2 == 0: raise Domain('even root of negative')
        rn, rd = exact_root(abs(x.numerator), q), exact_root(x.denominator, q)
        if rn is not None and rd is not None:
            return (-1 if neg and p % 2 else 1) * Fr(rn, rd) ** p
        return (-1.0 if neg and p % 2 else 1.0) * via2(mpmath.power, abs(x), y)
    return via2(mpmath.power, x, y)
def pyret_atan2(y, x):
    if y == 0 and x == 0: raise Domain('atan2(0,0)')
    r = mpmath.atan2(to_mp(y), to_mp(x))
    if r == 0: return math.copysign(0.0, ieee_float(y))
    return to_float(r + 2 * mpmath.pi if r < 0 else r)
def pyret_remainder(a, b):
    if b == 0: raise Domain('zero divisor')
    if isinstance(a, Fr) and isinstance(b, Fr):
        return a - b * math.trunc(a / b)
    da, db = ieee_float(a), ieee_float(b)   # IEEE fmod: fmod(x, inf) = x, fmod(inf, y) undefined, zero keeps x's sign
    if math.isinf(da): raise Domain('non-finite')
    if math.isinf(db): return da
    ea, eb = Fr(da), Fr(db)
    r = ea - eb * math.trunc(ea / eb)
    return math.copysign(0.0, da) if r == 0 else ieee_float(r)
def pyret_rel(cv, tv, delta, smoothed):
    cv, tv, delta = to_exact(cv), to_exact(tv), to_exact(delta)
    if delta < 0: raise Domain('negative tolerance')
    if cv == tv: return True
    err = abs(cv - tv)
    den = min(abs(cv), abs(tv))
    if smoothed: den = den + 1
    return err <= delta * den if delta <= 1 else err / den <= delta
def from_string(s):
    if not LIT.match(s): return None
    if '/' in s and int(s.split('/')[1]) == 0: return None
    if s.startswith('~'): return ieee_float(Fr(s[1:])) if '/' in s else float(s[1:])
    return Fr(s)
def make_bignum(s):
    if not BIGNUM_LIT.match(s): raise Domain('not an integer string')
    v = Fr(s)
    if v.denominator != 1: raise Domain('not an integer string')
    return v
def finite_only(x):
    if isinstance(x, float) and not math.isfinite(x): raise Domain('non-finite')
    return to_exact(x)
def roughly_equals(x, y, d):
    x, y, d = to_exact(x), to_exact(y), to_exact(d)
    if d < 0: raise Domain('negative tolerance')
    return abs(x - y) <= d
def to_repeating_decimal(n, d):
    int_only(n, d)
    if d <= 0: raise Domain('d <= 0')
    return n / d
def to_string_digits(n, d):
    int_only(d)
    t = Fr(10) ** int(d)
    return Fr(round_away(n * t)) / t

OPS = {
    'add':                (('num', 'num'), lambda a, b: a + b),
    'subtract':           (('num', 'num'), lambda a, b: a - b),
    'multiply':           (('num', 'num'), lambda a, b: a * b),
    'divide':             (('num', 'num'), lambda a, b: a / b),
    'equals':             (('num', 'num'), lambda a, b: a == b),
    'eqv':                (('num', 'num'), lambda a, b: isinstance(a, Fr) == isinstance(b, Fr) and a == b),
    'equalsAnyZero':      (('num',), lambda a: a == 0),
    'lessThan':           (('num', 'num'), lambda a, b: a < b),
    'lessThanOrEqual':    (('num', 'num'), lambda a, b: a <= b),
    'greaterThan':        (('num', 'num'), lambda a, b: a > b),
    'greaterThanOrEqual': (('num', 'num'), lambda a, b: a >= b),
    'expt':               (('num', 'num'), pyret_expt),
    'exp':                (('num',), lambda x: via1(mpmath.exp, x)),
    'log':                (('num',), lambda x: via1(mpmath.log, x)),
    'sin':                (('num',), lambda x: via1_signed(mpmath.sin, x)),
    'cos':                (('num',), lambda x: via1(mpmath.cos, x)),
    'tan':                (('num',), lambda x: via1_signed(mpmath.tan, x)),
    'asin':               (('num',), lambda x: via1_signed(mpmath.asin, x)),
    'acos':               (('num',), lambda x: via1(mpmath.acos, x)),
    'atan':               (('num',), lambda x: via1_signed(mpmath.atan, x)),
    'atan2':              (('num', 'num'), pyret_atan2),
    'sqrt':               (('num',), pyret_sqrt),
    'integerSqrt':        (('num',), lambda x: (int_only(x), Fr(math.isqrt(x.numerator)))[1]),
    'sqr':                (('num',), lambda x: x * x),
    'abs':                (('num',), lambda x: abs(x)),
    'modulo':             (('num', 'num'), lambda a, b: (int_only(a, b), Fr(a.numerator % b.numerator))[1]),
    'quotient':           (('num', 'num'), lambda a, b: (int_only(a, b), Fr(math.trunc(a / b)))[1]),
    'remainder':          (('num', 'num'), pyret_remainder),
    'gcd':                (('num', 'num'), lambda a, b: (int_only(a, b), Fr(math.gcd(a.numerator, b.numerator)))[1]),
    'lcm':                (('num', 'num'), lambda a, b: (int_only(a, b), Fr(math.lcm(a.numerator, b.numerator)))[1]),
    'floor':              (('num',), lambda x: Fr(math.floor(to_exact(x)))),
    'ceiling':            (('num',), lambda x: Fr(math.ceil(to_exact(x)))),
    'round':              (('num',), lambda x: Fr(round_away(x))),
    'roundEven':          (('num',), lambda x: Fr(round(to_exact(x)))),
    'numerator':          (('num',), lambda x: Fr(to_exact(x).numerator)),
    'denominator':        (('num',), lambda x: Fr(to_exact(x).denominator)),
    'toFixnum':           (('num',), ieee_float),
    'toRational':         (('num',), to_exact),
    'toExact':            (('num',), to_exact),
    'toRoughnum':         (('num',), ieee_float),
    'isInteger':          (('num',), lambda x: isinstance(x, Fr) and x.denominator == 1),
    'isRational':         (('num',), lambda x: isinstance(x, Fr)),
    'isExact':            (('num',), lambda x: isinstance(x, Fr)),
    'isReal':             (('num',), lambda x: True),
    'isRoughnum':         (('num',), lambda x: isinstance(x, float)),
    'isPositive':         (('num',), lambda x: x > 0),
    'isNegative':         (('num',), lambda x: x < 0),
    'isNonPositive':      (('num',), lambda x: x <= 0),
    'isNonNegative':      (('num',), lambda x: x >= 0),
    'isPyretNumber':      (('num',), lambda x: True),
    'fromString':         (('str',), from_string),
    'fromFixnum':         (('double',), finite_only),
    'makeBignum':         (('str',), make_bignum),
    'makeRational':       (('num', 'num'), lambda n, d: (int_only(n, d), n / d)[1]),
    'makeRoughnum':       (('double',), lambda x: x),
    'roughlyEquals':      (('num', 'num', 'num'), roughly_equals),
    'roughlyEqualsRel':   (('num', 'num', 'num', 'bool'), pyret_rel),
    'toRepeatingDecimal': (('num', 'num'), to_repeating_decimal),
    'toStringDigits':     (('num', 'num'), to_string_digits),
}

def parse_arg(kind, s):
    if kind in ('num', 'double'): return parse_num(s)
    if kind == 'str': return s
    if kind == 'bool': return s == 'true'
    raise ValueError(kind)

def run_line(line):
    fields = line.split('\t')
    cid, op, args = fields[0], fields[1], fields[2:]
    row = OPS.get(op)
    if row is None:
        result = 'skip'
    else:
        try:
            result = fmt(row[1](*[parse_arg(k, a) for k, a in zip(row[0], args)]))
        except ZeroDivisionError:
            result = 'div-by-zero'
        except (Domain, ValueError, OverflowError, TypeError, ArithmeticError) as e:
            result = 'domain:' + str(e).split('\n')[0]
    sys.stdout.write('%s\t%s\n' % (cid, result))

for line in sys.stdin:
    line = line.rstrip('\n')
    if line: run_line(line)
