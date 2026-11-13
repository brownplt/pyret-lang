# Oracle harness: reads TSV lines "id \t op \t arg..." on stdin, prints
# "id \t result" per line. Same result vocabulary as racket.rkt.
setprecision(BigFloat, 200)

struct Domain <: Exception
    msg::String
end

const LIT = r"^~?[+-]?[0-9]+(?:/[0-9]+|(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)$"
const BIGNUM_LIT = r"^[+-]?[0-9]+(?:(?:\.[0-9]+)?[eE]\+?[0-9]+)?$"
const EXACT = r"^([+-]?[0-9]+)(?:/([0-9]+))?$"
const DECIMAL = r"^([+-]?)([0-9]*)(?:\.([0-9]*))?(?:[eE]([+-]?[0-9]+))?$"

function parse_num(s)
    if startswith(s, "~")
        r = s[2:end]
        r == "inf" && return Inf
        r == "-inf" && return -Inf
        r == "nan" && return NaN
        return parse(Float64, r)
    end
    m = match(EXACT, s)
    m === nothing && error("bad exact $s")
    n = parse(BigInt, m[1])
    d = m[2] === nothing ? big(1) : parse(BigInt, m[2])
    return n // d
end

# exact value of a decimal literal (integer, decimal, exponent); nothing if malformed
function parse_decimal(s)
    m = match(DECIMAL, s)
    (m === nothing || (m[2] == "" && (m[3] === nothing || m[3] == ""))) && return nothing
    frac = m[3] === nothing ? "" : m[3]
    n = parse(BigInt, (m[2] == "" ? "0" : m[2]) * frac)
    e = (m[4] === nothing ? 0 : parse(Int, m[4])) - length(frac)
    m[1] == "-" && (n = -n)
    return e >= 0 ? (n * big(10)^e) // 1 : n // big(10)^(-e)
end

fmt(v::Bool) = v ? "bool:true" : "bool:false"
fmt(v::AbstractString) = "string:" * v
fmt(::Nothing) = "none"
fmt(v::Integer) = "exact:$(BigInt(v))"
function fmt(v::Rational)
    denominator(v) == 0 && throw(Domain("infinite rational"))
    denominator(v) == 1 ? "exact:$(numerator(v))" : "exact:$(numerator(v))/$(denominator(v))"
end
fmt(v::Float64) = "rough:" * string(reinterpret(UInt64, v), base=16, pad=16)
fmt(v::Complex) = "domain:complex"
fmt(v) = "harness-error:unexpected $(repr(v))"

# helpers
int_only(xs...) = for x in xs; (x isa Rational && denominator(x) == 1) || throw(Domain("not an exact integer")); end
fl(x) = x isa Float64 ? x : Float64(x)
# arithmetic on a mixed exact/rough pair happens in doubles (Julia would promote to BigFloat)
mixed(f, a, b) = (a isa Float64 || b isa Float64) ? f(fl(a), fl(b)) : f(a, b)
# a double read back as the decimal it prints as (js-numbers' Roughnum.toRational)
float_exact(x::Float64) = parse_decimal(string(x))
to_exact(x) = x isa Rational ? x : float_exact(x)
# evaluate at the double nearest x unless that loses the magnitude
tobf(x) = (d = fl(x); (isfinite(d) && (d != 0 || x == 0)) ? BigFloat(d) : BigFloat(x))
via1(f, x) = Float64(f(tobf(x)))
via2(f, x, y) = Float64(f(tobf(x), tobf(y)))
round_away(x) = (x isa Float64 && !isfinite(x) && throw(Domain("non-finite")); round(BigInt, to_exact(x), RoundNearestTiesAway))
function iroot(n::BigInt, k)
    n < 2 && return n
    x = big(1) << (div(ndigits(n, base=2) + k - 1, k) + 1)
    while true
        y = div((k - 1) * x + div(n, x^(k - 1)), k)
        y >= x && return x
        x = y
    end
end
exact_root(n, k) = (r = iroot(n, k); r^k == n ? r : nothing)
function pyret_sqrt(x)
    if x isa Rational && x >= 0
        rn, rd = exact_root(numerator(x), 2), exact_root(denominator(x), 2)
        rn !== nothing && rd !== nothing && return rn // rd
    end
    return via1(sqrt, x)
end
function pyret_expt(x, y)
    if x isa Rational && y isa Rational && denominator(y) == 1
        x == 0 && y < 0 && throw(DivideError())
        return x^numerator(y)
    end
    if x isa Rational && y isa Rational && denominator(y) <= 8
        p, q, neg = numerator(y), denominator(y), x < 0
        neg && iseven(q) && throw(Domain("even root of negative"))
        rn, rd = exact_root(abs(numerator(x)), q), exact_root(denominator(x), q)
        if rn !== nothing && rd !== nothing
            rn == 0 && p < 0 && throw(DivideError())
            return (neg && isodd(p) ? -1 : 1) * (rn // rd)^p
        end
        return (neg && isodd(p) ? -1.0 : 1.0) * via2(^, abs(x), y)
    end
    return via2(^, x, y)
end
function pyret_atan2(y, x)
    y == 0 && x == 0 && throw(Domain("atan2(0,0)"))
    r = atan(tobf(y), tobf(x))
    return Float64(r < 0 ? r + 2 * BigFloat(pi) : r)
end
function pyret_remainder(a, b)
    b == 0 && throw(Domain("zero divisor"))
    if a isa Rational && b isa Rational
        return a - b * trunc(BigInt, a / b)
    end
    da, db = fl(a), fl(b)   # IEEE fmod: fmod(x, inf) = x, fmod(inf, y) undefined, zero keeps x's sign
    isinf(da) && throw(Domain("non-finite"))
    isinf(db) && return da
    ea, eb = Rational{BigInt}(da), Rational{BigInt}(db)
    r = ea - eb * trunc(BigInt, ea / eb)
    return r == 0 ? copysign(0.0, da) : Float64(r)
end
function pyret_rel(cv, tv, delta, smoothed)
    cv, tv, delta = to_exact(cv), to_exact(tv), to_exact(delta)
    delta < 0 && throw(Domain("negative tolerance"))
    cv == tv && return true
    err = abs(cv - tv)
    den = min(abs(cv), abs(tv))
    smoothed && (den = den + 1)
    delta > 1 && den == 0 && throw(DivideError())
    return delta <= 1 ? err <= delta * den : err / den <= delta
end
function from_string(s)
    occursin(LIT, s) || return nothing
    if startswith(s, "~")
        v = parse_num_lit(s[2:end])
        v === nothing && return nothing
        f = fl(v)
        return (f == 0 && startswith(s, "~-")) ? -0.0 : f
    end
    return parse_num_lit(s)
end
function parse_num_lit(s)
    m = match(EXACT, s)
    if m !== nothing
        d = m[2] === nothing ? big(1) : parse(BigInt, m[2])
        d == 0 && return nothing
        return parse(BigInt, m[1]) // d
    end
    return parse_decimal(s)
end
function make_bignum(s)
    occursin(BIGNUM_LIT, s) || throw(Domain("not an integer string"))
    v = parse_decimal(s)
    (v === nothing || denominator(v) != 1) && throw(Domain("not an integer string"))
    return v
end
function to_string_digits(n, d)
    int_only(d)
    t = (big(10) // 1)^Int(numerator(d))
    return round_away(mixed(*, n, t)) // t
end
function finite_only(x)
    x isa Float64 && !isfinite(x) && throw(Domain("non-finite"))
    return to_exact(x)
end

const OPS = Dict{String,Tuple{Vector{Symbol},Function}}(
    "add"                => ([:num, :num], (a, b) -> mixed(+, a, b)),
    "subtract"           => ([:num, :num], (a, b) -> mixed(-, a, b)),
    "multiply"           => ([:num, :num], (a, b) -> mixed(*, a, b)),
    "divide"             => ([:num, :num], (a, b) -> (b == 0 && a isa Rational && b isa Rational && throw(DivideError()); mixed(/, a, b))),
    "equals"             => ([:num, :num], (a, b) -> a == b),
    "eqv"                => ([:num, :num], (a, b) -> (a isa Rational) == (b isa Rational) && a == b),
    "equalsAnyZero"      => ([:num], a -> a == 0),
    "lessThan"           => ([:num, :num], (a, b) -> a < b),
    "lessThanOrEqual"    => ([:num, :num], (a, b) -> a <= b),
    "greaterThan"        => ([:num, :num], (a, b) -> a > b),
    "greaterThanOrEqual" => ([:num, :num], (a, b) -> a >= b),
    "expt"               => ([:num, :num], pyret_expt),
    "exp"                => ([:num], x -> via1(exp, x)),
    "log"                => ([:num], x -> via1(log, x)),
    "sin"                => ([:num], x -> via1(sin, x)),
    "cos"                => ([:num], x -> via1(cos, x)),
    "tan"                => ([:num], x -> via1(tan, x)),
    "asin"               => ([:num], x -> via1(asin, x)),
    "acos"               => ([:num], x -> via1(acos, x)),
    "atan"               => ([:num], x -> via1(atan, x)),
    "atan2"              => ([:num, :num], pyret_atan2),
    "sqrt"               => ([:num], pyret_sqrt),
    "integerSqrt"        => ([:num], x -> (int_only(x); x < 0 && throw(Domain("negative")); isqrt(numerator(x)) // 1)),
    "sqr"                => ([:num], x -> x * x),
    "abs"                => ([:num], x -> abs(x)),
    "modulo"             => ([:num, :num], (a, b) -> (int_only(a, b); mod(numerator(a), numerator(b)) // 1)),
    "quotient"           => ([:num, :num], (a, b) -> (int_only(a, b); div(numerator(a), numerator(b)) // 1)),
    "remainder"          => ([:num, :num], pyret_remainder),
    "gcd"                => ([:num, :num], (a, b) -> (int_only(a, b); gcd(numerator(a), numerator(b)) // 1)),
    "lcm"                => ([:num, :num], (a, b) -> (int_only(a, b); lcm(numerator(a), numerator(b)) // 1)),
    "floor"              => ([:num], x -> floor(BigInt, to_exact(x)) // 1),
    "ceiling"            => ([:num], x -> ceil(BigInt, to_exact(x)) // 1),
    "round"              => ([:num], x -> round_away(x) // 1),
    "roundEven"          => ([:num], x -> round(BigInt, to_exact(x)) // 1),
    "numerator"          => ([:num], x -> numerator(to_exact(x)) // 1),
    "denominator"        => ([:num], x -> denominator(to_exact(x)) // 1),
    "toFixnum"           => ([:num], fl),
    "toRational"         => ([:num], to_exact),
    "toExact"            => ([:num], to_exact),
    "toRoughnum"         => ([:num], fl),
    "isInteger"          => ([:num], x -> x isa Rational && denominator(x) == 1),
    "isRational"         => ([:num], x -> x isa Rational),
    "isExact"            => ([:num], x -> x isa Rational),
    "isReal"             => ([:num], x -> true),
    "isRoughnum"         => ([:num], x -> x isa Float64),
    "isPositive"         => ([:num], x -> x > 0),
    "isNegative"         => ([:num], x -> x < 0),
    "isNonPositive"      => ([:num], x -> x <= 0),
    "isNonNegative"      => ([:num], x -> x >= 0),
    "isPyretNumber"      => ([:num], x -> true),
    "fromString"         => ([:str], from_string),
    "fromFixnum"         => ([:double], finite_only),
    "makeBignum"         => ([:str], make_bignum),
    "makeRational"       => ([:num, :num], (n, d) -> (int_only(n, d); d == 0 && throw(DivideError()); n / d)),
    "makeRoughnum"       => ([:double], x -> x),
    "roughlyEquals"      => ([:num, :num, :num], (x, y, d) -> (d = to_exact(d); d < 0 && throw(Domain("negative tolerance")); abs(to_exact(x) - to_exact(y)) <= d)),
    "roughlyEqualsRel"   => ([:num, :num, :num, :bool], pyret_rel),
    "toRepeatingDecimal" => ([:num, :num], (n, d) -> (int_only(n, d); d <= 0 && throw(Domain("d <= 0")); n / d)),
    "toStringDigits"     => ([:num, :num], to_string_digits),
)

function parse_arg(kind, s)
    kind == :num && return parse_num(s)
    kind == :double && return parse_num(s)
    kind == :str && return String(s)
    kind == :bool && return s == "true"
    error("kind $kind")
end

function run_line(line)
    fields = split(line, '\t')
    id, op, args = fields[1], fields[2], fields[3:end]
    row = get(OPS, op, nothing)
    result = if row === nothing
        "skip"
    else
        try
            fmt(row[2]([parse_arg(k, a) for (k, a) in zip(row[1], args)]...))
        catch e
            if e isa DivideError
                "div-by-zero"
            elseif e isa Domain
                "domain:" * e.msg
            elseif e isa DomainError || e isa InexactError || e isa ArgumentError || e isa OverflowError
                "domain:" * split(sprint(showerror, e), '\n')[1]
            else
                "harness-error:" * split(sprint(showerror, e), '\n')[1]
            end
        end
    end
    println(id, '\t', result)
end

for line in eachline(stdin)
    isempty(line) || run_line(line)
end
