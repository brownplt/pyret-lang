use context starter2024

provide: 
  *,
  type Posn,
  type TaggedFunction,
  module Eth,
  module Err,
  module Sets,
  module T,
  module SD,
  module R,
  module L,
  module Stats,
  module Math
end

# export every symbol from starter2024 except for those we override
import starter2024 as Starter
provide from Starter:
    * hiding(translate, filter, range, sort, sin, cos, tan)
end

include charts

import image as I
provide from I:
    * hiding(translate),
  type *,
  data *
end

import lists as L
provide from L: * hiding(filter, range, sort), type *, data * end

import color as C
import constants as Consts
provide from Consts: PI, E end
import reactors as R

import either as Eth
import error as Err
import sets as Sets
import tables as T
import string-dict as SD
import statistics as Stats
import math as Math

import gdrive-sheets as G
provide from G:
    * hiding(load-spreadsheet),
  type *,
  data *
end

# ---------------------------------------------------------------------------
# browser-test fixture marker -- NOT PRESENT UPSTREAM.
#
# This file is a verbatim copy of bootstrapworld/starter-files@82d7238
# libraries/core.arr, vendored into the vscode fixture workspace so that
# `import url-file(<pinned>/libraries, "core.arr")` resolves HERE, off the
# filesystem, under pyret-parley.urlFileMode = "local-if-present".
#
# The `provide: *` at the top of this file exports the binding below. A
# vscode-only test asserts it, so if the local branch ever regresses and the
# import silently falls back to the network, the upstream core.arr has no such
# binding and the test fails loudly instead of going green.
# ---------------------------------------------------------------------------
came-from-local-filesystem = "vscode-fixture-workspace"

var debugging = false


data Posn:
  | posn(x :: Number, y :: Number)
end

# override Pyret's native translate with put-image
shadow translate = I.put-image

# override Pyret's native color constructor with make-color
shadow make-color = C.color

# override Pyret's native range (list) with range (stats)
shadow range = lam(t :: Table, col :: String) block:
  l = t.column(col).sort()
  num-to-string-digits(Math.max(l) - Math.min(l), 2)
end

fun nth-root(n, r): expt(n, 1 / r) end

var display-chart = lam(c) block: 
  when debugging: print(c.get-spec()) end
  c.display() 
end

fun string-trim(s :: String) -> String:
  doc: "Strips leading and trailing whitespace from a string"
  is-space = lam(c): string-contains(" \t\n\r", c) end
  fun drop-leading(chars):
    if is-empty(chars) or not(is-space(chars.first)):
      chars
    else:
      drop-leading(chars.rest)
    end
  end
  chars = string-explode(s)
  L.reverse(drop-leading(L.reverse(drop-leading(chars)))).join-str("")
end

#################################################
# Numerical functions

fun round-digits(val, digits):
  shadow scale = expt(10, digits)
  num-round(val * scale) / scale
end

fun num-round-to(n :: Number, digits :: Number) -> Roughnum:
  num-to-roughnum(round-digits(n, digits))
end

fun log-base(base, val):
  lg = num-log(val) / num-log(base)
  lg-round = round-digits(lg, 4)
  if roughly-equal(lg-round, lg) and roughly-equal(expt(base, lg-round), val):
    lg-round
  else:
    lg
  end
end

fun log(n): log-base(10, n) end
ln = num-log
data ShrinkResult:
  | fits(s :: String)
  | overflow
  | cantfit
end


TRIG_ROUND_DIGITS = 10

fun rough-round-digits(val, digits):
  num-to-roughnum(num-round(val * expt(10, digits)) / expt(10, digits))
end


shadow sin = lam(n):
  rough-round-digits(num-sin(n), TRIG_ROUND_DIGITS)
end

shadow cos = lam(n):
  rough-round-digits(num-cos(n), TRIG_ROUND_DIGITS)
end

shadow tan = lam(n):
  rough-round-digits(num-tan(n), TRIG_ROUND_DIGITS)
end

# degree-consuming variants of sin, cos, and tan
fun sin-deg(n): sin(n * (PI / 180)) end
fun cos-deg(n): cos(n * (PI / 180)) end
fun tan-deg(n): tan(n * (PI / 180)) end

# --- string-form number utilities ---

fun string-to-number-i(s):
  cases(Option) string-to-number(s):
    | some(n) => n
    | none => -1e100
  end
end

fun strip-roughnum-prefix(s :: String) -> String:
  string-substring(s, 1, string-length(s))
end

fun normalize-exponent(s :: String) -> String:
  e-pos = string-index-of(s, 'e')
  if e-pos == -1: s
  else if string-char-at(s, e-pos + 1) == '+':
    string-substring(s, 0, e-pos) + 'e' +
    string-substring(s, e-pos + 2, string-length(s))
  else: s
  end
end

fun num-to-fixnum-str(n):
  normalize-exponent(strip-roughnum-prefix(num-to-string(num-to-roughnum(n))))
end

fun num-to-fixnum-no-exp-str(n):
  make-unsci(num-to-fixnum-str(n))
end

fun count-leading-zeros(s):
  fun helper(i):
    if i == string-length(s): i
    else if string-char-at(s, i) == '0': helper(i + 1)
    else: i
    end
  end
  helper(0)
end

fun str-girth(num-str):
  doc: ```
       Compute floor(log10(|n|)) directly from a no-exponent string
       form of n.  Returns 0 for "0".
       ```
  dot-pos = string-index-of(num-str, '.')
  int-str =
    if dot-pos < 0: num-str
    else: string-substring(num-str, 0, dot-pos)
    end
  if (int-str == '') or (int-str == '0'):
    dec-str =
      if dot-pos < 0: ''
      else: string-substring(num-str, dot-pos + 1, string-length(num-str))
      end
    leading-zeros = count-leading-zeros(dec-str)
    if leading-zeros == string-length(dec-str): 0
    else: 0 - (leading-zeros + 1)
    end
  else:
    string-length(int-str) - 1
  end
end

# --- shrinking ---

fun shrink-dec-part(dec-part, max-chars):
  doc: ```
       Truncate a fractional-digit string to fit `max-chars`, rounding
       the next digit if it's >= 5.  Returns:
         fits(s)   - the (possibly zero-padded) shrunk string
         overflow  - rounding carried past the leftmost digit
         cantfit   - max-chars too small to represent anything
       ```
  dec-part-len = string-length(dec-part)
  if max-chars < -1: cantfit
  else if max-chars == -1:
    if (dec-part-len > 0) and
      (string-to-number-i(string-char-at(dec-part, 0)) >= 5):
      overflow
    else:
      fits('')
    end
  else if dec-part-len == 0:
    fits('')
  else:
    head-str = string-substring(dec-part, 0, max-chars)
    next-digit = string-to-number-i(
      string-char-at(dec-part, max-chars))
    head-num = if head-str == '': 0 else: string-to-number-i(head-str) end
    rounded-num = if next-digit >= 5: head-num + 1 else: head-num end
    rounded-str = num-to-string(rounded-num)
    padding-len = max-chars - string-length(rounded-str)
    if padding-len < 0:
      overflow
    else:
      fits(string-repeat('0', padding-len) + rounded-str)
    end
  end
end

fun split-num-str(num-str):
  doc: "Decompose a numeric string into {int-part; frac-part; expt-part}."
  len = string-length(num-str)
  dot-pos = string-index-of(num-str, '.')
  if dot-pos < 0:
    e-pos = string-index-of(num-str, 'e')
    if e-pos < 0:
      {num-str; ''; ''}
    else:
      {string-substring(num-str, 0, e-pos);
        '';
        string-substring(num-str, e-pos, len)}
    end
  else:
    int-part = string-substring(num-str, 0, dot-pos)
    frac-expt-part = string-substring(num-str, dot-pos + 1, len)
    fe-len = string-length(frac-expt-part)
    e-pos = string-index-of(frac-expt-part, 'e')
    if e-pos < 0:
      {int-part; frac-expt-part; ''}
    else:
      {int-part;
        string-substring(frac-expt-part, 0, e-pos);
        string-substring(frac-expt-part, e-pos, fe-len)}
    end
  end
end

fun shrink-dec(num-str, max-chars):
  len = string-length(num-str)
  if len <= max-chars:
    fits(num-str)
  else:
    {int-part; frac-part; expt-part} = split-num-str(num-str)
    int-part-len = string-length(int-part)
    expt-part-len = string-length(expt-part)
    if int-part-len > max-chars:
      cantfit
    else:
      cases(ShrinkResult) shrink-dec-part(frac-part,
            max-chars - (int-part-len + expt-part-len + 1)):
        | cantfit => cantfit
        | overflow =>
          int-part-num = string-to-number-i(int-part)
          carried = num-to-string(int-part-num + 1) + expt-part
          if string-length(carried) <= max-chars: fits(carried)
          else: cantfit
          end
        | fits(s) =>
          if s == '': fits(int-part + expt-part)
          else: fits(int-part + '.' + s + expt-part)
          end
      end
    end
  end
end

# --- scientific notation construction ---

fun mantissa-parts(int-str, dec-str, girth):
  if girth >= 0:
    {string-char-at(int-str, 0);
      string-substring(int-str, 1, string-length(int-str)) + dec-str}
  else:
    neg-girth = 0 - girth
    dec-str-len = string-length(dec-str)
    {string-char-at(dec-str, neg-girth - 1);
      if neg-girth == dec-str-len: '0'
      else: string-substring(dec-str, neg-girth, dec-str-len)
      end}
  end
end

fun to-scientific(num-str, girth):
  len = string-length(num-str)
  dot-pos = string-index-of(num-str, '.')
  int-str =
    if dot-pos > -1: string-substring(num-str, 0, dot-pos)
    else: num-str
    end
  dec-str =
    if dot-pos > -1: string-substring(num-str, dot-pos + 1, len)
    else: ''
    end
  {m-int; m-frac} = mantissa-parts(int-str, dec-str, girth)
  expt-str =
    if girth == 0: ''
    else if girth > 0: 'e' + num-to-string(girth)
    else: 'e-' + num-to-string(0 - girth)
    end
  m-int + '.' + m-frac + expt-str
end

fun make-sci(underlying-num-str, max-chars) -> ShrinkResult:
  girth = str-girth(underlying-num-str)
  output =
    if (girth > 0) and (girth < max-chars):
      if string-index-of(underlying-num-str, '.') < 0:
        underlying-num-str + '.'
      else:
        underlying-num-str
      end
    else:
      to-scientific(underlying-num-str, girth)
    end
  if string-length(output) <= max-chars: fits(output)
  else: shrink-dec(output, max-chars)
  end
end

fun make-unsci(underlying-num-str):
  e-pos = string-index-of(underlying-num-str, 'e')
  if e-pos < 0: underlying-num-str
  else:
    underlying-num-str-len = string-length(underlying-num-str)
    mantissa-str = string-substring(underlying-num-str, 0, e-pos)
    exponent = string-to-number-i(string-substring(
        underlying-num-str, e-pos + 1, underlying-num-str-len))
    mantissa-len = string-length(mantissa-str)
    mantissa-dot-pos = string-index-of(mantissa-str, '.')
    mantissa-int-str =
      if mantissa-dot-pos > -1:
        string-substring(mantissa-str, 0, mantissa-dot-pos)
      else: mantissa-str
      end
    mantissa-frac-str =
      if mantissa-dot-pos > -1:
        string-substring(mantissa-str, mantissa-dot-pos + 1, mantissa-len)
      else: ''
      end
    if exponent == 0:
      underlying-num-str
    else if exponent > 0:
      mantissa-frac-len = string-length(mantissa-frac-str)
      if mantissa-frac-len == exponent:
        mantissa-int-str + mantissa-frac-str
      else if mantissa-frac-len < exponent:
        mantissa-int-str + mantissa-frac-str +
        string-repeat('0', exponent - mantissa-frac-len)
      else:
        mantissa-int-str +
        string-substring(mantissa-frac-str, 0, exponent) + '.' +
        string-substring(mantissa-frac-str, exponent, mantissa-frac-len)
      end
    else:
      shadow exponent = 0 - exponent
      mantissa-int-len = string-length(mantissa-int-str)
      if mantissa-int-len == exponent:
        '0.' + mantissa-int-str + mantissa-frac-str
      else if mantissa-int-len < exponent:
        '0.' + string-repeat('0', (exponent - mantissa-int-len) - 1) +
        mantissa-int-str + mantissa-frac-str
      else:
        string-substring(mantissa-int-str, 0, mantissa-int-len - exponent) +
        '.' +
        string-substring(mantissa-int-str, mantissa-int-len - exponent,
          mantissa-int-len)
      end
    end
  end
end

# --- top-level ---

fun compute-prefix(n):
  (if num-is-roughnum(n): '~' else: '' end) +
  (if n < 0: '-' else: '' end)
end

fun natural-min-len(full-no-exp :: String) -> Number:
  doc: ```
       Chars natural decimal needs just to convey magnitude + 1 sig digit.
       ```
  dot-pos = string-index-of(full-no-exp, '.')
  if dot-pos < 0:
    string-length(full-no-exp)
  else:
    int-str = string-substring(full-no-exp, 0, dot-pos)
    if int-str == '0':
      dec-str = string-substring(full-no-exp, dot-pos + 1,
        string-length(full-no-exp))
      2 + count-leading-zeros(dec-str) + 1
    else:
      dot-pos
    end
  end
end

fun easy-num-repr(n :: Number, max-chars :: Number) -> String:
  doc: ```
       Render `n` as a string of at most `max-chars` characters using
       the most accuracy-preserving non-rational decimal representation
       that fits.  Prefers natural decimal; falls back to scientific
       when natural would lose magnitude or fail to fit.  Raises if
       even scientific cannot fit.
       ```
  prefix = compute-prefix(n)
  budget = max-chars - string-length(prefix)

  result =
    if num-is-rational(n) and (n == 0):
      fits('0')
    else if num-is-rational(n) and (abs(n) == 1):
      fits('1')
    else:
      full-no-exp = num-to-fixnum-no-exp-str(abs(n))
      if natural-min-len(full-no-exp) <= budget:
        shrink-dec(full-no-exp, budget)
      else:
        full-with-exp = num-to-fixnum-str(abs(n))
        if string-contains(full-with-exp, 'e'):
          if string-length(full-with-exp) <= budget:
            fits(full-with-exp)
          else:
            shrink-dec(full-with-exp, budget)
          end
        else:
          make-sci(full-with-exp, budget)
        end
      end
    end

  cases(ShrinkResult) result:
    | fits(s) => prefix + s
    | cantfit =>
      raise('Could not fit ' + prefix +
        num-to-fixnum-no-exp-str(abs(n)) +
        ' into ' + tostring(max-chars) + ' chars')
  end
end



#################################################################################
# image-url
# shadow the native one with a wrapper than can identify and rewrite google share URLs
shadow image-url = lam(str) block:
  google-check = string-split-all(str, "https://drive.google.com/file/d/")
  if (google-check.length() > 1) block:
    download-prefix = "https://drive.google.com/uc?export=download&id="
    split-slashes = string-split-all(google-check.get(1), "/")
    I.image-url(download-prefix + split-slashes.get(0))
  else:
    I.image-url(str)
  end
end



#################################################################################
# Table Functions
# check that the table isn't empty, has all the necessary columns, and contains no blanks

# check to ensure that every value is actually a number
is-all-numbers = _.all(is-number)
fun ensure-numbers(l :: List<Number>%(is-all-numbers)): l end

fun check-integrity(t :: Table, cols :: List<String>) block:
  t-cols = t.column-names()
  check-col = lam(c): if t-cols.member(c): nothing
    else: raise(Err.message-exception("'" + c + "' is not a column in this table. Columns are: " + t-cols.join-str(", ")))
    end
  end
  cols.each(check-col)
  if (t.all-rows().length() == 0):
    raise(Err.message-exception("This table contains no data rows (it's empty!)"))
  else:
    L.each(lam(c):
        if is-Option(t.row-n(0)[c]):
          raise(Err.message-exception("This table contains blank cells in column " + c))
        else:
          nothing
        end
      end,
      t.column-names())
  end
end


# shadow the built-in function, with one naive twist:
# if it looks like a sheets URL, try parsing out the fileID
# for the user. Otherwise, just fall back to the native
# library function
fun load-spreadsheet(url :: String) block:
  if (string-length(url) < 39):
    G.load-spreadsheet(url)
  else if (string-substring(url, 0, 39) <> "https://docs.google.com/spreadsheets/d/"):
    G.load-spreadsheet(url)
  else:
    rest = string-substring(url, 39, string-length(url))
    G.load-spreadsheet(string-split(rest,"/").get(0))
  end
end

# Strings, Integers and numbers with 2 decimals are displayed exactly
# more than 2 decimals are displayed as roughnums
fun get-labels(t, ls) block:
  ls-col = t.column(ls)
  if is-number(ls-col.get(0)):
    ls-col.map(lam(x) block:
        rounded = num-round(x * 100) / 100
        if num-is-integer(x): num-to-string-digits(x, 0)
        else if (x == rounded): num-to-string-digits(x, 2)
        else: "~" + num-to-string-digits(x, 2)
        end
      end)
  else:
    ls-col.map(to-string)
  end
end

# Optimally-distinct list of colors taken from
# https://stackoverflow.com/a/12224359/12026982
COLORS = [list:
  make-color(51,102,204, 1),
  make-color(220,57,18, 1),
  make-color(255,153,0, 1),
  make-color(16,150,24, 1),
  make-color(153,0,153, 1),
  make-color(0,153,198, 1),
  make-color(221,68,119, 1),
  make-color(102,170,0, 1),
  make-color(184,46,46, 1),
  make-color(49,99,149, 1),
  make-color(153,68,153, 1),
  make-color(34,170,153, 1),
  make-color(170,170,17, 1),
  make-color(102,51,204, 1),
  make-color(230,115,0, 1),
  make-color(139,7,7, 1),
  make-color(101,16,103, 1),
  make-color(50,146,98, 1),
  make-color(85,116,166, 1),
  make-color(59,62,172, 1),
  make-color(183,115,34, 1),
  make-color(22,214,32, 1),
  make-color(185,19,131, 1),
  make-color(244,53,158, 1),
  make-color(156,89,53, 1),
  make-color(169,196,19, 1),
  make-color(42,119,141, 1),
  make-color(102,141,28, 1),
  make-color(190,164,19, 1),
  make-color(12,89,34, 1),
  make-color(116,52,17, 1)]

# maintain a mutable dict mapping string values to colors, so that any
# categorical display will have the same colors for the same value across
# program-opens (closing and re-opening the program will clear the dict).
var string-colors = [SD.mutable-string-dict:]
var colorIdx = 0

fun nextColor() block:
  if (colorIdx < (COLORS.length() - 1)):
    colorIdx := colorIdx + 1
  else: colorIdx := 0
  end
  COLORS.get(colorIdx)
end

# Given a table and a column name, extract distinct string values from
# that col and get the associated colors in the string-colors dict. If
# no color is assigned, grab a new one from the colors list and assign it
# Given a table and a column name, extract distinct string values from
# that col and get the associated colors in the string-colors dict. If
# no color is assigned, grab a new one from the colors list and assign it
distinct-colors :: (t :: Table, col :: String) -> Table
fun distinct-colors(t, col):
  t.build-column("_color", lam(r) block:
      key = to-repr(r.get-value(col))
      when not(string-colors.has-key-now(key)):
        string-colors.set-now(key, nextColor())
      end
      string-colors.get-value-now(key)
    end)
end


fun add-margin(img) block:
  margin = 75
  overlay(
    center-pinhole(img),
    rectangle(
      image-width(img) + margin,
      image-height(img) + margin,
      "outline",
      "transparent"))
end

fun make-title(str-list):
  fold_n(
    lam(i, acc, str):
      beside(
        acc,
        if num-is-integer(i / 2): text(" " + str + " ", 18, "black")
        else: text-font(str, 16, "blue", "Courier",
            "swiss", "normal", "normal", false)
        end)
    end,
    0,
    text("",18,"black"),
    str-list)
end

fun minimum(t :: Table, col :: String) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the minimum, because the '" + col + "' column does not contain quantitative data"))
  else:
    Math.min(t.column(col))
  end
end

fun maximum(t :: Table, col :: String) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the maximum, because the '" + col + "' column does not contain quantitative data"))
  else:
    Math.max(t.column(col))
  end
end

fun iqr(t :: Table, col :: String) block:
  check-integrity(t, [list: col])
  l = t.column(col).sort()
  first-half = l.split-at(num-floor(l.length() / 2)).prefix
  second-half = l.split-at(num-ceiling(l.length() / 2)).suffix
  num-to-string-digits(Stats.median(second-half) - Stats.median(first-half),2)
end
fun IQR(t, col): iqr(t, col) end

fun get-5-num-summary(t :: Table, col :: String) block:
  check-integrity(t, [list: col])
  l = t.column(col)
  shadow l = l.sort()
  first-half = l.split-at(num-floor(l.length() / 2)).prefix
  second-half = l.split-at(num-ceiling(l.length() / 2)).suffix
  shadow minimum = num-to-string-digits(Math.min(l), 2)
  Q1 = num-to-string-digits(Stats.median(first-half), 2)
  Q2 = num-to-string-digits(Stats.median(l), 1)
  Q3 = num-to-string-digits(Stats.median(second-half), 2)
  shadow maximum = num-to-string-digits(Math.max(l), 2)
  R = num-to-string-digits(Math.max(l) - Math.min(l), 2)
  [list: "Min:", minimum, ",  Q1:", Q1, ",  Median:", Q2, ",  Q3:", Q3, ",  Max:", maximum].join-str("")
end


fun row-n(t :: Table, n :: Number) block:
  check-integrity(t, [list: ])
  t.row-n(n)
end

# if the column is a boolean, sort by its repr; if it's a string, sort
# case-insensitively; otherwise sort directly
shadow sort = lam(t :: Table, col :: String, asc :: Boolean):
  if (t.all-rows().length() == 0):
    t.order-by(col, asc)
  else if is-boolean(t.row-n(0)[col]):
    t.build-column("tmp", lam(r): to-repr(r[col]) end).order-by("tmp", asc).drop("tmp")
  else if is-string(t.row-n(0)[col]):
    t.build-column("tmp", lam(r): string-to-lower(r[col]) end).order-by("tmp", asc).drop("tmp")
  else:
    t.order-by(col, asc)
  end
end

shadow filter = lam(t :: Table, fn :: (Row->Boolean)):
  t.filter(fn)
end

fun build-column(t :: Table, col :: String, fn :: (Row->Any)):
  t.build-column(col, fn)
end

fun transform-column(t :: Table, col :: String, fn :: (Row->Any)):
  t.transform-column(col, fn)
end

fun find-by-id(t :: Table, id):
  id-col = t.column-names().get(0)
  row-n(filter(t, lam(r): r[id-col] == id end), 0)
end

fun stack-table(t1 :: Table, t2 :: Table): t1.stack(t2) end

fun stack-tables(ts :: List<Table>): 
  L.fold({(base, t): base.stack(t)}, ts.first, ts.rest)
end

## CENTER AND SPREAD #############################################
mean :: (t :: Table, col :: String) -> Number
fun mean(t, col) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the mean, because the specified column does not contain numeric data"))
  else:
    Stats.mean(ensure-numbers(t.column(col)))
  end
end

median :: (t :: Table, col :: String) -> Number
fun median(t, col) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the median, because the specified column does not contain numeric data"))
  else:
    Stats.median(ensure-numbers(t.column(col)))
  end
end

modes  :: (t :: Table, col :: String) -> List<Number>
fun modes( t, col) block:
  check-integrity(t, [list: col])
  Stats.modes(t.column(col))
end

shadow sum = lam(t :: Table, col :: String) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the sum, because the specified column does not contain numeric data"))
  else:
    Math.sum(ensure-numbers(t.column(col)))
  end
end

stdev  :: (t :: Table, col :: String) -> Number
fun stdev( t, col) block:
  check-integrity(t, [list: col])
  if not(is-number(t.column(col).get(0))):
    raise(Err.message-exception("Cannot compute the mean, because the specified column does not contain numeric data"))
  else:
    Stats.stdev-sample(ensure-numbers(t.column(col)))
  end
end

r-value:: (t :: Table, xs :: String, ys :: String) -> Number
fun r-value(t, xs, ys) block:
  check-integrity(t, [list: xs, ys])
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot compute the mean, because the specified columns do not contain numeric data"))
  else:
    fn = Stats.linear-regression(
      ensure-numbers(t.column(xs)),
      ensure-numbers(t.column(ys)))
    dir = if ((fn(1) - fn(0)) < 0): -1 else: 1 end
    dir * num-sqrt(Stats.r-squared(t.column(xs), t.column(ys), fn))
  end
end

## PIE AND BAR CHARTS ###########################################

# given a summary table with columns <col> and "frequency",
# apply a function <f> to each row and produce the
# resulting image-list, or a helpful error
fun make-images-from-grouped-rows(summary, col, f):
  cases(Eth.Either) run-task(lam():
          summary.all-rows().map(f)
        end):
    | left(v) => v
    | right(v) => raise(Err.message-exception("Could not find an image for one of the values in the '" + col + "' column. Check to make sure that your drawing function correctly produces an image for each unique entry"))
  end
end

fun pie-chart-raw(t, ls, vs, column-name) block:
  labels = get-labels(t, ls)
  series = from-list.pie-chart(labels, ensure-numbers(t.column(vs)))
    .colors(t.column("_color"))
  chart = render-chart(series).width(600).height(400)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", column-name])
  above(title, add-margin(img))
end

# no need to check integrity - all parent functions do it first
fun bar-chart-raw(t, ls, vs, column-name) block:
  labels = get-labels(t, ls)
  series = from-list.bar-chart(labels, ensure-numbers(t.column(vs)))
    .colors(t.column("_color"))
  chart = render-chart(series).width(600).height(400)
    .x-axis(column-name)
    .y-axis(vs)
    .y-min(0)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", column-name])
  above(title, img)
end

# wrappers for raw charts: extract a summary table
# and compute the colors, and display
pie-chart :: (t :: Table, col :: String) -> Image
fun pie-chart(t, col) block:
  check-integrity(t, [list: col])
  title = "Cases for column: '" + col + "'"
  summary = count(t, col)
  color-table = distinct-colors(summary, col)
  pie-chart-raw(color-table, col, "frequency", col)
end


color-pie-chart :: (t :: Table, col :: String, f :: (Row -> String)) -> Image
fun color-pie-chart(t, col, f) block:
  check-integrity(t, [list: col])
  summary = count(t, col)
  color-strs = make-images-from-grouped-rows(summary, col, f)
  colors = color-strs.map(color-named)
  series = from-list.pie-chart(
    get-labels(summary, col),
    ensure-numbers(summary.column("frequency")))
    .colors(colors)
  chart = render-chart(series).width(600).height(400)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", col])
  above(title, add-margin(img))
end

#|
   image-pie-chart :: (t :: Table, col :: String, f :: (Row -> Image)) -> Image
   fun image-pie-chart(t, col, f) block:
  check-integrity(t, [list: col])
  summary = count(t, col)
  images = make-images-from-grouped-rows(summary, col, f)
  series = from-list.image-pie-chart(
    images,
    get-labels(summary, col),
    ensure-numbers(summary.column("frequency")))
  chart = render-chart(series)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", col])
  above(title, add-margin(img))
   end
|#
bar-chart :: (t :: Table, col :: String) -> Image
fun bar-chart(t, col) block:
  check-integrity(t, [list: col])
  summary = count(t, col)
  color-table = distinct-colors(summary, col)
  bar-chart-raw(color-table, col, "frequency", col)
end


color-bar-chart :: (t :: Table, col :: String, f :: (Row -> String)) -> Image
fun color-bar-chart(t, col, f) block:
  fun image-from-row(r): square(10, "solid", f(r)) end
  image-bar-chart(t, col, image-from-row)
end


image-bar-chart :: (t :: Table, col :: String, f :: (Row -> Image)) -> Image
fun image-bar-chart(t, col, f) block:
  check-integrity(t, [list: col])
  summary = count(t, col)
  images = make-images-from-grouped-rows(summary, col, f)
  series = from-list.image-bar-chart(
    images,
    get-labels(summary, col),
    ensure-numbers(summary.column("frequency")))
  chart = render-chart(series).width(600).height(400).y-min(0)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", col])
  above(title, add-margin(img))
end

# wrappers for summarized charts: check for numeric
# data, extract the colors, and display
fun pie-chart-summarized(t, ls, vs) block:
  check-integrity(t, [list: ls, vs])
  if not(is-number(t.column(vs).get(0))):
    raise(Err.message-exception("Cannot make a summarized pie chart, because the 'values' column does not contain numeric data"))
  else:
    color-table = distinct-colors(t, ls)
    pie-chart-raw(color-table, ls, vs, ls)
  end
end

fun bar-chart-summarized(t, ls, vs) block:
  check-integrity(t, [list: ls, vs])
  if not(is-number(t.column(vs).get(0))):
    raise(Err.message-exception("Cannot make a summarized bar chart, because the 'values' column does not contain numeric data"))
  else:
    color-table = distinct-colors(t, vs)
    bar-chart-raw(color-table, ls, vs, ls)
  end
end


stacked-bar-chart :: (t :: Table, col :: String, subcol :: String) -> Image
fun stacked-bar-chart(t, col, subcol) block:
  check-integrity(t, [list: col, subcol])
  shadow segments = Sets.list-to-set(t.get-column(subcol).map(to-repr)).to-list().sort()
  color-list = segments.map(lam(_): nextColor() end)
  tab = group-and-subgroup(t, col, subcol)
  series = from-list.stacked-bar-chart(
    tab.get-column("group").map(to-repr),
    tab.get-column("data"),
    segments)
    .stacking-type(percent)
    .colors(color-list)
  chart = render-chart(series).width(600).height(400)
    .x-axis(col).y-axis(subcol)
  img = display-chart(chart)
  title = make-title([list:"Distribution of", subcol, "by", col])
  above(title, add-margin(img))
end

fun stacked-bar-chart-summarized(t, categories, column-list) block:
  check-integrity(t, [list: categories].append(column-list))
  color-list = column-list.map(lam(_): nextColor() end)
  groups = t.get-column(categories).map(to-repr)
  raw_data = map(lam(col): t.get-column(col) end, column-list)
  zipped_data = map_n(lam(n, _):
      map_n(lam(m,_): raw_data.get(m).get(n) end, 0, raw_data)
    end, 0, raw_data.get(0))
  series = from-list.stacked-bar-chart(
    groups,
    zipped_data,
    column-list)
    .colors(color-list)
  chart = render-chart(series).width(600).height(400)
  display-chart(chart)
end


multi-bar-chart :: (t :: Table, col :: String, subcol :: String) -> Image
fun multi-bar-chart(t, col, subcol) block:
  check-integrity(t, [list: col, subcol])
  shadow segments = Sets.list-to-set(t.get-column(subcol).map(to-repr))
    .to-list().sort()
  color-list = segments.map(lam(_): nextColor() end)
  tab = group-and-subgroup(t, col, subcol)
  series = from-list.grouped-bar-chart(
    tab.get-column("group").map(to-repr),
    tab.get-column("data"),
    segments)
    .colors(color-list)
  chart = render-chart(series).width(600).height(400)
    .x-axis(col + " ⋲ " + subcol)
    .y-axis("frequency")
  img = display-chart(chart)
  title = make-title([list:"Distribution of", subcol, "by", col])
  above(title, add-margin(img))
end

fun multi-bar-chart-summarized(t, categories, column-list) block:
  check-integrity(t, [list: categories].append(column-list))
  color-list = column-list.map(lam(_): nextColor() end)
  groups = t.get-column(categories).map(to-repr)
  raw_data = map(lam(col): t.get-column(col) end, column-list)
  zipped_data = map_n(lam(n, _):
      map_n(lam(m,_): raw_data.get(m).get(n) end, 0, raw_data)
    end, 0, raw_data.get(0))
  series = from-list.grouped-bar-chart(
    groups,
    zipped_data,
    column-list)
    .colors(color-list)
  chart = render-chart(series).width(600).height(400)
  display-chart(chart)
end



## DOT PLOTS #############################################

simple-dot-plot :: (t :: Table, vals :: String) -> Image
fun simple-dot-plot(t, vals) block:
  check-integrity(t, [list: vals])
  is-quant = is-number(t.column(vals).get(0))
  vs = if is-quant: t.column(vals) else: t.column(vals).map(to-string) end
  series = if is-quant:
    from-list.num-dot-chart(vs)
  else:
    from-list.dot-chart(vs)
  end
  chart = render-chart(series).width(600).height(400)
    .x-axis(vals).y-axis("frequency")
  img = display-chart(chart)
  title = make-title([list:"Dot Plot of", vals])
  above(title, add-margin(img))
end

dot-plot :: (t :: Table, labels :: String, vals :: String) -> Image
fun dot-plot(t, labels, vals) block:
  check-integrity(t, [list: labels, vals])
  ls = t.column(labels).map(to-repr)
  is-quant = is-number(t.column(vals).get(0))
  vs = if is-quant: t.column(vals) else: t.column(vals).map(to-string) end
  series = if is-quant:
    from-list.labeled-num-dot-chart(ls, vs)
  else:
    from-list.dot-chart(vs).labels(ls)
  end
  chart = render-chart(series).width(600).height(400)
    .x-axis(vals)
    .y-axis("frequency")
  img = display-chart(chart)
  title = make-title([list:"Dot Plot of", vals])
  above(title, add-margin(img))
end


fun color-dot-plot(t, vals, f :: (Row -> Image)) block:
  fun image-from-row(r): square(10, "solid", f(r)) end
  image-dot-plot(t, vals, image-from-row)
end

fun image-dot-plot(t, vals, f :: (Row -> Image)) block:
  check-integrity(t, [list: vals])
  is-quant = is-number(t.column(vals).get(0))
  vs = if is-quant: t.column(vals) else: t.column(vals).map(to-string) end
  images = t.all-rows().map(f)
  max-height = images.map(image-height).foldl(num-max, 0)
  series = if is-quant:
    from-list.image-num-dot-chart(images, vs)
  else:
    from-list.image-dot-chart(images, vs)
  end
  chart = render-chart(series).width(600).height(400)
    .x-axis(vals).y-axis("frequency")
  img = display-chart(chart)
  title = make-title([list:"Dot Plot of", vals])
  above(title, add-margin(img))
end



## HISTOGRAMS #############################################
simple-histogram :: (t :: Table, vals :: String, bin-width :: Number) -> Image
fun simple-histogram(t, vals, bin-width) block:
  doc: "wrap histogram so that the bin-width is set"
  check-integrity(t, [list: vals])
  if not(is-number(t.column(vals).get(0))):
    raise(Err.message-exception("Cannot make a histogram, because the '" + vals + "' column does not contain quantitative data"))
  else:
    series = from-list.histogram(ensure-numbers(t.column(vals))).bin-width(bin-width)
    chart = render-chart(series).width(600).height(400)
      .x-axis(vals)
      .y-axis("frequency")
    img = display-chart(chart)
    title = make-title([list:"Distribution of", vals])
    above(title, add-margin(img))
  end
end

histogram :: (t :: Table, labels :: String, vals :: String, bin-width :: Number) -> Image
fun histogram(t, labels, vals, bin-width) block:
  doc: "wrap histogram so that the bin-width is set"
  check-integrity(t, [list: labels, vals])
  if not(is-number(t.column(vals).get(0))):
    raise(Err.message-exception("Cannot make a histogram, because the '" + vals + "' column does not contain quantitative data"))
  else:
    series = from-list.labeled-histogram(t.column(labels).map(to-repr), ensure-numbers(t.column(vals)))
      .bin-width(bin-width)
    chart = render-chart(series).width(600).height(400)
      .x-axis(vals)
      .y-axis("frequency")
    img = display-chart(chart)
    title = make-title([list:"Distribution of", vals])
    above(title, add-margin(img))
  end
end

color-histogram :: (t :: Table, vals :: String, bin-width :: Number, f :: (Row -> String)) -> Image
fun color-histogram(t, vals, bin-width, f) block:
  fun image-from-row(r): square(10, "solid", f(r)) end
  image-histogram(t, vals, bin-width, image-from-row)
end

image-histogram :: (t :: Table, vals :: String, bin-width :: Number, f :: (Row -> Image)) -> Image
fun image-histogram(t, vals, bin-width, f) block:
  check-integrity(t, [list: vals])
  images = t.all-rows().map(f)
  if not(is-number(t.column(vals).get(0))):
    raise(Err.message-exception("Cannot make a histogram, because the '" + vals + "' column does not contain quantitative data"))
  else:
    series = from-list.image-histogram(images, ensure-numbers(t.column(vals)))
      .bin-width(bin-width)
    chart = render-chart(series).width(600).height(400)
      .x-axis(vals)
      .y-axis("frequency")
    img = display-chart(chart)
    title = make-title([list:"Distribution of", vals])
    above(title, add-margin(img))
  end
end

scaled-histogram :: (t :: Table, vals :: String, bin-width :: Number, low :: Number, high :: Number) -> Image
fun scaled-histogram(t, vals, bin-width, low, high) block:
  doc: "wrap histogram so that the bin-width is set"
  check-integrity(t, [list: vals])
  if not(is-number(t.column(vals).get(0))):
    raise(Err.message-exception("Cannot make a histogram, because the '" + vals + "' column does not contain quantitative data"))
  else:
    series = from-list.histogram(ensure-numbers(t.column(vals))).bin-width(bin-width)
    chart = render-chart(series).width(600).height(400)
      .x-axis(vals)
      .y-axis("frequency")
      .min(low).max(high)
    img = display-chart(chart)
    title = make-title([list:"Distribution of", vals])
    above(title, add-margin(img))
  end
end



## BOX PLOTS #############################################

box-plot-raw :: (t :: Table, vs :: String, low :: Number, high :: Number, horizontal :: Boolean, showOutliers :: Boolean) -> Image
fun box-plot-raw(t, vs, low, high, horizontal, showOutliers) block:
  l = ensure-numbers(t.column(vs))
  padding = (high - low) / 1000 # pad with 1000th the range
  if l.length() < 2:
    raise(Err.message-exception("At least two rows are needed to make a box plot"))
  else if not(is-number(l.get(0))):
    raise(Err.message-exception("Cannot make a box plot, because the 'values' column does not contain numeric data"))
  else if (low > high):
    raise(Err.message-exception("Min value must be lower than Max value"))
  else:
    series = from-list.labeled-box-plot([list: vs], [list: l])
      .horizontal(horizontal).show-outliers(showOutliers)
      .color(make-color(0,0,100,1))
    chart = render-chart(series).width(600).height(200)
      .title(get-5-num-summary(t, vs))
      .min(low)
      .max(high + padding)
    img = display-chart(chart)
    title = make-title([list:"Distribution of", vs])
    above(title, add-margin(img))
  end
end

box-plot :: (t :: Table, vs :: String) -> Image
# pass the min as 0 and the max as the largest value in the column
fun box-plot(t, vs) block:
  check-integrity(t, [list: vs])
  lo = Math.min(t.column(vs))
  hi = Math.max(t.column(vs))
  box-plot-raw(t, vs, lo, hi, true, false)
end

box-plot-scaled :: (t :: Table, vs :: String, lo :: Number, hi :: Number) -> Image
fun box-plot-scaled(t, vs, lo, hi) block:
  check-integrity(t, [list: vs])
  box-plot-raw(t, vs, lo, hi, true, false)
end

modified-box-plot :: (t :: Table, vs :: String) -> Image
fun modified-box-plot(t, vs) block:
  check-integrity(t, [list: vs])
  lo = Math.min(t.column(vs))
  hi = Math.max(t.column(vs))
  box-plot-raw(t, vs, lo, hi, true, true)
end

modified-box-plot-scaled :: (t :: Table, vs :: String, lo :: Number, hi :: Number) -> Image
fun modified-box-plot-scaled(t, vs, lo, hi) block:
  check-integrity(t, [list: vs])
  box-plot-raw(t, vs, lo, hi, true, true)
end

vert-box-plot :: (t :: Table, vs :: String) -> Image
fun vert-box-plot(t, vs) block:
  check-integrity(t, [list: vs])
  lo = Math.min(t.column(vs))
  hi = Math.max(t.column(vs))
  box-plot-raw(t, vs, lo, hi, false, false)
end

modified-vert-box-plot :: (t :: Table, vs :: String) -> Image
fun modified-vert-box-plot(t, vs) block:
  check-integrity(t, [list: vs])
  lo = Math.min(t.column(vs))
  hi = Math.max(t.column(vs))
  box-plot-raw(t, vs, lo, hi, false, true)
end

modified-vert-box-plot-scaled :: (t :: Table, vs :: String, lo :: Number, hi :: Number) -> Image
fun modified-vert-box-plot-scaled(t, vs, lo, hi) block:
  check-integrity(t, [list: vs])
  box-plot-raw(t, vs, lo, hi, false, true)
end

## LINE GRAPHS ######################################################
line-graph :: (t :: Table, labels :: String, xs :: String, ys :: String) -> Image
fun line-graph(t, labels, xs, ys) block:
  check-integrity(t, [list: xs, ys])
  l = ensure-numbers(t.column(xs))
  l2 = ensure-numbers(t.column(ys))
  ls = get-labels(t, labels)
  sorted = t.order-by(xs, true) # sort the table by x-axis
  series = from-list.line-plot(sorted.column(xs), sorted.column(ys))
  scatter-series= from-list.labeled-scatter-plot(ls, sorted.column(xs), sorted.column(ys))
  chart = render-charts([list: series, scatter-series]).width(600).height(400)
    .x-axis(xs)
    .y-axis(ys)
  img = display-chart(chart)
  title = make-title([list:"", ys, "vs.", xs])
  above(title, add-margin(img))
end


## LR AND SCATTER PLOTS #############################################
scatter-plot :: (t :: Table, labels :: String, xs :: String, ys :: String) -> Image
fun scatter-plot(t, labels, xs, ys) block:
  check-integrity(t, [list: labels, xs, ys])
  ls = get-labels(t, labels)
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    series = from-list.labeled-scatter-plot(ls, ensure-numbers(t.column(xs)), ensure-numbers(t.column(ys)))
    padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
    chart = render-chart(series).width(600).height(400)
      .x-axis(xs)
      .y-axis(ys)
      .y-min(Math.min(t.column(ys)) - padding)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

simple-scatter-plot :: (t :: Table, xs :: String, ys :: String) -> Image
fun simple-scatter-plot(t, xs, ys) block:
  check-integrity(t, [list: xs, ys])
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    series = from-list.scatter-plot(ensure-numbers(t.column(xs)), ensure-numbers(t.column(ys)))
    padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
    chart = render-chart(series).width(600).height(400)
      .x-axis(xs)
      .y-axis(ys)
      .y-min(Math.min(t.column(ys)) - padding)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

color-scatter-plot :: (t :: Table, xs :: String, ys :: String, f :: (Row -> Image)) -> Image
fun color-scatter-plot(t, xs, ys, f) block:
  fun image-from-row(r): circle(5, "solid", f(r)) end
  image-scatter-plot(t, xs, ys, image-from-row)
end

image-scatter-plot :: (t :: Table, xs :: String, ys :: String, f :: (Row -> Image)) -> Image
fun image-scatter-plot(t, xs, ys, f) block:
  check-integrity(t, [list: xs, ys])
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make an image scatter plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    x-vals = ensure-numbers(t.column(xs))
    y-vals = ensure-numbers(t.column(ys))
    images = t.all-rows().map(f)
    maxImgH = Math.max(images.map(image-height))
    maxImgW = Math.max(images.map(image-width))
    minX = Math.min(x-vals)
    maxX = Math.max(x-vals)
    minY = Math.min(y-vals)
    maxY = Math.max(y-vals)
    # compute padding using default window size (800pxx600px)
    paddingX = (maxX - minX) * (maxImgW / 800)
    paddingY = (maxY - minY) * (maxImgH / 600)
    series = from-list.image-scatter-plot(images, x-vals, y-vals)
    chart = render-chart(series).width(600).height(400)
      .x-axis(xs)
      .y-axis(ys)
      .x-min(minX - paddingX)
      .x-max(maxX + paddingX)
      .y-min(minY - paddingY)
      .y-max(maxY + paddingY)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

scatter-plot-3d :: (t :: Table, labels :: String, xs :: String, ys :: String, zs :: String) -> Image
fun scatter-plot-3d(t, labels, xs, ys, zs) block:
  check-integrity(t, [list: labels, xs, ys, zs])
  ls = get-labels(t, labels)
  if not(
      is-number(t.column(xs).get(0)) and 
      is-number(t.column(ys).get(0)) and 
      is-number(t.column(zs).get(0))):
    raise(Err.message-exception("Cannot make a scatter plot, because the 'xs', 'ys', and 'zs' columns must all contain numeric data"))
  else:
    series = from-list.labeled-scatter-plot-3d(ls, ensure-numbers(t.column(xs)), ensure-numbers(t.column(ys)), ensure-numbers(t.column(zs)))
    chart = render-chart(series).width(600).height(400)
      .x-axis(xs)
      .y-axis(ys)
      .z-axis(zs)
    img = display-chart(chart)
    title = make-title([list:"", zs, "vs.", xs, "vs.", ys])
    above(title, add-margin(img))
  end
end


fun make-lr-title(fn, r-sqr-num, s-num) :
  r-num = (if  (fn(1) - fn(0)) < 0: -1 else: 1 end) * num-sqrt(r-sqr-num)
  alpha  = fn(2) - fn(1)
  alpha-str = easy-num-repr(fn(2) - fn(1), 8)
  beta-str =  easy-num-repr(fn(0), 8)
  r-str = easy-num-repr(r-num, 6)
  r-sqr-str = easy-num-repr(r-sqr-num, 6)
  S-str     = easy-num-repr(s-num, 9)
  "y=" + alpha-str + "x + " + beta-str + "  r: " + r-str + "  R²: " + r-sqr-str + " S: " + S-str
end

lr-plot :: (t :: Table, ls :: String, xs :: String, ys :: String) -> Image
fun lr-plot(t, ls, xs, ys) block:
  check-integrity(t, [list: ls, xs, ys])
  labels = get-labels(t, ls)
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make an lr-plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    scatter = from-list.labeled-scatter-plot(labels, ensure-numbers(t.column(xs)), ensure-numbers(t.column(ys)))
      .legend("Data")
    padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
    fn = Stats.linear-regression(t.column(xs), t.column(ys))
    fn-plot = from-list.function-plot(fn)
      .legend("Model")
    # wrap the xs in a list, and fn in a row-consuming fn
    s-num = regression-model-S(t, [list: xs], ys, {(r): fn(r[xs])})
    r-sqr-num = Stats.r-squared(t.column(xs), t.column(ys), fn)
    chart = render-charts([list: scatter, fn-plot]).width(600).height(400)
      .title(make-lr-title(fn, r-sqr-num, s-num))
      .x-axis(xs)
      .y-axis(ys)
      .y-min(Math.min(t.column(ys)) - padding)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

simple-lr-plot :: (t :: Table, xs :: String, ys :: String) -> Image
fun simple-lr-plot(t, xs, ys) block:
  check-integrity(t, [list: xs, ys])
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make an lr-plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    scatter = from-list.scatter-plot(
      ensure-numbers(t.column(xs)),
      ensure-numbers(t.column(ys)))
    padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
    fn = Stats.linear-regression(t.column(xs), t.column(ys))
    fn-plot = from-list.function-plot(fn)
    # wrap the xs in a list, and fn in a row-consuming fn
    s-num = regression-model-S(t, [list: xs], ys, {(r): fn(r[xs])})
    r-sqr-num = Stats.r-squared(t.column(xs), t.column(ys), fn)
    chart = render-charts([list: scatter, fn-plot]).width(600).height(400)
      .title(make-lr-title(fn, r-sqr-num, s-num))
      .x-axis(xs)
      .y-axis(ys)
      .y-min(Math.min(t.column(ys)) - padding)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

image-lr-plot :: (t :: Table, xs :: String, ys :: String, f :: (Row -> Image)) -> Image
fun image-lr-plot(t, xs, ys, f) block:
  check-integrity(t, [list: xs, ys])
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise(Err.message-exception("Cannot make an image-lr-plot, because the 'xs' and 'ys' columns must both contain numeric data"))
  else:
    images = t.all-rows().map(f)
    scatter = from-list.image-scatter-plot(
      images,
      ensure-numbers(t.column(xs)),
      ensure-numbers(t.column(ys)))
      .legend("Data")
    padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
    fn = Stats.linear-regression(t.column(xs), t.column(ys))
    fn-plot = from-list.function-plot(fn)
      .legend("Model")
    # wrap the xs in a list, and fn in a row-consuming fn
    s-num = regression-model-S(t, [list: xs], ys, {(r): fn(r[xs])})
    r-sqr-num = Stats.r-squared(t.column(xs), t.column(ys), fn)
    chart = render-charts([list: scatter, fn-plot]).width(600).height(400)
      .title(make-lr-title(fn, r-sqr-num, s-num))
      .x-axis(xs)
      .y-axis(ys)
      .y-min(Math.min(t.column(ys)) - padding)
    img = display-chart(chart)
    title = make-title([list:"", ys, "vs.", xs])
    above(title, add-margin(img))
  end
end

regression-model-coeffs :: (t :: Table, params :: List<String>, response :: String) -> Table
fun regression-model-coeffs(t, params, response) block:
  all-cols = link(response, params)
  check-integrity(t, all-cols)
  # check to make sure the cols exist
  if all-cols.any(lam(col): not(t.column-names().member(col)) end):
    raise(Err.message-exception("One or more of the columns (" + params.join-str(", ") + " or " + response + ") was not found in the table. The valid columns are: " + t.column-names().join-str(", ")))
    # check to make sure the cols are all numeric
  else if all-cols.any(lam(col): not(is-number(t.column(col).get(0))) end):
    raise(Err.message-exception("One or more of the columns (" + params.join-str(", ") + " or " + response + ") does not contain numeric data."))
    # check to make sure we have enough data
  else if params.length() > (t.length() + 1):
    raise(Err.message-exception("Cannot perform regression on these parameters on this table, because a model with " + params.length() + " parameters requires a table with at least " + (params.length() + 1) + " rows"))
  else:
    # generate the coefficients table, replacing Pyret's default "constant" with "intercept"
    Stats.multiple-regression-table(t, params, response)
      .transform-column("coefficient-name", {(v): if v == "constant": "intercept" else: v end})
  end
end

lr-coeffs :: (t :: Table, param :: String, response :: String) -> Table
fun lr-coeffs(t, param, response): regression-model-coeffs(t, [list: param], response) end

regression-model-fun :: (t :: Table, params :: List<String>, response :: String) -> (Row -> Number)
fun regression-model-fun(t, params, response) block:
  # generate the coefficients table
  coeffs = regression-model-coeffs(t, params, response)
  # return a wrapped function, which consumes a row of the 
  # table, extracts the values in explanation-order, and 
  # passes them to the raw function
  lam(r :: Row) block: 
    fun row-missing-col(n): is-none(r.get(n)) end
    when params.any(row-missing-col):
      raise(Err.message-exception("One or more of the columns needed by this model (" + params.join-str(", ") + " and " + response + ") was not found in the table. Are you sure you are fitting the right model to the right data?"))
    end

    fun fold-coeffs(acc, row): 
      if row["coefficient-name"] == "intercept": acc + row["coefficient-value"]
      else: acc + (r[row["coefficient-name"]] * row["coefficient-value"])
      end
    end
    L.foldr(fold-coeffs, 0, coeffs.all-rows())
  end
end

lr-fun :: (t :: Table, param :: String, response :: String) ->  (Row -> Number)
# just a special-case wrapper for multiple-regression-fun, which produces
# a function consuming a row and producing a number
fun lr-fun(t, param, response):
  regression-model-fun(t, [list: param], response)
end

regression-model-code :: (t :: Table, params :: List<String>, response :: String) -> Nothing
fun regression-model-code(t, params, response) block:
  # generate the coefficients table
  coeffs = regression-model-coeffs(t, params, response)
  
  fn-name = params.join-str("-") + "-predictor"

  params-string = if params.length() == 1: "a value for " + params.join-str("")
  else: "values for " + L.take(params.length() - 1, params).join-str(", ") + " and " + params.last()
  end
  
  # return a wrapped function, which consumes a row of the 
  # table, extracts the values in explanation-order, and 
  # passes them to the raw function
  fun fold-code(acc, row): 
    if row["coefficient-name"] == "intercept": 
      acc + num-to-string-digits(row["coefficient-value"], 2)
    else: 
      acc + "(" + num-to-string-digits(row["coefficient-value"], 2) +
      " * " + "r[\"" + row["coefficient-name"] + "\"]" +
      ") + "
    end
  end
  print(
    "# " + fn-name + " :: (r :: Row) -> Number\n" +
    "# Consumes a Row of containing " + params-string + "\n" +
    "# and produces the predicted " + response + "\n" +
    "fun " + fn-name + "(r):\n" + 
    "  " + L.foldr(fold-code, "", coeffs.all-rows()) + "\n" +
    "end")
  nothing
end

#|
lr-code :: (t :: Table, param :: String, response :: String) -> Nothing
# just a special-case wrapper for multiple-regression-fun, which produces
# a function consuming a row and producing a number
fun lr-code(t, param, response):
  regression-model-code(t, [list: param], response)
end
|#

predict-col :: (t :: Table, target-col :: String, predictor :: (Row -> Number)) -> Table
# Move the target column to the end of the table, alongside new 'predicted' and 'error' columns
fun predict-col(t, target-col, predictor) block:
  new-col = target-col + " (predicted)"
  p-table = build-column(t, new-col, predictor)
  if (t.column-names().member(target-col)):
    test-f = {(r): r[new-col] - r[target-col] }
    new-cols = t.column-names()
      .filter({(s): s <> target-col})
      .append([list: target-col, new-col, "Error"])
    p-table.build-column("Error", test-f)
      .select-columns(new-cols)
  else:
    p-table
  end
end


mr-residuals :: (t :: Table, explanations :: List<String>, response :: String, model :: (Row -> Number)) -> List<Number>
fun mr-residuals(t, explanations, response, model) block:
  all-cols = link(response, explanations)
  valid-columns = t.column-names()
  fun missing-column(c): not(valid-columns.member(c)) end
  when all-cols.any(missing-column):
    raise(Err.message-exception("One or more of the columns needed by this model (" + explanations.join-str(", ") + " and " + response + ") was not found in the table. Are you sure you are fitting the right model to the right data?"))
  end
  predictions = map(model, t.all-rows())
  ys = t.column(response)
  map2(lam(y, y-hat): y - y-hat end, ys, predictions)
end

residuals :: (t :: Table, explanation :: String, response :: String, model :: (Number -> Number)) -> List<Number>
fun residuals(t, explanation, response, model):
  fun f(r): model(r[explanation]) end
  mr-residuals(t, [list: explanation], response, f)
end

regression-model-S :: (t :: Table, explanations :: List<String>, response :: String, model :: (Row -> Number)) -> Number
fun regression-model-S(t, explanations, response, model) block:

  # error-checking
  all-cols = link(response, explanations)
  valid-columns = t.column-names()
  fun missing-column(c): not(valid-columns.member(c)) end
  when all-cols.any(missing-column):
    raise(Err.message-exception("One or more of the columns needed by this model (" + explanations.join-str(", ") + " and " + response + ") was not found in the table. Are you sure you are fitting the right model to the right data?"))
  end
  when t.length() < all-cols.length():
    raise(Err.message-exception("Cannot calculate S for this model and function, because a model with " + explanations.length() + " parameters requires a table with at least " + all-cols.length() + " rows"))
  end
  check-integrity(t, all-cols)

  # compute the S-value
  params = explanations.length()
  rows = t.length()
  residuals-sqr = mr-residuals(t, explanations, response, model).map(sqr)
  degrees-of-freedom = rows - params
  num-sqrt(Math.sum(residuals-sqr) / degrees-of-freedom)
end

fun regression-model-s(a, b, c, v):
  raise(Err.message-exception("In statistics, the S-value is always capitalized. Pyret does the same thing! Did you mean to use `regression-model-S`?"))
end

S :: (t :: Table, explanation :: String, response :: String, model :: (Row -> Number)) -> Number
fun S(t, explanation, response, model):
  regression-model-S(t, [list: explanation], response, {(r): model(r[explanation])})
end

fit-model :: (t :: Table, ls :: String, xs :: String, ys :: String, fn :: (Number -> Number)) -> Image
fun fit-model(t, ls, xs, ys, fn) block:
  check-integrity(t, [list: ls, xs, ys])
  labels = get-labels(t, ls)

  # the line below calls S, which does our error-checking
  # wrap the xs in a list, and fn in a row-consuming fn
  s-num = regression-model-S(t, [list: xs], ys, {(r): fn(r[xs])})
  R-sqr-value = Stats.r-squared(t.column(xs), t.column(ys), fn)
  S-str       = easy-num-repr(s-num, 10)
  #r-str       = if (R-sqr-value > 0): easy-num-repr(num-sqrt(R-sqr-value)) else: "N/A" end
  r-sqr-str   = easy-num-repr(R-sqr-value, 10)

  scatter = from-list.labeled-scatter-plot(
    labels,
    ensure-numbers(t.column(xs)),
    ensure-numbers(t.column(ys)))
    .legend("Data")
    .point-size(5)
  padding = (Math.max(t.column(ys)) - Math.min(t.column(ys))) / 100
  fn-plot = from-list.function-plot(fn)
    .color(C.red)
    .legend("Model")
  fun f(r): fn(r[xs]) end
  predictions = map(f, t.all-rows())
  intervals = from-list.interval-chart(
    t.column(xs),
    t.column(ys),
    mr-residuals(t, [list: xs], ys, lam(r :: Row): fn(r[xs]) end))
    .point-size(1)
    .pointer-color(C.green)
    .lineWidth(10)
    .color(C.black)
    .style("sticks")
    .legend("Residuals")
  title-str = "S: " + S-str + "   R²: " + r-sqr-str
  chart = render-charts([list: fn-plot, scatter, intervals]).width(600).height(400)
    .title(title-str)
    .x-axis(xs)
    .y-axis(ys)
    .y-min(Math.min(predictions) - padding)
  img = display-chart(chart)
  title = make-title([list:"", ys, "vs.", xs])
  above(title, add-margin(img))
end

# Given a size, produce a normal distribution of that size
# between 0-1 using  Box Muller transform described at
# https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
random-normal-distribution :: (size :: Number) -> List<Number>
fun random-normal-distribution(size) block:
  fun box-muller() block:
    u = (random(100) + 1) / 101
    v = (random(100) + 1) / 101
    num = num-sqrt(-2 * num-log(u)) * num-cos( 2.0 * PI * v )
    (num / 10) + 0.5 # divide and shift to cover (0,1)
  end
  L.range(1, size).map(lam(_): box-muller() end)
end

#####################################################################
## Making tables and graphs from definitions

def-to-table :: (f :: (Number -> Number)) -> Table
# Consumes a function, and produces an x/y table
fun def-to-table(f):
  start = num-random(20) - 10
  step = num-random(5) + 1
  xs = L.range-by(start, start + 100, step)
  ys = xs.map(f)
  [T.table-from-columns: {"x"; xs}, {"y"; ys}]
end

def-to-graph :: (f :: (Number -> Number)) -> Image
# Same as make-table, but makes a line-plot of the resulting table
fun def-to-graph(f) block:
  render-chart(from-list.function-plot(f))
    .x-axis("x")
    .y-axis("y")
    .x-min(-10)
    .x-max(10)
    .y-min(-10)
    .y-max(10)
    .display()
end

table-to-graph :: (t :: Table) -> Image
# Consumes a table, and makes a line-plot from the first 2 columns
fun table-to-graph(t) block:
  cols = t.column-names()
  if (cols.length() < 2):
    raise("The table must have at least two columns")
  else: nothing
  end

  check-integrity(t, [list: cols.get(0), cols.get(1)])

  xs = t.column(cols.get(0))
  ys = t.column(cols.get(1))

  xMin = if (num-round(Math.min(xs)) == num-round(Math.max(xs))):
    num-round(Math.min(xs)) - 5
  else: num-round(Math.min(xs))
  end
  xMax = if (num-round(Math.min(xs)) == num-round(Math.max(xs))):
    num-round(Math.min(xs)) + 5
  else: num-round(Math.max(xs))
  end

  yMin = if (num-round(Math.min(ys)) == num-round(Math.max(ys))):
    num-round(Math.min(ys)) - 5
  else: num-round(Math.min(ys))
  end
  yMax = if (num-round(Math.min(ys)) == num-round(Math.max(ys))):
    num-round(Math.min(ys)) + 5
  else: num-round(Math.max(ys))
  end

  render-chart(from-list.line-plot(xs, ys)).width(600).height(400)
    .x-axis(cols.get(0))
    .y-axis(cols.get(1))
    .x-min(xMin)
    .x-max(xMax)
    .y-min(yMin)
    .y-max(yMax)
    .display()
end


#####################################################################
## String Munging

word-frequency :: String -> Table
fun word-frequency(txt) block:

  fun isAsciiLetter(cp :: Number):
    ((cp > 64) and (cp < 91)) or ((cp > 96) and (cp < 123))
  end

  fun lettersOnly(word :: String):
    string-from-code-points(
      string-explode(word)
        .map(string-to-code-point)
        .filter(isAsciiLetter))
  end

  # Capitalize and split the the string into words, strip each
  # word of any non-ascii-letter characters, filter out any
  # words that are now just the empty string, and sort
  ascii-words = string-split-all(string-to-upper(txt), " ")
    .map(lettersOnly)
    .filter(lam(str): str <> "" end)
    .sort()

  # Walk through the (sorted) words, creating a tuple containing a
  # unique-word list and a list of counts
  unique-counts = L.foldl(
    lam(base, val) block:
      {labels; counts} = base
      if labels.member(val):
        {labels; counts.set(0, counts.get(0) + 1)}
      else:
        {link(val, labels); link(1, counts)}
      end
    end,
    {[list:]; [list:]},
    ascii-words
    )

  # Make a table from those two lists, then add a column that counts characters
  t = T.table-from-column("word", unique-counts.{0})
    .add-column("count", unique-counts.{1})
  t.build-column("characters", lam(r): string-length(r["word"]) end)
    .order-by("count", false)
end

#####################################################################
# used by shapes starter file
draw-shape :: Row -> Image
fun draw-shape(r):
  if r["name"] == "ellipse": ellipse(50, 100, "solid", r["color"])
  else if r["name"] == "circle": circle(50, "solid", r["color"])
  else: regular-polygon(30, r["corners"], "solid", r["color"])
  end
end


#################################################################################
# Live Survey Functions
fun live-display(gsheetID :: String, sheet-name :: String, columns :: List<String>, visualize)  -> Image block:
  fun get-table(t):
    sheet = load-spreadsheet(gsheetID).sheet-by-name(sheet-name, true)
    builtins.open-table(sheet.load(raw-array-from-list(columns), [raw-array: ]))
  end
  r = reactor:
    init: get-table(nothing),
    to-draw: visualize,
    seconds-per-tick: 2,
    on-tick: get-table
  end
  r.interact()
  visualize(get-table(nothing))
end


# live-survey :: (String, String, List<String> :: (Any -> Image)
fun live-survey(gsheetID, sheet-name, columns, visualize) block:
  live-display(gsheetID, sheet-name, columns, visualize)
end


## OUTLIER TOOLS ##########################################

q1 :: (t :: Table, col :: String) -> Number
fun q1(t, col) block:
  check-integrity(t, [list: col])
  values = t.get-column(col).sort()
  first-half = values.split-at(num-floor(values.length() / 2)).prefix
  Stats.median(first-half)
end


q3 :: (t :: Table, col :: String) -> Number
fun q3(t, col) block:
  check-integrity(t, [list: col])
  values = t.get-column(col).sort()
  second-half = values.split-at(num-ceiling(values.length() / 2)).suffix
  Stats.median(second-half)
end

compute-outliers :: (t :: Table, col :: String) -> Table
# consumes a table and a column, calculates both the lower boundary and upper boundary (fences), creates a new column called "is-outler" that is populated with "low", "high" and "no"
# (Credit to Jennifer Braun, CSPdWeek 2019)
fun compute-outliers(t, col) block:
  check-integrity(t, [list: col])
  lower-boundary = q1(t, col) - (1.5 * (q3(t, col) - q1(t, col)))
  upper-boundary = q3(t, col) + (1.5 * (q3(t, col) - q1(t, col)))
  t.build-column("is-outlier",
    (lam(r):
        ask:
          | (r[col] < lower-boundary) then: "low"
          | (r[col] > upper-boundary) then: "high"
          | otherwise: "no"
        end
      end))
end


outliers :: (t :: Table, col :: String) -> Table
# consumes a table and a column, and returns a table for which every row is an outlier
# (for that column)
fun outliers(t, col):
  compute-outliers(t, col).filter(lam(r): r["is-outlier"] <> "no" end).drop("is-outlier")
end

remove-outliers :: (t :: Table, col :: String) -> Table
# consumes a table and a column, calculates both the lower boundary and upper boundary (fences), and removes any rows that are beyond the fence
# (Credit to Jennifer Braun, CSPdWeek 2019)
fun remove-outliers(t, col):
  compute-outliers(t, col)
    .filter(lam(r): r["is-outlier"] == "no" end)
    .drop("is-outlier")
end

## TABLE MUNGING ########################################
row-id :: (t :: Table, id :: String) -> Row
fun row-id(t, id):
  id-col = t.column-names().get(0)
  matching-rows = t.filter(lam(r): r[id-col] == id end)
  if (matching-rows.length() > 1):
    raise(Err.message-exception("The identifier column should contain unique IDs, but this ID matched more than one row"))
  else if (matching-rows.length() == 0) :
    raise(Err.message-exception("No rows have this ID in their identifier column (did you check spelling and capitalization?"))
  else: matching-rows.row-n(0)
  end
end

split-and-reduce :: (
  t :: Table,
  col-to-split :: String,
  col-to-reduce :: String,
  reducer :: (Table, String -> Any)
  ) -> Table
fun split-and-reduce(t, col-to-split, col-to-reduce, reducer) block:
  fun wrapped-reducer(r):
    cases(Eth.Either) run-task(lam():
            reducer(r["subtable"], col-to-reduce)
          end):
      | left(v) => v
      | right(v) => 
        base = "An error occurred when trying to use your reducer on one of your subtables: "
        if Err.is-arity-mismatch(exn-unwrap(v)):
          raise(Err.message-exception(base + "Are you sure it consumes *only* a valid Table and column name?)"))
        else:
          raise(Err.message-exception(base + to-string(exn-unwrap(v))))
        end
    end
  end
  group(t, col-to-split)
    .build-column("result", wrapped-reducer)
    .drop("subtable")
end

first-n-rows :: (t :: Table, n :: Number) -> Table
fun first-n-rows(t, n):
  T.table-from-rows.make(raw-array-from-list(t.all-rows().take(n)))
end

last-n-rows :: (t :: Table, n :: Number) -> Table
fun last-n-rows(t, n):
  T.table-from-rows.make(raw-array-from-list(t.all-rows().reverse().take(n).reverse()))
end


fun group(tab, col):
  values = Sets.list-to-list-set(tab.get-column(col)).to-list()
  for fold(shadow grouped from table: value, subtable end, v from values):
    grouped.stack(table: value, subtable
        row: v, tab.filter-by(col, {(val): val == v})
      end)
  end
end

fun count(tab, col):
  g = group(tab, col).build-column("frequency", {(r): r["subtable"].length()}).drop("subtable")
  if is-boolean(g.column("value").get(0)): g
  else: order g: value ascending end
  end
    .rename-column("value", col)
end

#|
   fun count-many(tab, cols):
  for fold(shadow grouped from table: col, subtable end, c from cols):
    grouped.stack(table: col, subtable
        row: c, count(tab, c)
      end)
  end
   end
|#

fun group-and-subgroup(t :: Table, col :: String, subcol :: String) block:
  subgroups = Sets.list-to-set(t.get-column(subcol))
  tab = group(t, col)
    .rename-column("value", "group")
    .build-column(
    "data",
    # take a count of the subgroups, then see which subgroups are missing
    # for each one, add a row with a count of zero. Then order the rows
    # and extract the count as a list
    lam(r) block:
      segments = count(r["subtable"], subcol)
      missing = subgroups.difference(Sets.list-to-set(segments.get-column(subcol)))
      missing.fold(
        lam(table, subgroup):
          table.add-row([T.raw-row: {subcol; subgroup}, {"frequency"; 0}])
        end,
        segments)
        .build-column("sortable", lam(shadow r): to-repr(r[subcol]) end)
        .order-by("sortable", true)
        .get-column("frequency")
    end)
  # sort groups alphabetically
  sort(tab, "group", true)
end


# pivot-row :: r :: Row -> Table
fun pivot-row(r) block:

  # get-val :: (col :: String) -> Any
  fun get-val(col): r[col] end

  columns = r.get-column-names()
  values = columns.map(get-val)
  [T.table-from-columns:
    {"labels"; columns},
    {"values"; values}
  ]
end



fun transpose(t :: Table) block:
  cols = t.column-names()
  row-names = cols.drop(1)
  first-col = cols.get(0)
  # use the old header row as the first column in the new table
  var new_t = T.table-from-column(first-col, row-names)

  split = split-at(1, t.all-columns())
  new-cols = split.prefix.get(0)
  old-rows = split.suffix

  # for each column in our new table, mine the old rows for their values
  map_n(lam(n, col) block:
      new_t := new_t.add-column(col, map_n(lam(m, v): old-rows.get(m).get(n) end, 0, row-names))
    end,
    0,
    new-cols)
  new_t
end

fun random-rows(t, n):
  doc: ```
       if n <<< t.length(), it would be good to sample the rows with row-n and
       build up a table from that rather than doing this process, which is linear
       in the universe size. if n is a proportional to t.length(),
       then this works pretty well.
       ```

  fun filter-n(tabl, pred):
    var i = 0
    tabl.filter(lam(x) block:
        result = pred(i, x)
        i := i + 1
        result
      end)
  end

  fun make-sample-selections(shadow n :: Number, u :: Number) -> {Boolean; SD.StringDict<Boolean>} block:
    doc: ```
         Key idea: build a dictionary that's either "positive" (keep these keys)
         or "negative" (drop these keys). Do this according to if we want most
         of the keys to stay (use a negative map) or most to go (use a positive
         map). There isn't a good, fast way to remove from a table by index right
         now, so this plus filter is best and actually reasonably performant.
         ```
    when n > u:
      raise(Err.message-exception("make-sample-selections: num-samples too large"))
    end
    fun help(num-samples-remaining-to-get, dict-so-far):
      if num-samples-remaining-to-get == 0: dict-so-far
      else:
        r = num-random(u)
        k = num-to-string(r)
        if dict-so-far.has-key(k):
          help(num-samples-remaining-to-get, dict-so-far)
        else:
          updated = dict-so-far.set(k, true)
          help(num-samples-remaining-to-get - 1, updated)
        end
      end
    end
    if n < (u / 2):
      {true; help(n, [SD.string-dict:])}
    else:
      {false; help(u - n, [SD.string-dict:])}
    end
  end

  {keep; to-change} = make-sample-selections(n, t.length())
  filter-n(t, lam(index, _):
      k = to-string(index)
      #| could also be written keep == to-change.has-key(k),
      but I find that difficult to grok |#
      if keep: to-change.has-key(k)
      else: not(to-change.has-key(k))
      end
    end)
end

fun make-random-table(num-rows) block:
  var t = table: a end
  for each(i from L.range(0, num-rows)):
    t := t.add-row(t.row(num-random(num-rows)))
  end
  t
end

fun row-to-list(r):
  r.get-column-names().map(r[_])
end



## INFERENCE ##########################################

pop-variance :: (t :: Table, col :: String) -> Number
fun pop-variance(t, col) block:
  check-integrity(t, [list: col])
  _mean = Stats.mean(ensure-numbers(t.column(col)))
  shadow t = t.build-column("sq-diff", lam(r): num-sqr(r[col] - _mean) end)
  Math.sum(ensure-numbers(t.column("sq-diff"))) / t.length()
end

sample-variance :: (t :: Table, col :: String) -> Number
# sample variance for ungrouped data
fun sample-variance(t, col) block:
  check-integrity(t, [list: col])
  _mean = Stats.mean(ensure-numbers(t.column(col)))
  shadow t = t.build-column("sq-diff", lam(r): num-sqr(r[col] - _mean) end)
  Math.sum(ensure-numbers(t.column("sq-diff"))) / (t.length() - 1)
end



paired-t :: (t :: Table, col1 :: String, col2 :: String) -> Number
fun paired-t(t, col1, col2) block:
  check-integrity(t, [list: col1, col2])
  shadow t = t.build-column("diff", lam(r): r[col2] - r[col1] end)
  mean1 = Stats.mean(ensure-numbers(t.column(col1)))
  mean2 = Stats.mean(ensure-numbers(t.column(col2)))
  sdiff = stdev(t, "diff")
  n = t.length()
  df = n - 1
  (mean1 - mean2) / (sdiff / num-sqr(n))
end

eq-variance-t :: (t :: Table, col1 :: String, col2 :: String) -> Number
fun eq-variance-t(t, col1, col2) block:
  check-integrity(t, [list: col1, col2])
  shadow t = t.build-column("diff", lam(r): r[col2] - r[col1] end)
  mean1 = Stats.mean(ensure-numbers(t.column(col1)))
  mean2 = Stats.mean(ensure-numbers(t.column(col2)))
  var1 = sample-variance(t, col1)
  var2 = sample-variance(t, col2)
  n1 = t.length()
  n2 = t.length()
  (mean1 - mean2) / num-sqrt((var1 / n1) + (var2 / n2))
end

uneq-variance-t :: (t :: Table, col1 :: String, col2 :: String) -> Number
fun uneq-variance-t(t, col1, col2) block:
  check-integrity(t, [list: col1, col2])
  shadow t = t.build-column("diff", lam(r): r[col2] - r[col1] end)
  mean1 = Stats.mean(ensure-numbers(t.column(col1)))
  mean2 = Stats.mean(ensure-numbers(t.column(col2)))
  var1 = sample-variance(t, col1)
  var2 = sample-variance(t, col2)
  n1 = t.length()
  n2 = t.length()
  df = n1 + n2 + -2
  dfs-and-sq-vars = (((n1 - 1) * num-sqr(var1)) + ((n2 - 1) * num-sqr(var2)))
  (mean1 - mean2) / ((dfs-and-sq-vars / df) * num-sqrt((1 / n1) + (1 / n2)))
end

fun make-noisy-table(fn, min, max, noise-level) block:
  samples = L.range-by(min, max, Math.max([list: (max - min) / 500]))
  defined-points = samples.foldr(lam(sample, points): 
      cases (Option) maybe-get-value(lam(): fn(sample) end):
        | some(y) => link({sample;y}, points)
        | none => points
      end
    end, empty)
  xs = defined-points.map(lam(t): t.{0} end)
  fn_ys = defined-points.map(lam(t): t.{1} end)
  noise = random-normal-distribution(xs.length() + 1)
  ys = map2(lam(x, y): x + ((noise-level * (y - 0.5))) end, fn_ys, noise)
  [T.table-from-columns: {"x"; xs}, {"y"; ys}]
end

fun make-noisy-scatter-chart(fn, min, max, noise-level) block:
  xs = L.range-by(min, max, Math.max([list: (max - min) / 500]))
  fn_ys = xs.map(fn)
  noise = random-normal-distribution(xs.length() + 1)
  ys = map2(lam(x, y): x + ((noise-level * (y - 0.5))) end, fn_ys, noise)
  render-chart(from-list.scatter-plot(xs, ys))
end

################# UTILITY FUNCTIONS ###########################

fun maybe-get-value(f :: Function):
  cases (Eth.Either) run-task({(): f()}):
    | left(v) => some(v)
    | right(v) => none
  end
end

# courtesy of Ben Lerner
# first, we define a data structure that we can run cases on...
data TaggedFunction:
  | one(f :: (Number -> Number))
  | two(f :: (Number, Number -> Posn))
  | three(f :: (Number, Number, String -> Posn))
end

# then, we use exceptions (!) to figure out which structure to return..
fun guess-arity(f :: Function) -> TaggedFunction:
  cases (Eth.Either) run-task({(): f(0) }):
    | left(v) => one(f)
    | right(v) =>
      err = exn-unwrap(v)
      if Err.is-arity-mismatch(err):
        cases (Eth.Either) run-task({(): f(0, 0)}):
          | left(shadow v) => two(f)
          | right(shadow v) =>
            shadow err = exn-unwrap(v)
            if Err.is-arity-mismatch(err):
              cases (Eth.Either) run-task({(): f(0, 0, 0)}):
                | left(shadow v) => three(f)
                | right(shadow v) =>
                  shadow err = exn-unwrap(v)
                  if Err.is-arity-mismatch(err):
                    raise("Unknown function arity")
                  else:
                    three(f)
                  end
              end
            else:
              two(f)
            end
        end
      else:
        one(f)
      end
  end
end


shadow translate = put-image
shadow dilate = scale


##################################################################################
# Image Helpers

# luminance :: Color -> Number
# computes the perceptual luminance of a pixel using the
# standard ITU-R BT.709 coefficients
fun luminance(c) -> Number:
  (0.2126 * c.red) + (0.7152 * c.green) + (0.0722 * c.blue)
end

# Convert an RGB color to a grayscale intensity (0-255)
# Uses the standard luminance-weighted formula
fun color-to-gray(c :: C.Color) -> Number:
  num-round(
    (0.299 * c.red) +
    (0.587 * c.green) +
    (0.114 * c.blue)
    )
end

# pixels-to-image :: (List<Color>, Number, Number) -> Image
# shared helper that converts a pixel list back into an image,
# centering the pinhole
fun pixels-to-image(pixels, width, height) -> Image:
  color-list-to-image(pixels, width, height, num-round(width / 2), num-round(height / 2))
end

# dominant-rgb-colors :: Image -> String
# Given an image, it to a string of space-separated color names 
# ("red", "green", or "blue"), representing the dominant channel 
# of each non-transparent pixel
fun dominant-rgb-colors(img :: Image) -> String block:
  fun dominant-color(pixel) -> String:
    if (pixel.red == pixel.green) and (pixel.red == pixel.blue): ""
    else if (pixel.red >= pixel.green) and (pixel.red >= pixel.blue): "red"
    else if (pixel.green >= pixel.red) and (pixel.green >= pixel.blue): "green"
    else: "blue"
    end
  end
  image-to-color-list(img)
    .filter(lam(pixel): pixel.alpha > 0 end)
    .map(dominant-color)
    .filter(lam(s): s <> "" end)
    .join-str(" ")
end

# invert :: Image -> Image
# inverts the RGB channels of each pixel, preserving alpha
fun invert(img :: Image) -> Image:
  width  = image-width(img)
  height = image-height(img)
  pixels-to-image(
    image-to-color-list(img).map(lam(p):
        make-color(255 - p.red, 255 - p.green, 255 - p.blue, p.alpha)
      end),
    width, height)
end

# grayscale :: (Image) -> Image
# produces an identical image in which all pixels
# have been converted to grayscale
fun grayscale(img :: Image) -> Image:
  pixels-to-image(
    image-to-color-list(img).map(lam(p) block:
        g = color-to-gray(p)
        make-color(g, g, g, p.alpha)
      end),
    image-width(img),
    image-height(img))
end

greyscale = grayscale

# Consumes an image and computes the average luminance
# round to 10 digits and use exact
fun image-luminance(img :: Image) -> Number:
  avg-luminance = Stats.mean(image-to-color-list(img).map(luminance))
  num-exact(num-round-to(avg-luminance, 10))
end

# Consumes an image and computes the entropy
# round to 10 digits and use exact
fun image-entropy(img :: Image) -> Number block:
  # Sum -p*log2(p) over all keys in the frequency table
  fun entropy-sum(keys :: List, freq :: SD.StringDict, total, acc) -> Number:
    cases (List) keys:
      | empty => acc
      | link(k, rest) =>
        p = freq.get-value(k) / total
        contribution = p * (num-log(p) / num-log(2))
        entropy-sum(rest, freq, total, acc - contribution)
    end
  end

  # Build a frequency table of grayscale values as a StringDict
  fun build-freq(pixels :: List, acc :: SD.StringDict) -> SD.StringDict:
    cases (List) pixels:
      | empty => acc
      | link(px, rest) =>
        key = num-to-string(color-to-gray(px))
        shadow count = if acc.has-key(key): acc.get-value(key) else: 0 end
        build-freq(rest, acc.set(key, count + 1))
    end
  end

  pixels = image-to-color-list(img)
  total = pixels.length()
  freq = build-freq(pixels, [SD.string-dict:])
  entropy = entropy-sum(freq.keys().to-list(), freq, total, 0)
  num-exact(num-round-to(entropy, 10))
end

# combine-images :: (Image, Image, (Color, Color -> Color)) -> Image
# 1) Norms the image sizes by overlaying them onto a transparent
# rectangle drawn from their max width and height
# 2) Merges two normed images pixel-by-pixel using the given combinator
fun combine-images(
    img1 :: Image, 
    img2 :: Image, 
    combinator :: (C.Color, C.Color -> C.Color)
    ) -> Image:
  width  = num-max(image-width(img1),  image-width(img2))
  height = num-max(image-height(img1), image-height(img2))
  bg = rectangle(width, height, "solid", "transparent")
  normed_img1 = overlay(center-pinhole(img1), bg)
  normed_img2 = overlay(center-pinhole(img2), bg) 
  combined-pixels = for map2(
      c1 from image-to-color-list(normed_img1), 
      c2 from image-to-color-list(normed_img2)):
    combinator(c1, c2)
  end
  pixels-to-image(combined-pixels, width, height)
end

# lighter :: (Image, Image) -> Image
# produces an image where each pixel is whichever of the two
# input pixels has higher luminance. transparent pixels pass through.
fun lighter(img1 :: Image, img2 :: Image) -> Image:
  combine-images(img1, img2, lam(c1, c2):
      if c1.alpha == 0: c2
      else if c2.alpha == 0: c1
      else if luminance(c1) >= luminance(c2): c1
      else: c2
      end
    end)
end

# darker :: (Image, Image) -> Image
# produces an image where each pixel is whichever of the two
# input pixels has lower luminance. transparent pixels pass through.
fun darker(img1 :: Image, img2 :: Image) -> Image:
  combine-images(img1, img2, lam(c1, c2):
      if c1.alpha == 0: c2
      else if c2.alpha == 0: c1
      else if luminance(c1) <= luminance(c2): c1
      else: c2
      end
    end)
end

# blend-images :: (Image, Image) -> Image
# averages the RGB and alpha channels of each pair of pixels.
# transparent pixels in either image show through to the other.
# handles images of different sizes by padding both to the same dimensions.
shadow blend-images = lam(img1 :: Image, img2 :: Image) -> Image:
  combine-images(img1, img2, lam(c1, c2):
      if c1.alpha == 0: c2
      else if c2.alpha == 0: c1
      else:
        make-color(
          num-round((c1.red   + c2.red)   / 2),
          num-round((c1.green + c2.green) / 2),
          num-round((c1.blue  + c2.blue)  / 2),
          num-round((c1.alpha + c2.alpha) / 2))
      end
    end)
end

# compute symmetry along the horizontal and vertical axes
# 1 = totally symmetric, 0 = not at all symmetric
fun image-symmetry-vertical(img :: Image) -> Number:
  num-exact(num-round-to(
      1 - ((~0 + images-difference(img, flip-horizontal(img)).v) / 255),
      10))
end
fun image-symmetry-horizontal(img :: Image) -> Number:
  num-exact(num-round-to(
      1 - ((~0 + images-difference(img, flip-vertical(img)).v) / 255),
      10))
end

# Counts pixels in `img` where the given channel ("red", "green", or "blue")
# is strictly the largest of the three channel values
fun count-dominant-pixels(img :: Image, channel :: String) -> Number:
  # Returns true if `a` is strictly greater than both `b` and `c`
  fun is-dominant(a :: Number, b :: Number, c :: Number) -> Boolean:
    (a > b) and (a > c)
  end

  color-list = I.image-to-color-list(img)
  for fold(acc from 0, c from color-list):
    r = c.red
    g = c.green
    b = c.blue
    dominant = ask:
      | channel == "red"   then: is-dominant(r, g, b)
      | channel == "green" then: is-dominant(g, r, b)
      | channel == "blue"  then: is-dominant(b, r, g)
      | otherwise: false
    end
    if dominant: acc + 1 else: acc end
  end
end

fun image-red-pixels(img :: Image):   count-dominant-pixels(img, "red")   end
fun image-green-pixels(img :: Image): count-dominant-pixels(img, "green") end
fun image-blue-pixels(img :: Image):  count-dominant-pixels(img, "blud")  end


################################################################
###################### TEXT TOOLS ##################


num-words :: String -> Number
fun num-words(txt):
  string-split-all(txt, " ").filter({(w): w <> ""}).length()
end


num-sentences :: String -> Number
fun num-sentences(txt): string-split-all(txt, ". ").length() end

# count-syllables :: String -> Number
# consumes text, breaks it into words, then produces the
# sum of their syllables
# crude algorithm from https://stackoverflow.com/questions/49754440/count-syllables-function-scheme
fun num-syllables(txt):
  vowels = [list: "a", "e", "i", "o", "u"]
  fun syllables(lst):
    ask:
      | L.is-empty(lst) then: 0
      | vowels.member(lst.get(0)) then:
        1 + skip-vowels(lst.rest)
      | otherwise: syllables(lst.rest)
    end
  end

  fun skip-vowels(lst):
    ask:
      | L.is-empty(lst) then: syllables([list:])
      | vowels.member(lst.get(0)) then: skip-vowels(lst.rest)
      | otherwise: syllables(lst)
    end
  end
  words = string-split(txt, " ")
  words.foldl(lam(w, shadow count):
    count + syllables(string-explode(w)) end,
    0)
end

fun sort-strings-ci(lst :: List<String>) -> List<String>:
  lst.sort-by(
    {(a, b): string-to-lower(a) < string-to-lower(b)}, 
    {(a, b): string-to-lower(a) == string-to-lower(b)})
end

################################################################
###################### INEQUALITIES SIMULATOR ##################

fun draw-inequality(points, f, msg-img) block:

  # some constants for drawing, intervals, etc
  STRIP-WIDTH = 400
  STRIP-HEIGHT = 30
  tri = rotate(90, triangle(10, "solid", "black"))
  num-intervals = 10
  c1 = make-color(100, 100, 255, 0.5)
  strip = rectangle(STRIP-WIDTH + 48, STRIP-HEIGHT, "outline", "transparent")
  inequalities-bg = overlay(rectangle(STRIP-WIDTH, 2, "solid", "black"), strip)
  axis = translate(flip-horizontal(tri),
    STRIP-WIDTH + 38, STRIP-HEIGHT / 2,
    translate(tri, 10, STRIP-HEIGHT / 2, inequalities-bg))

  fun num-to-mark(n):
    if num-is-integer(n): num-to-string(n)
    else: num-to-string-digits(n, 2) end
  end

  # given a textcolor, a fn to draw the point, and a projection fn,
  # produce a function that consumes a point and an image, then draws
  # a dot of that color
  fun dot-builder(draw-point, project, shadow points, base):
    points.foldl(
      lam(p, img):
        translate(draw-point(p), project(p) + 24, STRIP-HEIGHT / 2, img)
      end,
      base)
  end

  # build a strip of labeled points
  fun label-builder(project, shadow points, alignment, c):
    points.foldl(
      lam(p, img):
        translate(
          overlay-align("center", alignment,
            rotate(90, text(num-to-mark(p), 10, c)),
            square(30, "solid", "transparent")),
          project(p) + 24, STRIP-HEIGHT / 2,
          img)
      end,
      strip)
  end

  # given an inequality fn and a reverse-projection fn, test every pixel
  # on a 600px range and mark "true" pixels as transparent blue
  fun draw-shade(rProject, c):
    range-by(0, STRIP-WIDTH, 2).foldl(lam(p, img):
        shadow color = if f(rProject(p)): c else: "transparent" end
        beside(img, rectangle(2, 15, "solid", color))
      end,
      rectangle(0,0,"solid","transparent"))
  end

  # find the start and stop of the range
  start = Math.min(points)
  stop = Math.max(points)
  # given one of the points on the range, project to pixels on the axis
  fun project(p): (p - start) * (STRIP-WIDTH / (stop - start)) end
  # given a pixel on the axis, reverse-project back to a point on the range
  fun rProject(p): (p / (STRIP-WIDTH / (stop - start))) + start end
  # make a list of all the interval coordinates
  intervals = range-by(start, stop, (stop - start) / num-intervals)
  # make a strip containing all those intervals
  intervals-strip = label-builder(project, intervals, "top", "darkgray")
  # make a strip containing all the points
  points-strip = label-builder(project, points, "bottom", "black")
  # starting with the blank axis, add the shade...
  axis-strip = translate(draw-shade(rProject, c1),
    (STRIP-WIDTH + 50) / 2, STRIP-HEIGHT / 2, axis)
  # add the interval dots
  black-dots-strip = dot-builder(lam(p): circle(2, "solid", "black") end, project, intervals, axis-strip)
  # add the points
  dots-strip = dot-builder(lam(p): translate(
        text(if f(p): "T" else: "F" end, 10, "black"),
        10, 8,
      circle(10, "solid", if f(p): "green" else: "red" end)) end,
    project, points, black-dots-strip)
  when (points.length() <> 8):
    raise("the list must contain exactly 8 points")
  end
  above-list([list: strip, points-strip, dots-strip, intervals-strip, msg-img])
end

# given a list of numbers and an inequality function,
# check to see if the list contains 8 elements, then graph
# the inequality and the points on a numberline
# report back which points passed
fun inequality(f, points):
  passed = points.foldl(lam(p, shadow count): (if f(p): count + 1 else: count end) end, 0)
  msg-img = if (passed == 4):
    text(" ", 20, "green")
  else: text("Challenge yourself: Find 4 true examples and 4 false", 20, "red")
  end
  draw-inequality(points, f, msg-img)
end

fun or-union(f1, f2, points):
  msg-img = text("All regions shaded blue are part of the solution", 20, "blue")
  overlay-align(
    "center", "top",
    text("UNION", 20, "black"),
    draw-inequality(points, lam(x): f1(x) or f2(x) end, msg-img))
end

fun and-intersection(f1, f2, points):
  start = Math.min(points)
  stop = Math.max(points)
  f = lam(x): f1(x) and f2(x) end
  has-intersection = L.any(f, range-by(0, 600, 1))
  msg-img = if has-intersection:
    text("All regions shaded blue are part of the solution", 20, "blue")
  else: text("No solution exists within this range!", 20, "blue")
  end
  overlay-align(
    "center", "top",
    text("INTERSECTION", 20, "black"),
    draw-inequality(points, lam(x): f1(x) and f2(x) end, msg-img))
end



################################################################
############### PERMUTATIONS AND COMBINATIONS ##################
factorial :: (n :: NumNonNegative) -> Number
fun factorial(n):
  if (n <= 1): 1
  else: n * factorial(n - 1)
  end
end

permute-wo-replace :: <A>(items :: L.List<A>, choose :: Number) -> L.List<List<A>>
fun permute-wo-replace(items, choose):
  if items.length() == 0: [list:]
  else if choose == 0: [list:]  # nothing to choose -> empty list
  else if choose == 1: items.map(lam(e): [list: e] end)
  else:
    items.foldl(lam(e, acc):
        rest = permute-wo-replace(items.remove(e), choose - 1).map(lam(p):
          link(e, p) end)
        acc.append(rest)
      end,
      [list:])
  end
end


permute-w-replace :: <A>(items :: L.List<A>, choose :: Number) -> L.List<List<A>>
fun permute-w-replace(items, choose):
  if items.length() == 0: [list:]
  else if choose == 0: [list:]
  else if choose == 1: items.map(lam(e): [list: e] end)
  else:
    items.foldl(lam(e, acc):
        rest = permute-w-replace(items, choose - 1).map(lam(p):
          link(e, p) end)
        acc.append(rest)
      end,
      [list:])
  end
end

combine-wo-replace :: <A>(items :: L.List<A>, choose :: Number) -> L.List<List<A>>
# from https://rosettacode.org/wiki/Combinations#Pyret
fun combine-wo-replace(items, choose):
  if items.length() < choose:
    raise(Err.message-exception("The list must be at least as long as the number of choices"))
  else if items.length() == choose: [list: items]
  else if choose == 1: items.map(lam(e): [list: e] end)
  else:
    # The main resursive step here is to consider
    # all the combinations of the list that have the
    # first element (aka head) and then those that don't
    # don't.
    cases(List) items:
      | empty => [list:]
      | link(first, rest) =>
        # All the subsets of our list either include the
        # first element of the list or they don't.
        with-first = combine-wo-replace(rest, choose - 1).map(
          lam(c): link(first, c) end)
        without-first = combine-wo-replace(rest, choose)
        with-first.append(without-first)
    end
  end
end

render-list :: (lst :: L.List) -> Image
fun render-list(lst):
  lst-imgs = lst.map(lam(l):
      elt-imgs = l.map(lam(e):
          if is-image(e): e
          else if is-string(to-repr(e)): text(e, 12, "black")
          else: to-repr(e)
          end
        end)
      beside-list(elt-imgs)
    end)
  unspoken-img(above-list(lst-imgs))
end

fun whats-missing(answer, test):
  answer-set = Sets.list-to-set(answer)
  test-set = Sets.list-to-set(test)
  diff = answer-set.difference(test-set)
  if diff.size() > 0: diff.to-list()
  else: "You got them all!"
  end
end

fun unspoken-img(img): color-list-to-image(image-to-color-list(img), image-width(img), image-height(img), image-pinhole-x(img), image-pinhole-y(img)) end


#########################################################
# Image transformation functions

# print-imgs :: (img-lst :: L.List<Image>) -> Image
# maximize height and add padding to each image, then
# sort them in order of decreasing heights
# stick them together until we hit 500px,
# then start a new row
fun print-imgs(img-lst) block:

  # how much padding to put around each image (in px)
  padding = 10

  fun img-compare(img1, img2):
    (image-height(img1) >= image-height(img2)) and
    (image-width(img1) > image-width(img2))
  end
  fun img-eq(img1, img2):
    image-height(img1) == image-height(img2)
  end

  # if an image is wider than it is tall, rotate it 90°
  fun maximize-height(img):
    if (image-height(img) > image-width(img)): img
    else: rotate(90, img)
    end
  end
  # Add an extra padding on either side an image
  fun add-padding(img):
    w = image-width(img) + padding
    h = image-height(img) + padding
    overlay(img, rectangle(w, h, "solid", "transparent"))
  end
  fun add-dimensions(img):
    w = image-width(img)
    h = image-height(img)
    wS = num-to-string(w)
    hS = num-to-string(h)
    txt = text([list:wS, "x", hS].join-str(""), 100, "black")
    scaled-txt = if (image-width(img) > image-height(img)):
      scale(0.5, scale(w / image-width(txt), txt))
    else:
      rotate(90, scale(0.5, scale(w / image-height(txt), txt)))
    end
    translate(
      scaled-txt,
      (w / 2),
      (h / 2),
      img)
  end


  # if it's the first image, just add it
  # otherwise grab the last row, and see if we can add the image
  # if not, start a new row
  fun processor(img, acc):
    if (acc.length() == 0): acc.push(img)
    else:
      var split = L.split-at(1,acc)
      var lastRow = split.prefix.last()
      if ((image-width(lastRow) + image-width(img)) > 500):
        acc.push(img)
      else:
        split.suffix.push(beside(lastRow,img))
      end
    end
  end

  img-lst
    .map(maximize-height)
    .sort-by(img-compare, img-eq)
    .map(add-dimensions)
    .map(add-padding)
    .foldl(processor, [list:])
    .foldl(lam(img, acc): below(img, acc) end,
    square(padding,"solid","transparent"))
end
