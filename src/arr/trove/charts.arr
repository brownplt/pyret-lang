provide:
  render-chart,
  render-charts,
  from-list,
  type DataSeries,
  type ChartWindow,
  data StackType,
  data TrendlineType,
  data PointShape
end

import global as G
import base as B
include lists
include option
import image-structs as I
import internal-image-untyped as IM
import sets as S
import charts-lib as CL
import either as E
import string-dict as SD
import valueskeleton as VS
import statistics as ST
import color as C
import render-error-display as RED

################################################################################
# CONSTANTS
################################################################################

SHOW-LENGTH = 3
FUNCTION-POINT-SIZE = 0.1
DEFAULT-RANGE = {-10; 10}

################################################################################
# DATA + TYPE SYNONYMS
################################################################################

type PlottableFunction = (Number -> Number)
type Posn = RawArray<Number>
type TableIntern = RawArray<RawArray<Any>>
data Pointer: 
  | pointer(label :: String, value :: Number)
end
data SciNumber: 
  | sci-notation(coeff :: Number, exponent :: Number, base :: Number)
end
data AxisData: 
  | axis-data(axisTop :: Number, axisBottom :: Number, ticks :: RawArray<Pointer>)
end
data StackType:
  | absolute
  | relative
  | percent
  | grouped
end

fun check-positive-degree(v :: Number) -> Boolean block: 
  when v < 0: 
    raise("degree: degree must be non-negative")
  end
  true
end

data TrendlineType:
  | no-trendline
  | linear
  | exponential
  | polynomial(degree :: NumInteger%(check-positive-degree))
end

fun check-positive-sides(v :: Number) -> Boolean block: 
  when v < 0: 
    raise("regular-polygon-shape: number of sides must be non-negative")
  end
  true
end

fun check-valid-dent(v :: Number) -> Boolean block: 
  when (v < 0) or (v > 1): 
    raise("regular-polygon-shape: dent must be between 0 and 1")
  end
  true
end

data PointShape: 
  | circle-shape
  | regular-polygon-shape(sides :: NumInteger%(check-positive-sides), dent :: Number%(check-valid-dent))
end

################################################################################
# HELPERS
################################################################################

fun check-num(v :: Number) -> Nothing: nothing end
fun check-string(v :: String) -> Nothing: nothing end
fun check-image(v :: IM.Image) -> Nothing: nothing end

fst = raw-array-get(_, 0)
snd = raw-array-get(_, 1)
thd = raw-array-get(_, 2)
posn = {(x :: Number, y :: Number): [raw-array: x, y]}

sprintf = (lam():
    generic-sprintf = lam(arr :: RawArray<Any>):
      raw-array-fold(lam(str, elt, _): str + tostring(elt) end, '', arr, 0)
    end
    {
      make5: {(a, b, c, d, e): generic-sprintf([raw-array: a, b, c, d, e])},
      make4: {(a, b, c, d): generic-sprintf([raw-array: a, b, c, d])},
      make3: {(a, b, c): generic-sprintf([raw-array: a, b, c])},
      make2: {(a, b): generic-sprintf([raw-array: a, b])},
      make1: tostring,
      make0: {(): ''},
      make: generic-sprintf
    }
  end)()

unsafe-equal = {(x :: Number, y :: Number): (x <= y) and (y <= x)}

fun to-table2(xs :: List<Any>, ys :: List<Any>) -> TableIntern:
  map2(raw-array.make2, xs, ys) ^ builtins.raw-array-from-list
end

fun to-table2-n(xs :: List<Any>, ys :: List<Any>) -> TableIntern:
  map2_n({(n, x, y): raw-array.make3(x, y, n)}, 0, xs, ys) ^ builtins.raw-array-from-list
end

fun to-table3(xs :: List<Any>, ys :: List<Any>, zs :: List<Any>) -> TableIntern:
  map3(raw-array.make3, xs, ys, zs) ^ builtins.raw-array-from-list
end

fun to-table3-n(xs :: List<Any>, ys :: List<Any>, zs :: List<Any>) -> TableIntern:
  map3_n({(n, x, y, z): raw-array.make4(x, y, z, n)}, 0, xs, ys, zs) ^ builtins.raw-array-from-list
end

fun to-table4(xs :: List<Any>, ys :: List<Any>, zs :: List<Any>, ks :: List<Any>) -> TableIntern:
  map4(raw-array.make4, xs, ys, zs, ks) ^ builtins.raw-array-from-list
end

fun list-to-table2<A>(table :: List<List<A>>) -> RawArray<RawArray<A>>:
  builtins.raw-array-from-list(table.map(builtins.raw-array-from-list))
end

fun table2-to-list<A>(table :: RawArray<RawArray<A>>) -> List<List<A>>:
  raw-array-to-list(table).map(raw-array-to-list)
end

fun get-vs-from-img(s :: String, raw-img :: IM.Image) -> VS.ValueSkeleton:
  I.color(190, 190, 190, 0.75)
    ^ IM.text-font(s, 72, _, "", "modern", "normal", "bold", false)
    ^ IM.overlay-align("center", "bottom", _, raw-img)
    ^ VS.vs-value
end

fun table-sorter<A,B>(
    t :: TableIntern, 
    value-getter :: (RawArray -> A), 
    scorer :: (A -> B), 
    cmp :: (B, B -> Boolean), 
    eq :: (B, B -> Boolean)): 
  doc: ```
       General Data Table Sorting Function:
       Value-getter grabs the Column of the Data table you want to use to sort
       Scorer Modifies the values in that Column to what you want to sort-by
       ```
  list-of-rows = t ^ raw-array-to-list

  scored-values = 
    map(
      {(row): {row; row ^ value-getter ^ scorer}}, 
      list-of-rows)

  sorted-by-score = 
    scored-values.sort-by(
      {(row-score, oth-row-score): cmp(row-score.{1}, oth-row-score.{1})}, 
      {(row-score, oth-row-score): eq(row-score.{1}, oth-row-score.{1})})

  sorted-rows = map({(row-score): row-score.{0}}, sorted-by-score)
  sorted-rows ^ builtins.raw-array-from-list
end

fun num-to-scientific(base :: Number) -> (Number -> SciNumber) block: 
  doc: ```
       Produces a function that takes a number and turns it into it's scientific representation. 
       Calculates the resulting Coeff, Exponent where number = coeff * base ^ Exponent.
       Currently only works with bases > 1.
       ```
  when base <= 1: 
    raise("Num-to-scientific: Only defined on bases > 1")
  end
  
  fun convert(n :: Number):
    if num-is-rational(n) and (n == 0): sci-notation(0, 0, base)
    else:
      pow = num-floor(num-log(num-abs(n)) / num-log(base))
      c = n / num-expt(base, pow)
      sci-notation(c, pow, base)
    end
  end
  convert
#|
where: 
  num-to-scientific(10)(0) is sci-notation(0, 0, 10)
  num-to-scientific(10)(3.214) is sci-notation(3.214, 0, 10)
  num-to-scientific(10)(513) is sci-notation(5.13, 2, 10)
  num-to-scientific(10)(-23) is sci-notation(-2.3, 1, 10)
  num-to-scientific(10)(0.00123) is sci-notation(1.23, -3, 10)
  num-to-scientific(10)(-0.0231) is sci-notation(-2.31, -2, 10)
  num-to-scientific(2)(256) is sci-notation(1, 8, 2)
  num-to-scientific(1) raises "Only defined on bases > 1"
  num-to-scientific(0.32) raises "Only defined on bases > 1"
  num-to-scientific(0) raises "Only defined on bases > 1"
  num-to-scientific(-50) raises "Only defined on bases > 1"
|#
end

fun string-to-stacktype(s :: String) -> StackType: 
  doc: ```Converts ['none', 'absolute', relative', percent'] to their respective stacktype```
  ask: 
    | string-equal(s, 'none') then: grouped
    | string-equal(s, 'absolute') then: absolute
    | string-equal(s, 'percent') then: percent
    | string-equal(s, 'relative') then: relative
    | otherwise: raise('type must be absolute, relative, percent, or none')
  end
end

fun prep-axis(values :: CL.LoN) -> {Number; Number}: 
  doc: ``` Calculate the max axis (top) and min axis (bottom) values for bar-chart-series```

  max-positive-height = fold(num-max, 0, values)
  max-negative-height = fold(num-min, 0, values)

  {max-positive-height; max-negative-height}
end

fun multi-prep-axis(stack-type :: StackType, value-lists :: CL.LoLoN) 
  -> {Number; Number}: 
  doc: ``` 
       Calculate the max axis (top) and min axis (bottom) values for multi-bar-chart-series
       ```

  ask:
    | stack-type == grouped then: 
      # Find the tallest bar in the entire group 
      # We know that the value lists have at least one value since we check for that when initializing the value list data. 
      positive-max-groups = map({(l): fold(num-max, l.first, l)}, value-lists)
      negative-max-groups = map({(l): fold(num-min, l.first, l)}, value-lists)
      max-positive-height = fold(num-max, 0, positive-max-groups)
      max-negative-height = fold(num-min, 0, negative-max-groups)
      {max-positive-height; max-negative-height}

    | stack-type == absolute then: 
      # Find height of stack using sum functions
      sum = {(l :: List<Number>): fold({(acc, elm): acc + elm}, 0, l)}
      positive-only-sum = {(l :: List<Number>): sum(filter({(e): e >= 0}, l))}
      negative-only-sum = {(l :: List<Number>): sum(filter({(e): e <= 0}, l))}
      positive-sums = map(positive-only-sum, value-lists)
      negative-sums = map(negative-only-sum, value-lists)
      max-positive-height = fold(num-max, 0, positive-sums)
      max-negative-height = fold(num-min, 0, negative-sums)
      {max-positive-height; max-negative-height}

    | otherwise: 
      has-pos = any({(l): any({(e): e > 0}, l)}, value-lists)
      has-neg = any({(l): any({(e): e < 0}, l)}, value-lists)
      ask: 
        | has-pos and has-neg then: {1; -1}
        | has-pos then: {1; 0}
        | has-neg then: {0; -1}
        | otherwise: {1; -1}
      end
  end
end


type BoxData = {
  label :: String,
  max-val :: Number,
  min-val :: Number,
  first-quartile :: Number,
  median :: Number,
  third-quartile :: Number,
  high-whisker :: Number,
  low-whisker :: Number,
  high-outliers :: RawArray<Number>,
  low-outliers :: RawArray<Number>
}

fun get-box-data(label :: String, lst :: List<Number>) -> BoxData:
    n = lst.length()
    shadow lst = lst.sort()
    median = ST.median(lst)
    {first-quartile; third-quartile} = if num-modulo(n, 2) == 0:
      splitted = lst.split-at(n / 2)
      {ST.median(splitted.prefix); ST.median(splitted.suffix)}
    else:
      splitted = lst.split-at((n - 1) / 2)
      {ST.median(splitted.prefix); ST.median(splitted.suffix.rest)}
    end
    iqr = third-quartile - first-quartile
    high-outliers = for filter(shadow n from lst):
      n > (third-quartile + (1.5 * iqr))
    end
    low-outliers = for filter(shadow n from lst):
      n < (first-quartile - (1.5 * iqr))
    end
    min-val = lst.first
    max-val = lst.last()
    low-whisker = lst.get(low-outliers.length())
    high-whisker = lst.get(n - high-outliers.length() - 1)
    {
     label: label,
      max-val: max-val,
      min-val: min-val,
      first-quartile: first-quartile,
      median: median,
      third-quartile: third-quartile,
      high-whisker: high-whisker,
      low-whisker: low-whisker,
      high-outliers: builtins.raw-array-from-list(high-outliers),
      low-outliers: builtins.raw-array-from-list(low-outliers)
    }
end

################################################################################
# METHODS
################################################################################

color-method = method(self, color :: I.Color):
  self.constr()(self.obj.{color: some(color)})
end

color-list-method = method(self, colors :: CL.LoC):
  cases (List) colors: 
    | empty => self.constr()(self.obj.{colors: none})
    | link(_, _) => 
      block:
        each({(c :: I.Color): c}, colors)
        self.constr()(self.obj.{colors: some(colors ^ builtins.raw-array-from-list)})
      end
  end
end

pointer-color-method = method(self, color :: I.Color):
  self.constr()(self.obj.{pointer-color: some(color)})
end

interval-color-method = method(self, color :: I.Color):
  self.constr()(self.obj.{default-interval-color: some(color)})
end

line-width-method = method(self, lineWidth :: Number) block:
  when lineWidth < 0: 
    raise("line-width: Line Width must be non-negative")
  end
  self.constr()(self.obj.{lineWidth: lineWidth})
end

style-method = method(self, style :: String) block:
  when not(string-equal(style, "sticks")) and not(string-equal(style, "bars")) and not(string-equal(style, "boxes")):
    raise("style: must be either sticks, bars, or boxes")
  end
  self.constr()(self.obj.{style: style})
end

curve-method = method(self, curved :: Boolean):
  if curved: self.constr()(self.obj.{curved: "function"})
  else: self.constr()(self.obj.{curved: "none"})
  end
end

labels-method = method(self, labels :: CL.LoS): 
  block:
    when self.obj!ps.length() <> labels.length():
      raise('plot: xs and labels should have the same length')
    end
    self.constr()(self.obj.{ps: map2({(arr, label): raw-array-set(arr, 2, label)}, self.obj!ps, labels)})
  end
end

image-labels-method = method(self, images :: CL.LoI):
  block:
    when self.obj!ps.length() <> images.length():
      raise('plot: xs and images should have the same length')
    end
    self.constr()(self.obj.{ps: map2({(arr, image): raw-array-set(arr, 3, image)}, self.obj!ps, images)})
  end
end

explode-method = method(self, offsets :: CL.LoN) block:
  when raw-array-length(self.obj!tab) <> offsets.length():
    raise('exploding-pie-chart: labels and offsets should have the same length')
  end
  for each(offset from offsets):
    when (offset < 0) or (offset > 1):
      raise('exploding-pie-chart: offset must be between 0 and 1')
    end
  end
  self.constr()(self.obj.{tab: raw-array-from-list(map2({(arr, offset): raw-array-set(arr, 2, offset)}, raw-array-to-list(self.obj!tab), offsets))})
end

histogram-label-method = method(self, labels :: CL.LoS) block:
    when raw-array-length(self.obj!tab) <> labels.length():
      raise('histogram: xs and labels should have the same length')
    end
  self.constr()(self.obj.{tab: raw-array-from-list(map2({(arr, label): raw-array-set(arr, 0, label)}, raw-array-to-list(self.obj!tab), labels))})
end

box-labels-method = method(self, labels :: CL.LoS) block:
  when labels.length() <> self.obj!values.length():
    raise('labeled-box-plot: labels and values should have the same length')
  end
  when labels.length() == 0:
    raise('labeled-box-plot: expect at least one box')
  end
  self.constr()(self.obj.{tab: map2(get-box-data, labels, self.obj!values) ^ builtins.raw-array-from-list,})
end

threeD-method = method(self, threeD :: Boolean):
  self.constr()(self.obj.{threeD: threeD})
end

piehole-method = method(self, piehole :: Number):
  if (piehole < 0) or (piehole > 1): raise("piehole: Value must be between 0 and 1")
  else: self.constr()(self.obj.{piehole: piehole})
  end
end

starting-angle-method = method(self, startingAngle :: Number):
  self.constr()(self.obj.{startingAngle: startingAngle})
end

collapse-threshold-method = method(self, collapseThreshold :: Number) block:
  when (collapseThreshold < 0) or (collapseThreshold > 1): 
    raise("collapse-threshold: Threshold must be between 0 and 1")
  end
  self.constr()(self.obj.{collapseThreshold: collapseThreshold})
end

trendline-type-method = method(self, trendlineType :: TrendlineType):
cases (TrendlineType) trendlineType: 
    | no-trendline => self.constr()(self.obj.{trendlineType: none})
    | linear => self.constr()(self.obj.{trendlineType: some("poly"), trendlineDegree: 1})
    | exponential => self.constr()(self.obj.{trendlineType: some("exp")})
    | polynomial(degree) => self.constr()(self.obj.{trendlineType: some("poly"), trendlineDegree: degree})
  end
  
end

trendline-color-method = method(self, color :: I.Color):
  self.constr()(self.obj.{trendlineColor: some(color)})
end

trendline-width-method = method(self, lineWidth :: Number) block:
  when lineWidth < 0: 
    raise("trendline-width: Trendline Width must be non-negative")
  end
  self.constr()(self.obj.{trendlineWidth: lineWidth})
end

trendline-opacity-method = method(self, opacity :: Number):
  if (opacity < 0) or (opacity > 1): raise("Trendline opacity: Value must be between 0 and 1")
  else: self.constr()(self.obj.{trendlineOpacity: opacity})
  end
end

dashed-line-method = method(self, dashed :: Boolean):
  self.constr()(self.obj.{dashedLine: dashed})
end

dashed-line-style-method = method(self, dashed-line-style :: CL.LoNi) block:
  when any({(n): n < 0}, dashed-line-style):
    raise("Dashed Line Style: Values must be non-negative")
  end
  self.constr()(self.obj.{dashedLine: true, dashlineStyle: raw-array-from-list(dashed-line-style)})
end

pointshape-method = method(self, pointshape :: PointShape):
  cases (PointShape) pointshape: 
    | circle-shape => self.constr()(self.obj.{pointshapeType: "circle"})
    | regular-polygon-shape(sides, dent) => self.constr()(self.obj.{pointshapeType: "polygon", pointshapeSides: sides, pointshapeDent: dent})
  end
end

select-multiple-method = method(self, multiple :: Boolean):
  self.constr()(self.obj.{multiple: multiple})
end

background-color-method = method(self, color :: I.Color):
  self.constr()(self.obj.{backgroundColor: some(color)})
end

background-border-method = method(self, border-size :: Number) block:
  when border-size < 0: 
    raise("border-size: Border Size must be non-negative")
  end
  self.constr()(self.obj.{borderSize: border-size})
end

border-color-method = method(self, border-color :: I.Color):
  self.constr()(self.obj.{borderColor: some(border-color)})
end

legend-method = method(self, legend :: String):
  self.constr()(self.obj.{legend: legend})
end

show-minor-grid-lines-method = method(self, is-showing :: Boolean):
  self.constr()(self.obj.{show-minor-grid-lines: is-showing})
end

gridlines-color-method = method(self, color ::  I.Color):
  self.constr()(self.obj.{gridlineColor: some(color)})
end

minor-gridlines-color-method = method(self, color ::  I.Color):
  self.constr()(self.obj.{show-minor-grid-lines: true, minorGridlineColor: some(color)})
end

gridlines-min-spacing-method = method(self, minspacing :: Number) block:
  when minspacing < 0: 
    raise("gridlines-minspacing: Min spacing must be non-negative")
  end
  self.constr()(self.obj.{gridlineMinspacing: some(minspacing)})
end

minor-gridlines-min-spacing-method = method(self, minspacing :: Number) block:
  when minspacing < 0: 
    raise("minor-gridlines-minspacing: Min spacing must be non-negative")
  end
  self.constr()(self.obj.{show-minor-grid-lines: true, minorGridlineMinspacing: minspacing})
end

x-axis-method = method(self, x-axis :: String):
  self.constr()(self.obj.{x-axis: x-axis})
end

y-axis-method = method(self, y-axis :: String):
  self.constr()(self.obj.{y-axis: y-axis})
end

x-min-method = method(self, x-min :: Number):
  self.constr()(self.obj.{x-min: some(x-min)})
end

x-max-method = method(self, x-max :: Number):
  self.constr()(self.obj.{x-max: some(x-max)})
end

y-min-method = method(self, y-min :: Number):
  self.constr()(self.obj.{y-min: some(y-min)})
end

y-max-method = method(self, y-max :: Number):
  self.constr()(self.obj.{y-max: some(y-max)})
end

sort-method = method(self, 
    cmp :: (Number, Number -> Boolean), 
    eq :: (Number, Number -> Boolean)): 

  fun get-value(row :: RawArray) -> Number: 
    doc:```
        VALUE GETTER: Gets the values from the row of data in Number form
        ASSUMES the row of data is ordered by [LABEL, VALUES, OTHER]
        ```
    raw-array-get(row, 1)
  end
  
  identity = {(x): x}
  sorted-table = table-sorter(self.obj!tab, get-value, identity, cmp, eq)
  self.constr()(self.obj.{tab: sorted-table})
end

default-sort-method = method(self): 
  self.sort-by({(a, b): a < b}, {(a, b): a == b})
end

label-sort-method = method(self, 
    cmp :: (String, String -> Boolean), 
    eq :: (String, String -> Boolean)): 
  
  fun get-label(row :: RawArray) -> String: 
    doc:```
        VALUE GETTER: Gets the values from the row of data in Number form
        ASSUMES the row of data is ordered by [LABEL, VALUES, OTHER]
        ```
    raw-array-get(row, 0)
  end
  
  identity = {(x): x}
  sorted-table = table-sorter(self.obj!tab, get-label, identity, cmp, eq)
  self.constr()(self.obj.{tab: sorted-table})
end

multi-sort-method = method(self, 
    scorer :: (List<Number> -> Number), 
    cmp :: (Number, Number -> Boolean), 
    eq :: (Number, Number -> Boolean)): 

  fun get-values(row :: RawArray) -> List<Number>: 
    doc:```
        VALUE GETTER: Gets the values from the row of data in List form
        ASSUMES the row of data is ordered by [LABEL, VALUES, OTHER]
        ```
    raw-array-get(row, 1) ^ raw-array-to-list
  end
  
  sorted-table = table-sorter(self.obj!tab, get-values, scorer, cmp, eq)
  self.constr()(self.obj.{tab: sorted-table})
end

default-multi-sort-method = method(self, 
    cmp :: (Number, Number -> Boolean), 
    eq :: (Number, Number -> Boolean)): 

  fun get-values(row :: RawArray) -> List<Number>: 
    doc:```
        VALUE GETTER: Gets the values from the row of data in List form
        ASSUMES the row of data is ordered by [LABEL, VALUES, OTHER]
        ```
    raw-array-get(row, 1) ^ raw-array-to-list
  end
  
  sum = {(l :: List<Number>): fold({(acc, elm): acc + elm}, 0, l)}
  sorted-table = table-sorter(self.obj!tab, get-values, sum, cmp, eq)
  self.constr()(self.obj.{tab: sorted-table})
end

super-default-multi-sort-method = method(self): 
  self.sort-by({(a, b): a < b}, {(a, b): a == b})
end

axis-pointer-method = method(self,
    tickValues :: CL.LoN, 
    tickLabels :: CL.LoS) block: 

  # Lengths of Lists
  TVLen = tickValues.length() 
  TLLen = tickLabels.length()
  distinctTVLen = distinct(tickValues).length()

  # Edge Case Error Checking
  when not(distinctTVLen == TVLen): 
    raise('add-pointers: pointers cannot overlap')
  end
  when not(TVLen == TLLen): 
    raise('add-pointers: pointers values and names should have the same length')
  end

  ticks = fold2({(acc, e1, e2): link(pointer(e1, e2), acc)}, empty, tickLabels, tickValues)
  self.constr()(self.obj.{pointers: some(distinct(ticks) ^ builtins.raw-array-from-list)})
end

make-axis-data-method = method(self,  pos-bar-height :: Number, neg-bar-height :: Number):
  step-types = [list: 0, 0.2, 0.25, 0.5, 1, 2]

  # Turn the numbers into Scientific Numbers
  scientific-b10 = num-to-scientific(10)
  pos-sci = scientific-b10(pos-bar-height)
  neg-sci = scientific-b10(neg-bar-height)

  # Calculate the step distance between gridlines
  pos-step = step-types.filter({(n): n >= num-abs(pos-sci.coeff / 9)}).get(0) * num-expt(10, pos-sci.exponent)
  neg-step = step-types.filter({(n): n >= num-abs(neg-sci.coeff / 9)}).get(0) * num-expt(10, neg-sci.exponent)
  step = num-max(pos-step, neg-step)
  step-sci = scientific-b10(step)

  # Use step distance to calculate Axis Properties
  name-tick = 
    {(n): 
      ask:
      | (step-sci.coeff == 2.5) and (step-sci.exponent <= 0) then: 
        pointer(num-to-string-digits(n, 2 - step-sci.exponent), n)
      | step-sci.exponent < 0 then: 
        pointer(num-to-string-digits(n, 1 - step-sci.exponent), n)
      | otherwise: 
        pointer(num-to-string(n), n)
      end}

  axisTop = num-max(0, step * num-ceiling(pos-bar-height / step))
  axisBottom = num-min(0, step * num-floor(neg-bar-height / step))
  pos-ticks = map(name-tick, range-by(0, axisTop + step, step))
  neg-ticks = map(name-tick, range-by(0, axisBottom - step, -1 * step))
  ticks = distinct(pos-ticks + neg-ticks) ^ builtins.raw-array-from-list
  self.constr()(
    self.obj.{axisdata: some(axis-data(axisTop, axisBottom, ticks))}
    )
end

format-axis-data-method = method(self, format-func :: (Number -> String)):
  cases (Option) self.obj!axisdata: 
    | none => 
      raise("Axis properties initialized improperly. Please report as a bug!")
    | some(ad) => 
      ad-tick-list = ad.ticks ^ raw-array-to-list
      new-ticks = map({(p): pointer(format-func(p.value), p.value)}, ad-tick-list) ^ builtins.raw-array-from-list
      self.constr()(self.obj.{axisdata: some(axis-data(ad.axisTop, ad.axisBottom, new-ticks))})
  end
end

scale-method = method(self, scale-fun :: (Number -> Number)): 
  exact-sf = {(n): n ^ scale-fun ^ num-to-rational}
  list-of-rows = self.obj!tab ^ raw-array-to-list
  scale-row = {(row): [raw-array: raw-array-get(row, 0), raw-array-get(row, 1) ^ exact-sf]}
  scaled-tab = map(scale-row, list-of-rows) ^ builtins.raw-array-from-list
  scaled-self = self.constr()(self.obj.{tab: scaled-tab})
  scaled-values = map({(row): raw-array-get(row, 1) ^ exact-sf}, list-of-rows)
  {max-positive-height; max-negative-height} = prep-axis(scaled-values)

  scaled-self.make-axis(max-positive-height, max-negative-height)
end

multi-scale-method = method(self, scale-fun :: (Number -> Number)): 
  exact-sf = {(n): n ^ scale-fun ^ num-to-rational}
  list-of-rows = self.obj!tab ^ raw-array-to-list
  get-values = {(row): raw-array-get(row, 1) ^ raw-array-to-list}
  scale-row = {(row): [raw-array: raw-array-get(row, 0), map(exact-sf, row ^ get-values) ^ builtins.raw-array-from-list]}
  scaled-tab = map(scale-row, list-of-rows) ^ builtins.raw-array-from-list
  scaled-self = self.constr()(self.obj.{tab: scaled-tab})
  scaled-values = map({(row): map(exact-sf, row ^ get-values)}, list-of-rows)
  {max-positive-height; max-negative-height} = 
    multi-prep-axis(string-to-stacktype(scaled-self.obj!is-stacked), scaled-values)

  scaled-self.make-axis(max-positive-height, max-negative-height)
end

stacking-type-method = method(self, stack-type :: StackType): 
  get-values = {(row): raw-array-get(row, 1) ^ raw-array-to-list}
  value-lists = map(get-values, self.obj!tab ^ raw-array-to-list)
  ask: 
    | stack-type == absolute then: 
      new-self = self.constr()(self.obj.{is-stacked: 'absolute'})
      {max-positive-height; max-negative-height} = 
        multi-prep-axis(absolute, value-lists)
      new-self.make-axis(max-positive-height, max-negative-height)
    | stack-type == relative then: 
      new-self = self.constr()(self.obj.{is-stacked: 'relative'})
      {max-positive-height; max-negative-height} = 
        multi-prep-axis(relative, value-lists)
      new-self.make-axis(max-positive-height, max-negative-height)
    | stack-type == percent then:
      new-self = self.constr()(self.obj.{is-stacked: 'percent'})
      {max-positive-height; max-negative-height} = 
        multi-prep-axis(percent, value-lists)
      new-self.make-axis(max-positive-height, max-negative-height)
              .format-axis({(n): num-to-string(n * 100) + "%"})
    | stack-type == grouped then: 
      new-self = self.constr()(self.obj.{is-stacked: 'none'})
      {max-positive-height; max-negative-height} = 
        multi-prep-axis(grouped, value-lists)
      new-self.make-axis(max-positive-height, max-negative-height)
    | otherwise: raise('stacking-type: type must be absolute, relative, percent, or grouped')
  end
end

annotations-method = method(self,
    annotations :: CL.LoLoOoS) block:
  # Annotations should match previous lengths
  expected-length = raw-array-length(self.obj.annotations)
  given-length = annotations.length()
  when given-length <> expected-length:
    raise("annotations: input dimensions mismatch. Expected length "
        + num-to-string(expected-length)
        + ", received "
        + num-to-string(given-length))
  end
  
  block:
    each({(l :: List<Option<String>>): 
      each({(o :: Option<String>): 
        cases (Option) o: 
          | none => true
          | some(s :: String) => true
        end}, l)}, annotations) 
    
    for each3(expected from raw-array-to-list(self.obj.annotations),
        given from annotations,
        index from range(0, annotations.length())):
      shadow expected-length = raw-array-length(expected)
      shadow given-length = given.length()
      when given-length <> expected-length:
        raise("annotations: length mismatch on row "
            + num-to-string(index)
            + ". Expected "
            + num-to-string(expected-length)
            + ", received "
            + num-to-string(given-length))
      end
    end
  end

  self.constr()(self.obj.{annotations: list-to-table2(annotations)})
end

single-annotations-method = method(self, annotations :: CL.LoOoS):
  self.{annotations-method: annotations-method}
    .annotations-method(annotations.map(link(_, empty)))
end

intervals-method = method(self, intervals :: CL.LoLoLoN) block:
  expected-length = raw-array-length(self.obj.intervals)
  given-length = intervals.length()
  when given-length <> expected-length:
    raise("intervals: input dimensions mismatch. Expected length "
        + num-to-string(expected-length)
        + ", received "
        + num-to-string(given-length))
  end
  block:
    each({(l :: List<List<Number>>): 
      each({(l1 :: List<Number>): 
        each({(n :: Number): true}, l1)}, l)}, intervals) 
    for each3(expected from raw-array-to-list(self.obj.intervals),
        given from intervals,
        index from range(0, intervals.length())):
      shadow expected-length = raw-array-length(expected)
      shadow given-length = given.length()
      when given-length <> expected-length:
        raise("intervals: length mismatch on row "
            + num-to-string(index)
            + ". Expected "
            + num-to-string(expected-length)
            + ", received "
            + num-to-string(given-length))
      end
    end
  end
  raw-intervals = intervals.map(_.map(raw-array-from-list)) ^ list-to-table2
  flatten = {(lol): fold({(acc, elm): acc + elm}, empty, lol)}
  curr-axis = self.obj!axisdata.value
  interval-max = fold(num-max, 0, flatten(flatten(intervals)))
  interval-min = fold(num-min, 0, flatten(flatten(intervals)))
  self.constr()(
    self.obj.{intervals: raw-intervals})
            .make-axis(
              num-max(curr-axis.axisTop, interval-max), 
              num-min(curr-axis.axisBottom, interval-min))
end

single-intervals-method = method(self, intervals :: CL.LoLoN):
  self.{intervals: intervals-method}.intervals(intervals.map(link(_, empty)))
end

error-bars-method = method(self, errors :: CL.LoLoLoN) block:
  doc: ```Given a list of one negative number and one positive number for every
       data point, set intervals to lower and upper error intervals.```
  expected-length = raw-array-length(self.obj.intervals)
  given-length = errors.length()
  when given-length <> expected-length:
    raise("error-bars: input dimensions mismatch. Expected length "
        + num-to-string(expected-length)
        + ", received "
        + num-to-string(given-length))
  end
  block:
    each({(l :: List<List<Number>>): 
      each({(l1 :: List<Number>): 
        each({(n :: Number): true}, l1)}, l)}, errors) 
    for each3(expected from raw-array-to-list(self.obj.intervals),
        given from errors,
        index from range(0, errors.length())):
      block:
        shadow expected-length = raw-array-length(expected)
        shadow given-length = given.length()
        row-str = num-to-string(index)
        when given-length <> expected-length:
          raise("error-bars: length mismatch on row " + row-str
              + ". Expected "
              + num-to-string(expected-length)
              + ", received "
              + num-to-string(given-length))
        end
        for each2(pair from given, column from range(0, given.length())):
          block:
            col-str = num-to-string(column)
            when pair.length() <> 2:
              raise("error-bars: on row " + row-str + " column " + col-str
                  + ", 2 intervals must be given.")
            end
            when pair.get(0) > 0:
              raise("error-bars: on row " + row-str + " column " + col-str
                  + ", first pair must be non-positive.")
            end
            when pair.get(1) < 0:
              raise("error-bars: on row " + row-str + " column " + col-str
                  + ", second pair must be non-negative.")
            end
          end
        end
      end
    end
  end
  # Defer to intervals-method
  raw-table-data = raw-array-map(raw-array-get(_, 1), self.obj.tab)
  table-data = table2-to-list(raw-table-data)
  intervals-at-end = for map2(data-row from table-data,
      error-row from errors):
    for map2(data-col from data-row, error-bounds from error-row):
      error-bounds.map(_ + data-col)
    end
  end
  self.intervals(intervals-at-end)
end

single-error-bars-method = method(self, errors :: CL.LoLoN) block:
  doc: ```Given a list of pairs of one positive and one negative number
       corresponding to upper and lower bounds, produces a chart with error
       bars using the given bounds.```
  expected-length = raw-array-length(self.obj.intervals)
  given-length = errors.length()
  when given-length <> expected-length:
    raise("error-bars: input dimensions mismatch. Expected length "
        + num-to-string(expected-length)
        + ", received "
        + num-to-string(given-length))
  end
  block:
      each({(l :: List<Number>): 
        each({(n :: Number): true}, l)}, errors)

    for each3(expected from raw-array-to-list(self.obj.intervals),
        given from errors,
        index from range(0, errors.length())):
      block:
        row-str = num-to-string(index)
        when given.length() <> 2:
          raise("error-bars: on row " + row-str
              + ", 2 intervals must be given (received "
              + num-to-string(given.length()) + ").")
        end
        when given.get(0) > 0:
          raise("error-bars: on row " + row-str
              + ", first pair must be non-positive.")
        end
        when given.get(1) < 0:
          raise("error-bars: on row " + row-str
              + ", second pair must be non-negative.")
        end
      end
    end
  end
  # Defer to intervals-method
  raw-table-data = raw-array-map(raw-array-get(_, 1), self.obj.tab)
  table-data = raw-array-to-list(raw-table-data)
  intervals-at-end = for map2(data-val from table-data,
      error from errors):
    error.map(_ + data-val)
  end
  self.intervals(intervals-at-end)
end

min-method = method(self, min :: Number):
  self.constr()(self.obj.{min: some(min)})
end

max-method = method(self, max :: Number):
  self.constr()(self.obj.{max: some(max)})
end

horizontal-method = method(self, b :: Boolean):
  self.constr()(self.obj.{horizontal: b})
end

################################################################################
# BOUNDING BOX
################################################################################

type BoundingBox = {
  x-min :: Number,
  x-max :: Number,
  y-min :: Number,
  y-max :: Number,
  is-valid :: Boolean
}
default-bounding-box :: BoundingBox = {
  x-min: 0,
  x-max: 0,
  y-min: 0,
  y-max: 0,
  is-valid: false,
}

fun get-bounding-box(ps :: List<Posn>) -> BoundingBox:
  cases (List<Number>) ps:
    | empty => default-bounding-box.{is-valid: false}
    | link(f, r) =>
      fun compute(p :: (Number, Number -> Number), accessor :: (Posn -> Number)):
        for fold(prev from accessor(f), e from r): p(prev, accessor(e)) end
      end
      default-bounding-box.{
        x-min: compute(num-min, fst),
        x-max: compute(num-max, fst),
        y-min: compute(num-min, snd),
        y-max: compute(num-max, snd),
        is-valid: true,
      }
  end
end

fun get-list-of-bounding-boxes(list-of-plots, self, other-accessor) -> List<BoundingBox>:
  for map(plot-pts from list-of-plots):
    for filter(pt from plot-pts):
      cases (Option) self.x-min:
        | none => true
        | some(v) => fst(pt) >= v
      end and
      cases (Option) self.x-max:
        | none => true
        | some(v) => fst(pt) <= v
      end and
      cases (Option) self.y-min:
        | none => true
        | some(v) => other-accessor(pt) >= v
      end and
      cases (Option) self.y-max:
        | none => true
        | some(v) => other-accessor(pt) <= v
      end
    end ^ get-bounding-box
  end
end

fun merge-bounding-box(bs :: List<BoundingBox>) -> BoundingBox:
  for fold(prev from default-bounding-box, e from bs):
    ask:
      | e.is-valid and prev.is-valid then:
        default-bounding-box.{
          x-min: num-min(e.x-min, prev.x-min),
          x-max: num-max(e.x-max, prev.x-max),
          y-min: num-min(e.y-min, prev.y-min),
          y-max: num-max(e.y-max, prev.y-max),
          is-valid: true,
        }
      | e.is-valid then: e
      | prev.is-valid then: prev
      | otherwise: default-bounding-box
    end
  end
end

################################################################################
# DEFAULT VALUES
################################################################################

type BoxChartSeries = {
  tab :: TableIntern,
  height :: Number, 
  horizontal :: Boolean,
  color :: Option<I.Color>,
}

default-box-plot-series = {
  horizontal: false,
  show-outliers: true,
  color: none, 
}

type PieChartSeries = {
  tab :: TableIntern,
  colors :: Option<RawArray<I.Color>>,
  threeD :: Boolean,
  piehole :: Number,
  startingAngle :: Number,
  collapseThreshold :: Number,
}

default-pie-chart-series = {
  colors: none,
  threeD: false,
  piehole: 0,
  startingAngle: 0,
  collapseThreshold: 0,
}

type BarChartSeries = {
  tab :: TableIntern,
  axisdata :: Option<AxisData>, 
  color :: Option<I.Color>,
  colors :: Option<RawArray<I.Color>>,
  pointers :: Option<RawArray<Pointer>>, 
  pointer-color :: Option<I.Color>, 
  horizontal :: Boolean,
  annotations :: RawArray<RawArray<Option<String>>>,
  intervals :: RawArray<RawArray<RawArray<Number>>>,
  default-interval-color :: Option<I.Color>,
}

default-bar-chart-series = {
  color: none,
  colors: none,
  pointers: none, 
  pointer-color: none,
  axisdata: none, 
  horizontal: false, 
  default-interval-color: none,
}

type MultiBarChartSeries = { 
  tab :: TableIntern,
  axisdata :: Option<AxisData>,
  legends :: RawArray<String>,
  is-stacked :: String,
  colors :: Option<RawArray<I.Color>>,
  pointers :: Option<RawArray<Pointer>>, 
  pointer-color :: Option<I.Color>, 
  horizontal :: Boolean,
  annotations :: RawArray<RawArray<Option<String>>>,
  intervals :: RawArray<RawArray<RawArray<Number>>>,
  default-interval-color :: Option<I.Color>
}

default-multi-bar-chart-series = {
  is-stacked: 'none',
  colors: some([raw-array: C.red, C.blue, C.green, C.orange, C.purple, C.black, C.brown]),
  pointers: none, 
  pointer-color: none,
  axisdata: none, 
  horizontal: false, 
  default-interval-color: none
}
  
type HistogramSeries = {
  tab :: TableIntern,
  bin-width :: Option<Number>,
  max-num-bins :: Option<Number>,
  min-num-bins :: Option<Number>,
  color :: Option<I.Color>,
}

default-histogram-series = {
  bin-width: none,
  max-num-bins: none,
  min-num-bins: none,
  color: none, 
}

type LinePlotSeries = {
  ps :: List<Posn>,
  color :: Option<I.Color>,
  legend :: String,
  curved :: String,
  lineWidth :: Number,
  trendlineType :: Option<String>,
  trendlineColor :: Option<I.Color>,
  trendlineWidth :: Number, 
  trendlineOpacity :: Number, 
  trendlineDegree :: NumInteger, 
  dashedLine :: Boolean, 
  dashlineStyle :: RawArray<NumInteger>, 
  point-size :: Number,
  useImageSizes :: Boolean,
  pointshapeType :: String, 
  pointshapeSides :: NumInteger, 
  pointshapeDent :: Number, 
  pointshapeRotation :: Number,
  horizontal :: Boolean,
}

default-line-plot-series = {
  color: none,
  legend: '',
  curved: 'none',
  lineWidth: 2,
  trendlineType: none, 
  trendlineColor: none, 
  trendlineWidth: 3, 
  trendlineOpacity: 0.3,
  trendlineDegree: 3,  
  dashedLine: false,
  dashlineStyle: [raw-array: 2, 2],
  point-size: 0,
  useImageSizes: true,
  pointshapeType: 'circle', 
  pointshapeSides: 5,
  pointshapeDent: 0.5,
  pointshapeRotation: 0,
  horizontal: false
}

type ScatterPoint = {
  x :: Number,
  y :: Number,
  label :: String,
  image :: Option<IM.Image>
}
          
type ScatterPlotSeries = {
  ps :: List<ScatterPoint>,
  color :: Option<I.Color>,
  legend :: String,
  point-size :: Number,
  useImageSizes :: Boolean,
  trendlineType :: Option<String>,
  trendlineColor :: Option<I.Color>,
  trendlineWidth :: Number, 
  trendlineOpacity :: Number, 
  trendlineDegree :: NumInteger, 
  pointshapeType :: String, 
  pointshapeSides :: NumInteger, 
  pointshapeDent :: Number, 
  pointshapeRotation :: Number,
  horizontal :: Boolean,
}

default-scatter-plot-series = {
  color: none,
  legend: '',
  point-size: 7,
  useImageSizes: true,
  pointshapeType: 'circle', 
  pointshapeSides: 5,
  pointshapeDent: 0.5,
  pointshapeRotation: 0,
  trendlineType: none, 
  trendlineColor: none, 
  trendlineWidth: 3, 
  trendlineOpacity: 0.3,
  trendlineDegree: 3,
  horizontal: false,
}

type DotPoint = {
  value :: Number,
  label :: String,
  image :: Option<IM.Image>
}

type DotPlotSeries = {
  ps :: List<DotPoint>,
  color :: Option<I.Color>,
  legend :: String,
  point-size :: Number,
  useImageSizes :: Boolean,
}

default-dot-plot-series = {
  color: none,
  legend: '',
  point-size: 8,
  useImageSizes: true,
}

type IntervalPoint = {
  label :: String,
  x :: Number,
  y :: Number,
  delta :: Number,
  image :: Option<IM.Image>
}
type IntervalChartSeries = {
  axisdata :: Option<AxisData>,
  color :: Option<I.Color>,
  pointers :: Option<RawArray<Pointer>>,
  pointer-color :: Option<I.Color>,
  point-size :: Number,
  lineWidth :: Number,
  stick-width :: Number,
  style :: String,
  horizontal :: Boolean,
  default-interval-color :: Option<I.Color>,
  legend :: String,
  trendlineType :: Option<String>,
  trendlineColor :: Option<I.Color>,
  trendlineWidth :: Number,
  trendlineOpacity :: Number,
  trendlineDegree :: NumInteger,
  # curved :: String,
  # dashedLine :: Boolean,
  # dashlineStyle :: RawArray<NumInteger>,
  pointshapeType :: String,
  pointshapeSides :: NumInteger,
  pointshapeDent :: Number,
  pointshapeRotation :: Number,
  ps :: List<IntervalPoint>,
}

default-interval-chart-series = {
  color: some(C.red),
  pointers: none,
  pointer-color: some(C.color(228, 147, 7, 1)),
  point-size: 4,
  lineWidth: 0,
  stick-width: 1,
  axisdata: none,
  horizontal: false,
  style: "bars",
  default-interval-color: none,
  legend: '',
  trendlineType: none,
  trendlineColor: none,
  trendlineWidth: 3,
  trendlineOpacity: 0.3,
  trendlineDegree: 3,
  pointshapeType: 'circle',
  pointshapeSides: 5,
  pointshapeDent: 0.5,
  pointshapeRotation: 0,
}

type FunctionPlotSeries = {
  f :: PlottableFunction,
  color :: Option<I.Color>,
  legend :: String,
  horizontal :: Boolean,
}

default-function-plot-series = {
  color: none,
  legend: '',
  horizontal: false
}

###########

type ChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image)
}

default-chart-window-object :: ChartWindowObject = {
  title: '',
  width: 800,
  height: 600,
  backgroundColor: none,
  borderSize: 0, 
  borderColor: none, 
  method render(self): raise('unimplemented') end,
}

type BoxChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  x-axis :: String,
  y-axis :: String,
  min :: Option<Number>,
  max :: Option<Number>,
  render :: ( -> IM.Image),
}

default-box-plot-chart-window-object :: BoxChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  min: none,
  max: none,
}

type PieChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image),
}

default-pie-chart-window-object :: PieChartWindowObject = default-chart-window-object

type DotChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String
}

default-dot-chart-window-object :: DotChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: ''
}

type BarChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String,
  y-min :: Option<Number>,
  y-max :: Option<Number>,
}

default-bar-chart-window-object :: BarChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  y-min: none,
  y-max: none,
}

type IntervalChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number,
  borderColor :: Option<I.Color>,
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String,
  y-min :: Option<Number>,
  y-max :: Option<Number>,
}

default-interval-chart-window-object :: IntervalChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  y-min: none,
  y-max: none,
}

type HistogramChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String,
  x-min :: Option<Number>,
  x-max :: Option<Number>,
  y-max :: Option<Number>,
}

default-histogram-chart-window-object :: HistogramChartWindowObject =
  default-chart-window-object.{
    x-axis: '',
    y-axis: '',
    x-min: none,
    x-max: none,
    y-max: none,
  }

type PlotChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  backgroundColor :: Option<I.Color>,
  borderSize :: Number, 
  borderColor :: Option<I.Color>, 
  render :: ( -> IM.Image),
  gridlineColor :: Option<I.Color>, 
  gridlineMinspacing :: Option<Number>, 
  minorGridlineColor :: Option<I.Color>, 
  minorGridlineMinspacing :: Number, 
  x-axis :: String,
  y-axis :: String,
  x-min :: Option<Number>,
  x-max :: Option<Number>,
  y-min :: Option<Number>,
  y-max :: Option<Number>,
  num-samples :: Number,
  multiple :: Boolean, 
}

default-plot-chart-window-object :: PlotChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  show-minor-grid-lines: false,
  x-min: none,
  x-max: none,
  y-min: none,
  y-max: none,
  num-samples: 1000,
  multiple: false,
  gridlineColor: none, 
  gridlineMinspacing: none,
  minorGridlineColor: none, 
  minorGridlineMinspacing: 10, 
}

################################################################################
# DATA DEFINITIONS
################################################################################

data DataSeries:
  | line-plot-series(obj :: LinePlotSeries) with:
    is-single: false,
    constr: {(): line-plot-series},
    color: color-method,
    legend: legend-method,
    curved: curve-method,
    linewidth: line-width-method, 
    trendline-type: trendline-type-method,
    trendline-color: trendline-color-method,
    trendline-width: trendline-width-method, 
    trendline-opacity: trendline-opacity-method, 
    dashed-line: dashed-line-method, 
    dashline-style: dashed-line-style-method,
    point-shape: pointshape-method, 
    labels: labels-method,
    image-labels: image-labels-method,
    method point-size(self, point-size :: Number) block:
      when point-size < 0: 
        raise("point-size: Point Size must be non-negative")
      end
      self.constr()(self.obj.{point-size: point-size})
    end,
    method use-image-sizes(self, use-image-sizes :: Boolean):
      self.constr()(self.obj.{useImageSizes: use-image-sizes})
    end,
    horizontal: horizontal-method,
  | scatter-plot-series(obj :: ScatterPlotSeries) with:
    trendline-type: trendline-type-method,
    trendline-color: trendline-color-method,
    trendline-width: trendline-width-method, 
    trendline-opacity: trendline-opacity-method, 
    is-single: false,
    constr: {(): scatter-plot-series},
    color: color-method,
    legend: legend-method,
    labels: labels-method,
    image-labels: image-labels-method,
    point-shape: pointshape-method, 
    method point-size(self, point-size :: Number) block:
      when point-size <= 0: 
        raise("point-size: Point Size must be positive")
      end
      self.constr()(self.obj.{point-size: point-size})
    end,
    method use-image-sizes(self, use-image-sizes :: Boolean):
      self.constr()(self.obj.{useImageSizes: use-image-sizes})
    end,
    horizontal: horizontal-method,
  | dot-plot-series(obj :: DotPlotSeries) with:
    is-single: true,
    constr: {(): dot-plot-series},
    color: color-method,
    legend: legend-method,
    image-labels: image-labels-method,
    labels: labels-method,
    method point-size(self, point-size :: Number) block:
      when point-size < 0: 
        raise("point-size: Point Size must be non-negative")
      end
      self.constr()(self.obj.{point-size: point-size})
    end,
    method use-image-sizes(self, use-image-sizes :: Boolean):
      self.constr()(self.obj.{useImageSizes: use-image-sizes})
    end,
  | function-plot-series(obj :: FunctionPlotSeries) with:
    is-single: false,
    constr: {(): function-plot-series},
    color: color-method,
    legend: legend-method,
    horizontal: horizontal-method,
  | pie-chart-series(obj :: PieChartSeries) with:
    is-single: true,
    explode: explode-method,
    colors: color-list-method,
    sort: default-sort-method,
    sort-by: sort-method,
    sort-by-label: label-sort-method,
    threeD: threeD-method,
    piehole: piehole-method,
    rotate: starting-angle-method,
    collapse-threshold: collapse-threshold-method,
    constr: {(): pie-chart-series},
  | bar-chart-series(obj :: BarChartSeries) with:
    is-single: true,
    color: color-method, 
    colors: color-list-method,
    sort: default-sort-method,
    sort-by: sort-method,
    sort-by-label: label-sort-method,
    add-pointers: axis-pointer-method,
    pointer-color: pointer-color-method,
    format-axis: format-axis-data-method, 
    make-axis: make-axis-data-method, 
    scale: scale-method,
    horizontal: horizontal-method,
    annotations: single-annotations-method,
    intervals: single-intervals-method,
    error-bars: single-error-bars-method,
    interval-color: interval-color-method, 
    constr: {(): bar-chart-series},
  | interval-chart-series(obj :: IntervalChartSeries) with:
    is-single: false,
    color: color-method,
    colors: color-list-method,
    legend: legend-method,
    sort: default-sort-method,
    sort-by: sort-method,
    sort-by-label: label-sort-method,
    add-pointers: axis-pointer-method,
    pointer-color: pointer-color-method,
    format-axis: format-axis-data-method,
    make-axis: make-axis-data-method,
    scale: scale-method,
    lineWidth: line-width-method,
    style: style-method,
    trendline-type: trendline-type-method,
    method point-size(self, point-size :: Number) block:
      when point-size < 0:
        raise("point-size: Point Size must be non-negative")
      end
      self.constr()(self.obj.{point-size: point-size})
    end,
    horizontal: horizontal-method,
    annotations: single-annotations-method,
    intervals: single-intervals-method,
    error-bars: single-error-bars-method,
    interval-color: interval-color-method,
    constr: {(): interval-chart-series},
  | multi-bar-chart-series(obj :: MultiBarChartSeries) with: 
    is-single: true,
    colors: color-list-method,
    sort: super-default-multi-sort-method,
    sort-by: default-multi-sort-method,
    sort-by-data: multi-sort-method, 
    sort-by-label: label-sort-method,
    add-pointers: axis-pointer-method, 
    pointer-color: pointer-color-method,
    format-axis: format-axis-data-method,
    make-axis: make-axis-data-method,
    scale: multi-scale-method,
    stacking-type: stacking-type-method, 
    horizontal: horizontal-method,
    annotations: annotations-method,
    intervals: intervals-method,
    error-bars: error-bars-method,
    interval-color: interval-color-method, 
    constr: {(): multi-bar-chart-series}
  | box-plot-series(obj :: BoxChartSeries) with:
    labels: box-labels-method,
    color: color-method, 
    is-single: true,
    constr: {(): box-plot-series},
    horizontal: horizontal-method,
    method show-outliers(self, show):
      self.constr()(self.obj.{show-outliers: show})
    end,
  | histogram-series(obj :: HistogramSeries) with:
    labels: histogram-label-method,
    color: color-method, 
    is-single: true,
    constr: {(): histogram-series},
    method bin-width(self, bin-width :: Number):
      histogram-series(self.obj.{bin-width: some(bin-width)})
    end,
    method max-num-bins(self, max-num-bins :: Number):
      histogram-series(self.obj.{max-num-bins: some(max-num-bins)})
    end,
    method min-num-bins(self, min-num-bins :: Number):
      histogram-series(self.obj.{min-num-bins: some(min-num-bins)})
    end,
    method num-bins(self, num-bins :: Number):
      histogram-series(self.obj.{
        min-num-bins: some(num-bins),
        max-num-bins: some(num-bins)
      })
    end,
sharing:
  method _output(self):
    cases (E.Either) run-task({(): get-vs-from-img("DataSeries", render-chart(self).get-image())}):
      | left(y) => y
      | right(err) => VS.vs-seq([list: VS.vs-str("Could not render this DataSeries because... \n"), 
                                       VS.vs-str(RED.display-to-string(exn-unwrap(err).render-reason(), tostring, empty))])
    end
  end
end

fun check-chart-window(p :: ChartWindowObject) -> Nothing:
  if (p.width <= 0) or (p.height <= 0):
    raise('render: width and height must be positive')
  else:
    nothing
  end
end

data ChartWindow:
  | pie-chart-window(obj :: PieChartWindowObject) with:
    constr: {(): pie-chart-window},
  | dot-chart-window(obj :: DotChartWindowObject) with:
    constr: {(): dot-chart-window},
    x-axis: x-axis-method,
    y-axis: y-axis-method,
  | box-plot-chart-window(obj :: BoxChartWindowObject) with:
    constr: {(): box-plot-chart-window},
    x-axis: x-axis-method,
    y-axis: y-axis-method,
    min: min-method,
    max: max-method,
  | bar-chart-window(obj :: BarChartWindowObject) with:
    constr: {(): bar-chart-window},
    x-axis: x-axis-method,
    y-axis: y-axis-method,
    y-min: y-min-method,
    y-max: y-max-method,
  | interval-chart-window(obj :: IntervalChartWindowObject) with:
    constr: {(): interval-chart-window},
    x-axis: x-axis-method,
    y-axis: y-axis-method,
    y-min: y-min-method,
    y-max: y-max-method,
  | histogram-chart-window(obj :: HistogramChartWindowObject) with:
    constr: {(): histogram-chart-window},
    x-axis: x-axis-method,
    y-axis: y-axis-method,
    x-min: x-min-method,
    x-max: x-max-method,
    y-max: y-max-method,
  | plot-chart-window(obj :: PlotChartWindowObject) with:
    constr: {(): plot-chart-window},
    show-minor-gridlines: show-minor-grid-lines-method,
    gridlines-color: gridlines-color-method, 
    gridlines-minspacing: gridlines-min-spacing-method, 
    minor-gridlines-color: minor-gridlines-color-method, 
    minor-gridlines-minspacing: minor-gridlines-min-spacing-method, 
    x-axis: x-axis-method,
    y-axis: y-axis-method,
    x-min: x-min-method,
    x-max: x-max-method,
    y-min: y-min-method,
    y-max: y-max-method,
    select-multiple: select-multiple-method,
    method num-samples(self, num-samples :: Number) block:
      when (num-samples <= 0) or (num-samples > 100000) or not(num-is-integer(num-samples)):
        raise('num-samples: value must be an ineger between 1 and 100000')
      end
      plot-chart-window(self.obj.{num-samples: num-samples})
    end,
sharing:
  background-color: background-color-method,
  border-size: background-border-method, 
  border-color: border-color-method,
  method display(self):
    _ = check-chart-window(self.obj)
    self.obj.{interact: true}.render()
  end,
  method get-image(self):
    _ = check-chart-window(self.obj)
    self.obj.{interact: false}.render()
  end,
  method title(self, title :: String):
    self.constr()(self.obj.{title: title})
  end,
  method width(self, width :: Number):
    self.constr()(self.obj.{width: width})
  end,
  method height(self, height :: Number):
    self.constr()(self.obj.{height: height})
  end,
  method _output(self):
    get-vs-from-img("ChartWindow", self.get-image())
  end
end

################################################################################
# FUNCTIONS
################################################################################

fun function-plot-from-list(f :: PlottableFunction) -> DataSeries:
  default-function-plot-series.{
    f: f,
  } ^ function-plot-series
end

fun line-plot-from-list(xs :: CL.LoN, ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('line-plot: xs and ys should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  default-line-plot-series.{
    ps: map4(get-scatter-point, xs, ys, xs.map({(_): ''}), xs.map({(_): none}))
  } ^ line-plot-series
end

fun labeled-line-plot-from-list(labels :: CL.LoS, xs :: CL.LoN, ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('labeled-line-plot: xs and ys should have the same length')
  end
  when xs.length() <> labels.length():
    raise('labeled-line-plot: xs and labels should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  default-line-plot-series.{
    ps: map4(get-scatter-point, xs, ys, labels, xs.map({(_): none}))
  } ^ line-plot-series
end

fun image-line-plot-from-list(images :: CL.LoI, xs :: CL.LoN, ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('image-line-plot: xs and ys should have the same length')
  end
  when xs.length() <> images.length():
    raise('image-line-plot: xs and images should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  default-line-plot-series.{
    ps: map4(get-scatter-point, xs, ys, xs.map({(_): ''}), images.map(some))
  } ^ line-plot-series
end

fun get-scatter-point(x :: Number, y :: Number, label :: String, optimg :: Option<IM.Image>) -> ScatterPoint:
  { x: x, y: y, label: label, image: optimg }
end
fun scatter-plot-from-list(xs :: CL.LoN, ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('scatter-plot: xs and ys should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  default-scatter-plot-series.{
    ps: map4(get-scatter-point, xs, ys, xs.map({(_): ''}), xs.map({(_): none}))
  } ^ scatter-plot-series
end

fun labeled-scatter-plot-from-list(
  labels :: CL.LoS,
  xs :: CL.LoN,
  ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('labeled-scatter-plot: xs and ys should have the same length')
  end
  when xs.length() <> labels.length():
    raise('labeled-scatter-plot: xs and labels should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  labels.each(check-string)
  default-scatter-plot-series.{
    ps: map4(get-scatter-point, xs, ys, labels, xs.map({(_): none}))
  } ^ scatter-plot-series
end

fun image-scatter-plot-from-list(
  images :: CL.LoI,
  xs :: CL.LoN,
  ys :: CL.LoN) -> DataSeries block:
  when xs.length() <> ys.length():
    raise('labeled-scatter-plot: xs and ys should have the same length')
  end
  when xs.length() <> images.length():
    raise('labeled-scatter-plot: xs and images should have the same length')
  end
  xs.each(check-num)
  ys.each(check-num)
  images.each(check-image)
  default-scatter-plot-series.{
    ps: map4(get-scatter-point, xs, ys, xs.map({(_): ''}), images.map(some))
  } ^ scatter-plot-series
end

fun image-bar-chart-from-list(
  images :: CL.LoI, 
  labels :: CL.LoS, 
  values :: CL.LoN) -> DataSeries block:

  doc: ```
       Consume images, labels, a list of string, and values, a list of numbers
       and construct a bar chart using images as bars
       ```

  # Type Checking
  images.each(check-image)
  values.each(check-num)
  labels.each(check-string)

  # Constants
  label-length = labels.length()
  value-length = values.length()
  rational-values = map(num-to-rational, values)


  # Edge Case Error Checking
  when value-length == 0:
    raise("bar-chart: can't have empty data")
  end
  when label-length <> value-length:
    raise('bar-chart: labels and values should have the same length')
  end

  {max-positive-height; max-negative-height} = prep-axis(rational-values)


  data-series = default-bar-chart-series.{
    tab: to-table3-n(labels, rational-values, images),
    axis-top: max-positive-height,
    axis-bottom: max-negative-height,
    annotations: values.map({(_): [list: none]}) ^ list-to-table2,
    intervals: values.map({(_): [list: [raw-array: ]]}) ^ list-to-table2,
  } ^ bar-chart-series

  data-series.make-axis(max-positive-height, max-negative-height)
end

fun exploding-pie-chart-from-list(
  labels :: CL.LoS,
  values :: CL.LoN,
  offsets :: CL.LoN
) -> DataSeries block:
  label-length = labels.length()
  value-length = values.length()
  for each(value from values):
    when value < 0:
      raise('exploding-pie-chart: values must be non-negative')
    end
  end
  when label-length <> value-length:
    raise('exploding-pie-chart: labels and values should have the same length')
  end
  offset-length = offsets.length()
  when label-length <> offset-length:
    raise('exploding-pie-chart: labels and offsets should have the same length')
  end
  when label-length == 0:
    raise('exploding-pie-chart: need at least one data')
  end
  for each(offset from offsets):
    when (offset < 0) or (offset > 1):
      raise('exploding-pie-chart: offset must be between 0 and 1')
    end
  end
  values.each(check-num)
  offsets.each(check-num)
  labels.each(check-string)
  default-pie-chart-series.{
    tab: to-table3-n(labels, values, offsets)
  } ^ pie-chart-series
end

fun pie-chart-from-list(labels :: CL.LoS, values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume labels, a list of string, and values, a list of numbers
       and construct a pie chart
       ```
  label-length = labels.length()
  value-length = values.length()
  for each(value from values):
    when value < 0:
      raise('pie-chart: values must be non-negative')
    end
  end
  when label-length <> value-length:
    raise('pie-chart: labels and values should have the same length')
  end
  when label-length == 0:
    raise('pie-chart: need at least one data')
  end
  values.each(check-num)
  labels.each(check-string)
  default-pie-chart-series.{
    tab: to-table3-n(labels, values, labels.map({(_): 0}))
  } ^ pie-chart-series
end

fun image-pie-chart-from-list(images :: CL.LoI, labels :: CL.LoS, values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume images, labels, a list of strings, and values, a list of numbers
       and construct a pie chart sticking images into the slices
       ```
  label-length = labels.length()
  value-length = values.length()
  for each(value from values):
    when value < 0:
      raise('pie-chart: values must be non-negative')
    end
  end
  when label-length <> value-length:
    raise('pie-chart: labels and values should have the same length')
  end
  when label-length == 0:
    raise('pie-chart: need at least one data')
  end
  images.each(check-image)
  values.each(check-num)
  labels.each(check-string)
  default-pie-chart-series.{
    tab: to-table4(labels, values, labels.map({(_): 0}), images)
  } ^ pie-chart-series
end

fun bar-chart-from-list(labels :: CL.LoS, values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume labels, a list of string, and values, a list of numbers
       and construct a bar chart
       ```
  # Type Checking
  values.each(check-num)
  labels.each(check-string)

  # Constants
  label-length = labels.length()
  value-length = values.length()
  rational-values = map(num-to-rational, values)

  # Edge Case Error Checking
  when value-length == 0:
    raise("bar-chart: can't have empty data")
  end
  when label-length <> value-length:
    raise('bar-chart: labels and values should have the same length')
  end

  {max-positive-height; max-negative-height} = prep-axis(rational-values)

  data-series = default-bar-chart-series.{
    tab: to-table2-n(labels, rational-values),
    axis-top: max-positive-height,
    axis-bottom: max-negative-height,
    annotations: values.map({(_): [list: none]}) ^ list-to-table2,
    intervals: values.map({(_): [list: [raw-array: ]]}) ^ list-to-table2,
  } ^ bar-chart-series

  data-series.make-axis(max-positive-height, max-negative-height)
end

fun get-dot-point(value :: Number, label :: String, optimg :: Option<IM.Image>) -> DotPoint:
  { value: value, label: label, image: optimg }
end
fun num-dot-chart-from-list(x-values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume a (possibly repeating, unordered) list of numbers
       and construct a dot chart
       ```
  x-values.each(check-num)
  when x-values.length() == 0:
    raise("num-dot-chart: can't have empty data")
  end
  default-dot-plot-series.{
    ps: map3(get-dot-point, x-values, x-values.map({(_): ''}), x-values.map({(_): none})),
  } ^ dot-plot-series
end

fun image-num-dot-chart-from-list(images :: CL.LoI, x-values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume unordered, possibly-repeating lists of image-labels and numbers, 
       and construct a dot chart
       ```
  x-values.each(check-num)
  when x-values.length() == 0:
    raise("num-dot-chart: can't have empty data")
  end
  images.each(check-image)
  when images.length() <> x-values.length():
    raise("num-dot-chart: the lists of numbers and images must have the same length")
  end
  default-dot-plot-series.{
    ps: map3(get-dot-point, x-values, x-values.map({(_): ''}), images.map(some)),
  } ^ dot-plot-series
end

fun labeled-num-dot-chart-from-list(labels :: CL.LoS, x-values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume unordered, possibly-repeating lists of labels and numbers, 
       and construct a dot chart
       ```
  x-values.each(check-num)
  when x-values.length() == 0:
    raise("num-dot-chart: can't have empty data")
  end
  labels.each(check-string)
  when labels.length() <> x-values.length():
    raise("num-dot-chart: the lists of numbers and labels must have the same length")
  end
  default-dot-plot-series.{
    ps: map3(get-dot-point, x-values, labels, x-values.map({(_): none})),
  } ^ dot-plot-series
end

fun dot-chart-from-list(input-labels :: CL.LoS) -> DataSeries block:
  doc: ```
       Consume a list of string-values and construct a dot chart
       ```

  # Edge Case Error Checking
  when input-labels.length() == 0:
    raise("dot-chart: can't have empty data")
  end

  # Type Checking
  input-labels.each(check-string)

  # Walk through the (sorted) values, creating lists of labels and counts
  unique-counts = foldl(
    lam(acc, elt):
      labels = acc.{0}
      counts = acc.{1}
      if labels.member(elt):
        {labels; counts.set(0, counts.get(0) + 1)}
      else:
        {link(elt, labels); link(1, counts)}
      end
    end,
    {[list: ]; [list: ]},
    input-labels.sort())

  labels = unique-counts.{0}
  values = unique-counts.{1}
  rational-values = map(num-to-rational, values)

  # set the vAxis values, and create the data series
  {max-positive-height; max-negative-height} = prep-axis(rational-values)

  data-series = default-bar-chart-series.{
    tab: to-table2-n(labels, rational-values),
    dot-chart: true,
    axis-top: max-positive-height,
    axis-bottom: max-negative-height,
    annotations: values.map({(_): [list: none]}) ^ list-to-table2,
    intervals: values.map({(_): [list: [raw-array: ]]}) ^ list-to-table2,
  } ^ bar-chart-series

  data-series.make-axis(max-positive-height, max-negative-height)
end

fun grouped-bar-chart-from-list(
  labels :: CL.LoS,
  value-lists :: CL.LoLoN,
  legends :: CL.LoS
) -> DataSeries block:
  doc: ```
       Produces a grouped bar chart where labels are bar group names, legends are bar names, 
       and value-lists contains the data of each bar seperated into seperate groups 
       ```
  # Constants
  label-length = labels.length()
  value-length = value-lists.length()
  legend-length = legends.length() 
  rational-values = value-lists.map({(row): map(num-to-rational, row)})

  # Edge Case Error Checking 
  when value-length == 0:
    raise("grouped-bar-chart: can't have empty data")
  end
  when legend-length == 0: 
    raise("grouped-bar-chart: can't have empty legends")
  end
  when label-length <> value-length:
    raise('grouped-bar-chart: labels and values should have the same length')
  end
  when any({(group): legend-length <> group.length()}, value-lists):
    raise('grouped-bar-chart: labels and legends should have the same length')
  end
  
  # Typechecking each input
  value-lists.each(_.each(check-num))
  labels.each(check-string)
  legends.each(check-string)

 {max-positive-height; max-negative-height} = multi-prep-axis(grouped, rational-values)

  # Constructing the Data Series
  data-series = default-multi-bar-chart-series.{
    tab: to-table2(labels, rational-values.map(builtins.raw-array-from-list)),
    axis-top: max-positive-height, 
    axis-bottom: max-negative-height,
    legends: builtins.raw-array-from-list(legends),
    annotations: list-to-table2(value-lists.map(_.map({(_): none}))),
    intervals: list-to-table2(value-lists.map(_.map({(_): [raw-array: ]}))),
  } ^ multi-bar-chart-series

  data-series.make-axis(max-positive-height, max-negative-height)
end

fun stacked-bar-chart-from-list(
  labels :: CL.LoS,
  value-lists :: CL.LoLoN,
  legends :: CL.LoS
) -> DataSeries block:
  doc: ```
       Produces a stacked bar chart where labels are bar stack names, legends are bar names, 
       and value-lists contains the data of each bar seperated into seperate stacks 
       ```
  # Constants
  label-length = labels.length()
  value-length = value-lists.length()
  legend-length = legends.length() 
  rational-values = value-lists.map({(row): map(num-to-rational, row)})

  # Edge Case Error Checking 
  when value-length == 0:
    raise("stacked-bar-chart: can't have empty data")
  end
  when legend-length == 0: 
    raise("stacked-bar-chart: can't have empty legends")
  end
  when label-length <> value-length:
    raise('stacked-bar-chart: labels and values should have the same length')
  end
  when any({(stack): legend-length <> stack.length()}, value-lists):
    raise('stacked-bar-chart: labels and legends should have the same length')
  end
  
  # Typechecking the input 
  value-lists.each(_.each(check-num))
  labels.each(check-string)
  legends.each(check-string)

  {max-positive-height; max-negative-height} = multi-prep-axis(absolute, rational-values)

  # Constructing the Data Series
  data-series = default-multi-bar-chart-series.{
    tab: to-table2(labels, rational-values.map(builtins.raw-array-from-list)),
    legends: builtins.raw-array-from-list(legends),
    annotations: list-to-table2(value-lists.map(_.map({(_): none}))),
    intervals: list-to-table2(value-lists.map(_.map({(_): [raw-array: ]}))),
    is-stacked: 'absolute'
  } ^ multi-bar-chart-series

  data-series.make-axis(max-positive-height, max-negative-height)
end

fun map5<a, b, c, d, e, f>(f :: (a, b, c, d, e -> f), l1 :: List<a>, l2 :: List<b>, l3 :: List<c>, l4 :: List<d>, l5 :: List<e>) -> List<f>:
  doc: "Returns a list made up of f(e1, e2, e3, e4, e5) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4, e5 in l5"
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4) or is-empty(l5):
    empty
  else:
    f(l1.first, l2.first, l3.first, l4.first, l5.first) ^ link(_, map5(f, l1.rest, l2.rest, l3.rest, l4.rest, l5.rest))
  end
end

fun get-interval-point(x :: Number, y :: Number, delta :: Number, label :: String, optimg :: Option<IM.Image>) -> IntervalPoint:
  { x: x, y: y, delta: delta, label: label, image: optimg }
end

fun labeled-interval-chart-from-list(
  labels :: CL.LoS,
  xs :: CL.LoN,
  ys :: CL.LoN,
  deltas :: CL.LoN
) -> DataSeries block:
  doc: ```
       Consumes a list of labels, a list of x's, a list of y's, and a list of deltas
       and constructs an line plot with stick intervals pointing
       from each y to the corresponding delta
       ```
  labels-length = labels.length()
  xs-length = xs.length()
  ys-length = ys.length()
  deltas-length = deltas.length()
  when xs-length <> ys-length:
    raise('interval-chart: xs and ys should have the same length')
  end
  when xs-length <> deltas-length:
    raise('interval-chart: deltas should have the same length as xs and ys')
  end
  when xs-length <> labels-length:
    raise('interval-chart: labels should have the same length as xs and ys')
  end
  when xs-length == 0:
    raise('interval-chart: need at least one datum')
  end
  xs.each(check-num)
  ys.each(check-num)
  deltas.each(check-num)

  default-interval-chart-series.{
    ps: map5(get-interval-point, xs, ys, deltas, labels, xs.map({(_): none}))
  } ^ interval-chart-series
end

fun interval-chart-from-list(xs :: CL.LoN, ys :: CL.LoN, deltas :: CL.LoN) -> DataSeries:
  labeled-interval-chart-from-list(xs.map({(_): ''}), xs, ys, deltas)
end

fun box-plot-from-list(values :: CL.LoLoN) -> DataSeries:
  doc: "Consume values, a list of list of numbers and construct a box chart"
  labels = for map_n(i from 1, _ from values): [sprintf: 'Box ', i] end
  labeled-box-plot-from-list(labels, values)
end

fun labeled-box-plot-from-list(
  labels :: CL.LoS,
  values :: CL.LoLoN
) -> DataSeries block:
  doc: ```
       Consume labels, a list of string, and values, a list of list of numbers
       and construct a box chart
       ```
  label-length = labels.length()
  value-length = values.length()
  when label-length <> value-length:
    raise('labeled-box-plot: labels and values should have the same length')
  end
  when label-length == 0:
    raise('labeled-box-plot: expect at least one box')
  end
  values.each(_.each(check-num))
  values.each(
    lam(lst):
      when lst.length() <= 1:
        raise('labeled-box-plot: the list length should be at least 2')
      end
    end)
  labels.each(check-string)

  max-height = for fold(cur from values.first.first, lst from values):
    num-max(lst.rest.foldl(num-max, lst.first), cur)
  end
  min-height = for fold(cur from values.first.first, lst from values):
    num-max(lst.rest.foldl(num-min, lst.first), cur)
  end

  default-box-plot-series.{
    tab: map2(get-box-data, labels, values) ^ builtins.raw-array-from-list,
    height: num-ceiling(max-height + ((max-height - min-height) / 5)),
    values: values,
  } ^ box-plot-series
end

fun freq-bar-chart-from-list(label :: CL.LoS) -> DataSeries:
  dict = for fold(prev from [SD.string-dict: ], e from label):
    prev.set(e, prev.get(e).or-else(0) + 1)
  end
  {ls; vs; _} = for fold({ls; vs; seen} from {empty; empty; S.empty-tree-set},
      e from label):
    if seen.member(e):
      {ls; vs; seen}
    else:
      {link(e, ls); link(dict.get-value(e), vs); seen.add(e)}
    end
  end
  bar-chart-from-list(ls.reverse(), vs.reverse())
end

fun histogram-from-list(values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume a list of numbers and construct a histogram
       ```
  values.each(check-num)
  default-histogram-series.{
    tab: to-table2(values.map({(_): ''}), values),
  } ^ histogram-series
end

fun labeled-histogram-from-list(labels :: CL.LoS, values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume a list of strings and a list of numbers and construct a histogram
       ```
  label-length = labels.length()
  value-length = values.length()
  when label-length <> value-length:
    raise('labeled-histogram: labels and values should have the same length')
  end
  values.each(check-num)
  labels.each(check-string)
  default-histogram-series.{
    tab: to-table2(labels, values),
  } ^ histogram-series
end

fun image-histogram-from-list(images :: CL.LoI, values :: CL.LoN) -> DataSeries block:
  doc: ```
       Consume images and numbers, then construct a histogram matching those
       images to the original histogram bricks
       ```
  # Type Checking
  images.each(check-image)
  values.each(check-num)

  default-histogram-series.{
    tab: to-table3(values.map({(_): ''}), values, images),
  } ^ histogram-series
end

################################################################################
# PLOTS
################################################################################

fun check-render-x-axis(self) -> Nothing:
  cases (Option) self.x-min:
    | some(x-min) =>
      cases (Option) self.x-max:
        | some(x-max) =>
          if x-min >= x-max:
            raise("render: x-min must be strictly less than x-max")
          else:
            nothing
          end
        | else => nothing
      end
    | else => nothing
  end
end

fun check-render-y-axis(self) -> Nothing:
  cases (Option) self.y-min:
    | some(y-min) =>
      cases (Option) self.y-max:
        | some(y-max) =>
          if y-min >= y-max:
            raise("render: y-min must be strictly less than y-max")
          else:
            nothing
          end
        | else => nothing
      end
    | else => nothing
  end
end

fun render-chart(s :: DataSeries) -> ChartWindow:
  doc: 'Render it!'
  cases (DataSeries) s:
    | line-plot-series(_) => render-charts([list: s])
    | function-plot-series(_) => render-charts([list: s])
    | scatter-plot-series(_) => render-charts([list: s])
    | interval-chart-series(_) => render-charts([list: s])
    | dot-plot-series(obj) =>
      default-dot-chart-window-object.{
        method render(self) block:
          CL.dot-chart(self, obj) 
        end
      } ^ dot-chart-window
    | pie-chart-series(obj) =>
      default-pie-chart-window-object.{
        method render(self): CL.pie-chart(self, obj) end
      } ^ pie-chart-window
    | bar-chart-series(obj) =>
      default-bar-chart-window-object.{
        method render(self):
          _ = check-render-y-axis(self)
          CL.bar-chart(self, obj)
        end
      } ^ bar-chart-window
    | multi-bar-chart-series(obj) => 
      default-bar-chart-window-object.{
        method render(self):
          _ = check-render-y-axis(self)
          CL.multi-bar-chart(self, obj)
        end
      } ^ bar-chart-window
    | box-plot-series(obj) =>
      default-box-plot-chart-window-object.{
        method render(self):
          CL.box-plot(self, obj)
        end
      } ^ box-plot-chart-window
    | histogram-series(obj) =>
      default-histogram-chart-window-object.{
        method render(self):
          shadow self = self.{y-min: none}
          _ = check-render-x-axis(self)
          _ = check-render-y-axis(self)
          CL.histogram(self, obj)
        end
      } ^ histogram-chart-window
  end
where:
  render-now = {(x): render-chart(x).get-image()}

  render-now(from-list.exploding-pie-chart(
      [list: 'asd', 'dsa', 'qwe'],
      [list: 1, 2, 3],
      [list: 0, 0.1, 0.2])) does-not-raise
  render-now(from-list.pie-chart([list: 'asd', 'dsa', 'qwe'], [list: 1, 2, 3])) does-not-raise
  render-now(from-list.histogram([list: 1, 1.2, 2, 3, 10, 3, 6, -1])) does-not-raise
  render-now(from-list.labeled-histogram(
      [list: 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'],
      [list: 1, 1.2, 2, 3, 10, 3, 6, -1])) does-not-raise
  render-now(from-list.grouped-bar-chart(
      [list: 'CA', 'TX', 'NY', 'FL', 'IL', 'PA'],
      [list:
        [list: 2704659,4499890,2159981,3853788,10604510,8819342,4114496],
        [list: 2027307,3277946,1420518,2454721,7017731,5656528,2472223],
        [list: 1208495,2141490,1058031,1999120,5355235,5120254,2607672],
        [list: 1140516,1938695,925060,1607297,4782119,4746856,3187797],
        [list: 894368,1558919,725973,1311479,3596343,3239173,1575308],
        [list: 737462,1345341,679201,1203944,3157759,3414001,1910571]],
      [list:
        'Under 5 Years',
        '5 to 13 Years',
        '14 to 17 Years',
        '18 to 24 Years',
        '25 to 44 Years',
        '45 to 64 Years',
        '65 Years and Over'])) does-not-raise
  render-now(from-list.stacked-bar-chart(
      [list: 'CA', 'TX', 'NY', 'FL', 'IL', 'PA'],
      [list:
        [list: 2704659,4499890,2159981,3853788,10604510,8819342,4114496],
        [list: 2027307,3277946,1420518,2454721,7017731,5656528,2472223],
        [list: 1208495,2141490,1058031,1999120,5355235,5120254,2607672],
        [list: 1140516,1938695,925060,1607297,4782119,4746856,3187797],
        [list: 894368,1558919,725973,1311479,3596343,3239173,1575308],
        [list: 737462,1345341,679201,1203944,3157759,3414001,1910571]],
      [list:
        'Under 5 Years',
        '5 to 13 Years',
        '14 to 17 Years',
        '18 to 24 Years',
        '25 to 44 Years',
        '45 to 64 Years',
        '65 Years and Over'])) does-not-raise
  render-now(from-list.function-plot(num-sin)) does-not-raise
  render-now(from-list.scatter-plot(
      [list: 1, 1, 4, 7, 4, 2],
      [list: 2, 3.1, 1, 3, 6, 5])) does-not-raise
  render-now(from-list.line-plot(
      [list: 1, 1, 4, 7, 4, 2],
      [list: 2, 3.1, 1, 3, 6, 5])) does-not-raise
  render-now(from-list.box-plot(
      [list: [list: 1, 2, 3, 4], [list: 1, 2, 3, 4, 5], [list: 10, 11]]
    )) does-not-raise
end

fun generate-xy(
    p :: FunctionPlotSeries,
    x-min :: Number,
    x-max :: Number,
    num-samples :: Number) -> ScatterPlotSeries:
  doc: 'Generate a scatter-plot from an function-plot'
  fraction = (x-max - x-min) / (num-samples - 1)

  ps = for filter-map(i from range(0, num-samples)):
    x = x-min + (fraction * i)
    cases (E.Either) run-task({(): p.f(x)}):
      | left(y) => some([raw-array: x, y])
      | right(_) => none
    end
  end

  default-scatter-plot-series.{
    ps: ps,
    point-size: FUNCTION-POINT-SIZE,
    color: p.color,
    legend: p.legend,
  }
where:
  generate-xy(from-list.function-plot(_ + 1).obj, 0, 100, 6).ps
    is=~ [list:
    posn(0, 1),
    posn(20, 21),
    posn(40, 41),
    posn(60, 61),
    posn(80, 81),
    posn(100, 101) # out of bound, will be filtered later
  ]
end

fun widen-range(min :: Number, max :: Number) -> {Number; Number}:
  offset = num-min((max - min) / 40, 1)
  shadow offset = if unsafe-equal(offset, 0): 1 else: offset end
  {min - offset; max + offset}
end

fun ps-to-arr(obj): obj.{ps: obj.ps ^ builtins.raw-array-from-list} end

fun in-bound-x(p :: Posn, self) -> Boolean:
  (self.x-min.value <= fst(p)) and (fst(p) <= self.x-max.value)
end

fun in-bound-y(p :: Posn, self) -> Boolean:
  (self.y-min.value <= snd(p)) and (snd(p) <= self.y-max.value)
end

fun in-bound-xy(p :: Posn, self) -> Boolean:
  in-bound-x(p, self) and in-bound-y(p, self)
end

fun dist(a :: Posn, b :: Posn) -> Number:
  num-sqr(fst(a) - fst(b)) + num-sqr(snd(a) - snd(b))
end

fun nearest(lst :: List<Posn>, p :: Posn) -> Option<Posn>:
  cases (List<Posn>) lst:
    | empty => none
    | link(f, r) =>
      {_; sol} = for fold({best; sol} from {dist(p, f); f}, e from lst):
        new-dist = dist(p, e)
        if new-dist < best:
          {new-dist; e}
        else:
          {best; sol}
        end
      end
      some(sol)
  end
end

fun find-pt-on-edge(in :: Posn, out :: Posn, self) -> Option<Posn>:
  px-max = num-min(num-max(fst(in), fst(out)), self.x-max.value)
  px-min = num-max(num-min(fst(in), fst(out)), self.x-min.value)
  py-max = num-min(num-max(snd(in), snd(out)), self.y-max.value)
  py-min = num-max(num-min(snd(in), snd(out)), self.y-min.value)

  candidates = if unsafe-equal(fst(in), fst(out)):
    [list: posn(fst(in), self.y-min.value), posn(fst(in), self.y-max.value)]
  else:
    #|
    y = m * x + c           [3]
    y2 = m * x2 + c         [3.1]
    y - y2 = m * (x - x2)   [5]   [by 3 - 3.1]
    m = (y - y2) / (x - x2) [1]   [rewrite 5]
    c = y - m * x           [2]   [rewrite 3]
    x = (y - c) / m         [4]   [rewrite 3]
    |#
    m = (snd(in) - snd(out)) / (fst(in) - fst(out)) # [1]
    c = snd(in) - (m * fst(in)) # [2]
    f = {(x): (m * x) + c} # [3]
    g = {(y): (y - c) / m} # [4]

    [list:
      posn(self.x-min.value, f(self.x-min.value)),
      posn(self.x-max.value, f(self.x-max.value))] +
    if unsafe-equal(m, 0):
      empty
    else:
      [list:
        posn(g(self.y-min.value), self.y-min.value),
        posn(g(self.y-max.value), self.y-max.value)]
    end
  end
  candidates.filter({(p): (px-min <= fst(p)) and (fst(p) <= px-max) and
                          (py-min <= snd(p)) and (snd(p) <= py-max)})
    ^ nearest(_, in)
end

fun line-plot-edge-cut(pts :: List<Posn>, self) -> List<Posn>:
  segments = cases (List<Posn>) pts:
    | empty => empty
    | link(f, r) =>
      {segments; _} = for fold({segments; start} from {empty; f}, stop from r):
        segment = ask:
          | in-bound-xy(start, self) and in-bound-xy(stop, self) then:
            [list: start, stop]
          | in-bound-xy(start, self) then:
            result = find-pt-on-edge(start, stop, self).value
            if unsafe-equal(fst(start), fst(result)) and
               unsafe-equal(snd(start), snd(result)):
              [list: start, find-pt-on-edge(stop, start, self).value]
            else:
              [list: start, result]
            end
          | in-bound-xy(stop, self) then:
            [list: find-pt-on-edge(start, stop, self).value, stop]
          | otherwise:
            cases (Option) find-pt-on-edge(start, stop, self):
              | none => empty
              | some(result) =>
                result2 = find-pt-on-edge(stop, start, self).value
                [list: result, result2]
            end
        end
        cases (List) segment:
          | empty => {segments; stop}
          | link(_, _) => {link(segment, segments); stop}
        end
      end
      segments
  end

  cases (List) segments:
    | empty => empty
    | link(f, r) =>
      {_; result} = for fold({prev; lst} from {f; f}, segment from r):
        pt-a = prev.get(0)
        pt-b = segment.get(1)
        new-lst = if unsafe-equal(fst(pt-a), fst(pt-b)) and unsafe-equal(snd(pt-a), snd(pt-b)):
          link(segment.get(0), lst)
        else:
          segment + link([raw-array: ], lst)
        end
        {segment; new-lst}
      end
      result
  end
end

data BoundResult:
  | exact-bound(n :: Number)
  | inferred-bound(n :: Number)
  | unknown-bound
end

fun bound-result-to-bounds(b-min :: BoundResult, b-max :: BoundResult) -> {Option<Number>; Option<Number>}:
  {l; r} = cases (BoundResult) b-min:
    | exact-bound(v-min) =>
      cases (BoundResult) b-max:
        | exact-bound(v-max) => {v-min; v-max}
        | inferred-bound(v-max) => {v-min; widen-range(v-min, v-max).{1}}
        | unknown-bound => {v-min; v-min + 10}
      end
    | inferred-bound(v-min) =>
      cases (BoundResult) b-max:
        | exact-bound(v-max) => {widen-range(v-min, v-max).{0}; v-max}
        | inferred-bound(v-max) => widen-range(v-min, v-max)
        | unknown-bound => {v-min - 1; (v-min - 1) + 10}
      end
    | unknown-bound =>
      cases (BoundResult) b-max:
        | exact-bound(v-max) => {v-max - 10; v-max}
        | inferred-bound(v-max) => {(v-max + 1) - 10; v-max + 1}
        | unknown-bound => {-10; 10}
      end
  end
  {some(l); some(r)}
end

fun get-bound-result(
  d :: Option<Number>,
  bbox :: BoundingBox,
  f :: (BoundingBox -> Number)
) -> BoundResult:
  cases (Option) d:
    | none => if bbox.is-valid: inferred-bound(f(bbox)) else: unknown-bound end
    | some(v) => exact-bound(v)
  end
end

fun render-charts(lst :: List<DataSeries>) -> ChartWindow block:
  doc: "Draw 'em all"
  cases (Option) find(_.is-single, lst):
    | some(v) => raise(
        [sprintf: "render-charts: can't draw ", v,
                  " with `render-charts`. Use `render-chart` instead."])
    | else => nothing
  end
  cases (List<DataSeries>) lst:
    | empty => raise('render-charts: need at least one series to plot')
    | else => nothing
  end
  when lst.any({(ds): ds.obj.horizontal}) and not(lst.all({(ds): ds.obj.horizontal})):
    raise("render-charts: Cannot render a mix of horizontal and vertical charts.  "
        + "Make sure all charts have the same direction.")
  end
    
  partitioned = partition(is-function-plot-series, lst)
  function-plots = partitioned.is-true.map(_.obj)
  is-show-samples = is-link(function-plots)
  shadow partitioned = partition(is-line-plot-series, partitioned.is-false)
  line-plots = partitioned.is-true.map(_.obj)
  shadow partitioned = partition(is-scatter-plot-series, partitioned.is-false)
  scatter-plots = partitioned.is-true.map(_.obj)
  interval-plots = partitioned.is-false.map(_.obj)

  default-plot-chart-window-object.{
    method render(self):
      shadow self = self.{is-show-samples: is-show-samples}

      # don't let Google Charts infer x-min, x-max, y-min, y-max
      # infer them from Pyret side

      _ = check-render-x-axis(self)
      _ = check-render-y-axis(self)

      bboxes-ls = get-list-of-bounding-boxes(empty #|line-plots.map(_.ps) + scatter-plots.map(_.ps)|#, self, snd)

      # i-xyy = interval-plots.map(_.bothys) # list of list of xyy arrays

      # bboxes-i-1 = get-list-of-bounding-boxes(i-xyy, self, snd)
         
      # bboxes-i-2 = get-list-of-bounding-boxes(i-xyy, self, thd)

      # bbox = (bboxes-ls.append(bboxes-i-1).append(bboxes-i-2)) ^ merge-bounding-box

      # {x-min; x-max} = bound-result-to-bounds(
      #   get-bound-result(self.x-min, bbox, _.x-min),
      #   get-bound-result(self.x-max, bbox, _.x-max))

      # shadow self = self.{x-min: x-min, x-max: x-max}

      # function-plots-data = function-plots
      #   .map(generate-xy(_, self.x-min.value, self.x-max.value, self.num-samples))

      # bbox-combined = link(bbox, function-plots-data.map(_.ps).map(get-bounding-box))
      #   ^ merge-bounding-box

      # {y-min; y-max} = bound-result-to-bounds(
      #   get-bound-result(self.y-min, bbox-combined, _.y-min),
      #   get-bound-result(self.y-max, bbox-combined, _.y-max))

      # shadow self = self.{y-min: y-min, y-max: y-max}

      fun helper(shadow self, shadow function-plots-data :: Option) -> IM.Image:
        shadow function-plots-data = cases (Option) function-plots-data:
          | none => function-plots
              .map(generate-xy(_, self.x-min.value, self.x-max.value, self.num-samples))
          | some(shadow function-plots-data) => function-plots-data
        end

        # scatters-arr = for map(p from scatter-plots + function-plots-data):
        #   ps-to-arr(p.{ps: p.ps.filter(in-bound-xy(_, self))})
        # end ^ reverse ^ builtins.raw-array-from-list

        # lines-arr = for map(p from line-plots):
        #   ps-to-arr(p.{ps: line-plot-edge-cut(p.ps, self)})
        # end ^ reverse ^ builtins.raw-array-from-list

        # intervals-arr = for map(p from interval-plots):
        #   ps-to-arr(p.{ps: p.ps.filter(in-bound-xy(_, self))})
        # end ^ reverse ^ builtins.raw-array-from-list

        ret = CL.plot(self, {
          scatters: builtins.raw-array-from-list(scatter-plots), 
          lines: builtins.raw-array-from-list(line-plots),
          intervals: builtins.raw-array-from-list(interval-plots),
          functions: builtins.raw-array-from-list(function-plots)
        })
        # NOTE(Ben): THIS IS A POLYFILL FOR NOW,
        # until I remove the Either callback hooking mechanism altogether.
        if (IM.is-image(ret)): ret
        else:
          cases (E.Either<Any, IM.Image>) ret:
            | left(new-self) => helper(new-self, none)
            | right(image) => image
          end
        end
      end
      helper(self, some(empty))
    end
  } ^ plot-chart-window
where:
  p1 = from-list.function-plot(lam(x): x * x end).color(I.red)
  p2 = from-list.line-plot([list: 1, 2, 3, 4], [list: 1, 4, 9, 16]).color(I.green)
  p3 = from-list.histogram([list: 1, 2, 3, 4])
  p4 = from-list.line-plot(
      [list: -1, 1,  2, 3, 11, 8, 9],
      [list: 10, -1, 11, 9,  9, 3, 2])
  render-charts([list: p1, p2, p3]) raises ''
  render-charts([list: p1, p2])
    .title('quadratic function and a scatter plot')
    .x-min(0)
    .x-max(20)
    .y-min(0)
    .y-max(20)
    .get-image() does-not-raise
  render-charts([list: p4])
    .x-min(0)
    .x-max(10)
    .y-min(0)
    .y-max(10)
    .get-image() does-not-raise
end

from-list = {
  line-plot: line-plot-from-list,
  labeled-line-plot: labeled-line-plot-from-list,
  image-line-plot: image-line-plot-from-list,
  labeled-scatter-plot: labeled-scatter-plot-from-list,
  image-scatter-plot: image-scatter-plot-from-list,
  scatter-plot: scatter-plot-from-list,
  function-plot: function-plot-from-list,
  histogram: histogram-from-list,
  labeled-histogram: labeled-histogram-from-list,
  image-histogram: image-histogram-from-list,
  pie-chart: pie-chart-from-list,
  exploding-pie-chart: exploding-pie-chart-from-list,
  image-pie-chart: image-pie-chart-from-list,
  bar-chart: bar-chart-from-list,
  dot-chart: dot-chart-from-list,
  num-dot-chart: num-dot-chart-from-list,
  image-num-dot-chart: image-num-dot-chart-from-list,
  labeled-num-dot-chart: labeled-num-dot-chart-from-list,
  image-bar-chart: image-bar-chart-from-list,
  grouped-bar-chart: grouped-bar-chart-from-list,
  stacked-bar-chart: stacked-bar-chart-from-list,
  freq-bar-chart: freq-bar-chart-from-list,
  labeled-box-plot: labeled-box-plot-from-list,
  box-plot: box-plot-from-list,
  interval-chart: interval-chart-from-list,
  labeled-interval-chart: labeled-interval-chart-from-list,
}
