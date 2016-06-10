provide {
  line-plot: line-plot,
  is-line-plot: is-line-plot,

  scatter-plot: scatter-plot,
  is-scatter-plot: is-scatter-plot,

  xy-plot: xy-plot,
  is-xy-plot: is-xy-plot,

  plot-xy: plot-xy,
  plot-scatter: plot-scatter,
  plot-line: plot-line,

  plot-multi: plot-multi,
  default-options: default-options,

  histogram: histogram,
  pie-chart: pie-chart
} end

# provide *

provide-types {
  Plot :: Plot,
  PlotOptions :: WrappedPlotOptions,
  PlotWindowOptions :: WrappedPlotWindowOptions
}

import plot-lib as P
import either as E
import string-dict as SD
import image-structs as I
import tables as T

## for debugging
# P2 = P

## in the console
# import plot as PP
# P = PP.P2

OFFSET = 1
MAX-SAMPLES = 100000

type PlotOptions = {
  color :: I.Color
}

plot-options :: PlotOptions = {
  color: I.blue
}

type PlotWindowOptions = {
  x-min :: Number,
  x-max :: Number,
  y-min :: Number,
  y-max :: Number,
  num-samples :: Number,
  infer-bounds :: Boolean
}

plot-window-options :: PlotWindowOptions = {
  x-min: -10,
  x-max: 10,
  y-min: -10,
  y-max: 10,
  num-samples: 1000,
  infer-bounds: false
}

type WrappedPlotOptions = (PlotOptions -> PlotOptions)
type WrappedPlotWindowOptions = (PlotWindowOptions -> PlotWindowOptions)
type PlottableFunction = (Number -> Number)
type Table = T.Table
type Posn = RawArray<Number>
type TableInt = RawArray<Posn>

data Plot:
  | line-plot(points :: Table, options :: WrappedPlotOptions)
  | scatter-plot(points :: Table, options :: WrappedPlotOptions)
  | xy-plot(f :: PlottableFunction, options :: WrappedPlotOptions)
end

data PlotInternal:
  | line-plot-int(points :: TableInt, options :: PlotOptions)
  | scatter-plot-int(points :: TableInt, options :: PlotOptions)
  | xy-plot-int(f :: PlottableFunction, options :: PlotOptions)
end

default-options = lam<A>(x :: A): x end
empty-posn = [raw-array: 0, 0]

data Pair<a, b>:
  | pair(first :: a, second :: b)
end

fun posn(x :: Number, y :: Number) -> Posn:
  [raw-array: x, y]
end

fun histogram(tab :: Table, n :: Number) -> Table:
  doc: "Consume a table with one column: `value`, and a number of bins, and show a histogram"
  when not(tab._header-raw-array =~ [raw-array: "value"]):
    raise("histogram: expect a table with a column named `value`")
  end
  when (n < 1) or (n > 100) or not(num-is-integer(n)):
    raise("histogram: expect `n` to be an integer between 1 and 100 (inclusive)")
  end
  when raw-array-length(tab._rows-raw-array) == 0:
    raise("histogram: expect the table to have at least one row")
  end
  P.histogram(tab._rows-raw-array, n)
  tab
end

fun pie-chart(tab :: Table) -> Table:
  doc: "Consume a table with two columns: `label` and `value`, and show a pie-chart"
  when not(tab._header-raw-array =~ [raw-array: "label", "value"]):
    raise("pie-chart: expect a table with columns named `label` and `value`")
  end
  P.pie-chart(tab._rows-raw-array)
  tab
end

fun raw-array-from-list<A>(lst :: List<A>) -> RawArray<A>:
  cases (List<A>) lst:
    | empty => [raw-array: ]
    | link(f, _) =>
      arr = raw-array-of(f, lst.length())
      for each_n(i from 0, e from lst):
        raw-array-set(arr, i, e)
      end
      arr
  end
where:
  raw-array-from-list(empty) is=~ [raw-array: ]
  raw-array-from-list([list: 1, 2, 3]) is=~ [raw-array: 1, 2, 3]
end

fun generate-xy(plot :: PlotInternal, win-opt :: PlotWindowOptions) -> PlotInternal:
  doc: "Generate a scatter-plot from an xy-plot"
  fraction = (win-opt.x-max - win-opt.x-min) / (win-opt.num-samples - 1)
  cases (PlotInternal) plot:
    | xy-plot-int(f, options) =>
      range(0, win-opt.num-samples)
        .map(
        lam(i :: Number) -> Option<Posn>:
          x = win-opt.x-min + (fraction * i)
          cases (E.Either) run-task(lam(): f(x) end):
            | left(y) => some([raw-array: x, y])
            | right(v) => none
          end
        end)
        .filter(is-some)
        .map(_.or-else(empty-posn))
        ^ raw-array-from-list
        ^ scatter-plot-int(_, options)
    | else => raise("plot: expect xy-plot, got other")
  end
where:
  win-options = {
    x-min: 0,
    x-max: 100,
    y-min: 0,
    y-max: 100,
    num-samples: 6,
    infer-bounds: false
  }
  generate-xy(xy-plot-int(_ + 1, plot-options), win-options)
    is scatter-plot-int([raw-array:
      posn(0, 1),
      posn(20, 21),
      posn(40, 41),
      posn(60, 61),
      posn(80, 81),
      posn(100, 101) # out of bound, will be filtered later
    ], plot-options)
end

fun plot-xy(f :: PlottableFunction) -> PlottableFunction:
  plot-multi([list: xy-plot(f, default-options)], default-options)
  f
end

fun plot-scatter(tab :: Table) -> Table:
  plot-multi([list: scatter-plot(tab, default-options)], default-options)
  tab
end

fun plot-line(tab :: Table) -> Table:
  plot-multi([list: line-plot(tab, default-options)], default-options)
  tab
end

fun plot-multi(plots :: List<Plot>, options-generator :: WrappedPlotWindowOptions) -> List<Plot>:
  options = options-generator(plot-window-options)
  when (options.x-min >= options.x-max) or (options.y-min >= options.y-max):
    raise("plot: x-min and y-min must be strictly less than x-max and y-max respectively")
  end
  when (options.num-samples > MAX-SAMPLES) or
       (options.num-samples <= 1) or
       not(num-is-integer(options.num-samples)):
    raise("plot: num-samples must be an an integer greater than 1 and do not exceed " + num-to-string(MAX-SAMPLES))
  end

  original-plots = plots
  shadow plots = plots.map(
    lam(plot :: Plot) -> PlotInternal:
      cases (Plot) plot:
        | scatter-plot(points, opt-gen) =>
          when not(points._header-raw-array =~ [raw-array: "x", "y"]):
            raise("plot: expect a table with two columns: `x` and `y`")
          end
          scatter-plot-int(points._rows-raw-array, opt-gen(plot-options).{opacity: 80,  size: 4, tip: true})
        | line-plot(points, opt-gen) =>
          when not(points._header-raw-array =~ [raw-array: "x", "y"]):
            raise("plot: expect a table with two columns: `x` and `y`")
          end
          line-plot-int(points._rows-raw-array, opt-gen(plot-options).{opacity: 100, size: 1, tip: false})
        | xy-plot(f, opt-gen) =>
          xy-plot-int(f, opt-gen(plot-options).{opacity: 100, size: 1, tip: false})
      end
    end)

  partitioned = partition(is-xy-plot-int, plots)
  xy-plots = partitioned.is-true
  line-and-scatter = partitioned.is-false

  fun bound(points :: RawArray<Posn>,
      base :: Posn,
      op :: (Number, Number -> Number),
      accessor :: (Posn -> Number)) -> Number:
    for raw-array-fold(acc from accessor(base), p from points, _ from 0):
      op(accessor(p), acc)
    end
  end

  shadow options = if options.infer-bounds:
    points = line-and-scatter.map(
      lam(plot :: PlotInternal, acc :: Number):
        cases (PlotInternal) plot:
          | xy-plot-int(_, _) => raise("plot: plot: xy-plot not filtered")
          | line-plot-int(points, _) => points
          | scatter-plot-int(points, _) => points
        end
      end).foldl(_ + _, empty)

    cases (List) points:
      | empty => options
      | link(f, r) =>
        win-opt-ret :: PlotWindowOptions = {
          x-min: bound(r, f, num-min, _.x) - OFFSET,
          x-max: bound(r, f, num-max, _.x) + OFFSET,
          y-min: bound(r, f, num-min, _.y) - OFFSET,
          y-max: bound(r, f, num-max, _.y) + OFFSET,
          num-samples: options.num-samples,
          infer-bounds: false
        }
        win-opt-ret
    end
  else:
    options
  end

  shadow partitioned = partition(is-line-plot-int, line-and-scatter)
  line-plots = partitioned.is-true
  scatter-plots = partitioned.is-false

  fun helper(shadow options :: PlotWindowOptions) -> List<Plot>:
    shadow xy-plots = xy-plots.map(generate-xy(_, options))
    maybe-new-options = P.plot-multi(xy-plots + scatter-plots, line-plots, options)
    cases (Option<PlotWindowOptions>) maybe-new-options:
      | none => original-plots
      | some(new-options) => helper(new-options)
    end
  end

  helper(options)
end
