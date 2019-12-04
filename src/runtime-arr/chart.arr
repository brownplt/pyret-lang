provide *
provide-types *

import global as G
import chart-lib as CL
import either as E
import image as IM
import list as L
import option as O

include from O: type Option end

################################################################################
# TYPE SYNONYMS
################################################################################

type PlottableFunction = (Number -> Number)
type Posn = RawArray<Number>
type TableIntern = RawArray<RawArray<Any>>

################################################################################
# HELPERS
################################################################################

posn = {(x :: Number, y :: Number): [G.raw-array: x, y]}

fun map2(xs :: L.List<Any>, ys :: L.List<Any>):
  L.map2({(x, y): [G.raw-array: x, y]}, xs, ys)
end

fun raw-array-from-list(l :: L.List<Any>) -> RawArray<Any>:
  L.to-raw-array(l)
end

fun to-table2(xs :: L.List<Any>, ys :: L.List<Any>) -> TableIntern:
  L.to-raw-array(L.map2({(x, y): [G.raw-array: x, y]}, xs, ys))
end

fun to-table3(xs :: L.List<Any>, ys :: L.List<Any>, zs :: L.List<Any>) -> TableIntern:
  L.to-raw-array(L.map3({(x, y, z): [G.raw-array: x, y, z]}, xs, ys, zs))
end

# TODO(tiffany): add in get-vs-from-img after VS is implemented

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

fun compute-min(ps :: RawArray<Number>) -> Number:
  G.raw-array-min(ps)
end

fun compute-max(ps :: RawArray<Number>) -> Number:
  G.raw-array-max(ps)
end

fun get-bounding-box(ps :: L.List<Posn>) -> BoundingBox:
  if L.length(ps) == 0:
    default-bounding-box.{is-valid: false}
  else:
    x-arr = G.raw-array-get(ps, 0)
    y-arr = G.raw-array-get(ps, 1)
    default-bounding-box.{
      x-min: compute-min(x-arr),
      x-max: compute-max(x-arr),
      y-min: compute-min(y-arr),
      y-max: compute-max(y-arr),
      is-valid: true,
    }
  end
end

fun merge-bounding-box(bs :: L.List<BoundingBox>) -> BoundingBox:
  for L.fold(prev from default-bounding-box, e from bs):
    ask:
      | e.is-valid and prev.is-valid then:
        default-bounding-box.{
          x-min: G.num-min(e.x-min, prev.x-min),
          x-max: G.num-max(e.x-max, prev.x-max),
          y-min: G.num-min(e.y-min, prev.y-min),
          y-max: G.num-max(e.y-max, prev.y-max),
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
  horizontal :: Boolean
}

default-box-plot-series = {
  horizontal: false
}

type PieChartSeries = {
  tab :: TableIntern,
}

default-pie-chart-series = {}

type BarChartSeries = {
  tab :: TableIntern,
  legends :: RawArray<String>,
  has-legend :: Boolean,
}

default-bar-chart-series = {}

type HistogramSeries = {
  tab :: TableIntern,
  bin-width :: Option<Number>,
  max-num-bins :: Option<Number>,
  min-num-bins :: Option<Number>,
}

default-histogram-series = {
  bin-width: O.none,
  max-num-bins: O.none,
  min-num-bins: O.none,
}

type LinePlotSeries = {
  ps :: L.List<Posn>,
  color :: Option<IM.Color>,
  legend :: String,
}

default-line-plot-series = {
  color: O.none,
  legend: '',
}

type ScatterPlotSeries = {
  ps :: L.List<Posn>,
  color :: Option<IM.Color>,
  legend :: String,
  point-size :: Number,
}

default-scatter-plot-series = {
  color: O.none,
  legend: '',
  point-size: 7,
}

type FunctionPlotSeries = {
  f :: PlottableFunction,
  color :: Option<IM.Color>,
  legend :: String,
}

default-function-plot-series = {
  color: none,
  legend: '',
}

###########

type ChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  render :: ( -> IM.Image)
}

default-chart-window-object :: ChartWindowObject = {
  title: '',
  width: 800,
  height: 600,
  render: method(self): G.raise('unimplemented') end,
}

type BoxChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  x-axis :: String,
  y-axis :: String,
  render :: ( -> IM.Image),
}

default-box-plot-chart-window-object :: BoxChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
}

type PieChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  render :: ( -> IM.Image),
}

default-pie-chart-window-object :: PieChartWindowObject = default-chart-window-object

type BarChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String,
  y-min :: O.Option<Number>,
  y-max :: O.Option<Number>,
}

default-bar-chart-window-object :: BarChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  y-min: O.none,
  y-max: O.none,
}

type HistogramChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
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
    x-min: O.none,
    x-max: O.none,
    y-max: O.none,
  }

type PlotChartWindowObject = {
  title :: String,
  width :: Number,
  height :: Number,
  render :: ( -> IM.Image),
  x-axis :: String,
  y-axis :: String,
  x-min :: Option<Number>,
  x-max :: Option<Number>,
  x-max :: Option<Number>,
  y-max :: Option<Number>,
  num-samples :: Number,
}

default-plot-chart-window-object :: PlotChartWindowObject = default-chart-window-object.{
  x-axis: '',
  y-axis: '',
  x-min: O.none,
  x-max: O.none,
  y-min: O.none,
  y-max: O.none,
  num-samples: 1000,
}

################################################################################
# DATA DEFINITIONS
################################################################################

data DataSeries:
  | scatter-plot-series(obj :: ScatterPlotSeries) with:
    is-single: false,
    color: method(self, color :: IM.Color):
      scatter-plot-series(self.obj.{color: O.some(color)})
    end,
    legend: method(self, legend :: String):
      scatter-plot-series(self.obj.{legend: legend})
    end,
    point-size: method(self, point-size :: Number):
      scatter-plot-series(self.obj.{point-size: point-size})
    end,
  | pie-chart-series(obj :: PieChartSeries) with:
    is-single: true,
  | bar-chart-series(obj :: BarChartSeries) with:
    is-single: true,
  # TODO(tiffany): add box-plot-series in by adding a |
  # box-plot-series(obj :: BoxChartSeries) with:
  #  is-single: true,
  #  horizontal: method(self, h):
  #    box-plot-series(self.obj.{horizontal: h})
  #  end
  | histogram-series(obj :: HistogramSeries) with:
    is-single: true,
    method bin-width(self, bin-width :: Number):
      histogram-series(self.obj.{bin-width: O.some(bin-width)})
    end,
    method max-num-bins(self, max-num-bins :: Number):
      histogram-series(self.obj.{max-num-bins: O.some(max-num-bins)})
    end,
    method min-num-bins(self, min-num-bins :: Number):
      histogram-series(self.obj.{min-num-bins: O.some(min-num-bins)})
    end,
    method num-bins(self, num-bins :: Number):
      histogram-series(self.obj.{
        min-num-bins: O.some(num-bins),
        max-num-bins: O.some(num-bins)
      })
    end,
# TODO(tiffany): add _output and test get-vs-from-img after VS is implemented
end

fun check-chart-window(p :: ChartWindowObject) -> Nothing:
  if (p.width <= 0) or (p.height <= 0):
    G.raise('render: width and height must be positive')
  else:
    G.nothing
  end
end

data ChartWindow:
  | pie-chart-window(obj :: PieChartWindowObject) with:
    title: method(self, title :: String): pie-chart-window(self.obj.{title: title}) end,
    width: method(self, width :: Number): pie-chart-window(self.obj.{width: width}) end,
    height: method(self, height :: Number): pie-chart-window(self.obj.{height: height}) end,
    display: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: true}.render()
    end,
    get-image: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: false}.render()
    end,
  # TODO(tiffany): add box-plot-chart-window in with a |
  # box-plot-chart-window(obj :: BoxChartWindowObject) with:
  #  x-axis: method(self, x-axis :: String): box-plot-chart-window(self.obj.{x-axis: x-axis}) end,
  #  y-axis: method(self, y-axis :: String): box-plot-chart-window(self.obj.{y-axis: y-axis}) end,
  #  title: method(self, title :: String): box-plot-chart-window(self.obj.{title: title}) end,
  #  width: method(self, width :: Number): box-plot-chart-window(self.obj.{width: width}) end,
  #  height: method(self, height :: Number): box-plot-chart-window(self.obj.{height: height}) end,
  #  display: method(self):
  #    _ = check-chart-window(self.obj)
  #    self.obj.{interact: true}.render()
  #  end,
  #  get-image: method(self):
  #    _ = check-chart-window(self.obj)
  #    self.obj.{interact: false}.render()
  #  end,
  | bar-chart-window(obj :: BarChartWindowObject) with:
    x-axis: method(self, x-axis :: String): bar-chart-window(self.obj.{x-axis: x-axis}) end,
    y-axis: method(self, y-axis :: String): bar-chart-window(self.obj.{y-axis: y-axis}) end,
    y-min: method(self, y-min :: Number): bar-chart-window(self.obj.{y-min: O.some(y-min)}) end,
    y-max: method(self, y-max :: Number): bar-chart-window(self.obj.{y-max: O.some(y-max)}) end,
    title: method(self, title :: String): bar-chart-window(self.obj.{title: title}) end,
    width: method(self, width :: Number): bar-chart-window(self.obj.{width: width}) end,
    height: method(self, height :: Number): bar-chart-window(self.obj.{height: height}) end,
    display: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: true}.render()
    end,
    get-image: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: false}.render()
    end,
  | histogram-chart-window(obj :: HistogramChartWindowObject) with:
    x-axis: method(self, x-axis :: String): histogram-chart-window(self.obj.{x-axis: x-axis}) end,
    y-axis: method(self, y-axis :: String): histogram-chart-window(self.obj.{y-axis: y-axis}) end,
    x-min: method(self, x-min :: Number): histogram-chart-window(self.obj.{x-min: O.some(x-min)}) end,
    x-max: method(self, x-max :: Number): histogram-chart-window(self.obj.{x-max: O.some(x-max)}) end,
    y-max: method(self, y-max :: Number): histogram-chart-window(self.obj.{y-max: O.some(y-max)}) end,
  | plot-chart-window(obj :: PlotChartWindowObject) with:
    x-axis: method(self, x-axis :: String): plot-chart-window(self.obj.{x-axis: x-axis}) end,
    y-axis: method(self, y-axis :: String): plot-chart-window(self.obj.{y-axis: y-axis}) end,
    x-min: method(self, x-min :: Number): plot-chart-window(self.obj.{x-min: O.some(x-min)}) end,
    x-max: method(self, x-max :: Number): plot-chart-window(self.obj.{x-max: O.some(x-max)}) end,
    y-min: method(self, y-min :: Number): plot-chart-window(self.obj.{y-min: O.some(y-min)}) end,
    y-max: method(self, y-max :: Number): plot-chart-window(self.obj.{y-max: O.some(y-max)}) end,
    num-samples: method(self, num-samples :: Number) block:
      when (num-samples <= 0) or (num-samples > 100000) or G.not(G.num-is-integer(num-samples)):
        G.raise('num-samples: value must be an ineger between 1 and 100000')
      end
      plot-chart-window(self.obj.{num-samples: num-samples})
    end,
    title: method(self, title :: String): plot-chart-window(self.obj.{title: title}) end,
    width: method(self, width :: Number): plot-chart-window(self.obj.{width: width}) end,
    height: method(self, height :: Number): plot-chart-window(self.obj.{height: height}) end,
    display: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: true}.render()
    end,
    get-image: method(self):
      _ = check-chart-window(self.obj)
      self.obj.{interact: false}.render()
    end,
  # TODO(tiffany): add _output and test get-vs-from-img after VS is implemented
end

################################################################################
# FUNCTIONS
################################################################################

fun scatter-plot-from-list(xs :: L.List<Number>, ys :: L.List<Number>) -> DataSeries block:
  when L.length(xs) <> L.length(ys):
    G.raise('scatter-plot: xs and ys should have the same length')
  end
  # TODO(tiffany): uncomment after implementing each
  #xs.each(check-num)
  #ys.each(check-num)
  scatter-plot-series(default-scatter-plot-series.{
    ps: L.map2({(x, y): [G.raw-array: x, y]}, xs, ys)
  })
end

fun pie-chart-from-list(labels :: L.List<String>, values :: L.List<Number>) -> DataSeries block:
  doc: ```
       Consume labels, a list of string, and values, a list of numbers
       and construct a pie chart
       ```
  label-length = L.length(labels)
  value-length = L.length(values)
  when label-length <> value-length:
    G.raise('pie-chart: labels and values should have the same length')
  end
  when label-length == 0:
    G.raise('pie-chart: need at least one data')
  end
  # TODO(tiffany): uncomment after implementing each
  #values.each(check-num)
  #labels.each(check-string)
  pie-chart-series(default-pie-chart-series.{
    tab: to-table2(labels, values)
  })
end

fun bar-chart-from-list(labels :: L.List<String>, values :: L.List<Number>) -> DataSeries block:
  doc: ```
       Consume labels, a list of string, and values, a list of numbers
       and construct a bar chart
       ```
  label-length = L.length(labels)
  value-length = L.length(values)
  when label-length <> value-length:
    G.raise('bar-chart: labels and values should have the same length')
  end
  # TODO(tiffany): uncomment after implementing each
  #values.each(check-num)
  #labels.each(check-string)
  bar-chart-series(default-bar-chart-series.{
    tab: to-table2(labels, values),
    legends: [G.raw-array: ''],
    has-legend: false,
  })
end

fun histogram-from-list(values :: L.List<Number>) -> DataSeries block:
  doc: ```
       Consume a list of numbers and construct a histogram
       ```
  # TODO(tiffany): uncomment after implementing each
  #values.each(check-num)
  histogram-series(default-histogram-series.{
    tab: to-table2(L.map({(_): ''}, values), values),
  })
end

################################################################################
# PLOTS
################################################################################

fun render-chart(s :: DataSeries) -> ChartWindow:
  doc: 'Render it!'
  cases (DataSeries) s:
    # TODO(tiffany): fix scatter-plot-series
    | scatter-plot-series(_) => plot-chart-window(default-plot-chart-window-object)
    | pie-chart-series(obj) =>
      pie-chart-window(default-pie-chart-window-object.{
        render: method(self): CL.pie-chart(obj.tab) end
      })
    | bar-chart-series(obj) =>
      bar-chart-window(default-bar-chart-window-object.{
        render: method(self):
          _ = cases (Option) self.y-min:
                | some(y-min) =>
                  cases (Option) self.y-max:
                    | some(y-max) =>
                      if y-min >= y-max:
                        G.raise("render: y-min must be strictly less than y-max")
                      else:
                        G.nothing
                      end
                    | else => G.nothing
                  end
                | else => G.nothing
              end
          CL.bar-chart(obj.tab)
        end
      })
    # TODO(tiffany): implement CL.box-plot
    # box-plot-series(obj) =>
    #  box-plot-chart-window(default-box-plot-chart-window-object.{
    #    render: method(self):
    #      CL.box-plot(self, obj)
    #    end
    #  })
    | histogram-series(obj) =>
      histogram-chart-window(default-histogram-chart-window-object.{
        render: method(self):
          shadow self = self.{y-min: O.none}
          _ = cases (Option) self.x-min:
                | some(x-min) =>
                  cases (Option) self.x-max:
                    | some(x-max) =>
                      if x-min >= x-max:
                        G.raise("render: x-min must be strictly less than x-max")
                      else:
                        G.nothing
                      end
                    | else => G.nothing
                  end
                | else => G.nothing
              end
          _ = cases (Option) self.y-min:
                | some(y-min) =>
                  cases (Option) self.y-max:
                    | some(y-max) =>
                      if y-min >= y-max:
                        G.raise("render: y-min must be strictly less than y-max")
                      else:
                        G.nothing
                      end
                    | else => G.nothing
                  end
                | else => G.nothing
              end
          CL.histogram(obj.tab)
        end
      })
  end
#where:
#  render-now = {(x): render-chart(x).get-image()}
end

