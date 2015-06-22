provide {
  posn: posn,
  line-plot: line-plot,
  scatter-plot: scatter-plot,
  xy-plot: xy-plot,
  histogram: histogram,
  pie-chart: pie-chart,
  plot-multi: plot-multi,
  plot-options: plot-options,
  plot-window-options: plot-window-options
} end

provide-types {
  Posn: Posn,
  Plot: Plot,
  PlotOptions: PlotOptions,
  PlotWindowOptions: PlotWindowOptions
}

import string-dict as SD
import image-structs as I
import plot-structs as P
import plot as J

type Plot = P.Plot
type PlotWindowOptions = P.PlotWindowOptions
type PlotOptions = P.PlotOptions
type Posn = P.Posn

posn = P.posn
line-plot = P.line-plot
scatter-plot = P.scatter-plot
xy-plot = P.xy-plot

plot-options :: PlotOptions = {
  color: I.blue
}

plot-window-options :: PlotWindowOptions = {
  x-min: -10,
  x-max: 10,
  y-min: -10,
  y-max: 10,
  infer-bounds: false,
  label: ""
}

fun plot-multi(
  lst :: List<Plot>,
  win-options :: PlotWindowOptions) -> List<Plot>:
  
  doc: "Show plots in `lst`."
  
  shadow win-options =
    if win-options.infer-bounds:
      bounds = J.infer-bounds(lst)
      win-options.{
        x-max: bounds.x-max,
        x-min: bounds.x-min,
        y-max: bounds.y-max,
        y-min: bounds.y-min
      }
    else:
      win-options
    end
  
  plots = map_n(
    lam(i :: Number, p :: Plot) -> P.PlotInt:
      cases (Plot) p:
        | line-plot(points :: List<Posn>, _) =>
          P.line-plot-int(points, i)
        | scatter-plot(points :: List<Posn>, _) =>
          P.scatter-plot-int(points, i)
        | xy-plot(f :: (Number -> Number), _) =>
          lolop = generate-xy(
            f, win-options.x-min, win-options.x-max,
            win-options.y-min, win-options.y-max)
          P.xy-plot-int(lolop.map(P.line-plot-int(_, i)), i)
      end
    end, 0, lst)

  # show xy-plots first so that they are behind data points
  ot-plots = plots.filter(
    lam(p :: P.PlotInt) -> Boolean:
      not(P.is-xy-plot-int(p)) 
    end)
  xy-plots = plots.filter(P.is-xy-plot-int).map(_.plots).foldl(_ + _, empty)

  J.generic-plot(
    xy-plots + ot-plots,
    win-options,
    lst.map(_.options),
    lst.length())
  lst
end

######################
### histogram-plot ###
######################

fun histogram(lst :: List<Number>, n :: Number) -> List<Number>:
  doc: "Show a histogram with frequencies from `lst` and `n` bins"
  J.histogram-plot(lst, n)
  lst
end

#################
### pie-chart ###
#################

fun pie-chart(sd :: SD.StringDict<Number>) -> SD.StringDict<Number>:
  doc: "Show a pie chart from `sd`"
  J.pie-chart(sd)
  sd
end


fun test-plot():
  plot-multi(
    [list:
      xy-plot(lam(x): num-sin(1 / x);, plot-options),
      scatter-plot(
        [list: posn(0, 0), posn(0.1, 0.1), posn(0.2, 0.2)],
        plot-options.{color: I.red})
    ],
    plot-window-options.{x-min: -1, x-max: 1, y-min: -1, y-max: 1})
end

fun test-pie-chart():
  # data from http://support.sas.com/documentation/cdl/en/grstatgraph/65377/HTML/default/viewer.htm#n0xlqsp9bjs2vzn1x2lp68wxoslk.htm
  pie-chart([SD.string-dict: "Asia", 158, "USA", 147, "EU", 123])
end

fun test-histogram():
  # data from https://www.mathsisfun.com/data/histograms.html
  puppy-data = [list: 0.5, 0.5, 0.3, -0.2, 1.6, 0, 0.1, 0.1, 0.6, 0.4]
  histogram(puppy-data, 5)
end