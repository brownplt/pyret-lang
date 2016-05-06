provide {
  is-Posn: is-Posn,
  posn: posn,
  is-posn: is-posn,

  line-plot: line-plot,
  is-line-plot: is-line-plot,

  scatter-plot: scatter-plot,
  is-scatter-plot: is-scatter-plot,

  xy-plot: xy-plot,
  is-xy-plot: is-xy-plot,

  plot-xy: plot-xy,
  plot-multi: plot-multi,
  plot-options: default-plot-options,
  plot-window-options: default-plot-window-options,

  histogram: histogram,
  pie-chart: pie-chart
} end

# provide *

provide-types {
  Posn :: Posn,
  Plot :: Plot,
  PlotOptions :: WrappedPlotOptions,
  PlotWindowOptions :: WrappedPlotWindowOptions
}

import plot-lib as P
import either as E
import string-dict as SD
import image-structs as I

## for debugging
# P2 = P

## in the console
# import plot as PP
# P = PP.P2

OFFSET = 1

data Posn:
  | posn(x :: Number, y :: Number)
end

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

data Plot:
  | line-plot(points :: List<Posn>, options :: WrappedPlotOptions)
  | scatter-plot(points :: List<Posn>, options :: WrappedPlotOptions)
  | xy-plot(f :: (Number -> Number), options :: WrappedPlotOptions)
end

data PlotInternal:
  | line-plot-int(points :: List<Posn>, options :: PlotOptions)
  | scatter-plot-int(points :: List<Posn>, options :: PlotOptions)
  | xy-plot-int(f :: (Number -> Number), options :: PlotOptions)
end

id = lam<A>(x :: A): x end
default-plot-options :: WrappedPlotOptions = id
default-plot-window-options :: WrappedPlotWindowOptions = id

data Pair<a, b>:
  | pair(first :: a, second :: b)
end

fun histogram(lst :: List<Number>, n :: Number) -> List<Number>:
  P.histogram(lst, n)
  lst
end

fun pie-chart(sd :: SD.StringDict<Number>) -> SD.StringDict<Number>:
  P.pie-chart(sd)
  sd
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
            | left(y) => some(posn(x, y))
            | right(v) => none
          end
        end)
        .filter(is-some)
        .map(_.value)
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
    is scatter-plot-int([list:
      posn(0, 1),
      posn(20, 21),
      posn(40, 41),
      posn(60, 61),
      posn(80, 81),
      posn(100, 101) # out of bound, will be filtered later
    ], plot-options)
end

fun in-bound-gen(x-min :: Number, x-max :: Number, y-min :: Number, y-max :: Number) -> (Posn -> Boolean):
  doc: "Return a function testing whether a Posn is in boundaries or not"
  lam(point :: Posn) -> Boolean:
    (x-min <= point.x) and (point.x <= x-max) and (y-min <= point.y) and (point.y <= y-max)
  end
where:
  in-bound = in-bound-gen(0, 0, 0, 0)
  posn(0, 0) satisfies in-bound
  posn(0, 1) violates in-bound

  shadow in-bound = in-bound-gen(-1, 1, -1, 1)
  posn(1/2, 1/2) satisfies in-bound
  posn(1/2, 3) violates in-bound
end

fun dist(a :: Posn, b :: Posn) -> Number:
  doc: "squared Euclidean distance"
  num-sqr(a.x - b.x) + num-sqr(a.y - b.y)
where:
  dist(posn(0, 0), posn(0, 0)) is 0
  dist(posn(1, 2), posn(4, 6)) is 25
end

fun nearest(lst :: List<Posn>, o :: Posn) -> Option<Posn>:
  doc: "Find a point in `lst` which is closest to `o`"

  lst.foldl(
    lam(elt :: Posn, acc :: Option<Pair<Posn, Number>>) -> Option<Pair<Posn, Number>>:
      cur-dist = dist(elt, o)
      cases (Option) acc:
        | none => some(pair(elt, cur-dist))
        | some(prev) =>
          if cur-dist < prev.second:
            some(pair(elt, cur-dist))
          else:
            acc
          end
      end
    end, none).and-then(_.first)
where:
  nearest(empty, posn(0, 0)) is none
  nearest([list: posn(10, 10)], posn(0, 0)) is some(posn(10, 10))
  nearest([list: posn(10, 10), posn(9, 9), posn(1, 1), posn(2, 2)], posn(0, 0)) is some(posn(1, 1))
end

fun find-point-on-edge(near :: Posn, far :: Posn, win-opt :: PlotWindowOptions) -> Option<Posn>:
  doc: ```
       Find a Posn on the border and on the line between `near` and `far`. If there are many,
       pick the one closest to `near`.

       Precondition: at least one of `near` or `far` is not in the border.
       ```

  x-max = num-min(num-max(near.x, far.x), win-opt.x-max)
  x-min = num-max(num-min(near.x, far.x), win-opt.x-min)
  y-max = num-min(num-max(near.y, far.y), win-opt.y-max)
  y-min = num-max(num-min(near.y, far.y), win-opt.y-min)
  
  fun equal(a :: Number, b :: Number) -> Boolean:
    (a <= b) and (a >= b)
  end

  # to workaround roughnum equality without providing tolerance
  candidates = if equal(near.x, far.x): 
    [list: posn(near.x, win-opt.y-min), posn(near.x, win-opt.y-max)]
  else:
    # y = m * x + c           [3]
    # y2 = m * x2 + c         [3.1]
    # y - y2 = m * (x - x2)   [5]   [by 3 - 3.1]
    # m = (y - y2) / (x - x2) [1]   [rewrite 5]
    # c = y - m * x           [2]   [rewrite 3]
    # x = (y - c) / m         [4]   [rewrite 3]

    m = (near.y - far.y) / (near.x - far.x)
    c = near.y - (m * near.x)

    f = lam(x :: Number): (m * x) + c end
    g = lam(y :: Number): (y - c) / m end

    [list: posn(win-opt.x-min, f(win-opt.x-min)), posn(win-opt.x-max, f(win-opt.x-max))] +
    if equal(m, 0):
      empty
    else:
      [list: posn(g(win-opt.y-min), win-opt.y-min), posn(g(win-opt.y-max), win-opt.y-max)]
    end
  end

  in-bound = in-bound-gen(x-min, x-max, y-min, y-max)
  nearest(candidates.filter(in-bound), near)
where:
  win-opt = plot-window-options.{x-min: 0, x-max: 10, y-min: 0, x-max: 10}

  find-point-on-edge(posn(-1, -1), posn(5, 5), win-opt) is some(posn(0, 0))
  find-point-on-edge(posn(-1, -1), posn(100, 100), win-opt) is some(posn(0, 0))
  find-point-on-edge(posn(100, 100), posn(-1, -1), win-opt) is some(posn(10, 10))
  find-point-on-edge(posn(-1, 1), posn(1, -1), win-opt) is some(posn(0, 0))
  find-point-on-edge(posn(9, 1), posn(11, 3), win-opt) is some(posn(10, 2))
  find-point-on-edge(posn(12, 1), posn(11, 3), win-opt) is none
  find-point-on-edge(posn(0, -1), posn(0, 11), win-opt) is some(posn(0, 0))
  find-point-on-edge(posn(0, 11), posn(0, -1), win-opt) is some(posn(0, 10))
  find-point-on-edge(posn(0, 1), posn(0, 11), win-opt) is some(posn(0, 10))
  find-point-on-edge(posn(-1, 0), posn(11, 0), win-opt) is some(posn(0, 0))
end

fun generate-line(plot :: PlotInternal, win-opt :: PlotWindowOptions) -> List<PlotInternal>:
  in-bound = in-bound-gen(win-opt.x-min, win-opt.x-max, win-opt.y-min, win-opt.y-max)
  cases (PlotInternal) plot:
    | line-plot-int(points, options) =>
      cases (List) points:
        | empty => empty
        | link(_, points2) =>
          all-lines = for map2(start from points, stop from points2):
            if in-bound(start):
              if in-bound(stop):
                [list: stop, start]
              else:
                cases (Option) find-point-on-edge(start, stop, win-opt):
                  | none => raise("plot: impossible")
                  | some(real-stop) => [list: real-stop, start]
                end
              end
            else:
              if in-bound(stop):
                cases (Option) find-point-on-edge(start, stop, win-opt):
                  | none => raise("plot: impossible")
                  | some(real-start) => [list: stop, real-start]
                end
              else:
                cases (Option) find-point-on-edge(stop, start, win-opt):
                  | none => empty
                  | some(real-stop) =>
                    cases (Option) find-point-on-edge(start, stop, win-opt):
                      | none => raise("plot: impossible")
                      | some(real-start) =>                          
                        [list: real-stop, real-start]
                    end
                end
              end
            end
          end 

          cases (List) all-lines.filter(is-link):
            | empty => empty
            | link(f, r) =>
              lst = r.foldl(
                lam(cur :: List<Posn>, acc :: List<List<Posn>>) -> List<List<Posn>>:                  
                  cases (List) acc:
                    | empty => raise("plot: impossible")
                    | link(last-line, rest-line) =>
                      cases (List) last-line:
                        | empty => raise("plot: impossible")
                        | link(last-point, rest-point) =>

                          start = cur.get(1)
                          stop = cur.get(0)

                          if start == last-point:
                            link(link(stop, last-line), rest-line)
                          else:
                            link(cur, acc)
                          end
                      end
                  end
                end, [list: f])
              lst.map(line-plot-int(_, options))
          end
      end
    | else => raise("plot: expect line-plot, got other")
  end
where:
  win-opt = plot-window-options.{x-min: 0, x-max: 10, y-min: 0, x-max: 10}
  
  generate-line(line-plot-int([list: posn(-1, -1), posn(1, 1), posn(1, 5), posn(5, 5), posn(5, 11), posn(7, 9), posn(11, 9), posn(9, 11)], plot-options), win-opt)
    is [list: [list: posn(10, 10), posn(10, 10)], [list: posn(10, 9), posn(7, 9), posn(6, 10)], [list: posn(5, 10), posn(5, 5), posn(1, 5), posn(1, 1), posn(0, 0)]].map(line-plot-int(_, plot-options))
end

fun plot-xy(f :: (Number -> Number), window-options :: WrappedPlotWindowOptions) -> (Number -> Number):
  plot-multi([list: xy-plot(f, default-plot-options)], window-options)
  f
end

fun plot-multi(plots :: List<Plot>, options-generator :: WrappedPlotWindowOptions) -> List<Plot>:
  options = options-generator(plot-window-options)
  when (options.x-min >= options.x-max) or (options.y-min >= options.y-max):
    raise("plot: x-min and y-min must be strictly less than x-max and y-max respectively")
  end

  original-plots = plots
  shadow plots = plots.map(
    lam(plot :: Plot) -> PlotInternal:
      cases (Plot) plot:
        | scatter-plot(points, opt-gen) => 
          scatter-plot-int(points, opt-gen(plot-options).{opacity: 80,  size: 4, tip: true})
        | line-plot(points, opt-gen) => 
          line-plot-int(points,    opt-gen(plot-options).{opacity: 100, size: 1, tip: false})
        | xy-plot(f, opt-gen) => 
          xy-plot-int(f,           opt-gen(plot-options).{opacity: 100, size: 1, tip: false})
      end
    end)

  partitioned = partition(is-xy-plot-int, plots)
  xy-plots = partitioned.is-true
  line-and-scatter = partitioned.is-false

  fun bound(points :: List<Posn>,
      base :: Posn,
      op :: (Number, Number -> Number),
      accessor :: (Posn -> Number)) -> Number:
    points.foldl(lam(p :: Posn, acc :: Number): op(accessor(p), acc) end, accessor(base))
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
    in-bound = in-bound-gen(options.x-min, options.x-max, options.y-min, options.y-max)

    shadow line-plots = line-plots.map(generate-line(_, options)).foldl(_ + _, empty)
    shadow xy-plots = xy-plots.map(generate-xy(_, options))

    combined-scatter-plots = (xy-plots + scatter-plots).map(
      lam(plot :: PlotInternal) -> PlotInternal:
        cases (PlotInternal) plot:
          | scatter-plot-int(points, opt) => scatter-plot-int(points.filter(in-bound), opt)
          | else => raise("plot: expect scatter-plot, got other")
        end
      end)

    maybe-new-options = P.generic-plot(combined-scatter-plots, line-plots, options)

    cases (Option<PlotWindowOptions>) maybe-new-options:
      | none => original-plots
      | some(new-options) => helper(new-options)
    end
  end

  helper(options)
end