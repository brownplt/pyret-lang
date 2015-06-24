#lang pyret

provide *
provide-types *

import image-structs as I

data Posn:
  | posn(x :: Number, y :: Number)
end

type PlotOptions = {
  color :: I.Color
}

type PlotWindowOptions = {
  x-min :: Number,
  x-max :: Number,
  y-min :: Number,
  y-max :: Number,
  infer-bounds :: Boolean,
  label :: String
}

type PointInt = {
  x :: Number,
  y :: Option<Number>
}

data Plot:
  | line-plot(points :: List<Posn>, options :: PlotOptions)
  | scatter-plot(points :: List<Posn>, options :: PlotOptions)
  | xy-plot(f :: (Number -> Number), options :: PlotOptions)
end

data PlotInt:
  | line-plot-int(points :: List<Posn>, id :: Number)
  | scatter-plot-int(points :: List<Posn>, id :: Number)
  | xy-plot-int(plots :: List<PlotInt>, id :: Number)
end