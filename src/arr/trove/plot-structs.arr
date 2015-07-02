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
  label :: String,
  safe :: Boolean
}

data Plot:
  | line-plot(points :: List<Posn>, options :: PlotOptions)
  | scatter-plot(points :: List<Posn>, options :: PlotOptions)
  | xy-plot(f :: (Number -> Number), options :: PlotOptions)
end
