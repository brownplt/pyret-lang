provide:
  data StackType,
  data PointShape,
  data TrendlineType,
  data AxisType
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
  | tl-linear
  | tl-exponential
  | tl-polynomial(degree :: NumInteger%(check-positive-degree))
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


fun check-non-neg-base(n :: Number) -> Boolean block:
  when n <= 0:
    raise("Axis scale: exponential and logarithmic scales must have positive base")
  end
  true
end


fun check-non-neg-power(n :: Number) -> Boolean block:
  when n <= 0:
    raise("Axis scale: power must be positive")
  end
  true
end

data AxisType:
  | at-linear
  | at-power(pow :: Number%(check-non-neg-power))
  | at-log(base :: Number%(check-non-neg-base))
  | at-symlog(constant :: Number%(check-non-neg-base))
end
