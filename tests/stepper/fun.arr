#lang pyret

fun inc(x):
  x + 1
end

inc(inc(inc(0)))