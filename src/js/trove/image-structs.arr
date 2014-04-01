#lang pyret

data Color:
  | color(
      red :: Number(between(0, _, 255)),
      green :: Number(between(0, _, 255)),
      blue :: Number(between(0, _, 255)),
      alpha :: Number(between(0, _, 255)))
end

