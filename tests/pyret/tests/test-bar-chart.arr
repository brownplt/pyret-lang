use context essentials2020
#import chart as GC
import charts as VC
import color as C
import image-typed as I
#|

colors =       [list: C.red,   C.orange, C.yellow, C.green, C.blue,   C.purple, C.pink]
langs =        [list: "Pyret", "OCaml",  "C",      "C++",   "Python", "Racket", "Smalltalk"]
popularities = [list: 10,      -6,        1,        3,       5,        8,        9]
# images = colors.map(I.circle(30, I.mode-solid, _))
images = colors.map(I.triangle(30, I.mode-solid, _))
states = [list: 'CA', 'TX', 'NY', 'FL', 'IL', 'PA']
ages = [list:
    'Under 5 Years',
    '5 to 13 Years',
    '14 to 17 Years',
    '18 to 24 Years',
    '25 to 44 Years',
    '45 to 64 Years',
    '65 Years and Over']

va-series = VC.from-list.grouped-bar-chart(
  states,
  [list:
    [list: 2704659,4499890,2159981,3853788,10604510,8819342,4114496],
    [list: 2027307,3277946,1420518,2454721,7017731,5656528,2472223],
    [list: 1208495,2141490,1058031,1999120,5355235,5120254,2607672],
    [list: 1140516,1938695,925060,1607297,4782119,4746856,3187797],
    [list: 894368,1558919,725973,1311479,3596343,3239173,1575308],
    [list: 737462,1345341,679201,1203944,3157759,3414001,1910571]],
  ages)
 # .horizontal(true)
 .colors(colors.reverse())
  .add-pointers([list: 0.2500000, 0.3500000], [list: "hi", "bye"])
  .stacking-type(VC.relative)
  .pointer-color(C.maroon)
  # .intervals(for map_n(series from 0, _ from states):
  #   for map_n(legend from -5, _ from ages):
  #     [list: series, legend, (series + legend)].map(500000 * _)
  #   end
  # end)
# .annotations(states.map({(s): ages.map({(a): some(s + "/" + a)})}))
#  .annotations(langs.map({(l): some(string-explode(l).reverse().join-str(""))}))
 # .intervals(for map_n(i from 0, _ from colors):
 #   [list: -5 + i, i, 5 + i, 3 + i]
 # end)
 .interval-color(C.chartreuse)
 .format-axis({(v): "Label " + to-string(v)})

#|
vb-series = VC.from-list.#|image-|#bar-chart(#|images, |#langs, popularities)
  .horizontal(true)
  .colors(colors.reverse())
  .add-pointers([list: 2.5, 3.5], [list: "hi", "bye"])
  .pointer-color(C.maroon)
  .annotations(langs.map({(l): some(string-explode(l).reverse().join-str(""))}))
  .intervals(for map_n(i from 0, _ from colors):
    [list: -5 + i, i, 5 + i, 3 + i]
  end)
  .interval-color(C.chartreuse)
  .format-axis({(v): "Label " + to-string(v)})

rendered = VC.render-chart(va-series).background-color(C.gray).title("Hi!")
.x-axis("This way!").y-axis("That way!")
|#


# va-hist = VC.from-list.labeled-histogram(
#   range(1, 100).map(lam(x): "foo " + num-to-string(x) end),
#   range(1, 100).map(lam(_): num-random(1000) end))
# rendered = VC.render-chart(va-hist).background-color(C.gray)
#   .x-axis("This way!").y-axis("That way!")
 

rendered = VC.render-chart(VC.from-list.exploding-pie-chart(
    range(1, 10).map({(i): "Label " + to-string(i)}),
    range(1, 10).map(_ * 3),
    range(1, 10).map({(i): 0.05 * (num-sin(i) + 1)}))
    .rotate(90)
    .collapse-threshold(0.07)
    .piehole(0.25)
  ).background-color(C.gray)

I.save-image(rendered.get-image(), 'test-bar-chart.png')

|#


va-box = VC.from-list.box-plot(for map(x from range(0, 5)):
    # for map(y from range(0, 10) + range(50, 60) + range(100, 200)):
    #   (100 * x) + num-sqr(y) + num-expt(5, x)
    # end
    (range(40, 43) + range(70, 73) + range(50, 60)).map(_ + x)
  end).color(C.steel-blue).horizontal(true)
rendered = VC.render-chart(va-box).background-color(C.gray).title("Hi!")
#.min(15500).max(30500)
.x-axis("This way!").y-axis("That way!")
I.save-image(rendered.get-image(), 'test-bar-chart.png')
