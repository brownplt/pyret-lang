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


# va-box = VC.from-list.box-plot(for map(x from range(0, 5)):
#     # for map(y from range(0, 10) + range(50, 60) + range(100, 200)):
#     #   (100 * x) + num-sqr(y) + num-expt(5, x)
#     # end
#     (range(40, 43) + range(70, 73) + range(50, 60)).map(_ + x)
#   end).color(C.steel-blue).horizontal(true)
# rendered = VC.render-chart(va-box).background-color(C.gray).title("Hi!")
# #.min(15500).max(30500)
# .x-axis("This way!").y-axis("That way!")

# va-hist = VC.from-list.labeled-histogram(
#   range(0, 500).map(lam(x): "foo " + num-to-string(x) end),
#   range(0, 500).map(num-sqrt))
# rendered = VC.render-chart(va-hist).background-color(C.gray)
#   .x-axis("This way!").y-axis("That way!").height(400)
# N = 500
# vals = range(0, N)
# colors = vals.map({(v): I.color(num-modulo(v, 250), num-max(0, v - 250), 0, 1)})
# imgs = colors.map({(c): I.circle(30, I.mode-solid, c)})


# rendered = VC.render-chart(VC.from-list.image-histogram(imgs, vals.map(num-sqrt))).height(452)


ages = [list: 10.1, 0.4, 2, 5, 4.7, 0.3, 1, 1, 0.3, 0.8, 4, 0.4, 6.2, 1, 3, 10.7, 5.8, 3, 3, 1, 1.4, 8.3, 3.3, 5, 2.6, 4, 0.9, 4, 8.4, 0.5, 2, 11.3, 0.1, 8, 2, 0.8, 3.5, 1, 3, 5.3, 11, 2, 1, 8.3, 3, 12, 0.29, 5, 9.3, 7.3, 9.2, 1.1, 1, 9.5, 0.9, 3, 9, 2, 7.6, 1, 1, 7, 0.9, 2, 0.6, 3, 4, 5.7, 0.1, 2.2, 3, 10.9, 3, 12, 9.9, 6.3, 10, 0.7, 0.3, 0.3, 12.8, 12.4, 4.3, 1, 2, 1, 4.4, 3, 11.1, 0.3, 3.3, 3, 0.11, 7.8, 2.8, 3, 4.2, 0.8, 0.2, 0.6, 7, 5, 4.2, 0.5, 5, 3.7, 0.2, 9.9, 5.2, 6.3, 1, 8, 0.7, 3.2, 5, 5.5, 11, 2, 8.9, 5.3, 3.1, 5, 3, 0.3, 3.1, 2.4, 12.6, 0.25, 0.3, 12.2, 6.3, 16, 7, 8, 1.8, 6, 12.6, 5, 12, 2.9, 6.8, 0.3, 9.8, 0.2, 8.6, 2.7, 7.25, 4, 3, 8.6, 12, 6.9, 3, 6.1, 8, 6.7, 0.7, 0.25, 4.1, 14, 3, 9, 0.7, 1, 8.8, 8.2, 1, 3, 1, 0.2, 2, 10.5, 1, 2, 7.1, 0.3, 2.1, 8.5, 2.3, 12.9, 5, 2, 10, 0.3, 3.8, 12, 0.1, 0.3, 7.2, 6.4, 12.7, 0.1, 9.6, 0.2, 12.5, 0.2, 5.8, 2.5, 1, 3.4, 11.6, 8, 7.5, 1.8, 9.4, 0.1, 0.3, 9.1, 2, 4.9, 9.6, 0.5, 1.6, 0.6, 0.5, 3, 10.7, 5.1, 4, 0.5, 4.4, 0.6, 3, 4, 3, 1.4, 7, 5, 3.7, 2.1, 1, 10, 9.5, 0.6, 2, 2, 3.2, 4, 7.1, 1.2, 10.3, 12, 10, 1.4, 10.1, 11.9, 1, 5.2, 2.2, 4, 5, 10.4, 6, 0.7, 8, 4, 11.7, 3, 0.8, 6.6, 9.9, 4.3, 2, 0.7, 6.7, 2, 5.2, 11.4, 0.5, 3, 7.7, 13.9, 6.1, 0.4, 5.4, 0.3, 0.4, 0.1, 0.3, 9.6, 0.1, 10, 12.1, 3, 5, 9, 1, 10.4, 10.6, 3.5, 9.5, 0.4, 11.5, 0.3, 1, 4.5, 3, 0.9, 7, 5, 0.4, 8.7, 5.5, 0.6, 2, 8.1, 1, 5.8, 6.9, 3.2, 10, 6.2, 0.3, 2.4, 0.5, 1, 2, 1, 5, 8.7, 7.9, 11.7, 6.9, 3.9, 0.9, 5.4, 7, 3, 12.5, 10.2, 6.8, 8.1, 7.8, 0.7, 10.2, 2.3, 3.1, 2, 0.6, 18, 8, 2, 8.5, 3, 2, 2, 3.6, 12.3, 4, 10.8, 0.2, 1.5, 3.6, 12.2, 5, 2.6, 0.6, 5.1, 4, 0.1, 4, 2, 5, 2.5, 2, 6.1, 4.2, 1, 1, 1.5, 2, 3, 2, 12.7, 3.3, 8.2, 0.1, 0.9, 5.7, 0.4, 3.8, 9.8, 1, 4.5, 7.2, 9.4, 0.1, 5, 4, 0.5, 1, 3, 0.1, 8.9, 8.3, 6.8, 3.4, 0.2, 10.8, 5.8, 0.4, 3, 2, 12, 12.5, 12.4, 4.8].sort()
weights = [list: 40, 5.2, 32, 0.5, 7.6, 33, 1.7, 5, 22, 0.07, 4, 2.7, 8.5, 1.8, 8, 24, 8.5, 4.7, 63, 1.9, 72, 82, 42, 7.7, 9.9, 77, 0.1, 53, 61, 0.1, 4.1, 9.7, 15, 76.1, 2.9, 0.19, 66, 0.08, 3.6, 8.2, 123, 1.8, 2.1, 12, 1.6, 21, 3.5, 6, 8.6, 7.9, 8.5, 7.7, 3, 7.4, 8.6, 4.6, 0.35, 7, 84, 2.6, 3.4, 59, 5.8, 0.3, 40, 3.7, 30, 67, 3.6, 8.2, 1, 8.6, 1.7, 9.8, 70, 63, 0.22, 4.4, 33, 39, 10.3, 8.4, 44, 3.9, 8, 3.3, 9.7, 1.5, 8.3, 4, 9, 6, 0.8, 33, 73, 3.2, 68, 80, 12, 6.8, 73, 68, 44, 6, 4, 8.4, 2, 80, 10.4, 73, 90, 0.2, 0.09, 7.3, 5, 7.1, 6.9, 3.1, 9.6, 82, 9.1, 7, 3.5, 4.4, 7.5, 82, 8.2, 3, 7, 9.8, 76, 9.2, 72, 54, 7.6, 0.28, 7.9, 7, 9.9, 67, 9.5, 2.8, 70, 8.5, 9.7, 110, 86, 92, 5, 9.5, 7.5, 25, 1.2, 44, 88, 95, 0.18, 3.2, 8.7, 60, 1.6, 69, 6.4, 5, 72, 8.6, 2.1, 13.4, 2.6, 31, 3.4, 9.9, 2, 3.3, 40, 8.8, 45, 62, 10, 8.9, 2, 89, 9.1, 2.5, 36, 0.2, 7.3, 16, 46, 9.9, 10.4, 9, 9.8, 17, 14, 0.075, 97, 55, 4.2, 10, 11, 172, 10.3, 67, 62, 26, 3.4, 8.5, 4.9, 64, 42, 5, 12, 0.09, 6.3, 45.4, 9.1, 8.9, 62, 3, 78, 0.14, 51.6, 0.2, 52.8, 8, 8.8, 112, 49, 9.6, 1, 59, 87, 19, 7.4, 2, 23, 3, 51, 6.5, 9.1, 45, 161, 39, 5, 9.4, 2, 75, 58, 0.45, 3, 7.8, 4.3, 5.2, 9.9, 8.7, 8.9, 35.3, 0.11, 19, 11, 16, 3.3, 8.1, 52, 7, 35, 7.6, 0.075, 0.31, 63, 51, 51, 3.4, 47, 17, 56, 0.03, 3, 58, 2.6, 7.3, 8.2, 0.25, 24, 60, 5, 50, 7.6, 7.7, 82, 42, 9.6, 4, 28.9, 70, 2.3, 43, 63, 5, 3, 27, 103, 64, 3.8, 10.3, 2.1, 10, 81, 8.7, 7.4, 86, 0.06, 8.7, 7, 4, 3, 6.5, 7, 8.2, 107, 44, 8.6, 8.6, 9.3, 8.9, 8.4, 3.5, 8.6, 8.1, 71, 40, 71, 55, 29, 65, 79, 4.7, 3.7, 0.375, 0.3, 6.5, 78, 3.5, 0.1, 1, 7.4, 9.6, 6, 38, 0.8, 46, 84, 39, 8.1, 15, 54, 85, 5, 1, 4, 1.6, 1, 7.2, 3.8, 8.5, 8.6, 4, 1.7, 0.19, 82, 48, 4.2, 18, 9.3, 70, 3, 8.3, 8.5, 20, 8.8, 8.3, 2.5, 54, 72, 30, 14, 55, 5, 35, 3.2, 4.2, 13, 51, 9.3, 32, 17, 2.8, 8.8, 55, 34, 100, 4, 5.5, 8.5, 5.7, 9.6].sort()
labels = range(0, weights.length()).map({(n): "Label " + to-string(n)})
images = ages.map({(n): I.star(50, I.mode-outline, I.red)})
  
scatter = VC.from-list.image-line-plot(images, ages, weights)
  .legend("Hi!").trendline-type(VC.polynomial(3)).point-size(15)

# rendered = VC.render-chart(VC.from-list.histogram(vals).bin-width(7.15)).height(400).background-color(C.gray)
rendered = VC.render-chart(scatter).height(600).background-color(C.gray)

I.save-image(rendered.get-image(), 'test-bar-chart.png')
