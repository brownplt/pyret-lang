include image-typed
include charts
include image-structs

check "chart pinholes":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  chart = render-chart(from-list.scatter-plot(ys, xs)).get-image()
  # prior to fixing #1813, this would fail
  image-pinhole-x(chart) is image-width(chart) / 2
  image-pinhole-y(chart) is image-height(chart) / 2
end

check "chart spec":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  chart = render-chart(from-list.scatter-plot(ys, xs)).get-spec()
  chart satisfies is-string
end

check "render-chart":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  p1 = from-list.function-plot(lam(x): x * x end).color(red)
  p2 = from-list.line-plot([list: 1, 2, 3, 4], [list: 1, 4, 9, 16]).color(green)
  p3 = from-list.histogram([list: 1, 2, 3, 4])
  p4 = from-list.line-plot(
      [list: -1, 1,  2, 3, 11, 8, 9],
      [list: 10, -1, 11, 9,  9, 3, 2])
  p5 = from-list.scatter-plot(ys, xs)
  render-charts([list: p1, p2, p3]) raises ''
  render-charts([list: p1, p2])
    .title('quadratic function and a scatter plot')
    .x-min(0)
    .x-max(20)
    .y-min(0)
    .y-max(20)
    .get-image() does-not-raise
  render-charts([list: p4])
    .x-min(0)
    .x-max(10)
    .y-min(0)
    .y-max(10)
    .get-image() does-not-raise
  render-chart(p5) does-not-raise
end
