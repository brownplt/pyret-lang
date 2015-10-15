#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (type T) (a-id T (xref "plot" T)))
@(define (Color) (a-id "Color" (xref "image-structs" "Color")))
@(define (SD-of typ) (a-app (a-id "StringDict" (xref "string-dict" "StringDict")) typ))
@(define (no-variant T v) @list{@type-spec[T (list)]
  There are no variants for @pyret-id[T], and programs cannot use @pyret{cases} statements with @pyret-id[T].
  Instead, a @pyret-id[T] value named @tt[v] is provided, and a new @pyret-id[T]
  can be constructed via the @;@secref{"s:extend-expr"}
  Extend Expression.})

@(append-gen-docs
  `(module "plot"
    (path "src/js/base/runtime-anf.js")
    (fun-spec (name "plot-multi") (arity 2))
    (fun-spec (name "histogram"))
    (fun-spec (name "pie-chart"))
    (fun-spec (name "plot"))
    (type-spec (name "PlotOptions"))
    (type-spec (name "PlotWindowOptions"))
    (data-spec
      (name "Posn")
      (variants ("posn")))
    (data-spec
      (name "Plot")
      (variants ("line-plot" "scatter-plot" "xy-plot")))
  ))

@docmodule["plot"]{
  The Pyret Plot library. It consists of plot, chart, and data visualization tools.

  @section{The Posn Type}

  @data-spec2["Posn" (list) (list
  @constructor-spec["Posn" "posn" (list `("x" ("type" "normal") ("contract" ,N))
                                        `("y" ("type" "normal") ("contract" ,N)))])]

  @nested[#:style 'inset]{
  @constructor-doc["Posn" "posn" (list `("x" ("type" "normal") ("contract" ,N))
                                       `("y" ("type" "normal") ("contract" ,N))) (type "Posn")]{
    A position of a point
    @member-spec["x" #:type "normal" #:contract N]{
      x-coordinate of a point
    }
    @member-spec["y" #:type "normal" #:contract N]{
      y-coordinate of a point
    }
  }

  }

  @section{The Plot Type}

  @data-spec2["Plot" (list) (list
  @constructor-spec["Plot" "line-plot" (list `("points" ("type" "normal") ("contract" ,(L-of (type "Posn"))))
                                             `("options" ("type" "normal") ("contract" ,(type "PlotOptions"))))]
  @constructor-spec["Plot" "scatter-plot" (list `("points" ("type" "normal") ("contract" ,(L-of (type "Posn"))))
                                                `("options" ("type" "normal") ("contract" ,(type "PlotOptions"))))]
  @constructor-spec["Plot" "xy-plot" (list `("f" ("type" "normal") ("contract" ,(a-arrow N N)))
                                           `("options" ("type" "normal") ("contract" ,(type "PlotOptions"))))])]

  @nested[#:style 'inset]{
  @constructor-doc["Plot" "line-plot" (list `("points" ("type" "normal") ("contract" ,(L-of (type "Posn"))))
                                            `("options" ("type" "normal") ("contract" ,(type "PlotOptions")))) (type "Plot")]{
    A line plot or line chart, used to display "information as a series of data points called 'markers'
    connected by straight line segments." (@url["https://en.wikipedia.org/wiki/Line_chart"])

    @member-spec["points" #:type "normal" #:contract (L-of (type "Posn"))]{
      A list of data points. Because two consecutive data points will be connected by a line segment as they are,
      the list of data points should have been sorted by x-axis.
    }
    @member-spec["options" #:type "normal" #:contract (type "PlotOptions")]
  }

  @constructor-doc["Plot" "scatter-plot" (list `("points" ("type" "normal") ("contract" ,(L-of (type "Posn"))))
                                               `("options" ("type" "normal") ("contract" ,(type "PlotOptions")))) (type "Plot")]{
    A scatter plot or scatter chart, used "to display values for two variables for a set of data." (@url["https://en.wikipedia.org/wiki/Scatter_plot"])

    @member-spec["points" #:type "normal" #:contract (L-of (type "Posn"))]{
      A list of data points. The order of points in this list does not matter.
    }
    @member-spec["options" #:type "normal" #:contract (type "PlotOptions")]
  }

  @constructor-doc["Plot" "xy-plot" (list `("f" ("type" "normal") ("contract" ,(a-arrow N N)))
                                          `("options" ("type" "normal") ("contract" ,(type "PlotOptions")))) (type "Plot")]{
    A graph of a function of one variable.

    @member-spec["f" #:type "normal" #:contract (a-arrow N N)]{
      A function to be graphed. It can yield error for some @pyret{x}
      (such as division by zero or resulting in imaginary number).
    }
    @member-spec["options" #:type "normal" #:contract (type "PlotOptions")]
  }
  }

  @examples{
    my-plot = xy-plot(lam(x): num-sqrt(x) end, plot-options)
  }

  @section{The PlotOptions Type and plot-options Value}

  @no-variant["PlotOptions" "plot-options"]

  @pyret-id{PlotOptions} consists of the following fields:
  @a-record[(a-field "color" (Color))]

  The value of @tt{plot-options} is @tt{{color: blue}}

  @examples{
    import image-structs as I
    my-plot-options = plot-options.{color: I.red}
  }

  @section{The PlotWindowOptions Type and plot-window-options Value}

  @no-variant["PlotWindowOptions" "plot-window-options"]

  @pyret-id{PlotWindowOptions} consists of the following fields:
  @a-record[(a-field "x-min" N) (a-field "x-max" N) (a-field "y-min" N) (a-field "y-max" N) (a-field "inferred-bound" B) (a-field "label" S)]

  The value of @tt{plot-window-options} is @tt{{x-min: -10,
                                                x-max: 10,
                                                y-min: -10,
                                                y-max: 10,
                                                inferred-bound: false,
                                                label: ""}}

  @section{Plot Functions}

  These functions are available on the @tt{plot} module object.  So, for
  example, if you used @pyret{import plot as P}, you would write
  @pyret{P.plot-multi} to access @pyret{plot-multi} below.

  @function["plot-multi"
    #:contract (a-arrow (L-of (type "Plot")) (type "PlotWindowOptions") (L-of (type "Plot")))
    #:args (list (list "lst" #f) (list "options" #f))
    #:return (L-of (type "Plot"))
  ]{

  Display all @pyret-id{Plot}s on a window.

  @examples{
  import image-structs as I
  p1 = xy-plot(lam(x): x * x end, plot-options.{color: I.red})
  p2 = line-plot([list: posn(1, 1), posn(2, 4), posn(3, 9), posn(4, 16)], plot-options.{color: I.green})
  plot-multi([list: p1, p2], plot-window-options.{x-min: 0, x-max: 20, y-min: 0, y-max: 20})
  }
  }

  @function["histogram"
    #:contract (a-arrow (L-of N) N (L-of N))
    #:args (list (list "lst" #f) (list "n" #f))
    #:return (L-of N)
  ]{

  Display a histogram using data from @pyret{lst} with @pyret{n} bins. Range of the histogram is automatically inferred from the data.
  }

  @function["pie-chart"
    #:contract (a-arrow (SD-of N) (SD-of N))
    #:args (list (list "sd" #f))
    #:return (SD-of N)
  ]{

  Display a pie chart with texts being the key of @pyret{sd}
  }
}
