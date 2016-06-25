#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (link T) (a-id T (xref "plot" T)))
@(define Color (a-id "Color" (xref "image-structs" "Color")))
@(define (t-field name ty) (a-field (tt name) ty))
@(define (t-record . rest) (a-record (apply tt rest)))

@(append-gen-docs
  `(module "plot"
    (path "src/arr/trove/plot.arr")

    (fun-spec (name "histogram") (arity 2))
    (fun-spec (name "pie-chart"))

    (fun-spec (name "plot-function"))
    (fun-spec (name "plot-scatter"))
    (fun-spec (name "plot-line"))

    (fun-spec (name "plot-multi") (arity 2))

    (type-spec (name "PlotOptions"))
    (type-spec (name "PlotWindowOptions"))
    (data-spec
      (name "Plot")
      (variants ("line-plot" "scatter-plot" "function-plot")))
  ))

@docmodule["plot"]{
  The Pyret Plot library. It consists of plot, chart, and data visualization tools.
  The visualization will appear on a dialog.

  @itemlist[
    @item{To close the dialog, click the "X" button at the left toolbar or press @tt{esc}}
    @item{To save a snapshot of the visualization, click the save button at the left toolbar and choose a location to save the image}
  ]

  Every function in this library is available on the @tt{plot} module object. For
  example, if you used @pyret{import plot as P}, you would write
  @pyret{P.plot-function} to access @pyret{plot-function} below.

  @;############################################################################
  @section{The Plot Type}

  (If you do not wish to customize the plotting, feel free to skip this section.
  There will be a link referring back to this section when necessary)

  @data-spec2["Plot" (list) (list
  @constructor-spec["Plot" "function-plot" `(("f" ("type" "normal") ("contract" ,(a-arrow N N)))
                                       ("options" ("type" "normal") ("contract" ,(link "PlotOptions"))))]
  @constructor-spec["Plot" "line-plot" `(("points" ("type" "normal") ("contract" ,TA))
                                         ("options" ("type" "normal") ("contract" ,(link "PlotOptions"))))]
  @constructor-spec["Plot" "scatter-plot" `(("points" ("type" "normal") ("contract" ,TA))
                                            ("options" ("type" "normal") ("contract" ,(link "PlotOptions"))))])]

  @nested[#:style 'inset]{

  @constructor-doc["Plot" "function-plot" (list `("f" ("type" "normal") ("contract" ,(a-arrow N N)))
                                          `("options" ("type" "normal") ("contract" ,(link "PlotOptions")))) (link "Plot")]{
    A graph of a function of one variable.

    @member-spec["f" #:type "normal" #:contract (a-arrow N N)]{
      A function to be graphed. The function doesn't need to be total:
      it can yield an error for some @pyret{x} (such as division by zero
      or resulting in an imaginary number).
    }
    @member-spec["options" #:type "normal" #:contract (link "PlotOptions")]
  }

  @constructor-doc["Plot" "line-plot" `(("points" ("type" "normal") ("contract" ,TA))
                                        ("options" ("type" "normal") ("contract" ,(link "PlotOptions")))) (link "Plot")]{
    A line plot or line chart, used to display "information as a series of data points called `markers'
    connected by straight line segments." (see @url["https://en.wikipedia.org/wiki/Line_chart"])

    @member-spec["points" #:type "normal" #:contract TA]{
      A table of two columns: @t-field["x" N] and @t-field["y" N]

      Because two consecutive data points will be connected by a line segment as they are,
      the rows of the table should have been sorted by the x-value.
    }
    @member-spec["options" #:type "normal" #:contract (link "PlotOptions")]
  }

  @constructor-doc["Plot" "scatter-plot" `(("points" ("type" "normal") ("contract" ,TA))
                                           ("options" ("type" "normal") ("contract" ,(link "PlotOptions")))) (link "Plot")]{
    A scatter plot or scatter chart, used "to display values for two variables for a set of data."
    (see @url["https://en.wikipedia.org/wiki/Scatter_plot"])

    @member-spec["points" #:type "normal" #:contract TA]{
      A table of two columns: @t-field["x" N] and @t-field["y" N].
      The order of rows in this table does not matter.
    }
    @member-spec["options" #:type "normal" #:contract (link "PlotOptions")]
  }
  }

  @examples{
    my-plot = function-plot(lam(x): num-sqrt(x) end, plot-options)
  }

  @;############################################################################
  @section{Plot Functions}

  All plot functions will populate a dialog with controllers (textboxes and buttons)
  on the right which can be used to change the window boundaries and number of sample points.
  To zoom in at a specific region, you can click and drag on the plotting
  region. To zoom out, press @tt{shift} and click on the plotting region.
  To reset to the initial window boundaries, simply click on the plotting
  region.

  All changes by the controllers will not take an effect until the redraw button
  is pressed.

  The window boundaries could be any kind of real number (e.g., fraction, roughnum).
  However, when processing, it will be converted to a decimal number.
  For example, @pyret{1/3} will be converted to @pyret{0.3333...33} which
  is actually @pyret{3333...33/10000...00}. This incurs the numerical imprecision,
  but allows us to read the number easily.

  For function plot, we make a deliberate decision to show points (the tendency of the function)
  instead of connecting lines between them. This is to avoid the problem of inaccurate plotting
  causing from, for example, discontinuity of the function, or a function which oscillates infinitely.

  @function["plot-function"
    #:contract (a-arrow (a-arrow N N) (a-arrow N N))
    #:args '(("f" #f))
    #:return (a-arrow N N)
  ]{
    A shorthand to construct an @link{function-plot} with default options and then
    display it. See @link{function-plot} for more information.
  }

  @function["plot-line"
    #:contract (a-arrow TA TA)
    #:args '(("tab" #f))
    #:return TA
  ]{
  A shorthand to construct a @link{line-plot} with default options and then
  display it. See @link{line-plot} for more information.
  }

  @function["plot-scatter"
    #:contract (a-arrow TA TA)
    #:args '(("tab" #f))
    #:return TA
  ]{
  A shorthand to construct a @link{scatter-plot} with default options and then
  display it. See @link{scatter-plot} for more information.
  }

  @function["plot-multi"
    #:contract (a-arrow (L-of (link "Plot")) (link "PlotWindowOptions") (L-of (link "Plot")))
    #:args '(("lst" #f) ("options" #f))
    #:return (L-of (link "Plot"))
  ]{

  Display all @pyret-id{Plot}s in @pyret{lst} on a window with the configuration
  from @pyret{options}.

  @examples{
  import image-structs as I
  p1 = function-plot(lam(x): x * x end, _.{color: I.red})
  p2 = line-plot(table: x :: Number, y :: Number
      row: 1, 1
      row: 2, 4
      row: 3, 9
      row: 4, 16
    end, _.{color: I.green})
  plot-multi([list: p1, p2], _.{x-min: 0, x-max: 20, y-min: 0, y-max: 20})
  }

  The above example will plot a function @tt{y = x^2} using red color, and show
  a line chart connecting points in the table using green color. The left, right,
  top, bottom window boundary are 0, 20, 0, 20 respectively.
  }

  @;############################################################################
  @section{Visualization Functions}

  @function["histogram"
    #:contract (a-arrow TA N TA)
    #:args '(("tab" #f) ("n" #f))
    #:return TA
  ]{
  Display a histogram with @pyret{n} bins using data from @pyret{tab}
  which is a table with one column: @t-field["value" N].
  The range of the histogram is automatically inferred from the data.
  }

  @function["pie-chart"
    #:contract (a-arrow TA TA)
    #:args '(("tab" #f))
    #:return TA
  ]{
  Display a pie chart using data from @pyret{tab} which is a table with two columns:
  @t-field["label" S] and @t-field["value" N].
  }

  @;############################################################################
  @section{The Options Types and Default Values}

  An Option type is actually a function type consuming a default config and
  produces a desired config.

  To use a default config, you could construct
  @pyret-block{lam(default-configs): default-configs end}
  which consumes a default config and merely returns it. We provide a value
  @pyret{default-options} which is the above value for convenience.

  A new Option can be constructed by the using @secref["s:extend-expr"] on
  the default config.

  @pyret-block{
    new-options = lam(default-configs): default-configs.{val1: ..., val2: ...} end
  }

  Combining the @secref["s:extend-expr"] with the @secref["s:curried-apply-expr"],
  the above can be rewritten as:

  @pyret-block{
    new-options = _.{val1: ..., val2: ...}
  }

  @type-spec["PlotOptions" '()]

  A config associated with @pyret-id{PlotOptions} consists of the following fields:
  @a-record[(t-field "color" Color)]

  The default config is @t-record{color: blue}

  @examples{
    import image-structs as I
    my-plot-options-1 = _.{color: I.red}
    my-plot-options-2 = default-options
  }

  @type-spec["PlotWindowOptions" '()]

  A config associated with @pyret-id{PlotWindowOptions} consists of the following fields:
  @a-record[(t-field "x-min" N)
            (t-field "x-max" N)
            (t-field "y-min" N)
            (t-field "y-max" N)
            (t-field "infer-bounds" B)]

  The default config is @tt{plot-window-options} is
  @t-record{x-min: -10
            x-max: 10
            y-min: -10
            y-max: 10
            infer-bounds: false}

  If @pyret{infer-bounds} is true,
  @pyret{x-min}, @pyret{x-max}, @pyret{y-min}, @pyret{y-max} will be inferred,
  and old values will be overwritten.
}
