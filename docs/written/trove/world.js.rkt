#lang scribble/base
@(require "../../scribble-api.rkt"
          "../abbrevs.rkt")
@(define WC (a-id "WorldConfig" (xref "world" "WorldConfig")))
@docmodule["world"]{
  The Pyret world library is based on the universe teachpack in HtDP, and borrows much of the language for documentation. You can find documentation for the teachpack here:

  @url["http://docs.racket-lang.org/teachpack/2htdpuniverse.html"]

  @section{Functions}
  @function["big-bang"
            #:contract (a-arrow "a"
                                (a-app L
                                       WC)
                                "a")
            #:args (list '("init" "")
                         '("handlers" ""))]{
    This function starts a world program in the initial state specified
    by @pyret["init"]. Its behaviour is defined via the handler functions
    @pyret["handlers"]. These handler functions allow you to define various
    program behaviours: rendering, mouse and keyboard events, timed events,
    when to shut down, etc. A world specification may not contain more than
    one @secref[(tag-name "world" "to-draw")] or @secref[(tag-name "world"
    "on-tick")] handlers. This function will return the last world state
    when the stop condition is satisfied (see @secref[(tag-name "world"
    "stop-when")]) or when the canvas is closed.
  }
  @function["to-draw"
            #:contract (a-arrow (a-arrow "a"
                                         (a-id "Scene" (xref "image" "Scene")))
                                WC)
            #:args (list '("drawer" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will inform the world program
    what to draw.
  }
  @function["on-tick"
            #:contract (a-arrow (a-arrow "a"
                                         "a")
                                WC)
            #:args (list '("handler" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will be called each program tick
    with the current world state.
  }
  @function["on-tick-n"
            #:contract (a-arrow (a-arrow "a"
                                         "a")
                                N
                                WC)
            #:args (list '("handler" "")
                         '("n" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will be called every @pyret["n"]
    program ticks with the current world state.
  }
  @function["on-key"
            #:contract (a-arrow (a-arrow "a"
                                         S
                                         "a")
                                WC)
            #:args (list '("onKey" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will be called every time a
    key is pressed. The function is called with the current world state
    and a @secref[(tag-name "<global>" "String")] representing the pressed
    key. For most keys, this is just the corresponding single character. Some
    examples of single character strings that you may receive are @pyret{"a"},
    @pyret{"b"}, @pyret{"c"}, @pyret{" "}, @pyret{"\r"} (return/enter key),
    @pyret{"\t"} (tab key), @pyret{"\b"} (backspace). In some cases, you
    may receive multiple characters. For a full enumeration of these, you
    should reference the Racket world documentation on @hyperlink["http://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28tech._world._keyevent%29"]{key events}.
  }
  @function["on-mouse"
            #:contract (a-arrow (a-arrow "a"
                                         N N S
                                         "a")
                                WC)
            #:args (list '("onMouse" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will be called on every sampled
    mouse movement. The function will receive the world state, the current
    @pyret["x"] and @pyret["y"] positions of the mouse, and a @secref[(tag-name
    "<global>" "String")] representing a mouse event. Possible mouse
    events are:


    @itemlist[(item (pyret "\"button-down\"") 
                    " signals that the computer user has pushed a mouse button down;")
              (item (pyret "\"button-up\"") 
                    " signals that the computer user has let go of a mouse button;")
              (item (pyret "\"drag\"") 
                    " signals that the computer user is dragging the mouse. A dragging event occurs when the mouse moves while a mouse button is pressed.")
              (item (pyret "\"move\"") 
                    " signals that the computer user has moved the mouse;")
              (item (pyret "\"enter\"") 
                    " signals that the computer user has moved the mouse into the canvas area; and")
              (item (pyret "\"leave\"") 
                    " signals that the computer user has moved the mouse out of the canvas area.")]
  }
  @function["stop-when"
            #:contract (a-arrow (a-arrow "a" B)
                                WC)
            #:args (list '("stopper" ""))]{
    Consumes a function and returns a handler that, when passed to
    @secref[(tag-name "world" "big-bang")], will be called to determine if
    the world should stop running. If the function returns @pyret["true"],
    then no other handlers will be called. The @secref[(tag-name "world" "big-bang")] 
    function will return this last world state.
  }
  @function["is-world-config"
            #:contract (a-arrow "Any" B)
            #:args (list '("v" ""))]{
    Tests if the input is of type @secref[(tag-name "world" "WorldConfig")].
  }
  @function["is-key-equal"
            #:contract (a-arrow S
                                S
                                B)
            #:args (list '("key1" "")
                         '("key2" ""))]{
    Tests if two key events are equals to each other.
  }
}
