#lang scribble/base
@(require "../../scribble-api.rkt"
          "../abbrevs.rkt")
@(define WC (a-id "WorldConfig" (xref "world" "WorldConfig")))
@docmodule["world"]{
  @section{On WorldState and HandlerResult}
  @section{Functions}
  @function["big-bang"
            #:contract (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                (a-app L
                                       WC)
                                (a-id "WorldState" (xref "world" "WorldState")))
            #:args (list '("init" "")
                         '("handlers" ""))]
  @function["to-draw"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                         (a-id "Scene" (xref "image" "Scene")))
                                WC)
            #:args (list '("drawer" ""))]
  @function["on-tick"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                         (a-id "HandlerResult" (xref "world" "HandlerResult")))
                                WC)
            #:args (list '("handler" ""))]
  @function["on-tick-n"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                         (a-id "HandlerResult" (xref "world" "HandlerResult")))
                                N
                                WC)
            #:args (list '("handler" "")
                         '("n" ""))]
  @function["on-key"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                         S
                                         (a-id "HandlerResult" (xref "world" "HandlerResult")))
                                WC)
            #:args (list '("onKey" ""))]
  @function["on-mouse"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState"))
                                         N N S
                                         (a-id "HandlerResult" (xref "world" "HandlerResult")))
                                WC)
            #:args (list '("onMouse" ""))]
  @function["stop-when"
            #:contract (a-arrow (a-arrow (a-id "WorldState" (xref "world" "WorldState")) B)
                                WC)
            #:args (list '("stopper" ""))]
  @function["is-world-config"
            #:contract (a-arrow "Any" B)
            #:args (list '("v" ""))]
  @function["is-key-equal"
            #:contract (a-arrow S
                                S
                                B)
            #:args (list '("key1" "")
                         '("key2" ""))]
}
