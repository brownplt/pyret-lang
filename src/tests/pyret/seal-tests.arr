#lang pyret

import "test.arr" as T
provide { run-tests: run-tests } end

fun run-tests():
  
  T.check-equals("simple seal access",
    \( seal({x:5}, ["x"]).x ),
    5)

  T.check-equals("double seal access",
    \( seal({x:5, y:10}, ["y"])^seal(["y"]).y ),
    10)

  T.check-equals("double seal access 2",
    \( seal({x:5, y:10, z: 15}, ["y", "z"])^seal(["y"]).y ),
    10)

  T.check-exn("seal block",
    \( seal({x:5}, []).x ),
    error.is-field-not-found)

  T.check-exn("seal block 2",
    \( seal(seal({x:5, y:5}, ['y']), ['y']).x ),
    error.is-field-not-found)

  T.check-exn("seal added doesn't re-access",
    \( {x:5, y:10}^seal(["x"])^seal(["y", "x"]).y ),
    error.is-field-not-found)

  T.check-exn("builtin seal",
    \( seal(2, ['minus']).add(2, 3) ),
    error.is-field-not-found)

  T.check-equals("builtin seal 2",
    \( seal(2, ['minus']).minus(2) ),
    0)

  T.check-equals("builtin seal creates new number",
    \( seal(2, ['minus']).minus(2).add(1) ),
    1)

  T.check-exn("adding is an error outside the seal",
    \( {x:5}^seal([]).{x : 10} ),
    \e: (e.message.contains("seal")))

  T.check-equals("adding is OK inside",
    \( {x:5}^seal(["x", "y"]).{ y: 10 }.y ),
    10)

  T.check-equals("updating is OK inside",
    \( {x:5}^seal(["x"]).{ x: 10 }.x ),
    10)

end
