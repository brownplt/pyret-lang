#lang racket/base

(require
  rackunit
  "../lang/unparser.rkt"
  "../lang/get-syntax-errors.rkt")


(check-exn exn:fail:read:pyret:missing-end?
  (lambda ()
    (parse-program (get-syntax-errors "test"
      (open-input-string "
      fun update-target-y(player-y, key):
        if key == 'right' : player-y + 2
        else if key == 'left': player-y - 2
        else : player-y
      end
      "))))
  "missing-end")

(check-exn exn:fail:read:pyret:else-colon?
  (lambda ()
    (parse-program (get-syntax-errors "test"
      (open-input-string "fun update-target-y(player-y, key):
        if key == 'right' : player-y + 2
        else if key == 'left': player-y - 2
        else : player-y
        end
      end"))))
  "else-colon")

(check-exn exn:fail:read:pyret:caret-no-parens?
  (lambda ()
    (parse-program (get-syntax-errors "caret-no-parens"
      (open-input-string "
check:
  fun add4(n): n + 4;
  3^add4 is 7
end
"))))
    "caret-no-parens")
