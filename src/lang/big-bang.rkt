#lang racket/gui

;; This sketch comes from Matthias Felleisen:
;; http://lists.racket-lang.org/users/archive/2013-April/057407.html

(require 2htdp/private/world 2htdp/image)
(provide my-bb run-it)

;; -----------------------------------------------------------------------------
;; small adapter 

(define (my-bb world0 ht)
  (lambda ()
    (new world% 
         (world0 world0)
         (on-draw    (hash-ref ht 'to-draw))
         (on-tick    (hash-ref ht 'on-tick))
         (on-mouse   (hash-ref ht 'on-mouse void))
         (on-key     (hash-ref ht 'on-key void))
         (record?    (hash-ref ht 'record #f))
         (stop-when  (hash-ref ht 'stop-when (lambda _ (displayln _) (lambda _ #f))))
         (state      (hash-ref ht 'state #f))
         (check-with (hash-ref ht 'check-with (lambda _ (lambda _ #t))))
         (on-release (hash-ref ht 'on-release void))
         (on-pad     (hash-ref ht 'on-pad #f))
         (name       (hash-ref ht 'name "no  name"))
         (register   (hash-ref ht 'register #f))
         (on-receive (hash-ref ht 'on-receive void))
         )))

;; (-> Object) -> Any
(define (run-it o)
  (define esp (make-eventspace))
  (define thd (eventspace-handler-thread esp))
  (with-handlers ((exn:break? (lambda (x) (break-thread thd))))
    (define obj:ch (make-channel))
    (parameterize ([current-eventspace esp])
      (queue-callback (lambda () (displayln o) (channel-put obj:ch (o)))))
    (send (channel-get obj:ch) last)))

