#lang racket/base

(require ragg/examples/01-equal
         rackunit)

(check-equal? (syntax->datum (parse ""))
              '(equal))
(check-equal? (syntax->datum (parse "01"))
              '(equal (zero (equal) #\0)
                      (one (equal) #\1)))
(check-equal? (syntax->datum (parse "10"))
              '(equal (one (equal) #\1)
                      (zero (equal) #\0)))
(check-equal? (syntax->datum (parse "0011"))
              '(equal (zero (equal) #\0)
                      (one (equal (zero (equal) #\0) 
                                  (one (equal) #\1))
                           #\1)))
(check-equal? (syntax->datum (parse "0110"))
              '(equal (one (equal (zero (equal) #\0)
                                  (one (equal) #\1))
                           #\1)
                      (zero (equal) #\0)))

(check-equal? (syntax->datum (parse "1100"))
              '(equal (one (equal) #\1) 
                      (zero (equal (one (equal) #\1) 
                                   (zero (equal) #\0))
                            #\0)))

