(module cmdline (file
                 "/home/joe/src/pyret-lang/src/lang/pyret-lang-whalesong.rkt")
  (r:require pyret/lang/pyret-lib/libs)
  (r:begin
   (r:begin
    (r:with-handlers
     ((p:exn:fail:pyret?
       (r:lambda
        (%exn)
        (r:define e (p:mk-exn %exn))
        (r:letrec
         ((g1589
           ((p:check-fun
             ((p:check-fun
               (p:mk-fun
                (r:λ
                 (contract1585)
                 (r:letrec
                  ((g1590
                    (p:extend
                     (r:list "cmdline" 3 0 24 117)
                     (p:mk-fun
                      (r:λ
                       (arg1586)
                       (r:letrec
                        ((g1591
                          ((p:check-fun
                            (p:mk-fun
                             (r:λ
                              (specimen1588)
                              (r:letrec
                               ((g1592
                                 ((p:check-fun
                                   check-brand
                                   (r:list "cmdline" 3 0 24 117))
                                  Any?
                                  specimen1588
                                  (p:mk-str "Any"))))
                               g1592))
                             "internal contract for Any")
                            (r:list "cmdline" 3 0 24 117))
                           ((p:check-fun
                             contract1585
                             (r:list "cmdline" 3 0 24 117))
                            ((p:check-fun
                              (p:mk-fun
                               (r:λ
                                (specimen1587)
                                (r:letrec
                                 ((g1593
                                   ((p:check-fun
                                     check-brand
                                     (r:list "cmdline" 3 0 24 117))
                                    Any?
                                    specimen1587
                                    (p:mk-str "Any"))))
                                 g1593))
                               "internal contract for Any")
                              (r:list "cmdline" 3 0 24 117))
                             arg1586)))))
                        g1591))
                      "internal contract for (Any -> Any)")
                     (r:make-hash
                      (r:list
                       (r:cons
                        (p:p-str-s (p:mk-str "doc"))
                        (p:get-field
                         (r:list "cmdline" 3 0 24 117)
                         contract1585
                         (p:p-str-s (p:mk-str "doc")))))))))
                  g1590))
                "internal contract for (Any -> Any)")
               (r:list "cmdline" 3 0 24 117))
              (p:mk-fun
               (r:λ
                (e)
                (r:letrec
                 ((g1594
                   ((p:check-fun print (r:list "cmdline" 6 2 79 47))
                    (p:mk-str "Should be 'just an exceptional value':")))
                  (g1595 ((p:check-fun print (r:list "cmdline" 7 2 129 8)) e)))
                 g1595))
               ""))
             (r:list "cmdline" 3 0 24 117))
            ((p:check-fun
              (p:get-field
               (r:list "cmdline" 3 0 24 117)
               error
               (p:p-str-s (p:mk-str "make-error")))
              (r:list "cmdline" 3 0 24 117))
             e))))
         g1589))))
     (r:letrec
      ((g1596
        ((p:check-fun raise (r:list "cmdline" 4 2 31 34))
         (p:mk-str "just an exceptional value"))))
      g1596)))))
