(module ../src/tests/benchmarks/list-creation.arr (file
                                                   "/home/joe/src/pyret-checkouts/pyret-lang/src/lang/pyret-lang-racket.rkt")
  (r:require
   (r:only-in racket/base current-read-interaction current-print void))
  (void (current-read-interaction repl-eval-pyret))
  (void (current-print print-pyret))
  (r:begin
   (r:require
    (r:rename-in
     (r:file
      "/home/joe/src/pyret-checkouts/pyret-lang/src/lang/racket-ffi/profile.rkt")
     (%PYRET-PROVIDE P)))
   (r:begin
    (r:define
     build-some-lists
     (p:pλ
      (n)
      ""
      (r:with-continuation-mark
       (r:quote pyret-mark)
       (r:srcloc "../src/tests/benchmarks/list-creation.arr" 5 0 35 133)
       (r:letrec
        ((g1687
          (r:cond
           ((p:pyret-true?
             (r:with-continuation-mark
              (r:quote pyret-mark)
              (r:srcloc "../src/tests/benchmarks/list-creation.arr" 7 7 75 6)
              (r:let*
               ((%obj n)
                (%field
                 (p:get-raw-field
                  (r:list "../src/tests/benchmarks/list-creation.arr" 7 7 75 6)
                  %obj
                  "_lessequal"))
                (arg1688 (p:mk-num 0)))
               ((p:p-base-method %field) %obj arg1688))))
            (r:letrec
             ((g1689
               (p:get-field
                (r:list "../src/tests/benchmarks/list-creation.arr" 7 18 86 2)
                list
                "empty")))
             g1689))
           ((p:pyret-true?
             (r:with-continuation-mark
              (r:quote pyret-mark)
              (r:srcloc "../src/tests/benchmarks/list-creation.arr" 8 6 95 5)
              (r:let*
               ((%obj n)
                (%field
                 (p:get-raw-field
                  (r:list "../src/tests/benchmarks/list-creation.arr" 8 6 95 5)
                  %obj
                  "_greaterthan"))
                (arg1690 (p:mk-num 0)))
               ((p:p-base-method %field) %obj arg1690))))
            (r:letrec
             ((g1691
               (r:with-continuation-mark
                (r:quote pyret-mark)
                (r:srcloc
                 "../src/tests/benchmarks/list-creation.arr"
                 8
                 15
                 104
                 54)
                (r:let*
                 ((%obj list)
                  (%field
                   (p:get-raw-field
                    (r:list
                     "../src/tests/benchmarks/list-creation.arr"
                     8
                     15
                     104
                     54)
                    %obj
                    "link"))
                  (arg1695
                   (r:with-continuation-mark
                    (r:quote pyret-mark)
                    (r:srcloc
                     "../src/tests/benchmarks/list-creation.arr"
                     8
                     25
                     114
                     18)
                    (r:let*
                     ((%obj list)
                      (%field
                       (p:get-raw-field
                        (r:list
                         "../src/tests/benchmarks/list-creation.arr"
                         8
                         25
                         114
                         18)
                        %obj
                        "range"))
                      (arg1692 (p:mk-num 0))
                      (arg1693 (p:mk-num 100)))
                     ((p:p-base-method %field) %obj arg1692 arg1693))))
                  (arg1696
                   (r:with-continuation-mark
                    (r:quote pyret-mark)
                    (r:srcloc
                     "../src/tests/benchmarks/list-creation.arr"
                     8
                     45
                     134
                     23)
                    ((p:p-base-app build-some-lists)
                     (r:with-continuation-mark
                      (r:quote pyret-mark)
                      (r:srcloc
                       "../src/tests/benchmarks/list-creation.arr"
                       8
                       62
                       151
                       5)
                      (r:let*
                       ((%obj n)
                        (%field
                         (p:get-raw-field
                          (r:list
                           "../src/tests/benchmarks/list-creation.arr"
                           8
                           62
                           151
                           5)
                          %obj
                          "_minus"))
                        (arg1694 (p:mk-num 1)))
                       ((p:p-base-method %field) %obj arg1694)))))))
                 ((p:p-base-method %field) %obj arg1695 arg1696)))))
             g1691))
           ((p:pyret-true? p:p-true)
            (r:letrec
             ((g1697
               (r:with-continuation-mark
                (r:quote pyret-mark)
                (r:srcloc
                 "../src/tests/benchmarks/list-creation.arr"
                 6
                 2
                 62
                 102)
                ((p:p-base-app raise) (p:mk-str "case: no cases matched")))))
             g1697)))))
        g1687))))
    (r:with-continuation-mark
     (r:quote pyret-mark)
     (r:srcloc "../src/tests/benchmarks/list-creation.arr" 12 0 170 23)
     (r:void ((p:p-base-app build-some-lists) (p:mk-num 5000)))))))
