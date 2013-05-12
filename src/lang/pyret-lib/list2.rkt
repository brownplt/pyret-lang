(module cmdline (file
                 "/home/joe/src/pyret-lang/src/lang/pyret-lang-whalesong.rkt")
  (r:begin
   (r:begin
    (r:define
     List16288
     ((p:check-fun brander (r:list "cmdline" 15 0 163 1953))))
    (r:define
     is-List
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16296)
         (r:letrec
          ((g16329
            (p:extend
             (r:list "cmdline" 15 0 163 1953)
             (p:mk-fun
              (r:λ
               (arg16297)
               (r:letrec
                ((g16330
                  ((p:check-fun contract16296 (r:list "cmdline" 15 0 163 1953))
                   ((p:check-fun
                     (p:mk-fun
                      (r:λ
                       (specimen16298)
                       (r:letrec
                        ((g16331
                          ((p:check-fun
                            check-brand
                            (r:list "cmdline" 15 0 163 1953))
                           Any?
                           specimen16298
                           (p:mk-str "Any"))))
                        g16331))
                      "internal contract for Any")
                     (r:list "cmdline" 15 0 163 1953))
                    arg16297))))
                g16330))
              "internal contract for (Any -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 15 0 163 1953)
                 contract16296
                 "doc")))))))
          g16329))
        "internal contract for (Any -> Any)")
       (r:list "cmdline" 15 0 163 1953))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16293)
          (r:letrec
           ((g16332
             (p:extend
              (r:list "cmdline" 15 0 163 1953)
              (p:mk-fun
               (r:λ
                (arg16294)
                (r:letrec
                 ((g16333
                   ((p:check-fun
                     contract16293
                     (r:list "cmdline" 15 0 163 1953))
                    ((p:check-fun
                      (p:mk-fun
                       (r:λ
                        (specimen16295)
                        (r:letrec
                         ((g16334
                           ((p:check-fun
                             check-brand
                             (r:list "cmdline" 15 0 163 1953))
                            Any?
                            specimen16295
                            (p:mk-str "Any"))))
                         g16334))
                       "internal contract for Any")
                      (r:list "cmdline" 15 0 163 1953))
                     arg16294))))
                 g16333))
               "internal contract for (Any -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 15 0 163 1953)
                  contract16293
                  "doc")))))))
           g16332))
         "internal contract for (Any -> Any)")
        (r:list "cmdline" 15 0 163 1953))
       (p:mk-fun
        (r:λ
         (specimen)
         (r:letrec
          ((g16335
            (r:let*
             ((%obj List16288)
              (%field
               (p:get-raw-field (r:list "cmdline" 15 0 163 1953) %obj "check"))
              (arg16336 specimen))
             (r:cond
              ((p:p-method? %field) ((p:p-method-f %field) %obj arg16336))
              (else
               ((p:check-fun %field (r:list "cmdline" 15 0 163 1953))
                arg16336))))))
          g16335))
        "is-List: This function checks that its argument is an\n             instance of the List type."))))
    (r:define List? is-List)
    (r:define
     empty_base16290
     (p:mk-object
      (r:make-immutable-hash
       (r:list
        (r:cons
         (p:p-str-s (p:mk-str "push"))
         (p:mk-method
          (r:λ
           (self elt)
           (r:letrec
            ((g16337
              ((p:check-fun link (r:list "cmdline" 102 19 2097 15)) elt self)))
            g16337))))
        (r:cons
         (p:p-str-s (p:mk-str "length"))
         (p:mk-method (r:λ (self) (r:letrec ((g16338 (p:mk-num 0))) g16338))))
        (r:cons
         (p:p-str-s (p:mk-str "map"))
         (p:mk-method
          (r:λ
           (self f)
           (r:letrec
            ((g16339 ((p:check-fun empty (r:list "cmdline" 20 18 232 7)))))
            g16339))))
        (r:cons
         (p:p-str-s (p:mk-str "filter"))
         (p:mk-method
          (r:λ
           (self f)
           (r:letrec
            ((g16340 ((p:check-fun empty (r:list "cmdline" 22 21 263 7)))))
            g16340))))
        (r:cons
         (p:p-str-s (p:mk-str "foldr"))
         (p:mk-method (r:λ (self f base) (r:letrec ((g16341 base)) g16341))))
        (r:cons
         (p:p-str-s (p:mk-str "foldl"))
         (p:mk-method (r:λ (self f base) (r:letrec ((g16342 base)) g16342))))
        (r:cons
         (p:p-str-s (p:mk-str "member"))
         (p:mk-method
          (r:λ (self elt) (r:letrec ((g16343 (p:mk-bool #f))) g16343))))
        (r:cons
         (p:p-str-s (p:mk-str "take"))
         (p:mk-method
          (r:λ
           (self n)
           (r:letrec
            ((g16344
              (r:cond
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 32 10 411 11)
                     %obj
                     "equals"))
                   (arg16345 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16345))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 32 10 411 11))
                     arg16345)))))
                (r:letrec
                 ((g16346
                   ((p:check-fun empty (r:list "cmdline" 32 25 426 7)))))
                 g16346))
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 33 10 444 16)
                     %obj
                     "greaterthan"))
                   (arg16347 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16347))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 33 10 444 16))
                     arg16347)))))
                (r:letrec
                 ((g16348
                   ((p:check-fun raise (r:list "cmdline" 33 30 464 28))
                    (p:mk-str "take: took too many"))))
                 g16348))
               ((p:pyret-true? else)
                (r:letrec
                 ((g16349
                   ((p:check-fun raise (r:list "cmdline" 34 18 511 31))
                    (p:mk-str "take: invalid argument"))))
                 g16349))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16350
                   ((p:check-fun raise (r:list "cmdline" 31 6 395 157))
                    (p:mk-str "cond: no cases matched"))))
                 g16350))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16351
                   ((p:check-fun raise (r:list "cmdline" 31 6 395 157))
                    (p:mk-str "cond: no cases matched"))))
                 g16351)))))
            g16344))))
        (r:cons
         (p:p-str-s (p:mk-str "drop"))
         (p:mk-method
          (r:λ
           (self n)
           (r:letrec
            ((g16352
              (r:cond
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 40 10 604 11)
                     %obj
                     "equals"))
                   (arg16353 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16353))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 40 10 604 11))
                     arg16353)))))
                (r:letrec
                 ((g16354
                   ((p:check-fun empty (r:list "cmdline" 40 25 619 7)))))
                 g16354))
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 41 10 637 16)
                     %obj
                     "greaterthan"))
                   (arg16355 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16355))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 41 10 637 16))
                     arg16355)))))
                (r:letrec
                 ((g16356
                   ((p:check-fun raise (r:list "cmdline" 41 30 657 31))
                    (p:mk-str "drop: dropped too many"))))
                 g16356))
               ((p:pyret-true? else)
                (r:letrec
                 ((g16357
                   ((p:check-fun raise (r:list "cmdline" 42 18 707 31))
                    (p:mk-str "drop: invalid argument"))))
                 g16357))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16358
                   ((p:check-fun raise (r:list "cmdline" 39 6 588 160))
                    (p:mk-str "cond: no cases matched"))))
                 g16358))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16359
                   ((p:check-fun raise (r:list "cmdline" 39 6 588 160))
                    (p:mk-str "cond: no cases matched"))))
                 g16359)))))
            g16352))))
        (r:cons
         (p:p-str-s (p:mk-str "equals"))
         (p:mk-method
          (r:λ
           (self other)
           (r:letrec
            ((g16360
              ((p:check-fun is-empty (r:list "cmdline" 47 6 790 15)) other)))
            g16360))))
        (r:cons
         (p:p-str-s (p:mk-str "tostring"))
         (p:mk-method
          (r:λ (self) (r:letrec ((g16361 (p:mk-str "[]"))) g16361))))))))
    (r:define
     empty16289
     ((p:check-fun brander (r:list "cmdline" 16 2 176 664))))
    (r:define
     is-empty
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16302)
         (r:letrec
          ((g16362
            (p:extend
             (r:list "cmdline" 16 2 176 664)
             (p:mk-fun
              (r:λ
               (arg16303)
               (r:letrec
                ((g16363
                  ((p:check-fun contract16302 (r:list "cmdline" 16 2 176 664))
                   ((p:check-fun
                     (p:mk-fun
                      (r:λ
                       (specimen16304)
                       (r:letrec
                        ((g16364
                          ((p:check-fun
                            check-brand
                            (r:list "cmdline" 16 2 176 664))
                           Any?
                           specimen16304
                           (p:mk-str "Any"))))
                        g16364))
                      "internal contract for Any")
                     (r:list "cmdline" 16 2 176 664))
                    arg16303))))
                g16363))
              "internal contract for (Any -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 16 2 176 664)
                 contract16302
                 "doc")))))))
          g16362))
        "internal contract for (Any -> Any)")
       (r:list "cmdline" 16 2 176 664))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16299)
          (r:letrec
           ((g16365
             (p:extend
              (r:list "cmdline" 16 2 176 664)
              (p:mk-fun
               (r:λ
                (arg16300)
                (r:letrec
                 ((g16366
                   ((p:check-fun contract16299 (r:list "cmdline" 16 2 176 664))
                    ((p:check-fun
                      (p:mk-fun
                       (r:λ
                        (specimen16301)
                        (r:letrec
                         ((g16367
                           ((p:check-fun
                             check-brand
                             (r:list "cmdline" 16 2 176 664))
                            Any?
                            specimen16301
                            (p:mk-str "Any"))))
                         g16367))
                       "internal contract for Any")
                      (r:list "cmdline" 16 2 176 664))
                     arg16300))))
                 g16366))
               "internal contract for (Any -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 16 2 176 664)
                  contract16299
                  "doc")))))))
           g16365))
         "internal contract for (Any -> Any)")
        (r:list "cmdline" 16 2 176 664))
       (p:mk-fun
        (r:λ
         (specimen)
         (r:letrec
          ((g16368
            (r:let*
             ((%obj empty16289)
              (%field
               (p:get-raw-field (r:list "cmdline" 16 2 176 664) %obj "check"))
              (arg16369 specimen))
             (r:cond
              ((p:p-method? %field) ((p:p-method-f %field) %obj arg16369))
              (else
               ((p:check-fun %field (r:list "cmdline" 16 2 176 664))
                arg16369))))))
          g16368))
        "is-empty: This function checks that its argument is an\n             instance of the empty type."))))
    (r:define empty? is-empty)
    (r:define
     empty
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16306)
         (r:letrec
          ((g16370
            (p:extend
             (r:list "cmdline" 16 2 176 664)
             (p:mk-fun
              (r:λ
               ()
               (r:letrec
                ((g16371
                  ((p:check-fun
                    contract16306
                    (r:list "cmdline" 16 2 176 664)))))
                g16371))
              "internal contract for ( -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 16 2 176 664)
                 contract16306
                 "doc")))))))
          g16370))
        "internal contract for ( -> Any)")
       (r:list "cmdline" 16 2 176 664))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16305)
          (r:letrec
           ((g16372
             (p:extend
              (r:list "cmdline" 16 2 176 664)
              (p:mk-fun
               (r:λ
                ()
                (r:letrec
                 ((g16373
                   ((p:check-fun
                     contract16305
                     (r:list "cmdline" 16 2 176 664)))))
                 g16373))
               "internal contract for ( -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 16 2 176 664)
                  contract16305
                  "doc")))))))
           g16372))
         "internal contract for ( -> Any)")
        (r:list "cmdline" 16 2 176 664))
       (p:mk-fun
        (r:λ
         ()
         (r:letrec
          ((g16374
            (r:let*
             ((%obj List16288)
              (%field
               (p:get-raw-field (r:list "cmdline" 16 2 176 664) %obj "brand"))
              (arg16376
               (r:let*
                ((%obj empty16289)
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 16 2 176 664)
                   %obj
                   "brand"))
                 (arg16375
                  (p:extend
                   (r:list "cmdline" 16 2 176 664)
                   empty_base16290
                   (r:make-hash (r:list)))))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16375))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 16 2 176 664))
                   arg16375))))))
             (r:cond
              ((p:p-method? %field) ((p:p-method-f %field) %obj arg16376))
              (else
               ((p:check-fun %field (r:list "cmdline" 16 2 176 664))
                arg16376))))))
          g16374))
        "empty: Creates an instance of empty"))))
    (r:define
     link_base16292
     (p:mk-object
      (r:make-immutable-hash
       (r:list
        (r:cons
         (p:p-str-s (p:mk-str "push"))
         (p:mk-method
          (r:λ
           (self elt)
           (r:letrec
            ((g16377
              ((p:check-fun link (r:list "cmdline" 102 19 2097 15)) elt self)))
            g16377))))
        (r:cons
         (p:p-str-s (p:mk-str "length"))
         (p:mk-method
          (r:λ
           (self)
           (r:letrec
            ((g16378
              (r:let*
               ((%obj (p:mk-num 1))
                (%field
                 (p:get-raw-field (r:list "cmdline" 54 18 888 25) %obj "add"))
                (arg16379
                 (r:let*
                  ((%obj
                    (p:get-field (r:list "cmdline" 54 24 894 9) self "rest"))
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 54 24 894 18)
                     %obj
                     "length")))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 54 24 894 18))))))))
               (r:cond
                ((p:p-method? %field) ((p:p-method-f %field) %obj arg16379))
                (else
                 ((p:check-fun %field (r:list "cmdline" 54 18 888 25))
                  arg16379))))))
            g16378))))
        (r:cons
         (p:p-str-s (p:mk-str "map"))
         (p:mk-method
          (r:λ
           (self f)
           (r:letrec
            ((g16380
              ((p:check-fun link (r:list "cmdline" 56 18 934 36))
               ((p:check-fun f (r:list "cmdline" 56 18 934 13))
                (p:get-field (r:list "cmdline" 56 20 936 10) self "first"))
               (r:let*
                ((%obj
                  (p:get-field (r:list "cmdline" 56 37 953 9) self "rest"))
                 (%field
                  (p:get-raw-field (r:list "cmdline" 56 37 953 16) %obj "map"))
                 (arg16381 f))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16381))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 56 37 953 16))
                   arg16381)))))))
            g16380))))
        (r:cons
         (p:p-str-s (p:mk-str "filter"))
         (p:mk-method
          (r:λ
           (self f)
           (r:letrec
            ((g16382
              (r:cond
               ((p:pyret-true?
                 ((p:check-fun f (r:list "cmdline" 60 10 1016 13))
                  (p:get-field (r:list "cmdline" 60 12 1018 10) self "first")))
                (r:letrec
                 ((g16383
                   ((p:check-fun link (r:list "cmdline" 60 27 1033 39))
                    ((p:check-fun f (r:list "cmdline" 60 27 1033 13))
                     (p:get-field
                      (r:list "cmdline" 60 29 1035 10)
                      self
                      "first"))
                    (r:let*
                     ((%obj
                       (p:get-field
                        (r:list "cmdline" 60 46 1052 9)
                        self
                        "rest"))
                      (%field
                       (p:get-raw-field
                        (r:list "cmdline" 60 46 1052 19)
                        %obj
                        "filter"))
                      (arg16384 f))
                     (r:cond
                      ((p:p-method? %field)
                       ((p:p-method-f %field) %obj arg16384))
                      (else
                       ((p:check-fun %field (r:list "cmdline" 60 46 1052 19))
                        arg16384)))))))
                 g16383))
               ((p:pyret-true? else)
                (r:letrec
                 ((g16385
                   (r:let*
                    ((%obj
                      (p:get-field
                       (r:list "cmdline" 61 18 1091 9)
                       self
                       "rest"))
                     (%field
                      (p:get-raw-field
                       (r:list "cmdline" 61 18 1091 19)
                       %obj
                       "filter"))
                     (arg16386 f))
                    (r:cond
                     ((p:p-method? %field)
                      ((p:p-method-f %field) %obj arg16386))
                     (else
                      ((p:check-fun %field (r:list "cmdline" 61 18 1091 19))
                       arg16386))))))
                 g16385))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16387
                   ((p:check-fun raise (r:list "cmdline" 59 6 1000 120))
                    (p:mk-str "cond: no cases matched"))))
                 g16387))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16388
                   ((p:check-fun raise (r:list "cmdline" 59 6 1000 120))
                    (p:mk-str "cond: no cases matched"))))
                 g16388)))))
            g16382))))
        (r:cons
         (p:p-str-s (p:mk-str "member"))
         (p:mk-method
          (r:λ
           (self elt)
           (r:letrec
            ((g16389
              (r:let*
               ((%obj
                 (r:let*
                  ((%obj elt)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 65 23 1154 22)
                     %obj
                     "equals"))
                   (arg16390
                    (p:get-field
                     (r:list "cmdline" 65 34 1165 10)
                     self
                     "first")))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16390))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 65 23 1154 22))
                     arg16390)))))
                (%field
                 (p:get-raw-field (r:list "cmdline" 65 23 1154 48) %obj "or"))
                (arg16392
                 (r:let*
                  ((%obj
                    (p:get-field (r:list "cmdline" 65 49 1180 9) self "rest"))
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 65 49 1180 21)
                     %obj
                     "member"))
                   (arg16391 elt))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16391))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 65 49 1180 21))
                     arg16391))))))
               (r:cond
                ((p:p-method? %field) ((p:p-method-f %field) %obj arg16392))
                (else
                 ((p:check-fun %field (r:list "cmdline" 65 23 1154 48))
                  arg16392))))))
            g16389))))
        (r:cons
         (p:p-str-s (p:mk-str "foldr"))
         (p:mk-method
          (r:λ
           (self f base)
           (r:letrec
            ((g16393
              ((p:check-fun f (r:list "cmdline" 67 26 1231 39))
               (p:get-field (r:list "cmdline" 67 28 1233 10) self "first")
               (r:let*
                ((%obj
                  (p:get-field (r:list "cmdline" 67 40 1245 9) self "rest"))
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 67 40 1245 24)
                   %obj
                   "foldr"))
                 (arg16394 f)
                 (arg16395 base))
                (r:cond
                 ((p:p-method? %field)
                  ((p:p-method-f %field) %obj arg16394 arg16395))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 67 40 1245 24))
                   arg16394
                   arg16395)))))))
            g16393))))
        (r:cons
         (p:p-str-s (p:mk-str "foldl"))
         (p:mk-method
          (r:λ
           (self f base)
           (r:letrec
            ((g16396
              (r:let*
               ((%obj
                 (p:get-field (r:list "cmdline" 69 26 1299 9) self "rest"))
                (%field
                 (p:get-raw-field
                  (r:list "cmdline" 69 26 1299 39)
                  %obj
                  "foldl"))
                (arg16397 f)
                (arg16398
                 ((p:check-fun f (r:list "cmdline" 69 45 1318 19))
                  (p:get-field (r:list "cmdline" 69 47 1320 10) self "first")
                  base)))
               (r:cond
                ((p:p-method? %field)
                 ((p:p-method-f %field) %obj arg16397 arg16398))
                (else
                 ((p:check-fun %field (r:list "cmdline" 69 26 1299 39))
                  arg16397
                  arg16398))))))
            g16396))))
        (r:cons
         (p:p-str-s (p:mk-str "take"))
         (p:mk-method
          (r:λ
           (self n)
           (r:letrec
            ((g16399
              (r:cond
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 73 10 1382 11)
                     %obj
                     "equals"))
                   (arg16400 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16400))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 73 10 1382 11))
                     arg16400)))))
                (r:letrec
                 ((g16401
                   ((p:check-fun empty (r:list "cmdline" 73 25 1397 7)))))
                 g16401))
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 74 10 1415 16)
                     %obj
                     "greaterthan"))
                   (arg16402 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16402))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 74 10 1415 16))
                     arg16402)))))
                (r:letrec
                 ((g16403
                   ((p:check-fun link (r:list "cmdline" 74 30 1435 43))
                    (p:get-field (r:list "cmdline" 74 30 1435 10) self "first")
                    (r:let*
                     ((%obj
                       (p:get-field
                        (r:list "cmdline" 74 46 1451 9)
                        self
                        "rest"))
                      (%field
                       (p:get-raw-field
                        (r:list "cmdline" 74 46 1451 26)
                        %obj
                        "take"))
                      (arg16405
                       (r:let*
                        ((%obj n)
                         (%field
                          (p:get-raw-field
                           (r:list "cmdline" 74 61 1466 10)
                           %obj
                           "minus"))
                         (arg16404 (p:mk-num 1)))
                        (r:cond
                         ((p:p-method? %field)
                          ((p:p-method-f %field) %obj arg16404))
                         (else
                          ((p:check-fun
                            %field
                            (r:list "cmdline" 74 61 1466 10))
                           arg16404))))))
                     (r:cond
                      ((p:p-method? %field)
                       ((p:p-method-f %field) %obj arg16405))
                      (else
                       ((p:check-fun %field (r:list "cmdline" 74 46 1451 26))
                        arg16405)))))))
                 g16403))
               ((p:pyret-true? else)
                (r:letrec
                 ((g16406
                   ((p:check-fun raise (r:list "cmdline" 75 18 1497 31))
                    (p:mk-str "take: invalid argument"))))
                 g16406))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16407
                   ((p:check-fun raise (r:list "cmdline" 72 6 1366 172))
                    (p:mk-str "cond: no cases matched"))))
                 g16407))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16408
                   ((p:check-fun raise (r:list "cmdline" 72 6 1366 172))
                    (p:mk-str "cond: no cases matched"))))
                 g16408)))))
            g16399))))
        (r:cons
         (p:p-str-s (p:mk-str "drop"))
         (p:mk-method
          (r:λ
           (self n)
           (r:letrec
            ((g16409
              (r:cond
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 81 10 1590 11)
                     %obj
                     "equals"))
                   (arg16410 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16410))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 81 10 1590 11))
                     arg16410)))))
                (r:letrec ((g16411 self)) g16411))
               ((p:pyret-true?
                 (r:let*
                  ((%obj n)
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 82 10 1620 16)
                     %obj
                     "greaterthan"))
                   (arg16412 (p:mk-num 0)))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16412))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 82 10 1620 16))
                     arg16412)))))
                (r:letrec
                 ((g16413
                   (r:let*
                    ((%obj
                      (p:get-field
                       (r:list "cmdline" 82 30 1640 9)
                       self
                       "rest"))
                     (%field
                      (p:get-raw-field
                       (r:list "cmdline" 82 30 1640 26)
                       %obj
                       "drop"))
                     (arg16415
                      (r:let*
                       ((%obj n)
                        (%field
                         (p:get-raw-field
                          (r:list "cmdline" 82 45 1655 10)
                          %obj
                          "minus"))
                        (arg16414 (p:mk-num 1)))
                       (r:cond
                        ((p:p-method? %field)
                         ((p:p-method-f %field) %obj arg16414))
                        (else
                         ((p:check-fun %field (r:list "cmdline" 82 45 1655 10))
                          arg16414))))))
                    (r:cond
                     ((p:p-method? %field)
                      ((p:p-method-f %field) %obj arg16415))
                     (else
                      ((p:check-fun %field (r:list "cmdline" 82 30 1640 26))
                       arg16415))))))
                 g16413))
               ((p:pyret-true? else)
                (r:letrec
                 ((g16416
                   ((p:check-fun raise (r:list "cmdline" 83 18 1685 31))
                    (p:mk-str "drop: invalid argument"))))
                 g16416))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16417
                   ((p:check-fun raise (r:list "cmdline" 80 6 1574 152))
                    (p:mk-str "cond: no cases matched"))))
                 g16417))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16418
                   ((p:check-fun raise (r:list "cmdline" 80 6 1574 152))
                    (p:mk-str "cond: no cases matched"))))
                 g16418)))))
            g16409))))
        (r:cons
         (p:p-str-s (p:mk-str "equals"))
         (p:mk-method
          (r:λ
           (self other)
           (r:letrec
            ((g16419
              (r:cond
               ((p:pyret-true?
                 ((p:check-fun is-link (r:list "cmdline" 89 10 1784 14))
                  other))
                (r:letrec
                 ((g16420
                   (r:let*
                    ((%obj
                      (r:let*
                       ((%obj
                         (p:get-field
                          (r:list "cmdline" 89 28 1802 10)
                          self
                          "first"))
                        (%field
                         (p:get-raw-field
                          (r:list "cmdline" 89 28 1802 30)
                          %obj
                          "equals"))
                        (arg16421
                         (p:get-field
                          (r:list "cmdline" 89 46 1820 11)
                          other
                          "first")))
                       (r:cond
                        ((p:p-method? %field)
                         ((p:p-method-f %field) %obj arg16421))
                        (else
                         ((p:check-fun %field (r:list "cmdline" 89 28 1802 30))
                          arg16421)))))
                     (%field
                      (p:get-raw-field
                       (r:list "cmdline" 89 28 1802 64)
                       %obj
                       "and"))
                     (arg16423
                      (r:let*
                       ((%obj
                         (p:get-field
                          (r:list "cmdline" 89 63 1837 9)
                          self
                          "rest"))
                        (%field
                         (p:get-raw-field
                          (r:list "cmdline" 89 63 1837 28)
                          %obj
                          "equals"))
                        (arg16422
                         (p:get-field
                          (r:list "cmdline" 89 80 1854 10)
                          other
                          "rest")))
                       (r:cond
                        ((p:p-method? %field)
                         ((p:p-method-f %field) %obj arg16422))
                        (else
                         ((p:check-fun %field (r:list "cmdline" 89 63 1837 28))
                          arg16422))))))
                    (r:cond
                     ((p:p-method? %field)
                      ((p:p-method-f %field) %obj arg16423))
                     (else
                      ((p:check-fun %field (r:list "cmdline" 89 28 1802 64))
                       arg16423))))))
                 g16420))
               ((p:pyret-true? else)
                (r:letrec ((g16424 (p:mk-bool #f))) g16424))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16425
                   ((p:check-fun raise (r:list "cmdline" 88 6 1768 132))
                    (p:mk-str "cond: no cases matched"))))
                 g16425))
               ((p:pyret-true? (p:mk-bool #t))
                (r:letrec
                 ((g16426
                   ((p:check-fun raise (r:list "cmdline" 88 6 1768 132))
                    (p:mk-str "cond: no cases matched"))))
                 g16426)))))
            g16419))))
        (r:cons
         (p:p-str-s (p:mk-str "tostring"))
         (p:mk-method
          (r:λ
           (self)
           (r:letrec
            ((g16427
              (r:let*
               ((%obj
                 (r:let*
                  ((%obj (p:mk-str "["))
                   (%field
                    (p:get-raw-field
                     (r:list "cmdline" 95 4 1935 121)
                     %obj
                     "append"))
                   (arg16435
                    (r:let*
                     ((%obj
                       (p:get-field
                        (r:list "cmdline" 96 6 1953 9)
                        self
                        "rest"))
                      (%field
                       (p:get-raw-field
                        (r:list "cmdline" 96 6 1953 99)
                        %obj
                        "foldl"))
                      (arg16433
                       ((p:check-fun
                         (p:mk-fun
                          (r:λ
                           (contract16307)
                           (r:letrec
                            ((g16428
                              (p:extend
                               (r:list "cmdline" 97 8 1978 47)
                               (p:mk-fun
                                (r:λ
                                 (arg16308 arg16309)
                                 (r:letrec
                                  ((g16429
                                    ((p:check-fun
                                      contract16307
                                      (r:list "cmdline" 97 8 1978 47))
                                     arg16308
                                     arg16309)))
                                  g16429))
                                "internal contract for (Any, Any -> Any)")
                               (r:make-hash
                                (r:list
                                 (r:cons
                                  (p:p-str-s (p:mk-str "doc"))
                                  (p:get-field
                                   (r:list "cmdline" 97 8 1978 47)
                                   contract16307
                                   "doc")))))))
                            g16428))
                          "internal contract for (Any, Any -> Any)")
                         (r:list "cmdline" 97 8 1978 47))
                        (p:mk-fun
                         (r:λ
                          (elt s)
                          (r:letrec
                           ((g16430
                             (r:let*
                              ((%obj
                                (r:let*
                                 ((%obj s)
                                  (%field
                                   (p:get-raw-field
                                    (r:list "cmdline" 97 18 1988 14)
                                    %obj
                                    "append"))
                                  (arg16431 (p:mk-str ", ")))
                                 (r:cond
                                  ((p:p-method? %field)
                                   ((p:p-method-f %field) %obj arg16431))
                                  (else
                                   ((p:check-fun
                                     %field
                                     (r:list "cmdline" 97 18 1988 14))
                                    arg16431)))))
                               (%field
                                (p:get-raw-field
                                 (r:list "cmdline" 97 18 1988 36)
                                 %obj
                                 "append"))
                               (arg16432
                                ((p:check-fun
                                  tostring
                                  (r:list "cmdline" 97 40 2010 13))
                                 elt)))
                              (r:cond
                               ((p:p-method? %field)
                                ((p:p-method-f %field) %obj arg16432))
                               (else
                                ((p:check-fun
                                  %field
                                  (r:list "cmdline" 97 18 1988 36))
                                 arg16432))))))
                           g16430))
                         "")))
                      (arg16434
                       ((p:check-fun tostring (r:list "cmdline" 98 4 2031 20))
                        (p:get-field
                         (r:list "cmdline" 98 13 2040 10)
                         self
                         "first"))))
                     (r:cond
                      ((p:p-method? %field)
                       ((p:p-method-f %field) %obj arg16433 arg16434))
                      (else
                       ((p:check-fun %field (r:list "cmdline" 96 6 1953 99))
                        arg16433
                        arg16434))))))
                  (r:cond
                   ((p:p-method? %field) ((p:p-method-f %field) %obj arg16435))
                   (else
                    ((p:check-fun %field (r:list "cmdline" 95 4 1935 121))
                     arg16435)))))
                (%field
                 (p:get-raw-field
                  (r:list "cmdline" 95 4 1935 133)
                  %obj
                  "append"))
                (arg16436 (p:mk-str "]")))
               (r:cond
                ((p:p-method? %field) ((p:p-method-f %field) %obj arg16436))
                (else
                 ((p:check-fun %field (r:list "cmdline" 95 4 1935 133))
                  arg16436))))))
            g16427))))))))
    (r:define
     link16291
     ((p:check-fun brander (r:list "cmdline" 52 2 844 1224))))
    (r:define
     is-link
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16313)
         (r:letrec
          ((g16437
            (p:extend
             (r:list "cmdline" 52 2 844 1224)
             (p:mk-fun
              (r:λ
               (arg16314)
               (r:letrec
                ((g16438
                  ((p:check-fun contract16313 (r:list "cmdline" 52 2 844 1224))
                   ((p:check-fun
                     (p:mk-fun
                      (r:λ
                       (specimen16315)
                       (r:letrec
                        ((g16439
                          ((p:check-fun
                            check-brand
                            (r:list "cmdline" 52 2 844 1224))
                           Any?
                           specimen16315
                           (p:mk-str "Any"))))
                        g16439))
                      "internal contract for Any")
                     (r:list "cmdline" 52 2 844 1224))
                    arg16314))))
                g16438))
              "internal contract for (Any -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 52 2 844 1224)
                 contract16313
                 "doc")))))))
          g16437))
        "internal contract for (Any -> Any)")
       (r:list "cmdline" 52 2 844 1224))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16310)
          (r:letrec
           ((g16440
             (p:extend
              (r:list "cmdline" 52 2 844 1224)
              (p:mk-fun
               (r:λ
                (arg16311)
                (r:letrec
                 ((g16441
                   ((p:check-fun
                     contract16310
                     (r:list "cmdline" 52 2 844 1224))
                    ((p:check-fun
                      (p:mk-fun
                       (r:λ
                        (specimen16312)
                        (r:letrec
                         ((g16442
                           ((p:check-fun
                             check-brand
                             (r:list "cmdline" 52 2 844 1224))
                            Any?
                            specimen16312
                            (p:mk-str "Any"))))
                         g16442))
                       "internal contract for Any")
                      (r:list "cmdline" 52 2 844 1224))
                     arg16311))))
                 g16441))
               "internal contract for (Any -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 52 2 844 1224)
                  contract16310
                  "doc")))))))
           g16440))
         "internal contract for (Any -> Any)")
        (r:list "cmdline" 52 2 844 1224))
       (p:mk-fun
        (r:λ
         (specimen)
         (r:letrec
          ((g16443
            (r:let*
             ((%obj link16291)
              (%field
               (p:get-raw-field (r:list "cmdline" 52 2 844 1224) %obj "check"))
              (arg16444 specimen))
             (r:cond
              ((p:p-method? %field) ((p:p-method-f %field) %obj arg16444))
              (else
               ((p:check-fun %field (r:list "cmdline" 52 2 844 1224))
                arg16444))))))
          g16443))
        "is-link: This function checks that its argument is an\n             instance of the link type."))))
    (r:define link? is-link)
    (r:define
     link
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16319)
         (r:letrec
          ((g16445
            (p:extend
             (r:list "cmdline" 52 2 844 1224)
             (p:mk-fun
              (r:λ
               (arg16320 arg16321)
               (r:letrec
                ((g16446
                  ((p:check-fun contract16319 (r:list "cmdline" 52 2 844 1224))
                   arg16320
                   arg16321)))
                g16446))
              "internal contract for (Any, Any -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 52 2 844 1224)
                 contract16319
                 "doc")))))))
          g16445))
        "internal contract for (Any, Any -> Any)")
       (r:list "cmdline" 52 2 844 1224))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16316)
          (r:letrec
           ((g16447
             (p:extend
              (r:list "cmdline" 52 2 844 1224)
              (p:mk-fun
               (r:λ
                (arg16317 arg16318)
                (r:letrec
                 ((g16448
                   ((p:check-fun
                     contract16316
                     (r:list "cmdline" 52 2 844 1224))
                    arg16317
                    arg16318)))
                 g16448))
               "internal contract for (Any, Any -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 52 2 844 1224)
                  contract16316
                  "doc")))))))
           g16447))
         "internal contract for (Any, Any -> Any)")
        (r:list "cmdline" 52 2 844 1224))
       (p:mk-fun
        (r:λ
         (first rest)
         (r:letrec
          ((g16449
            (r:let*
             ((%obj List16288)
              (%field
               (p:get-raw-field (r:list "cmdline" 52 2 844 1224) %obj "brand"))
              (arg16451
               (r:let*
                ((%obj link16291)
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 52 2 844 1224)
                   %obj
                   "brand"))
                 (arg16450
                  (p:extend
                   (r:list "cmdline" 52 2 844 1224)
                   link_base16292
                   (r:make-hash
                    (r:list
                     (r:cons (p:p-str-s (p:mk-str "first")) first)
                     (r:cons (p:p-str-s (p:mk-str "rest")) rest))))))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16450))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 52 2 844 1224))
                   arg16450))))))
             (r:cond
              ((p:p-method? %field) ((p:p-method-f %field) %obj arg16451))
              (else
               ((p:check-fun %field (r:list "cmdline" 52 2 844 1224))
                arg16451))))))
          g16449))
        "link: Creates an instance of link"))))
    (r:define
     range
     ((p:check-fun
       (p:mk-fun
        (r:λ
         (contract16325)
         (r:letrec
          ((g16452
            (p:extend
             (r:list "cmdline" 105 0 2118 227)
             (p:mk-fun
              (r:λ
               (arg16326 arg16327)
               (r:letrec
                ((g16453
                  ((p:check-fun
                    contract16325
                    (r:list "cmdline" 105 0 2118 227))
                   arg16326
                   arg16327)))
                g16453))
              "internal contract for (Any, Any -> Any)")
             (r:make-hash
              (r:list
               (r:cons
                (p:p-str-s (p:mk-str "doc"))
                (p:get-field
                 (r:list "cmdline" 105 0 2118 227)
                 contract16325
                 "doc")))))))
          g16452))
        "internal contract for (Any, Any -> Any)")
       (r:list "cmdline" 105 0 2118 227))
      ((p:check-fun
        (p:mk-fun
         (r:λ
          (contract16322)
          (r:letrec
           ((g16454
             (p:extend
              (r:list "cmdline" 105 0 2118 227)
              (p:mk-fun
               (r:λ
                (arg16323 arg16324)
                (r:letrec
                 ((g16455
                   ((p:check-fun
                     contract16322
                     (r:list "cmdline" 105 0 2118 227))
                    arg16323
                    arg16324)))
                 g16455))
               "internal contract for (Any, Any -> Any)")
              (r:make-hash
               (r:list
                (r:cons
                 (p:p-str-s (p:mk-str "doc"))
                 (p:get-field
                  (r:list "cmdline" 105 0 2118 227)
                  contract16322
                  "doc")))))))
           g16454))
         "internal contract for (Any, Any -> Any)")
        (r:list "cmdline" 105 0 2118 227))
       (p:mk-fun
        (r:λ
         (start stop)
         (r:letrec
          ((g16456
            (r:cond
             ((p:pyret-true?
               (r:let*
                ((%obj start)
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 107 6 2156 23)
                   %obj
                   "greaterthan"))
                 (arg16457 stop))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16457))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 107 6 2156 23))
                   arg16457)))))
              (r:letrec
               ((g16458
                 ((p:check-fun raise (r:list "cmdline" 107 33 2183 39))
                  (p:mk-str "range: start greater than stop"))))
               g16458))
             ((p:pyret-true?
               (r:let*
                ((%obj start)
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 108 6 2229 18)
                   %obj
                   "equals"))
                 (arg16459 stop))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16459))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 108 6 2229 18))
                   arg16459)))))
              (r:letrec
               ((g16460
                 ((p:check-fun empty (r:list "cmdline" 108 33 2256 7)))))
               g16460))
             ((p:pyret-true?
               (r:let*
                ((%obj start)
                 (%field
                  (p:get-raw-field
                   (r:list "cmdline" 109 6 2270 20)
                   %obj
                   "lessthan"))
                 (arg16461 stop))
                (r:cond
                 ((p:p-method? %field) ((p:p-method-f %field) %obj arg16461))
                 (else
                  ((p:check-fun %field (r:list "cmdline" 109 6 2270 20))
                   arg16461)))))
              (r:letrec
               ((g16462
                 ((p:check-fun link (r:list "cmdline" 109 33 2297 38))
                  start
                  ((p:check-fun range (r:list "cmdline" 109 45 2309 25))
                   (r:let*
                    ((%obj start)
                     (%field
                      (p:get-raw-field
                       (r:list "cmdline" 109 51 2315 12)
                       %obj
                       "add"))
                     (arg16463 (p:mk-num 1)))
                    (r:cond
                     ((p:p-method? %field)
                      ((p:p-method-f %field) %obj arg16463))
                     (else
                      ((p:check-fun %field (r:list "cmdline" 109 51 2315 12))
                       arg16463))))
                   stop))))
               g16462))
             ((p:pyret-true? (p:mk-bool #t))
              (r:letrec
               ((g16464
                 ((p:check-fun raise (r:list "cmdline" 106 2 2144 197))
                  (p:mk-str "cond: no cases matched"))))
               g16464)))))
          g16456))
        "")))))
   (r:begin
    (r:define
     module-provide16328
     (p:mk-object
      (r:make-immutable-hash
       (r:list
        (r:cons (p:p-str-s (p:mk-str "is-List")) is-List)
        (r:cons (p:p-str-s (p:mk-str "is-empty")) is-empty)
        (r:cons (p:p-str-s (p:mk-str "is-link")) is-link)
        (r:cons (p:p-str-s (p:mk-str "empty")) empty)
        (r:cons (p:p-str-s (p:mk-str "link")) link)
        (r:cons (p:p-str-s (p:mk-str "range")) range)))))
    (r:provide (r:rename-out (module-provide16328 %PYRET-PROVIDE))))))
