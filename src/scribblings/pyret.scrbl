#lang scribble/manual

@(require racket/sandbox
          "../tests/test-utils.rkt"
          "../lang/eval.rkt"
          scribble/eval)

@(define-syntax-rule (pyret-interaction thing ...)
  (begin
    "> " (code #:lang "pyret" thing ...)
    (define result
      (eval-pyret/libs (string-append thing ...)))
    (linebreak)
    (code (pyret-to-printable result)))))

@(define-syntax-rule (pyret-block thing ...)
  (codeblock "#:lang pyret\n" thing ...))



@pyret-block{
  "hello world"
}

@pyret-interaction{
  "hello world"
}
