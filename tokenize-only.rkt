#lang racket
(require brag/support "tokenizer.rkt")
(require br/syntax br/define)

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (datum->syntax #f
    `(module neologia-tokens-mod neologia/tokenize-only ,tokens)))
(module+ reader (provide read-syntax))

(define-macro (tokenize-only-mb TOKEN ...)
  #'(#%module-begin
     (quote TOKEN ...)))
(provide (rename-out [tokenize-only-mb #%module-begin]))