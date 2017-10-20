#lang racket

(require "lexer.rkt" brag/support)

(provide neologia-colorer)



(define (neologia-colorer port)
    (define (handle-lexer-error excn)
        (define excn-srclocs (exn:fail:read-srclocs excn))
        (srcloc-token (token 'ERROR) (car excn-srclocs)))
    (define srcloc-tok
        (with-handlers ([exn:fail:read? handle-lexer-error])
            (neologia-lexer port)))
    (match srcloc-tok
        [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
        [else
            (match-define
                (srcloc-token
                    (token-struct type val _ _ _ _ _)
                    (srcloc _ _ _ posn span)) srcloc-tok)
            (define start posn)
            (define end (+ start span))
            (define cat
                (match type
                    ['COMMENT 'comment]
                    ['IDENT 'no-color]
                    ['NUMBER 'constant]
                    ['STRING 'string]
                    [else
                        (if (symbol=? type (string->symbol ","))
                            'no-color
                            'hash-colon-keyword)]))
            (values val cat #f start end)]))