#lang racket
(require brag/support)

(define neologia-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [whitespace (token 'WS lexeme #:skip? #t)]
        [(from/to "--" "\n") (token 'COMMENT lexeme #:skip? #t)]
        [(:or "Parts" "Order" ":" ">" "=" ",")
         (token lexeme lexeme)]
        [(:seq (:+ numeric) "." (from/to "" "."))
         (token 'DEFINITION lexeme)]))

(provide neologia-lexer)