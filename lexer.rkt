#lang racket
(require brag/support)

(define neologia-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [whitespace (token 'WS lexeme #:skip? #t)]
        [(from/to "--" "\n") (token 'COMMENT lexeme #:skip? #t)]
        [(:or "Parts" "Order" ":" ">" "=" ",")
         (token lexeme lexeme)]
        [(:seq (:+ numeric) ".")
         (token 'NUMBER lexeme)]
        [(:seq alphabetic (:* (:or alphabetic "-")))
         (token 'IDENT lexeme)]
        [(from/to "\"" "\"")
         (token 'STRING (substring lexeme 1 (sub1 (string-length lexeme))))]))

(provide neologia-lexer)