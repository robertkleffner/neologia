#lang racket

(require br/syntax br/define)

(module+ test
    (require rackunit))

(define-macro (n-module-begin PARSE-TREE)
  #'(#%module-begin
     (interpret (quote PARSE-TREE))))
(provide (rename-out [n-module-begin #%module-begin]))

(provide interpret)

(struct entry (word part defs) #:transparent)

(define (interpret stx)
    (match stx
        [(list 'n-file (list 'n-config config ...) items ...)
         (define parts (append (get-with-default 'n-parts empty config) (list "???")))
         (define order (get-with-default 'n-order empty config))
         (displayln "Generating...")
         (flush-output)
         (generate parts order (get-entries items parts))]))

(define (get-with-default sym default items)
        (for/fold ([res default])
                  ([stx (in-list items)]
                   #:when (symbol=? (car stx) sym))
            (cdr stx)))

(define (get-entries stx parts)
    (define (part-id part-name)
        (index-of parts part-name))

    (define (make-entry estx)
        (match estx
            [(list 'n-entry word part defs ...)
             (entry word part defs)]))

    (map make-entry stx))

(define (generate parts order entries)
    (warn-duplicates entries)
    (warn-undefined parts entries)
    (define sorted (sort entries (curry lesseq-by-order? order) #:key entry-word))
    (render parts sorted))

(define (render parts words)
    (define out (open-output-file "./generated.md" #:exists 'replace #:mode 'text))
    (render-title out "Dictionary")
    (displayln "" out)
    (map (curry render-entry out parts) words)
    (close-output-port out)
    
    (displayln "Generated dictionary successfully, check 'generated.md' for the results."))

(define (render-entry out parts word)
    (define (get-group-display-name)
        (define part (entry-part word))
        (if (member part parts)
            part
            "???"))
    (displayln (string-append (entry-word word) " : **" (get-group-display-name) "**") out)
    (for/fold ([idx 1])
              ([def (in-list (entry-defs word))])
        (displayln (string-append (number->string idx) ". *" def "*") out)
        (add1 idx))
    (displayln "" out))

(define (render-title out arg)
    (displayln (string-append "# " arg) out))

(define (lesseq-by-order? order i1 i2)
    (define plus-one-order
        (for/fold ([acc empty])
                  ([s (in-list order)]
                   #:when (> (string-length s) 1))
            (cons s acc)))

    (define (get-prefix s)
        (for/fold ([res (substring s 0 1)])
                  ([pref (in-list plus-one-order)]
                   #:when (and (string-prefix? s pref) (> (string-length pref) (string-length res))))
            pref))
    
    (define (default-to-neg-one arg)
        (define idx (index-of order arg))
        (when (and (not idx) (not (empty? order)))
            (displayln (string-append "Warning: could not determine order of sound '" arg "'")))
        (if (number? idx) idx -1))

    (define p1 (get-prefix i1))
    (define p2 (get-prefix i2))
    (define l1 (string-length p1))
    (if (string=? p1 p2)
        (lesseq-by-order? order (substring i1 l1) (substring i2 l1))
        (< (default-to-neg-one p1)
           (default-to-neg-one p2))))

(define (warn-duplicates entries)
    (for/fold ([acc empty])
              ([e (in-list entries)])
        (when (member (entry-word e) acc)
              (displayln (string-append "Warning: more than one entry for '" (entry-word e) "'")))
        (cons (entry-word e) acc)))

(define (warn-undefined parts entries)
    (for ([e (in-list entries)]
          #:when (not (member (entry-part e) parts)))
        (displayln (string-append "Warning: word '" (entry-word e) "' has undefined part of speech '" (entry-part e) "'"))))