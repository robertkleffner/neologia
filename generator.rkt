#lang racket

(require br/syntax br/define)

(module+ test
    (require rackunit))

(define-macro (n-module-begin PARSE-TREE)
  #'(#%module-begin
     (interpret (quote PARSE-TREE))))
(provide (rename-out [n-module-begin #%module-begin]))

(provide interpret)

(struct section (name related) #:transparent)
(struct entry (word part defs sections) #:transparent)

(define (interpret stx)
    (match stx
        [(list 'n-top (list 'n-config config ...) sections items ...)
         (define parts (append (get-with-default 'n-parts empty config) (list "???")))
         (define order (get-with-default 'n-order empty config))
         (define out-pathname (car (get-with-default 'n-path (list (path->string (current-directory))) config)))
         (define out-filename (string-append (car (get-with-default 'n-file (list "generated") config)) ".md"))
         (define section-defs (get-section-defs sections))
         (define entries (get-entries items parts section-defs))
         (displayln "Generating...")
         (flush-output)
         (generate (string-append out-pathname out-filename) parts order section-defs entries)]))

(define (get-with-default sym default items)
        (for/fold ([res default])
                  ([stx (in-list items)]
                   #:when (symbol=? (car stx) sym))
            (cdr stx)))

(define (get-entries stx parts section-defs)
    (define section-keys (hash-keys section-defs))

    (define (part-id part-name)
        (index-of parts part-name))
    
    (define (make-section sstx)
        (match sstx
            [(list 'n-section kind related ...)
             (when (not (member kind section-keys))
                (error 'get-entries (format "Undefined section '~a' referenced in a definition" kind)))
             (section kind related)]))

    (define (make-entry estx)
        (match estx
            [(list 'n-entry word part defs ... (list 'n-sections sections ...))
             (entry word part defs (map make-section sections))]))

    (map make-entry stx))

(define (get-section-defs stx)
    (define (get-def stx)
        (cons (second stx) (third stx)))
    
    (match stx
        [(list 'n-section-defs def-stxs ...)
         (define defs (map get-def def-stxs))
         (warn-duplicate-sections (map car defs))
         (make-immutable-hash defs)]))

(define (generate outfile parts order sections entries)
    (define words (map entry-word entries))
    (warn-duplicate-entries words)
    (warn-undefined parts words entries)
    (define sorted (sort entries (curry lesseq-by-order? order) #:key entry-word))
    (render outfile parts sorted sections))

(define (render outfile parts words sections)
    (define out (open-output-file outfile #:exists 'replace #:mode 'text))
    (render-title out "Dictionary")
    (displayln "" out)
    (map (curry render-entry out parts sections) words)
    (close-output-port out)
    
    (displayln (format "Generated dictionary successfully, check '~a' for the results." outfile)))

(define (render-entry out parts sections word)
    (define (get-group-display-name)
        (define part (entry-part word))
        (if (member part parts)
            part
            "???"))
    (define ortho (entry-word word))
    (displayln (format "<a href=\"~a\">~a</a> : **~a**" ortho ortho (get-group-display-name)) out)
    (for/fold ([idx 1])
              ([def (in-list (entry-defs word))])
        (displayln (format "~a. *~a*" (number->string idx) def) out)
        (add1 idx))
    (displayln "" out)
    (map (curry render-section out sections) (entry-sections word))
    (displayln "" out))

(define (render-section out sections sec)
    (displayln (format "~a: ~a"
                       (hash-ref sections (section-name sec))
                       (string-join (map (lambda (a) (format "[~a](#~a)" a a)) (section-related sec)) ", "))
               out))

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
            (displayln (format "Warning: could not determine order of sound '~a'" arg)))
        (if (number? idx) idx -1))

    (define p1 (get-prefix i1))
    (define p2 (get-prefix i2))
    (define l1 (string-length p1))
    (if (string=? p1 p2)
        (lesseq-by-order? order (substring i1 l1) (substring i2 l1))
        (< (default-to-neg-one p1)
           (default-to-neg-one p2))))

(define (warn-duplicates msg items)
    (for/fold ([acc empty])
              ([e (in-list items)])
        (when (member e acc)
              (displayln (format msg e)))
        (cons e acc)))
(define warn-duplicate-sections (curry warn-duplicates "Warning: duplicate definition for section '~a'"))
(define warn-duplicate-entries (curry warn-duplicates "Warning: more than one entry for '~a'"))

(define (warn-undefined parts words entries)
    (for ([e (in-list entries)])
        (when (not (member (entry-part e) parts))
            (displayln (format "Warning: word '~a' has undefined part of speech '~a'" (entry-word e) (entry-part e))))
        (for ([s (in-list (entry-sections e))])
            (for ([i (in-list (section-related s))])
                (when (not (member i words))
                    (displayln (format "Warning: word '~a' referenced in section '~a' of entry '~a' does not have it's own entry"
                                       i (section-name s) (entry-word e))))))))