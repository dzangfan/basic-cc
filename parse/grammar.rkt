#lang racket

(require threading)
(require racket/set)
(require "common.rkt")

(struct exn:fail:cc:parse:build-grammar exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:build-grammar)

(struct exn:fail:cc:parse:unknown-variable exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:unknown-variable)

(struct exn:fail:cc:parse:badly-recursive exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:badly-recursive)

(provide (struct-out exn:fail:cc:parse:build-grammar)
         (struct-out exn:fail:cc:parse:unknown-variable)
         (struct-out exn:fail:cc:parse:badly-recursive))

(struct standard-grammar (alphabet variables product-table start-variable)
  #:transparent)

(provide (struct-out standard-grammar))

(define (cannot-build what lst message)
  (raise (make-exn:fail:cc:parse:build-grammar
          (format "Cannot build ~A from list ~A: ~A" what lst message)
          (current-continuation-marks))))

(define symbol-all-upper-case?
  (lambda~>> symbol->string string->list (andmap char-upper-case?)))

(provide symbol-all-upper-case?)

(define (collect-product product alphabet variables product-table is-token?)
  (match product
    [(list head _ ...)
     #:when (and (symbol? head) (is-token? head))
     (cannot-build "product" product (format "Using a token-symbol [~A] as a variable" head))]
    [(list 'epsilon _ ...) (cannot-build "product" product (format "Cannot use keyword `epsilon' as a variable"))]
    [(list head tail)
     #:when (symbol? tail)
     (collect-product (list head (list tail)) alphabet variables product-table is-token?)]
    [(list head tail)
     #:when (list? tail)
     (for ([sym (in-list tail)] #:unless (eq? 'epsilon sym))
       (cond [(and (symbol? sym) (is-token? sym)) (set-add! alphabet sym)]
             [(symbol? sym) (set-add! variables sym)]
             [else (cannot-build "product" product (format "Unknown component ~A" sym))]))
     (define existing-clauses (hash-ref product-table head null))
     (set-add! variables head)
     (hash-set! product-table head (cons tail existing-clauses))
     head]
    [(list head tail#0 tail ...)
     #:when (not (null? tail))
     (collect-product (list head tail#0) alphabet variables product-table is-token?)
     (collect-product (list* head tail) alphabet variables product-table is-token?)]
    [_ (cannot-build "product" product
                     (format "Product must have form: (non-token-symbol clauses+)"))]))

(define (build-standard-grammar lst #:token? [is-token? symbol-all-upper-case?])
  (when (null? lst) (cannot-build "grammar" lst "Grammar must have at least one product"))
  (match-define (list first-product rest-products ...) lst)
  (define alphabet (mutable-seteq 'EOF 'EOL))
  (define variables (mutable-seteq))
  (define product-table (make-hasheq))
  (define start-variable (collect-product first-product alphabet variables product-table is-token?))
  (for-each (lambda (product) (collect-product product alphabet variables product-table is-token?)) rest-products)
  (define product-table*
    (for/hash ([(head tails) (in-hash product-table)])
      (values head (reverse tails))))
  (for ([variable (in-set variables)])
    (unless (hash-has-key? product-table* variable)
      (cannot-build "grammar" variable (format "Using undefined variable ~A" variable))))
  (standard-grammar alphabet variables product-table* start-variable))

(define (find-product grammar variable)
  (hash-ref (standard-grammar-product-table grammar) variable
            (lambda ()
              (raise (make-exn:fail:cc:parse:unknown-variable
                      (format "Unknown variable: ~A" variable)
                      (current-continuation-marks))))))

(define (terminal? grammar variable-or-token)
  (set-member? (standard-grammar-alphabet grammar) variable-or-token))

(define (non-terminal? grammar variable-or-token)
  (set-member? (standard-grammar-variables grammar) variable-or-token))

(provide build-standard-grammar find-product terminal? non-terminal?)

(define (augmented-grammar? grammar)
  (and (standard-grammar? grammar)
       (let ([products (find-product grammar (standard-grammar-start-variable grammar))])
         (and (= 1 (length products))
              (= 1 (length (first products)))
              (non-terminal? grammar (first (first products)))))))

(provide augmented-grammar?)

(define (FIRST/1 grammar product computing-stack)
  (let forward ([symbols product] [first-symbol-set (seteq)])
    (match symbols
      [(list) (set-add first-symbol-set 'epsilon)]
      [(list first-symbol rest-symbols ...)
       (define firsts (FIRST grammar first-symbol #:-computing computing-stack))
       (if (not (set-member? firsts 'epsilon))
           (set-union first-symbol-set firsts)
           (let ([firsts* (set-remove firsts 'epsilon)])
             (forward rest-symbols (set-union first-symbol-set firsts*))))])))

(define (construct-recursive-link computing-stack variable)
  (~> (append (list variable)
              (reverse (takef computing-stack (lambda (x) (not (eq? x variable)))))
              (list variable))
      (map symbol->string _)
      (string-join " -> ")))

(define (FIRST grammar variable-or-token #:-computing [computing-stack null])
  (match variable-or-token
    ['epsilon (seteq 'epsilon)]
    [product #:when (list? product) (FIRST/1 grammar product computing-stack)]
    [symbol #:when (terminal? grammar symbol) (seteq symbol)]
    [variable
     #:when (and (symbol? variable) (memq variable computing-stack))
     #;(raise (make-exn:fail:cc:parse:badly-recursive
               (format "Variable ~A is left-recursive, ~A" variable
                       (construct-recursive-link computing-stack variable))
               (current-continuation-marks)))
     (seteq)]
    [variable
     #:when (symbol? variable)
     (define product-list (find-product grammar variable))
     (apply set-union
            (for/list ([product (in-list product-list)])
              (FIRST/1 grammar product (cons variable computing-stack))))]))

(define (FOLLOW/1 grammar variable head product follow-set computing-stack)
  (let scan ([symbol-list product])
    (match symbol-list
      [(list) (void)]
      [(list preceding following rest-symbols ...)
       #:when (eq? variable preceding)
       (define candiates (FIRST grammar following))
       (cond [(set-member? candiates 'epsilon)
              (set-union! follow-set (set-remove candiates 'epsilon))
              (unless (eq? variable head)
                (set-union! follow-set (FOLLOW grammar head #:-computing computing-stack)))]
             [else (set-union! follow-set candiates)])
       (scan rest-symbols)]
      [(list preceding)
       #:when (and (eq? variable preceding) (not (eq? variable head)))
       (set-union! follow-set (FOLLOW grammar head #:-computing computing-stack))]
      [(list other rest-symbols ...)
       (scan rest-symbols)])))

(define (FOLLOW grammar variable #:-computing [computing-stack null])
  (unless (non-terminal? grammar variable)
    (raise (make-exn:fail:cc:parse (format "Can only compute FOLLOW set for a variable, but got ~A" variable)
                                   (current-continuation-marks))))
  #;(when (memq variable computing-stack)
      (raise (make-exn:fail:cc:parse:badly-recursive
              (format "Variable ~A is badly right-recursive, ~A" variable
                      (construct-recursive-link computing-stack variable))
              (current-continuation-marks))))
  (cond [(memq variable computing-stack) (seteq)]
        [else (define follow-set (if (eq? variable (standard-grammar-start-variable grammar))
                                     (mutable-seteq 'EOF)
                                     (mutable-seteq)))
              (for ([(head tail) (in-hash (standard-grammar-product-table grammar))])
                (for ([product (in-list tail)])
                  (FOLLOW/1 grammar variable head product follow-set (cons variable computing-stack))))
              follow-set]))

(provide FIRST FOLLOW)

(define (augment-grammar grammar)
  (match-define (struct standard-grammar (alphabet variables product-table start-variable)) grammar)
  (define start-variable*
    (~> start-variable symbol->string (string-append "#") string->symbol))
  (when (set-member? variables start-variable*)
    (raise (make-exn:fail:cc:parse:build-grammar
            (format "Name of starting variable for augmented grammar has been used (~A)" start-variable*)
            (current-continuation-marks))))
  (let ([product-table* (hash-copy product-table)]
        [variables* (set-copy variables)])
    (hash-set! product-table* start-variable* `((,start-variable)))
    (set-add! variables* start-variable*)
    (standard-grammar alphabet variables* product-table* start-variable*)))

(provide augment-grammar)

(module+ test
  (require rackunit)

  (test-case "Simple grammar"
    (define grammar
      (build-standard-grammar
       '((a (A B)
            (B)
            C)
         (b a)
         (c (a c))
         (a epsilon))))
    (check-equal? (standard-grammar-variables grammar) (list->mutable-seteq '(a b c)))
    (check-equal? (standard-grammar-alphabet grammar) (list->mutable-seteq '(A B C EOF EOL)))
    (check-eq? (standard-grammar-start-variable grammar) 'a)
    (check-equal? (standard-grammar-product-table grammar)
                  #hash((a . ((A B) (B) (C) (epsilon)))
                        (b . ((a)))
                        (c . ((a c))))))

  (test-case "Exceptional cases"
    (check-exn exn:fail:cc:parse:build-grammar?
               (lambda ()
                 (build-standard-grammar
                  '((FOO bar)))))
    (check-exn exn:fail:cc:parse:build-grammar?
               (lambda ()
                 (build-standard-grammar
                  '((epsilon bar)))))
    (check-exn exn:fail:cc:parse:build-grammar?
               (lambda ()
                 (build-standard-grammar
                  '((expr bar))))))

  #;(test-case "Test FIRST for left-recursive grammar"
      (define grammar
        (build-standard-grammar
         '([s (a A) B]
           [a (s D) epsilon])))
      (check-exn exn:fail:cc:parse:badly-recursive?
                 (lambda () (FIRST grammar 's))))

  (define grammar-4.28
    (build-standard-grammar
     '([e (t e*)]
       [e* (ADD t e*) epsilon]
       [t (f t*)]
       [t* (STAR f t*) epsilon]
       [f (LR e RR) ID])))

  (test-case "Test FIRST for grammar-4.28"
    (check-equal? (FIRST grammar-4.28 'f) (seteq 'LR 'ID))
    (check-equal? (FIRST grammar-4.28 't) (seteq 'LR 'ID))
    (check-equal? (FIRST grammar-4.28 'e) (seteq 'LR 'ID))
    (check-equal? (FIRST grammar-4.28 'e*) (seteq 'ADD 'epsilon))
    (check-equal? (FIRST grammar-4.28 't*) (seteq 'STAR 'epsilon))
    (check-equal? (FIRST grammar-4.28 '(t e*)) (FIRST grammar-4.28 'e))
    (check-equal? (FIRST grammar-4.28 '(STAR f t*)) (seteq 'STAR)))

  (test-case "Test FOLLOW for grammar-4.28"
    (check-equal? (FOLLOW grammar-4.28 'e) (mutable-seteq 'RR 'EOF))
    (check-equal? (FOLLOW grammar-4.28 'e*) (mutable-seteq 'RR 'EOF))
    (check-equal? (FOLLOW grammar-4.28 't) (mutable-seteq 'ADD 'RR 'EOF))
    (check-equal? (FOLLOW grammar-4.28 't*) (mutable-seteq 'ADD 'RR 'EOF))
    (check-equal? (FOLLOW grammar-4.28 'f) (mutable-seteq 'ADD 'STAR 'RR 'EOF))))
