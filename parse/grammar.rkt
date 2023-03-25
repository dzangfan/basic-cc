#lang debug racket

(require threading)
(require racket/set)
(require "common.rkt")

(struct standard-grammar (alphabet variables product-table start-variable)
  #:transparent)

(define (cannot-build-grammar lst)
  (raise (make-exn:fail:cc:parse:build-grammar
          (format "Cannot build grammar from list ~A" lst)
          (current-continuation-marks))))

(define (check-product-body body alphabet variables)
  (define-syntax-rule (flatten symbol)
    (lambda (body)
      (match (check-product-body body alphabet variables)
        [(list 'symbol rest (... ...)) rest]
        [other (list other)])))
  (match body
    [_ #:when (symbol? body)
       (if (~>> body symbol->string string->list (ormap char-lower-case?))
           (set-add! variables body)
           (set-add! alphabet body))
       body]
    [(list '-> seq ...) (append* '(->) (map (flatten ->) seq))]
    [(list 'or alt ...) (append* '(or) (map (flatten or) alt))]
    [_ (cannot-build-grammar body)]))

(define (build-standard-grammar/product product alphabet variables product-table)
  (match product
    [(list head (list 'or bodies ...))
     #:when (symbol? head)
     (set-add! variables head)
     (for/and ([body (in-list bodies)])
       (build-standard-grammar/product (list head body) alphabet variables product-table))]
    [(list head (and (list '-> _ _ ...) body))
     #:when (symbol? head)
     (set-add! variables head)
     (define body* (check-product-body body alphabet variables))
     (define existing-bodies (hash-ref product-table head null))
     (hash-set! product-table head (cons body* existing-bodies))
     head]
    [(list head body)
     #:when (symbol? body)
     (set-add! variables head)
     (define body* (list '-> (check-product-body body alphabet variables)))
     (define existing-bodies (hash-ref product-table head null))
     (hash-set! product-table head (cons body* existing-bodies))]
    [_ (cannot-build-grammar product)]))

(define (build-standard-grammar lst)
  (when (null? lst) (cannot-build-grammar lst))
  (define alphabet (mutable-seteq))
  (define variables (mutable-seteq))
  (define product-table (make-hasheq))
  (match-define (list first-product rest-products ...) lst)
  (define start-variable (build-standard-grammar/product first-product alphabet variables product-table))
  (for ([product (in-list rest-products)]) (build-standard-grammar/product product alphabet variables product-table))
  (define product-table*
    (for/hash ([(head bodies) product-table])
      (if (= 1 (length bodies))
          (values head (first bodies))
          (values head (cons 'or (reverse bodies))))))
  (standard-grammar alphabet variables product-table* start-variable))

(provide (struct-out standard-grammar)
         build-standard-grammar)

;; Waiting to test
