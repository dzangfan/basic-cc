#lang racket

(require threading)
(require "common.rkt")
(require "grammar.rkt")
(require "../tokenization.rkt")

(struct exn:fail:cc:parse:LL.1:conflicted exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:LL.1:conflicted)

(define (empty-LL-table) (make-hash))

(define (add-action! LL-table variable terminal variable+ single-product)
  (define key (list variable terminal))
  (define value (list variable+ single-product))
  (define existing-action (hash-ref LL-table key #f))
  (when existing-action
    (raise (make-exn:fail:cc:parse:LL.1:conflicted
            (format "LL(1) conflicted: (~A, ~A) [~A] <=> [~A]" variable terminal
                    existing-action value)
            (current-continuation-marks))))
  (hash-set! LL-table key value))

(define (build-LL.1-table grammar)
  (define LL-table (empty-LL-table))
  (for ([(head tail) (standard-grammar-product-table grammar)])
    (for ([product (in-list tail)])
      (define firsts (FIRST grammar product))
      (for ([token (in-set firsts)] #:unless (eq? 'epsilon token))
        (add-action! LL-table head token head product))
      (when (set-member? firsts 'epsilon)
        (define follows (FOLLOW grammar head))
        (for ([token (in-set follows)])
          (add-action! LL-table head token head product)))))
  LL-table)

(define (find-action LL-table variable terminal)
  (hash-ref LL-table (list variable terminal) #f))

(provide build-LL.1-table find-action (struct-out exn:fail:cc:parse:LL.1:conflicted))

(struct token-reader ([cursor #:mutable] buffer) #:transparent)

(define (make-reader lexicon in #:file [file "(string)"])
  (token-reader 0 (tokenize lexicon in #:file file)))

(define (read-token reader #:peek? [peek? #f])
  (match-define (struct token-reader (cursor buffer)) reader)
  (define size (vector-length buffer))
  (if (>= cursor size)
      (vector-ref buffer (sub1 size))
      (begin0 (vector-ref buffer cursor)
        (unless peek?
          (set-token-reader-cursor! reader (add1 cursor))))))

(provide (struct-out token-reader) make-reader read-token)

(struct LL.1-language* (lexicon grammar [LL-table #:mutable]) #:transparent
  #:constructor-name make-LL.1-language)

(define (LL.1-language lexicon grammar)
  (make-LL.1-language lexicon grammar #f))

(define (cache-LL-table LL.1-language)
  (~>> LL.1-language
      LL.1-language*-grammar
      build-LL.1-table
      (set-LL.1-language*-LL-table! LL.1-language)))

(define (LL-table-cached? LL.1-language)
  (LL.1-language*-LL-table LL.1-language))

(provide (struct-out LL.1-language*) cache-LL-table LL-table-cached? LL.1-language)

(module+ test

  (require rackunit)

  (define grammar-4.28
    (build-standard-grammar
     '([e (t e*)]
       [e* (ADD t e*) epsilon]
       [t (f t*)]
       [t* (STAR f t*) epsilon]
       [f (LR e RR) ID])))

  (test-case "Can build LL(1) table for Grammar 4.28"
    (check-not-exn (lambda () (build-LL.1-table grammar-4.28))))

  (define grammar-4.28-LL-table
    (build-LL.1-table grammar-4.28))

  (test-case "Test content of Grammar 4.28's LL(1) table"
    (check-equal? (find-action grammar-4.28-LL-table 'e 'ID) '(e (t e*)))
    (check-equal? (find-action grammar-4.28-LL-table 'e 'LR) '(e (t e*)))
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'ADD) '(e* (ADD t e*)))
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'RR) '(e* (epsilon)))
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'EOF) '(e* (epsilon)))
    (check-equal? (find-action grammar-4.28-LL-table 't 'ID) '(t (f t*)))
    (check-equal? (find-action grammar-4.28-LL-table 't 'LR) '(t (f t*)))
    (check-equal? (find-action grammar-4.28-LL-table 't* 'ADD) '(t* (epsilon)))
    (check-equal? (find-action grammar-4.28-LL-table 't* 'RR) '(t* (epsilon)))
    (check-equal? (find-action grammar-4.28-LL-table 't* 'EOF) '(t* (epsilon)))
    (check-equal? (find-action grammar-4.28-LL-table 't* 'STAR) '(t* (STAR f t*)))
    (check-equal? (find-action grammar-4.28-LL-table 'f 'ID) '(f (ID)))
    (check-equal? (find-action grammar-4.28-LL-table 'f 'LR) '(f (LR e RR)))
    (check-equal? (find-action grammar-4.28-LL-table 'e 'ADD) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e 'STAR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e 'RR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e 'EOF) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'ID) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'STAR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'e* 'LR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't 'ADD) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't 'STAR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't 'RR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't 'EOF) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't* 'ID) #f)
    (check-equal? (find-action grammar-4.28-LL-table 't* 'LR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'f 'ADD) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'f 'STAR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'f 'RR) #f)
    (check-equal? (find-action grammar-4.28-LL-table 'f 'EOF) #f)))
