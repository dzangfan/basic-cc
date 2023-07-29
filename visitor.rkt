#lang racket

(require (for-syntax threading))
(require (for-syntax racket/string))
(require (for-syntax "parse/grammar.rkt"))
(require "tokenization.rkt")

(begin-for-syntax
  (define (expand-condition stx)
    (syntax-case stx (_)
      [_ #'_]
      [symbol
       (and (identifier? #'symbol)
            (~> #'symbol syntax->datum symbol->string (string-prefix? "@")))
       (with-syntax ([symbol* (~> #'symbol syntax->datum symbol->string
                                  (substring 1) string->symbol (datum->syntax #'symbol _ #'symbol))])
         (with-syntax ([inner-condition (expand-condition #'symbol*)])
           #'(and symbol* inner-condition)))]
      [symbol
       (and (identifier? #'symbol)
            (~> #'symbol syntax->datum symbol->string (string-prefix? "$")))
       (~> #'symbol syntax->datum symbol->string
           (substring 1) string->symbol (datum->syntax #'symbol _ #'symbol))]
      [symbol
       (and (identifier? #'symbol)
            (~> #'symbol syntax->datum symbol-all-upper-case?))
       #'(struct token ('symbol _ _))]
      [symbol
       (identifier? #'symbol)
       #'(list 'symbol _ (... ...))]
      [(_ component ...)
       (with-syntax ([(match-condition ...) (~>> #'(component ...) syntax->list (map expand-condition))])
         #'(list _ match-condition ...))]
      [((name . _) component ...)
       (with-syntax ([(match-condition ...) (~>> #'(component ...) syntax->list (map expand-condition))])
         #'(list name match-condition ...))]
      [(symbol component ...)
       (and (identifier? #'symbol)
            (~> #'symbol syntax->datum symbol->string (string-prefix? "$")))
       (with-syntax ([name (~> #'symbol syntax->datum symbol->string
                               (substring 1) string->symbol (datum->syntax #'symbol _ #'symbol))])
         (with-syntax ([(match-condition ...) (~>> #'(component ...) syntax->list (map expand-condition))])
           #'(list name match-condition ...)))]
      [(symbol component ...)
       (identifier? #'symbol)
       (with-syntax ([(match-condition ...) (~>> #'(component ...) syntax->list (map expand-condition))])
         #'(list 'symbol match-condition ...))]
      [(name . component)
       (identifier? #'name)
       (with-syntax ([match-condition (expand-condition #'component)])
         #'(and name match-condition))])))

(define-syntax (define-visitor-helper stx)
  (syntax-case stx ()
    [(_ (name args ...) () (match-clauses ...))
     (with-syntax ([parse-tree-name (~> "*parse-tree*" string->symbol (datum->syntax #'name _ #'name))])
       #'(define (name tree args ...)
           (define parse-tree-name tree)
           (match tree
             match-clauses ...)))]
    [(_ name () (match-clauses ...))
     (with-syntax ([parse-tree-name (~> "*parse-tree*" string->symbol (datum->syntax #'name _ #'name))])
       #'(define (name tree)
           (define parse-tree-name tree)
           (match tree
             match-clauses ...)))]
    [(_ name ([condition body ...] rest-clauses ...) (match-clauses ...))
     (with-syntax ([match-condition (expand-condition #'condition)])
       #'(define-visitor-helper name
           (rest-clauses ...)
           (match-clauses ... [match-condition body ...])))]))

(define-syntax-rule (define-visitor name clauses ...)
  (define-visitor-helper name (clauses ...) ()))

(provide define-visitor)

(module+ test

  (require "language.rkt")
  (require threading)
  (require rackunit)

  (define-language language-4.1
    (ID "\\w+")
    (PLUS "\\+")
    (STAR "\\*")
    (LEFT "\\(")
    (RIGHT "\\)")
    "\\s*"
    (e (e PLUS t) t)
    (t (t STAR f) f)
    (f (LEFT e RIGHT) ID))

  (define-visitor language-4.1-eval
    [(ID . ID) (~> ID token-text string->number)]
    [(e (lhs . e) PLUS (rhs . t))
     (+ (language-4.1-eval lhs) (language-4.1-eval rhs))]
    [(e (t . t)) (language-4.1-eval t)]
    [(t (lhs . t) STAR (rhs . f))
     (* (language-4.1-eval lhs) (language-4.1-eval rhs))]
    [(t (f . f)) (language-4.1-eval f)]
    [(f LEFT (e . e) RIGHT) (language-4.1-eval e)]
    [(f (ID . ID)) (language-4.1-eval ID)])

  (define (eval/basic text)
    (~> text language-4.1-read language-4.1-eval))

  (test-case "Basic evaluator"
    (check = 1 (eval/basic "1"))
    (check = 2 (eval/basic "1 + 1"))
    (check = 4 (eval/basic "2 * 2"))
    (check = 12 (eval/basic "(1 + 2) * 4"))
    (check = 3 (eval/basic "((((1))))+ ((2))")))

  (define-visitor language-4.1-eval*
    [@ID (~> ID token-text string->number)]
    [(_ @e PLUS @t) (+ (language-4.1-eval* e) (language-4.1-eval* t))]
    [(_ @t STAR @f) (* (language-4.1-eval* t) (language-4.1-eval* f))]
    [(f _ @e _) (language-4.1-eval* e)]
    [(_ (any . _)) (language-4.1-eval* any)])

  (define (eval/advanced text)
    (~> text language-4.1-read language-4.1-eval*))

  (test-case "Advanced evaluator"
    (check = 1 (eval/advanced "1"))
    (check = 2 (eval/advanced "1 + 1"))
    (check = 4 (eval/advanced "2 * 2"))
    (check = 12 (eval/advanced "(1 + 2) * 4"))
    (check = 3 (eval/advanced "((((1))))+ ((2))")))

  (define-visitor (language-4.1-eval/magic factor)
    [@ID (* factor (~> ID token-text string->number))]
    [(_ @e PLUS @t) (+ (language-4.1-eval/magic e factor) (language-4.1-eval/magic t factor))]
    [(_ @t STAR @f) (* (language-4.1-eval/magic t factor) (language-4.1-eval/magic f factor))]
    [(f _ @e _) (language-4.1-eval/magic e factor)]
    [(_ $any) (language-4.1-eval/magic any factor)])

  
  (define (eval/10 text)
    (~> text language-4.1-read (language-4.1-eval/magic 10)))

  (test-case "Magic evaluator"
    (check = 10 (eval/10 "1"))
    (check = 20 (eval/10 "1 + 1"))
    (check = 400 (eval/10 "2 * 2"))
    (check = 1200 (eval/10 "(1 + 2) * 4"))
    (check = 30 (eval/10 "((((1))))+ ((2))"))))
