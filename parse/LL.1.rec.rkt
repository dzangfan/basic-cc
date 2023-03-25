#lang debug racket

(require threading)
(require "common.rkt")
(require "grammar.rkt")
(require "LL.1-table.rkt")
(require "../tokenization.rkt")

(struct exn:fail:cc:parse:LL.1:failed exn:fail:cc:parse (token stack) #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:LL.1:failed)

(define (starting-variable LL.1-language)
  (~> LL.1-language LL.1-language*-grammar standard-grammar-start-variable))

(define (parse* LL.1-language reader variable)
  (cond [(eq? 'epsilon variable) #f]
        [(~>> variable (terminal? (LL.1-language*-grammar LL.1-language)))
         (define next-token (read-token reader))
         (if (eq? variable (token-type next-token))
             next-token
             (raise (make-exn:fail:cc:parse:LL.1:failed
                     (format "Expected a [~A], but found a ~A" variable (token-type next-token))
                     (current-continuation-marks)
                     next-token (list variable))))]
        [else (define table (LL.1-language*-LL-table LL.1-language))
              (define next-token (read-token reader #:peek? #t))
              (define action (find-action table variable (token-type next-token)))
              (unless action (raise (make-exn:fail:cc:parse:LL.1:failed
                                     (format "[~A] cannot start with a ~A" variable (token-type next-token))
                                     (current-continuation-marks)
                                     next-token (list action))))
              (match-define (list head product) action)
              (define children
                (for/list ([symbol (in-list product)])
                  (with-handlers ([exn:fail:cc:parse:LL.1:failed?
                                   (lambda (e) (match-let ([(struct exn:fail:cc:parse:LL.1:failed (m c t s)) e])
                                                 (raise (make-exn:fail:cc:parse:LL.1:failed m c t (cons action s)))))])
                    (parse* LL.1-language reader symbol))))
              (cons variable (filter (lambda (x) x) children))]))

(define (parse LL.1-language in #:file [file "(string)"] #:as [variable (starting-variable LL.1-language)])
  (unless (LL-table-cached? LL.1-language) (cache-LL-table LL.1-language))
  (define reader (make-reader (LL.1-language*-lexicon LL.1-language) in #:file file))
  (with-handlers ([exn:fail:cc:parse:LL.1:failed?
                   (match-lambda [(struct exn:fail:cc:parse:LL.1:failed (m c t s))
                                  (define stack (map (lambda (product) (format "  in product ~A" product)) s))
                                  (match-define (struct token (type text (struct location (file line column)))) t)
                                  (define cause
                                    (format "Caused by token [~A~A], ~A, line ~A column ~A"
                                            type (if text (string-append " " text) "") file line column))
                                  (define message (string-join (list* m cause stack) "\n"))
                                  (raise (make-exn:fail:cc:parse message c))])])
    (parse* LL.1-language reader variable)))

(module+ test

  (require rackunit)
  
  (define-language/lexicon lexicon-4.28
    (ADD "\\+")
    (STAR "\\*")
    (LR "\\(")
    (RR "\\)")
    (ID "\\w+")
    "\\s*")

  (exclude-EOL-during-tokenization? #t)

  (define grammar-4.28
    (build-standard-grammar
     '([e (t e*)]
       [e* (ADD t e*) epsilon]
       [t (f t*)]
       [t* (STAR f t*) epsilon]
       [f (LR e RR) ID])))

  (define language-4.28 (LL.1-language lexicon-4.28 grammar-4.28))

  (test-case "Test for grammar-4.28 [1]"
    (check-equal? (parse language-4.28 "x + y * z")
                  `(e (t (f ,(token 'ID "x" (location "(string)" 0 0))) (t*))
                      (e* ,(token 'ADD "+" (location "(string)" 0 2))
                          (t (f ,(token 'ID "y" (location "(string)" 0 4)))
                             (t* ,(token 'STAR "*" (location "(string)" 0 6))
                                 (f ,(token 'ID "z" (location "(string)" 0 8)))
                                 (t*)))
                          (e*)))))

  (test-case "Test for grammar-4.28 [2]"
    (check-equal? (parse language-4.28 "(x + y) * z" #:file "(sdream)")
                  `(e (t (f ,(token 'LR "(" (location "(sdream)" 0 0))
                            (e (t (f ,(token 'ID "x" (location "(sdream)" 0 1))) (t*))
                               (e* ,(token 'ADD "+" (location "(sdream)" 0 3))
                                   (t (f ,(token 'ID "y" (location "(sdream)" 0 5)))
                                      (t*))
                                   (e*)))
                            ,(token 'RR ")" (location "(sdream)" 0 6)))
                         (t* ,(token 'STAR "*" (location "(sdream)" 0 8))
                             (f ,(token 'ID "z" (location "(sdream)" 0 10)))
                             (t*)))
                      (e*)))))
