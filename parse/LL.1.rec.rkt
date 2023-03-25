#lang racket

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
                     (format "Token ~A is not a ~A" next-token variable)
                     (current-continuation-marks)
                     next-token (list variable))))]
        [else (define table (LL.1-language*-LL-table LL.1-language))
              (define next-token (read-token reader #:peek? #t))
              (define action (find-action table variable next-token))
              (unless action (raise (make-exn:fail:cc:parse:LL.1:failed
                                     (format "Cannot parse ~A as a ~A" next-token variable)
                                     (current-continuation-marks)
                                     next-token (list variable))))
              (match-define (list head product) action)
              (define children
                (for/list ([symbol (in-list product)])
                  (with-handlers ([exn:fail:cc:parse:LL.1:failed?
                                   (lambda (e) (match-let ([(struct exn:fail:cc:parse:LL.1:failed (m c t s)) e])
                                                 (raise (make-exn:fail:cc:parse:LL.1:failed m c t (cons action s)))))])
                    (parse* LL.1-language reader symbol))))
              (cons variable (filter (lambda (x) x) children))]))

(define (parse LL.1-language in #:file [file "(string)"] #:as [variable (starting-variable LL.1-language)])
  (define reader (make-reader (LL.1-language*-lexicon LL.1-language) in #:file file))
  (parse* LL.1-language reader variable))
