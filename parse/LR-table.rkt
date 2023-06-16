#lang racket

(require "common.rkt")
(require "../tokenization.rkt")

(define (action-step-in state) (list 'S state))

(define (action-reduce variable clause) (list 'R variable clause))

(define (action-accept) 'A)

(define (empty-action-table) null)

(define (push-action action-table action #:state from-state #:when terminal)
  (set-add action-table (list (list from-state terminal) action)))

(define (empty-goto-table) null)

(define (push-goto goto-table to-state #:state from-state #:when variable)
  (set-add goto-table (list (list from-state variable) to-state)))

(define (find-in-table action/goto-table #:state from-state #:when variable/terminal)
  (match (assoc (list from-state variable/terminal) action/goto-table)
    [(list _ action/to-state) action/to-state]
    [#f #f]))

(struct LR-table (action-table goto-table) #:transparent)

(provide action-step-in action-reduce action-accept push-action empty-action-table
         push-goto empty-goto-table find-in-table
         (struct-out LR-table))

(define (find-conflicts table)
  (match-define (struct LR-table (action-table goto-table)) table)
  (define conflict-table (make-hash))
  (for ([action-map (in-list action-table)])
    (match-define (list condition action) action-map)
    (hash-set! conflict-table condition
               (cons action (hash-ref conflict-table condition null))))
  (for/hash ([(condition action-list) (in-hash conflict-table)] #:when (< 1 (length action-list)))
    (values condition action-list)))

(provide find-conflicts)

(define (empty-stack) null)

(define empty-stack? empty?)

(define (push-stack stack state parse-tree)
  (cons (list state parse-tree) stack))

(define (initialize-stack state0)
  (push-stack (empty-stack) state0 #f))

(define (pop-stack stack) (rest stack))

(define (peek-stack stack) (first stack))

(provide empty-stack empty-stack? push-stack initialize-stack pop-stack peek-stack)

(struct exn:fail:cc:parse:LR-no-action exn:fail:cc:parse (stack token)
  #:transparent #:extra-constructor-name make-exn:fail:cc:parse:LR-no-action)

(define (run-LR stack table reader)
  (match-define (struct LR-table (action-table goto-table)) table)
  (define next-token (read-token reader #:peek? #t))
  (match-define (list current-state current-parse-tree) (peek-stack stack))
  (match (find-in-table action-table #:state current-state #:when (token-type next-token))
    ['A (read-token reader) current-parse-tree]
    [(list 'S state)
     (read-token reader)
     (define stack* (push-stack stack state next-token))
     (run-LR stack* table reader)]
    [(list 'R variable clause)
     (define-values (stack- children)
       (for/fold ([stack* stack] [children null]) ([_ (in-range (length clause))])
         (values (pop-stack stack*) (cons (second (peek-stack stack*)) children))))
     (define parse-tree* (cons variable children))
     (define/contract state* (not/c #f)
       (find-in-table goto-table #:state (first (peek-stack stack-)) #:when variable))
     (define stack+ (push-stack stack- state* parse-tree*))
     (run-LR stack+ table reader)]
    [#f (raise (make-exn:fail:cc:parse:LR-no-action
                (format "Cannot find action for token ~A (in state ~A)" next-token current-state)
                (current-continuation-marks)
                stack next-token))]))

(provide (struct-out exn:fail:cc:parse:LR-no-action) run-LR)

(module+ test

  (require rackunit)

  (define-language/lexicon language-4.1/lexicon
    (ID "\\w+")
    (PLUS "\\+")
    (STAR "\\*")
    (LEFT "\\(")
    (RIGHT "\\)")
    "\\s*")

  (define action-table-4.1
    (let ([action-table `((0 ID ,(action-step-in 5))
                          (0 LEFT ,(action-step-in 4))
                          (1 PLUS ,(action-step-in 6))
                          (1 EOF ,(action-accept))
                          (2 PLUS ,(action-reduce 'e '(t)))
                          (2 STAR ,(action-step-in 7))
                          (2 RIGHT ,(action-reduce 'e '(t)))
                          (2 EOF ,(action-reduce 'e '(t)))
                          (3 PLUS ,(action-reduce 't '(f)))
                          (3 STAR ,(action-reduce 't '(f)))
                          (3 RIGHT ,(action-reduce 't '(f)))
                          (3 EOF ,(action-reduce 't '(f)))
                          (4 ID ,(action-step-in 5))
                          (4 LEFT ,(action-step-in 4))
                          (5 PLUS ,(action-reduce 'f '(ID)))
                          (5 STAR ,(action-reduce 'f '(ID)))
                          (5 RIGHT ,(action-reduce 'f '(ID)))
                          (5 EOF ,(action-reduce 'f '(ID)))
                          (6 ID ,(action-step-in 5))
                          (6 LEFT ,(action-step-in 4))
                          (7 ID ,(action-step-in 5))
                          (7 LEFT ,(action-step-in 4))
                          (8 PLUS ,(action-step-in 6))
                          (8 RIGHT ,(action-step-in 11))
                          (9 PLUS ,(action-reduce 'e '(e PLUS t)))
                          (9 STAR ,(action-step-in 7))
                          (9 RIGHT ,(action-reduce 'e '(e PLUS t)))
                          (9 EOF ,(action-reduce 'e '(e PLUS t)))
                          (10 PLUS ,(action-reduce 't '(t STAR f)))
                          (10 STAR ,(action-reduce 't '(t STAR f)))
                          (10 RIGHT ,(action-reduce 't '(t STAR f)))
                          (10 EOF ,(action-reduce 't '(t STAR f)))
                          (11 PLUS ,(action-reduce 'f '(LEFT e RIGHT)))
                          (11 STAR ,(action-reduce 'f '(LEFT e RIGHT)))
                          (11 RIGHT ,(action-reduce 'f '(LEFT e RIGHT)))
                          (11 EOF ,(action-reduce 'f '(LEFT e RIGHT))))])
      (let build-action-table ([table (empty-action-table)] [rest-actions action-table])
        (match rest-actions
          [(list) table]
          [(list (list state variable action) rest-actions* ...)
           (build-action-table (push-action table action #:state state #:when variable)
                               rest-actions*)]))))

  (define goto-table-4.1
    (let ([goto-table '((0 e 1)
                        (0 t 2)
                        (0 f 3)
                        (4 e 8)
                        (4 t 2)
                        (4 f 3)
                        (6 t 9)
                        (6 f 3)
                        (7 f 10))])
      (let build-goto-table ([table (empty-goto-table)] [rest-gotos goto-table])
        (match rest-gotos
          [(list) table]
          [(list (list state variable state+) rest-gotos* ...)
           (build-goto-table (push-goto table state+ #:state state #:when variable)
                             rest-gotos*)]))))

  (define LR-table-4.1 (LR-table action-table-4.1 goto-table-4.1))

  (define (parse-4.1 text)
    (parameterize ([exclude-EOL-during-tokenization? #t])
      (let ([stack (initialize-stack 0)]
            [reader (make-reader language-4.1/lexicon (open-input-string text))])
        (run-LR stack LR-table-4.1 reader))))
  
  (define (basically-equal? a b)
    (cond [(token? a)
           (and (token? b)
                (eq? (token-type a) (token-type b))
                (string=? (token-text a) (token-text b)))]
          [(list? a)
           (and (list? b)
                (andmap basically-equal? a b))]
          [else (equal? a b)]))

  (test-case "Single token"
    (check basically-equal?
           (parse-4.1 "hello") `(e (t (f ,(token 'ID "hello" #f))))))

  (test-case "Binary operation"
    (check basically-equal?
           (parse-4.1 "a + b")
           `(e (e (t (f ,(token 'ID "a" #f))))
               ,(token 'PLUS "+" #f)
               (t (f ,(token 'ID "b" #f)))))
    (check basically-equal?
           (parse-4.1 "a * b")
           `(e (t (t (f ,(token 'ID "a" #f)))
                  ,(token 'STAR "*" #f)
                  (f ,(token 'ID "b" #f))))))

  (test-case "Binary operation string"
    (check basically-equal?
           (parse-4.1 "a + b + c")
           `(e (e (e (t (f ,(token 'ID "a" #f))))
                  ,(token 'PLUS "+" #f)
                  (t (f ,(token 'ID "b" #f))))
               ,(token 'PLUS "+" #f)
               (t (f ,(token 'ID "c" #f)))))
    (check basically-equal?
           (parse-4.1 "a * b * c")
           `(e (t (t (t (f ,(token 'ID "a" #f)))
                     ,(token 'STAR "*" #f)
                     (f ,(token 'ID "b" #f)))
                  ,(token 'STAR "*" #f)
                  (f ,(token 'ID "c" #f))))))

  (test-case "Priority"
    (check basically-equal?
           (parse-4.1 "a + b * c")
           `(e (e (t (f ,(token 'ID "a" #f))))
               ,(token 'PLUS "+" #f)
               (t (t (f ,(token 'ID "b" #f)))
                  ,(token 'STAR "*" #f)
                  (f ,(token 'ID "c" #f)))))

    (check basically-equal?
           (parse-4.1 "a * b + c")
           `(e (e (t (t (f ,(token 'ID "a" #f)))
                     ,(token 'STAR "*" #f)
                     (f ,(token 'ID "b" #f))))
               ,(token 'PLUS "+" #f)
               (t (f ,(token 'ID "c" #f))))))

  (test-case "Grouping"
    (check basically-equal?
           (parse-4.1 "(a + b) * c")
           `(e (t (t (f ,(token 'LEFT "(" #f)
                        (e (e (t (f ,(token 'ID "a" #f))))
                           ,(token 'PLUS "+" #f)
                           (t (f ,(token 'ID "b" #f))))
                        ,(token 'RIGHT ")" #f)))
                  ,(token 'STAR "*" #f)
                  (f ,(token 'ID "c" #f)))))

    (check basically-equal?
           (parse-4.1 "(a) + ((b))")
           `(e (e (t (f ,(token 'LEFT "(" #f)
                        (e (t (f ,(token 'ID "a" #f))))
                        ,(token 'RIGHT ")" #f))))
               ,(token 'PLUS "+" #f)
               (t (f ,(token 'LEFT "(" #f)
                     (e (t (f ,(token 'LEFT "(" #f)
                              (e (t (f ,(token 'ID "b" #f))))
                              ,(token 'RIGHT ")" #f))))
                     ,(token 'RIGHT ")" #f)))))))

