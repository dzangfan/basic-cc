#lang debug racket

(require racket/set)
(require threading)
(require "parse/LR-table.rkt")
(require "parse/LR.0.rkt")
(require (prefix-in LR.1: "parse/LR.1.rkt"))
(require (prefix-in LR.0: "parse/LR.0.rkt"))

(provide find-conflicts)

(define (LR-state-syntactic-symbol state)
  (define/contract syntactic-symbols
    (or/c #f (lambda~> set-count (= 1)))
    (for/set ([item (in-set (~> state LR-state-itemset LR-itemset-core))])
      (item-prev-component item)))
  (and syntactic-symbols
       (set-first syntactic-symbols)))

(define (find-state automaton state-id)
  (findf (lambda~> LR-state-id (= state-id)) automaton))

(define (item->string item)
  (define (list->string lst  #:prefix [prefix ""])
    (if (null? lst)
        ""
        (~>> lst
             (map symbol->string)
             (string-join _ " ")
             (string-append prefix))))
  (define (SLR-item->string item)
    (define-values (head tail) (split-at (LR.0:item-clause item) (LR.0:item-point item)))
    (format "~A ->~A .~A"
            (LR.0:item-variable item)
            (list->string head #:prefix " ")
            (list->string tail #:prefix " ")))
  (cond [(LR.1:item? item)
         (format "[~A / ~A]" (SLR-item->string item) (LR.1:item-condition item))]
        [else (format "[~A]" (SLR-item->string item))]))

(define (describe-conflict-reason state-id terminal reason-list)
  (define (order->string num #:space [space #f])
    (define odr (format "  ~A." num))
    (if space
        (make-string (add1 (string-length odr)) #\Space)
        odr))
  (define header
    (format "~A conflicts occur when state[~A] encounters ~A:" (length reason-list) state-id terminal))
  (define reasons
    (for/list ([order (in-naturals 1)] [reason (in-list reason-list)])
      (match reason
        [(list 'step-in next-state input related-item-list)
         (format "~A Automaton tries to step into ~A, because of items~%~A"
                 (order->string order) next-state
                 (~>> related-item-list
                      (map (lambda~>> item->string (string-append (order->string order #:space #t))))
                      (string-join _ "\n")))]
        [(list 'reduce item)
         (format "~A Automaton tries to reduce because of item ~A"
                 (order->string order)
                 (item->string  item))])))
  (displayln (string-append header "\n"
                            (string-join reasons "\n"))))

(define (describe-conflict automaton LR-table state-id terminal)
  (define conflicts (find-conflicts LR-table))
  (define/contract state LR-state? (find-state automaton state-id))
  (define itemset
    (match-let ([(struct LR-itemset (core closure)) (LR-state-itemset state)])
      (set-union core closure)))
  (define/contract conflict pair? (hash-ref conflicts (list state-id terminal) null))
  (define conflict-reason
    (for/list ([action conflict])
      (match action
        [(list 'S next-state)
         (define/contract input (not/c #f)
           (~>> next-state (find-state automaton) LR-state-syntactic-symbol))
         (define related-item-list
           (for/list ([item (in-set itemset)] #:when (eq? (item-next-component item) input))
             item))
         (list 'step-in next-state input related-item-list)]
        [(list 'R variable clause)
         (define related-item
           (for/first ([item (in-set itemset)]
                       #:when (and (eq? (item-variable item) variable)
                                   (eq? (item-clause item) clause)
                                   (not (item-next-component item))))
             item))
         (list 'reduce related-item)]
        [action (list 'unknown action)])))
  (describe-conflict-reason state-id terminal conflict-reason))

(define (describe-all-conflicts automaton LR-table)
  (define conflicts (find-conflicts LR-table))
  (cond [(hash-empty? conflicts)
         (displayln "Grammar does not have conflicts")]
        [else (define bar (format "*** ~A conflicts have been detected ***" (hash-count conflicts)))
              (displayln bar)
              (for ([condition (in-hash-keys conflicts)])
                (describe-conflict automaton LR-table (first condition) (second condition)))
              (displayln bar)]))

(provide describe-conflict describe-all-conflicts)

(module+ test

  (require "language.rkt")
  
  (define-language juhz
    (EQ "=") (SEMICOLON ";") (DOT "\\.") (COMMA ",")
    (ROUNDLEFT "\\(") (ROUNDRIGHT "\\)")
    (SQUARELEFT "\\[") (SQUARERIGHT "\\]")
    (CURLYLEFT "\\{") (CURLYRIGHT "\\}")
    (IF "if") (ELSE "else") (WHILE "while") (PACKAGE "package")
    (IDENT "[a-zA-Z_][a-zA-Z0-9_]*")
    (NUMBER "[0-9]|[1-9][0-9]+")
    (STRING "\"(?:\\\\\"|\\\\n|\\\\\\\\|[^\"\\\\])*\"")
    (TRUE "true") (FALSE "false")
    (program statement (statement program))
    (statement expression
               (left-value EQ expression SEMICOLON)
               (left-value EQ CURLYLEFT program CURLYRIGHT)
               (IF expression CURLYLEFT program CURLYRIGHT)
               (IF expression CURLYLEFT program CURLYRIGHT ELSE CURLYLEFT program CURLYRIGHT)
               (WHILE expression CURLYLEFT program CURLYRIGHT))
    (left-value IDENT (IDENT ROUNDLEFT ROUNDRIGHT) (IDENT ROUNDLEFT parameter-list ROUNDRIGHT))
    (parameter-list IDENT (IDENT COMMA parameter-list))
    (expression atom callable array package)
    (atom NUMBER STRING TRUE FALSE)
    (callable IDENT call indexing selection)
    (call (callable ROUNDLEFT ROUNDRIGHT) (callable ROUNDLEFT argument-list ROUNDRIGHT))
    (array (SQUARELEFT SQUARERIGHT) (SQUARELEFT argument-list SQUARERIGHT))
    (package (PACKAGE ROUNDLEFT program ROUNDRIGHT))
    (indexing (callable SQUARELEFT expression SQUARERIGHT))
    (selection selection-head (selection DOT IDENT))
    (selection-head callable PACKAGE)
    (argument-list expression (expression COMMA argument-list))
    (#:allow-conflict)
    (#:make-automaton)))

