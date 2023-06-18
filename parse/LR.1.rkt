#lang racket

(require threading)
(require racket/set)
(require data/queue)
(require "../tokenization.rkt")
(require "common.rkt")
(require "grammar.rkt")
(require "LR-table.rkt")
(require (prefix-in SLR: "LR.0.rkt"))
(require (only-in "LR.0.rkt" LR-itemset))

(struct item SLR:item (condition) #:transparent)

(define (item-forward-component item0)
  (match-define (struct SLR:item (variable clause point))
    (SLR:item-forward-component item0))
  (item variable clause point (item-condition item0)))

(define (item-suffix item*)
  (match-define (struct item (_ clause point condition)) item*)
  (let drop ([rest-symbols clause] [downcount point])
    (cond [(null? rest-symbols) (list condition)]
          [(zero? downcount) (append rest-symbols (list condition))]
          [else (drop (rest rest-symbols) (sub1 downcount))])))

(define (extend-itemset grammar item*)
  (define next-component (SLR:item-next-component item*))
  (if (or (not next-component) (terminal? grammar next-component))
      (set)
      (~> (for/list ([condition (in-set (~>> item* item-forward-component item-suffix (FIRST grammar)))])
            (~>> (find-product grammar next-component)
                 (map (lambda (clause) (item next-component clause 0 condition)))))
          flatten
          list->set)))

(define (CLOSURE grammar itemset-core)
  (let build-closure ([waiting-set itemset-core]
                      [itemset-closure (set)]
                      [itemset-processed (set)])
    (cond [(set-empty? waiting-set) (LR-itemset itemset-core itemset-closure)]
          [else (define next-item (set-first waiting-set))
                (define waiting-set+ (set-rest waiting-set))
                (define itemset-processed+
                  (set-add itemset-processed next-item))
                (define new-itemset
                  (~>> (extend-itemset grammar next-item)
                       (set-subtract _ itemset-processed)))
                (build-closure (set-union waiting-set+ new-itemset)
                               (set-union itemset-closure new-itemset)
                               itemset-processed+)])))

(provide CLOSURE)

(define (itemset->cores itemset)
  (let collect-cores ([cores null]
                      [rest-itemset (set-union (SLR:LR-itemset-core itemset) (SLR:LR-itemset-closure itemset))])
    (cond [(set-empty? rest-itemset) cores]
          [else (define next-item (set-first rest-itemset))
                (define next-variable (SLR:item-next-component next-item))
                (define-values (group core)
                  (for/lists (group core)
                             ([item (in-set rest-itemset)] #:when (eq? next-variable (SLR:item-next-component item)))
                    (values item (item-forward-component item))))
                (collect-cores (if next-variable (cons (list->set core) cores) cores)
                               (set-subtract rest-itemset (list->set group)))])))

(define/contract (build-LR-automaton grammar)
  (-> augmented-grammar? (listof SLR:LR-state?))
  (define core-queue0 (make-queue))
  (define table/core->id0 (make-hash))
  (let* ([start-variable (standard-grammar-start-variable grammar)]
         [clause (first (find-product grammar start-variable))]
         [item (item start-variable clause 0 'EOF)]
         [itemset (set item)])
    (enqueue! core-queue0 itemset)
    (hash-set! table/core->id0 itemset 0))
  (let build ([id-pool 0]
              [state-list null]
              [core-queue core-queue0]
              [table/core->id table/core->id0])
    (cond [(queue-empty? core-queue)
           (for ([state (in-list state-list)])
             (for ([core+ (in-list (~> state SLR:LR-state-itemset itemset->cores))])
               (define id+ (hash-ref table/core->id core+))
               (define variable+ (SLR:item-prev-component (set-first core+)))
               (SLR:LR-state-add-goto! state variable+ id+)))
           state-list]
          [else (define next-core (dequeue! core-queue))
                (define new-itemset (CLOSURE grammar next-core))
                (define new-id (hash-ref table/core->id next-core))
                (for ([core+ (in-list (itemset->cores new-itemset))] #:unless (hash-has-key? table/core->id core+))
                  (set! id-pool (add1 id-pool))
                  (enqueue! core-queue core+)
                  (hash-set! table/core->id core+ id-pool))
                (define new-state (SLR:LR-state new-id new-itemset null))
                (build id-pool (cons new-state state-list) core-queue table/core->id)])))


(define (LR-automaton->LR-table grammar automaton)
  (let transform ([action-table (empty-action-table)]
                  [goto-table (empty-goto-table)]
                  [state-list automaton])
    (match state-list
      [(list) (LR-table action-table goto-table)]
      [(list (struct SLR:LR-state (id (struct LR-itemset (core closure)) goto)) state-list* ...)
       (define action-table+
         (for/fold ([action-table+ action-table] #:result action-table+)
                   ([item (in-set (set-union core closure))])
           (cond [(and (eq? (SLR:item-variable item) (standard-grammar-start-variable grammar))
                       (positive-integer? (SLR:item-point item)))
                  (push-action action-table+ (action-accept)
                               #:state id #:when 'EOF)]
                 [(SLR:item-next-component item)
                  (define variable/terminal (SLR:item-next-component item))
                  (define next-state/id (second (assoc variable/terminal goto)))
                  (if (terminal? grammar variable/terminal)
                      (push-action action-table+ (action-step-in next-state/id)
                                   #:state id #:when variable/terminal)
                      action-table+)]
                 [else (push-action action-table+
                                    (action-reduce (SLR:item-variable item) (SLR:item-clause item))
                                    #:state id #:when (item-condition item))])))
       (define goto-table+
         (for/fold ([goto-table+ goto-table] #:result goto-table+)
                   ([variable/terminal+id+ (in-list goto)])
           (match-define (list variable/terminal id+) variable/terminal+id+)
           (if (non-terminal? grammar variable/terminal)
               (push-goto goto-table+ id+ #:state id #:when variable/terminal)
               goto-table+)))
       (transform action-table+ goto-table+ state-list*)])))

(define (build-LR.1-table grammar)
  (~>> grammar build-LR-automaton (LR-automaton->LR-table grammar)))

(provide build-LR-automaton LR-automaton->LR-table build-LR.1-table)

(module+ test

  (require rackunit)

  (define language-4.55/grammar
    (build-standard-grammar
     '((s# s) (s (c c)) (c (C c) D))))

  (test-case "CLOSURE for LR(1)"
    (check-equal? (CLOSURE language-4.55/grammar (set (item 's# '(s) 0 'EOF)))
                  (LR-itemset (set (item 's# '(s) 0 'EOF))
                              (set (item 's '(c c) 0 'EOF)
                                   (item 'c '(C c) 0 'C)
                                   (item 'c '(C c) 0 'D)
                                   (item 'c '(D) 0 'C)
                                   (item 'c '(D) 0 'D))))
    (check-equal? (CLOSURE language-4.55/grammar (set (item 'c '(C c) 1 'C) (item 'c '(C c) 1 'D)))
                  (LR-itemset (set (item 'c '(C c) 1 'C) (item 'c '(C c) 1 'D))
                              (set (item 'c '(C c) 0 'C) (item 'c '(C c) 0 'D)
                                   (item 'c '(D) 0 'C) (item 'c '(D) 0 'D))))
    (check-equal? (CLOSURE language-4.55/grammar (set (item 'c '(C c) 1 'EOF)))
                  (LR-itemset (set (item 'c '(C c) 1 'EOF))
                              (set (item 'c '(C c) 0 'EOF)
                                   (item 'c '(D) 0 'EOF))))
    (check-equal? (CLOSURE language-4.55/grammar (set (item 'c '(D) 1 'C) (item 'c '(D) 1 'D)))
                  (LR-itemset (set (item 'c '(D) 1 'C) (item 'c '(D) 1 'D))
                              (set))))

  (define-language/lexicon language-4.1/lexicon
    (ID "\\w+")
    (PLUS "\\+")
    (STAR "\\*")
    (LEFT "\\(")
    (RIGHT "\\)")
    "\\s*")

  (define language/grammar-4.1
    (build-standard-grammar
     '((e+ e)
       (e (e PLUS t) t)
       (t (t STAR f) f)
       (f (LEFT e RIGHT) ID))))

  (define LR-table-4.1
    (build-LR.1-table language/grammar-4.1))

  (define (parse-4.1 text)
    (parameterize ([exclude-EOL-during-tokenization? #t])
      (let ([reader (make-reader language-4.1/lexicon (open-input-string text))])
        (run-LR/simple-error LR-table-4.1 reader))))

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
                     ,(token 'RIGHT ")" #f))))))

  (define language-4.49/grammar
    (build-standard-grammar
     '((s (l EQ r) r)
       (l (DE r) ID)
       (r l))))

  (define language-4.49/table!SLR
    (~> language-4.49/grammar augment-grammar SLR:build-LR.0-table ))

  (define language-4.49/table!LR.1
    (~> language-4.49/grammar augment-grammar build-LR.1-table))

  (test-case "Avoid conflicts"
    (check-pred (lambda~> hash-empty? not)
                (find-conflicts language-4.49/table!SLR))
    (check-pred hash-empty? (find-conflicts language-4.49/table!LR.1))))
