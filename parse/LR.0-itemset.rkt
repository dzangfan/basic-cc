#lang racket

(require racket/set)
(require racket/list)
(require data/queue)
(require threading)
(require "grammar.rkt")
(require "LR-table.rkt")

(struct item (variable clause point) #:transparent)

(define (item-next-component item)
  (let drop ([clause (item-clause item)] [downcount (item-point item)])
    (and (pair? clause)
         (if (zero? downcount)
             (first clause)
             (drop (rest clause) (sub1 downcount))))))

(define (item-prev-component item*)
  (match-define (struct item (variable clause point)) item*)
  (and (> point 0)
       (or (item-next-component (item variable clause (sub1 point)))
           (last clause))))

(define (item-forward-component item0)
  (match-define (struct item (variable clause point)) item0)
  (item variable clause (add1 point)))

(provide (struct-out item) item-next-component item-prev-component item-forward-component)

(struct LR-itemset (core closure) #:transparent)

(define (itemset->cores itemset)
  (let collect-cores ([cores null]
                      [rest-itemset (set-union (LR-itemset-core itemset) (LR-itemset-closure itemset))])
    (cond [(set-empty? rest-itemset) cores]
          [else (define next-item (set-first rest-itemset))
                (define next-variable (item-next-component next-item))
                (define-values (group core)
                  (for/lists (group core)
                             ([item (in-set rest-itemset)] #:when (eq? next-variable (item-next-component item)))
                    (values item (item-forward-component item))))
                (collect-cores (if next-variable (cons (list->set core) cores) cores)
                               (set-subtract rest-itemset (list->set group)))])))

(define (extend-itemset grammar variable)
  (~>> (find-product grammar variable)
       (map (lambda (clause) (item variable clause 0)))
       list->set))

(define (CLOSURE grammar itemset-core)
  (let build-closure ([waiting-set itemset-core]
                      [itemset-closure (set)])
    (cond [(set-empty? waiting-set) (LR-itemset itemset-core itemset-closure)]
          [else (define next-item (set-first waiting-set))
                (define waiting-set+ (set-rest waiting-set))
                (define next-variable (item-next-component next-item))
                (cond [(not (non-terminal? grammar next-variable))
                       (build-closure waiting-set+ itemset-closure)]
                      [else (define new-itemset (extend-itemset grammar next-variable))
                            (define new-itemset* (set-subtract new-itemset itemset-closure))
                            (build-closure (set-union waiting-set+ new-itemset*)
                                           (set-union new-itemset itemset-closure))])])))

(provide (struct-out LR-itemset) CLOSURE)

(struct LR-state (id itemset [goto #:mutable]) #:transparent)

(define (LR-state-add-goto! state variable id+)
  (define goto0 (LR-state-goto state))
  (set-LR-state-goto! state (cons (list variable id+) goto0)))

(define (augmented-grammar? grammar)
  (and (standard-grammar? grammar)
       (let ([products (find-product grammar (standard-grammar-start-variable grammar))])
         (and (= 1 (length products))
              (= 1 (length (first products)))
              (non-terminal? (first (first products)))))))

(define (build-LR-automaton grammar)
  (define core-queue0 (make-queue))
  (define table/core->id0 (make-hash))
  (let* ([start-variable (standard-grammar-start-variable grammar)]
         [clause (first (find-product grammar start-variable))]
         [item (item start-variable clause 0)]
         [itemset (set item)])
    (enqueue! core-queue0 itemset)
    (hash-set! table/core->id0 itemset 0))
  (let build ([id-pool 0]
              [state-list null]
              [core-queue core-queue0]
              [table/core->id table/core->id0])
    (cond [(queue-empty? core-queue)
           (for ([state (in-list state-list)])
             (for ([core+ (in-list (~> state LR-state-itemset itemset->cores))])
               (define id+ (hash-ref table/core->id core+))
               (define variable+ (item-prev-component (set-first core+)))
               (LR-state-add-goto! state variable+ id+)))
           state-list]
          [else (define next-core (dequeue! core-queue))
                (define new-itemset (CLOSURE grammar next-core))
                (define new-id (hash-ref table/core->id next-core))
                (for ([core+ (in-list (itemset->cores new-itemset))] #:unless (hash-has-key? table/core->id core+))
                  (set! id-pool (add1 id-pool))
                  (enqueue! core-queue core+)
                  (hash-set! table/core->id core+ id-pool))
                (define new-state (LR-state new-id new-itemset null))
                (build id-pool (cons new-state state-list) core-queue table/core->id)])))

(provide (struct-out LR-state)
         (contract-out (build-LR-automaton (-> augmented-grammar? (listof LR-state?)))))

(define (LR-automaton->LR-table grammar automaton)
  (let transform ([action-table (empty-action-table)]
                  [goto-table (empty-goto-table)]
                  [state-list automaton])
    (match state-list
      [(list) (LR-table action-table goto-table)]
      [(list (struct LR-state (id (struct LR-itemset (core closure)) goto)) state-list* ...)
       (define action-table+
         (for/fold ([action-table+ action-table] #:result action-table+)
                   ([item (in-set (set-union core closure))])
           (cond [(and (eq? (item-variable item) (standard-grammar-start-variable grammar))
                       (positive-integer? (item-point item)))
                  (push-action action-table+ (action-accept)
                               #:state id #:when 'EOF)]
                 [(item-next-component item)
                  (define variable/terminal (item-next-component item))
                  (define next-state/id (second (assoc variable/terminal goto)))
                  (if (terminal? grammar variable/terminal)
                      (push-action action-table+ (action-step-in next-state/id)
                                   #:state id #:when variable/terminal)
                      action-table+)]
                 [else (for/fold ([action-table++ action-table+] #:result action-table++)
                                 ([variable* (in-set (FOLLOW grammar (item-variable item)))])
                         (push-action action-table++ (action-reduce (item-variable item) (item-clause item))
                                      #:state id #:when variable*))])))
       (define goto-table+
         (for/fold ([goto-table+ goto-table] #:result goto-table+)
                   ([variable/terminal+id+ (in-list goto)])
           (match-define (list variable/terminal id+) variable/terminal+id+)
           (if (non-terminal? grammar variable/terminal)
               (push-goto goto-table+ id+ #:state id #:when variable/terminal)
               goto-table+)))
       (transform action-table+ goto-table+ state-list*)])))

(provide LR-automaton->LR-table)

(module+ test

  (require "common.rkt")
  (require "../tokenization.rkt")
  (require rackunit)

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
    (LR-automaton->LR-table language/grammar-4.1 (build-LR-automaton language/grammar-4.1)))

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


