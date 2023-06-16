#lang racket

(require (for-syntax threading) threading)
(require (for-syntax racket/list))
(require "tokenization.rkt")
(require "parse/common.rkt")
(require "parse/grammar.rkt")
(require "parse/LR.0.rkt")

(begin-for-syntax
  (define (generate-name base-id #:prefix [prefix ""] #:suffix [suffix ""])
    (define base-id-text (~> base-id syntax->datum symbol->string))
    (define name (string-append prefix base-id-text suffix))
    (datum->syntax base-id
                   (string->symbol name)
                   base-id))

  (define (find-option keyword options)
    (let ([option (assoc keyword options)])
      (and option
           (rest option)))))

(define-syntax (collect-lexicon&grammar&options stx)
  (syntax-case stx ()
    [(_ name () (lexicon ...) grammar options)
     (let ([options (syntax->datum #'options)])
       (with-syntax ([language/lexicon (generate-name #'name #:suffix "/lexicon")]
                     [language/grammar (generate-name #'name #:suffix "/grammar")]
                     [language-cut (generate-name #'name #:suffix "-cut")]
                     [language-read (generate-name #'name #:suffix "-read")]
                     [exclude-EOL? (if (find-option '#:enable-EOL options) #'#f #'#t)])
         #'(begin
             (define-language/lexicon language/lexicon
               lexicon ...)
             (define language/grammar
               (augment-grammar
                (build-standard-grammar
                 (quote grammar))))
             (define (language-cut in #:file [file "(string)"])
               (parameterize ([exclude-EOL-during-tokenization? exclude-EOL?])
                 (tokenize language/lexicon in #:file file)))
             (define language-read
               (let ([SLR-table (~>> language/grammar build-LR-automaton (LR-automaton->LR-table language/grammar))])
                 (lambda (in #:file [file "(string)"])
                   (parameterize ([exclude-EOL-during-tokenization? exclude-EOL?])
                     (define reader (make-reader language/lexicon in #:file file))
                     (run-SLR SLR-table reader))))))))]
    [(_ name (s clauses ...) (lexicon ...) grammar options)
     (string? (syntax-e #'s))
     #'(collect-lexicon&grammar&options name (clauses ...) (lexicon ... s) grammar options)]
    [(_ name ((type regex-str) clauses ...) (lexicon ...) grammar options)
     (and (identifier? #'type) (string? (syntax-e #'regex-str)))
     #'(collect-lexicon&grammar&options name (clauses ...) (lexicon ... (type regex-str)) grammar options)]
    [(_ name ((keyword parameters ...) clauses ...) lexicon grammar (options ...))
     (keyword? (syntax-e #'keyword))
     #'(collect-lexicon&grammar&options name (clauses ...) lexicon grammar (options ... (keyword parameters ...)))]
    [(_ name ((product ...) clauses ...) lexicon (grammar ...) options)
     #'(collect-lexicon&grammar&options name (clauses ...) lexicon (grammar ... (product ...)) options)]))

(define-syntax-rule (define-language name clauses ...)
  (collect-lexicon&grammar&options name (clauses ...) () () ()))

(provide define-language)

(module+ test

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

  (define parse-4.1 language-4.1-read)

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
