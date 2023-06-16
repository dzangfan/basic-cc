#lang racket

(require "common.rkt")

(require threading)
(require racket/struct)

(provide (struct-out exn:fail:cc:tokenization)
         (struct-out location) (struct-out token)
         make-tokenizer tokenize-line! tokenize
         exclude-EOL-during-tokenization?
         define-language/lexicon build-lexicon)

(struct exn:fail:cc:tokenization exn:fail:cc () #:transparent
  #:extra-constructor-name make-exn:fail:cc:tokenization)

(struct location (file line column) #:transparent)

(struct token (type text location)
  #:transparent
  #:property prop:custom-print-quotable 'always
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (token-type obj))
      (lambda (obj) (list (token-text obj)))))])

(struct language/lexicon (regexp handler) #:transparent)

(struct tokenizer (lexicon [location #:mutable] in-port) #:transparent)

(define (report-error tokenizer message)
  (define location (tokenizer-location tokenizer))
  (raise (make-exn:fail:cc:tokenization
          (format "Tokenization error[~A:L~A] ~A"
                  (location-file location) (location-line location) message)
          (current-continuation-marks))))

(define-syntax-rule (report-error* tokenizer format-string v ...)
  (report-error tokenizer (format format-string v ...)))

(define exclude-EOL-during-tokenization? (make-parameter #false))

(define (update-location-column base-location column)
  (match-let ([(struct location (file line _)) base-location])
    (location file line column)))

(define (make-tokenizer lexicon file in-port)
  (tokenizer lexicon (location file 0 0) in-port))

(define (matched-positions->strings matched-positions original-text tokenizer)
  (let collect ([last-end 0] [rest-positions matched-positions] [collected null])
    (define (reject)
      (report-error* tokenizer "Unknown token in column ~A: ~A >~A< ~A"
                     last-end (if (zero? last-end) "" "...")
                     (substring original-text last-end (add1 last-end))
                     (substring original-text (add1 last-end))))
    (cond [(= last-end (string-length original-text)) (reverse collected)]
          [(null? rest-positions) (reject)]
          [else (define next-position (first rest-positions))
                (match-define (cons start end) (first next-position))
                (unless (= start last-end) (reject))
                (define main-matched/str (substring original-text start end))
                (define sub-matched/str
                  (for/list ([position (in-list (rest next-position))])
                    (and position (substring original-text (car position) (cdr position)))))
                (define matched/str (list (cons main-matched/str sub-matched/str) start))
                (collect end (rest rest-positions) (cons matched/str collected))])))

(define (tokenize-line! tokenizer)
  (define next-line (read-line (tokenizer-in-port tokenizer)))
  (define current-line-location (tokenizer-location tokenizer))
  (if (eof-object? next-line)
      (list (token 'EOF #f current-line-location))
      (let ([matched-list (~> tokenizer tokenizer-lexicon language/lexicon-regexp (regexp-match-positions* next-line #:match-select values))])
        (unless matched-list (report-error* tokenizer "Unable to tokenize line: ~A" next-line))
        (define matched-list/str (matched-positions->strings matched-list next-line tokenizer))
        (define handler (~> tokenizer tokenizer-lexicon language/lexicon-handler))
        (define token-list
          (for/list ([matched+start-position (in-list matched-list/str)]
                     #:do [(match-define (list matched start-position) matched+start-position)]
                     #:do [(define maybe-token (handler matched current-line-location start-position))]
                     #:when maybe-token)
            maybe-token))
        (match-define (struct location (file line _)) current-line-location)
        (set-tokenizer-location! tokenizer (location file (add1 line) 0))
        (if (exclude-EOL-during-tokenization?)
            token-list
            (append token-list
                    (list (token 'EOL #f (update-location-column current-line-location (string-length next-line)))))))))

(define (tokenize lexicon in #:file [file "(string)"])
  (let ([tokenizer (make-tokenizer lexicon file (if (string? in) (open-input-string in) in))])
    (apply vector-append
           (for/list ([_ (in-naturals)]
                        #:do [(define token-list (tokenize-line! tokenizer))]
                        #:final (and (not (null? token-list)) (eq? 'EOF (~> token-list first token-type))))
             (list->vector token-list)))))

(define (build-lexicon rule-list)
  (let rule-list->lexison ([rest-rule-list rule-list] [options null] [token-type-list null])
    (if (null? rest-rule-list)
        (language/lexicon (pregexp (string-join options "|"))
                          (lambda (matched location column)
                            (for/or ([sub-matched (in-list (rest matched))]
                                     [type (in-list token-type-list)])
                              (and sub-matched (token type sub-matched (update-location-column location column))))))
        (match-let ([(list regexp-str type capture?) (first rest-rule-list)])
          (rule-list->lexison (rest rest-rule-list)
                              (cons (if capture? (format "(~A)" regexp-str) (format "(?:~A)" regexp-str)) options)
                              (if capture? (cons type token-type-list) token-type-list))))))

(define-syntax define-language/lexicon-aux
  (lambda (expr)
    (syntax-case expr ()
      [(_ (type regexp-str)) (and (identifier? #'type) (string? (syntax->datum #'regexp-str))) #'(list regexp-str 'type #t)]
      [(_ regexp-str) (string? (syntax->datum #'regexp-str)) #'(list regexp-str #f #f)])))

(define-syntax-rule (define-language/lexicon name clause ...)
  (define name
    (build-lexicon
     (list (define-language/lexicon-aux clause) ...))))

(module+ test

  (require rackunit)

  (test-case "Test matched-positions->string"
    (define (destructive-matching regexp string)
      (~> regexp (regexp-match-positions* string #:match-select values)
          (matched-positions->strings string (make-tokenizer #f "(string)" #f))))

    (check-equal? (destructive-matching #px"\\d" "1234")
                  '((("1") 0) (("2") 1) (("3") 2) (("4") 3)))

    (check-equal? (destructive-matching #px"a|(b)|(c)" "ababc")
                  '((("a" #f #f) 0) (("b" "b" #f) 1) (("a" #f #f) 2) (("b" "b" #f) 3) (("c" #f "c") 4)))

    (check-exn exn:fail:cc:tokenization? (lambda () (destructive-matching #px"\\d" "123 4"))))

  (define simple-lexicon
    (build-lexicon
     '(("\\w+" WORD #t)
       ("\\d+" NUMBER #t)
       ("\\s" #f #f))))

  (test-case "Test simple lexicon"
    (define tokenizer (make-tokenizer simple-lexicon "(string)" (open-input-string "hello 100 world")))
    (check-equal? (tokenize-line! tokenizer)
                  (list (token 'WORD "hello" (location "(string)" 0 0))
                        (token 'NUMBER "100" (location "(string)" 0 6))
                        (token 'WORD "world" (location "(string)" 0 10))
                        (token 'EOL #f (location "(string)" 0 15))))
    (check-equal? (tokenize-line! tokenizer)
                  (list (token 'EOF #f (location "(string)" 1 0)))))

  (test-case "Test parameter excluding EOL"
    (define source-code "hello\nworld")
    (check-equal? (tokenize simple-lexicon source-code)
                  (vector (token 'WORD "hello" (location "(string)" 0 0)) (token 'EOL #f (location "(string)" 0 5))
                          (token 'WORD "world" (location "(string)" 1 0)) (token 'EOL #f (location "(string)" 1 5))
                          (token 'EOF #f (location "(string)" 2 0))))
    (parameterize ([exclude-EOL-during-tokenization? #t])
      (check-equal? (tokenize simple-lexicon source-code)
                    (vector (token 'WORD "hello" (location "(string)" 0 0))
                            (token 'WORD "world" (location "(string)" 1 0))
                            (token 'EOF #f (location "(string)" 2 0))))))

  (define-language/lexicon stone-lexicon
    (NUM "[0-9]+")
    (STR "\"(?:\\\\\"|\\\\n|\\\\\\\\|[^\"\\\\])*\"")
    (ID "[A-Za-z_][A-Za-z_0-9]*|=|\\(|\\)|\\{|\\}|;|\\+|-|\\*|/|%|<|>|==|<=|>=|&&|\\|\\|")
    "\\s"
    "#.*")

  (test-case "Test Stone Lexer"
    (define source-code
      "
       i = \"#0\"
       # sum = 0
       while i < 10 {
         sum = sum + i
         i = i + 1
       }")
    (define token-vector
      (vector (token 'EOL #f (location "(string)" 0 0))
              (token 'ID "i" (location "(string)" 1 7))
              (token 'ID "=" (location "(string)" 1 9))
              (token 'STR "\"#0\"" (location "(string)" 1 11))
              (token 'EOL #f (location "(string)" 1 15))
              (token 'EOL #f (location "(string)" 2 16))
              (token 'ID "while" (location "(string)" 3 7))
              (token 'ID "i" (location "(string)" 3 13))
              (token 'ID "<" (location "(string)" 3 15))
              (token 'NUM "10" (location "(string)" 3 17))
              (token 'ID "{" (location "(string)" 3 20))
              (token 'EOL #f (location "(string)" 3 21))
              (token 'ID "sum" (location "(string)" 4 9))
              (token 'ID "=" (location "(string)" 4 13))
              (token 'ID "sum" (location "(string)" 4 15))
              (token 'ID "+" (location "(string)" 4 19))
              (token 'ID "i" (location "(string)" 4 21))
              (token 'EOL #f (location "(string)" 4 22))
              (token 'ID "i" (location "(string)" 5 9))
              (token 'ID "=" (location "(string)" 5 11))
              (token 'ID "i" (location "(string)" 5 13))
              (token 'ID "+" (location "(string)" 5 15))
              (token 'NUM "1" (location "(string)" 5 17))
              (token 'EOL #f (location "(string)" 5 18))
              (token 'ID "}" (location "(string)" 6 7))
              (token 'EOL #f (location "(string)" 6 8))
              (token 'EOF #f (location "(string)" 7 0))))
    (check-equal? (tokenize stone-lexicon source-code)
                  token-vector)))
