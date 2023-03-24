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

(struct location (file line) #:transparent)

(struct token (type text location)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (token-type obj))
      (lambda (obj) (list (token-text obj)
                          (format "~A:~A"
                                  (~> obj token-location location-file)
                                  (~> obj token-location location-line))))))])

(struct language/lexicon (regexp handler) #:transparent)

(struct tokenizer (lexicon [location #:mutable] in-port) #:transparent)

(define (report-error tokenizer message)
  (define location (tokenizer-location tokenizer))
  (raise (make-exn:fail:cc:tokenization
          (format "Tokenization error[~A:~A] ~A" (location-file location) (location-line location) message)
          (current-continuation-marks))))

(define-syntax-rule (report-error* tokenizer format-string v ...)
  (report-error tokenizer (format format-string v ...)))

(define exclude-EOL-during-tokenization? (make-parameter #false))

(define (make-tokenizer lexicon file in-port)
  (tokenizer lexicon (location file 0) in-port))

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
                (define matched/str (cons main-matched/str sub-matched/str))
                (collect end (rest rest-positions) (cons matched/str collected))])))

(define (tokenize-line! tokenizer)
  (define next-line (read-line (tokenizer-in-port tokenizer)))
  (define current-location (tokenizer-location tokenizer))
  (if (eof-object? next-line)
      (list (token 'EOF #f current-location))
      (let ([matched-list (~> tokenizer tokenizer-lexicon language/lexicon-regexp (regexp-match-positions* next-line #:match-select values))])
        (unless matched-list (report-error* tokenizer "Unable to tokenize line: ~A" next-line))
        (define matched-list/str (matched-positions->strings matched-list next-line tokenizer))
        (define handler (~> tokenizer tokenizer-lexicon language/lexicon-handler))
        (define token-list
          (for/list ([matched (in-list matched-list/str)]
                     #:do [(define maybe-token (handler matched current-location))]
                     #:when maybe-token)
            maybe-token))
        (set-tokenizer-location! tokenizer (location (location-file current-location) (add1 (location-line current-location))))
        (if (exclude-EOL-during-tokenization?)
            token-list
            (append token-list (list (token 'EOL #f current-location)))))))

(define (tokenize lexicon in #:file [file "(string)"])
  (let ([tokenizer (make-tokenizer lexicon file (if (string? in) (open-input-string in) in))])
    (apply vector-append
           (for/list ([_ (in-naturals)]
                        #:do [(define token-list (tokenize-line! tokenizer))]
                        #:final (eq? 'EOF (~> token-list first token-type)))
             (list->vector token-list)))))

(define (build-lexicon rule-list)
  (let rule-list->lexison ([rest-rule-list rule-list] [options null] [token-type-list null])
    (if (null? rest-rule-list)
        (language/lexicon (pregexp (string-join options "|"))
                          (lambda (matched location)
                            (for/or ([sub-matched (in-list (rest matched))]
                                     [type (in-list token-type-list)])
                              (and sub-matched (token type sub-matched location)))))
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
                  '(("1") ("2") ("3") ("4")))

    (check-equal? (destructive-matching #px"a|(b)|(c)" "ababc")
                  '(("a" #f #f) ("b" "b" #f) ("a" #f #f) ("b" "b" #f) ("c" #f "c")))

    (check-exn exn:fail:cc:tokenization? (lambda () (destructive-matching #px"\\d" "123 4"))))

  (define simple-lexicon
    (build-lexicon
     '(("\\w+" WORD #t)
       ("\\d+" NUMBER #t)
       ("\\s" #f #f))))

  (test-case "Test simple lexicon"
    (define tokenizer (make-tokenizer simple-lexicon "(string)" (open-input-string "hello 100 world")))
    (check-equal? (tokenize-line! tokenizer)
                  (list (token 'WORD "hello" (location "(string)" 0))
                        (token 'NUMBER "100" (location "(string)" 0))
                        (token 'WORD "world" (location "(string)" 0))
                        (token 'EOL #f (location "(string)" 0))))
    (check-equal? (tokenize-line! tokenizer)
                  (list (token 'EOF #f (location "(string)" 1)))))

  (test-case "Test parameter excluding EOL"
    (define source-code "hello\nworld")
    (check-equal? (tokenize simple-lexicon source-code)
                  (vector (token 'WORD "hello" (location "(string)" 0)) (token 'EOL #f (location "(string)" 0))
                          (token 'WORD "world" (location "(string)" 1)) (token 'EOL #f (location "(string)" 1))
                          (token 'EOF #f (location "(string)" 2))))
    (parameterize ([exclude-EOL-during-tokenization? #t])
      (check-equal? (tokenize simple-lexicon source-code)
                    (vector (token 'WORD "hello" (location "(string)" 0))
                            (token 'WORD "world" (location "(string)" 1))
                            (token 'EOF #f (location "(string)" 2))))))

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
      (vector (token 'EOL #f (location "(string)" 0))
              (token 'ID "i" (location "(string)" 1))
              (token 'ID "=" (location "(string)" 1))
              (token 'STR "\"#0\"" (location "(string)" 1))
              (token 'EOL #f (location "(string)" 1))
              (token 'EOL #f (location "(string)" 2))
              (token 'ID "while" (location "(string)" 3))
              (token 'ID "i" (location "(string)" 3))
              (token 'ID "<" (location "(string)" 3))
              (token 'NUM "10" (location "(string)" 3))
              (token 'ID "{" (location "(string)" 3))
              (token 'EOL #f (location "(string)" 3))
              (token 'ID "sum" (location "(string)" 4))
              (token 'ID "=" (location "(string)" 4))
              (token 'ID "sum" (location "(string)" 4))
              (token 'ID "+" (location "(string)" 4))
              (token 'ID "i" (location "(string)" 4))
              (token 'EOL #f (location "(string)" 4))
              (token 'ID "i" (location "(string)" 5))
              (token 'ID "=" (location "(string)" 5))
              (token 'ID "i" (location "(string)" 5))
              (token 'ID "+" (location "(string)" 5))
              (token 'NUM "1" (location "(string)" 5))
              (token 'EOL #f (location "(string)" 5))
              (token 'ID "}" (location "(string)" 6))
              (token 'EOL #f (location "(string)" 6))
              (token 'EOF #f (location "(string)" 7))))
    (check-equal? (tokenize stone-lexicon source-code)
                  token-vector)))
