#lang racket

(require "../common.rkt")
(require "../tokenization.rkt")

(struct exn:fail:cc:parse exn:fail:cc () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse)

(provide (struct-out exn:fail:cc:parse) make-exn:fail:cc:parse)

(struct token-reader ([cursor #:mutable] buffer) #:transparent)

(define (make-reader lexicon in #:file [file "(string)"])
  (token-reader 0 (tokenize lexicon in #:file file)))

(define (read-token reader #:peek? [peek? #f])
  (match-define (struct token-reader (cursor buffer)) reader)
  (define size (vector-length buffer))
  (if (>= cursor size)
      (vector-ref buffer (sub1 size))
      (begin0 (vector-ref buffer cursor)
        (unless peek?
          (set-token-reader-cursor! reader (add1 cursor))))))

(provide (struct-out token-reader) make-reader read-token)
