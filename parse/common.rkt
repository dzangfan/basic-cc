#lang racket

(require "../common.rkt")

(struct exn:fail:cc:parse exn:fail:cc () #:transparent)

(struct exn:fail:cc:parse:build-grammar exn:fail:cc () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:build-grammar)

(provide (struct-out exn:fail:cc:parse)
         (struct-out exn:fail:cc:parse:build-grammar))
