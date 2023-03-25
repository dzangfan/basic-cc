#lang racket

(require "../common.rkt")

(struct exn:fail:cc:parse exn:fail:cc () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse)

(struct exn:fail:cc:parse:build-grammar exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:build-grammar)

(struct exn:fail:cc:parse:unknown-variable exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:unknown-variable)

(struct exn:fail:cc:parse:badly-recursive exn:fail:cc:parse () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse:badly-recursive)

(provide (struct-out exn:fail:cc:parse)
         (struct-out exn:fail:cc:parse:build-grammar)
         (struct-out exn:fail:cc:parse:unknown-variable)
         (struct-out exn:fail:cc:parse:badly-recursive))
