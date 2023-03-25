#lang racket

(require "../common.rkt")

(struct exn:fail:cc:parse exn:fail:cc () #:transparent
  #:extra-constructor-name make-exn:fail:cc:parse)

(provide (struct-out exn:fail:cc:parse))
