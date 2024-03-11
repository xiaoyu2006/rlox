#lang racket/base

(module+ main
  (require racket/match)
  (require "rlox/entry.rkt")
  (require "rlox/exn.rkt")

  (define version "0.1.0")

  (define (show-help) (displayln "Usage: rlox [lox source]"))
  (define (show-version) (displayln (format "rlox ~a" version)))

  (define (main)
    (define args (current-command-line-arguments))
    (match args
      [(vector "-h") (show-help)]
      [(vector "-v") (show-version)]
      [(vector file) (run-file file)]
      [(vector) (run-prompt)]
      [_ (show-help)]))

  (with-handlers ([exn:lox?
                   (lambda (e)
                     (displayln (format-lox-error e))
                     (exit 65))])
    (main)))
