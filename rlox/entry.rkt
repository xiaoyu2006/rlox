#lang racket/base

(require racket/file)
(require "exn.rkt")
(require "scanner.rkt")

(define (run-file path)
  (let ([code (file->string path)])
    (run code)))

(define (run-prompt)
  (let loop ()
    (display "> ")
    (let ([input (read-line)])
      (unless (eof-object? input)
        (displayln (run input))
        (loop)))))

(define (run code)
  (scan-tokens code))

(provide run-file run-prompt)
