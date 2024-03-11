#lang racket/base

(struct exn:lox exn:fail:user (line msg))

(define (raise-lox-error line msg)
  (raise (exn:lox "This should be reported to user" (current-continuation-marks) line msg)))

(define (format-lox-error e)
  (format "Error at line ~a: ~a" (exn:lox-line e) (exn:lox-msg e)))

(provide (struct-out exn:lox)
         raise-lox-error
         format-lox-error)
