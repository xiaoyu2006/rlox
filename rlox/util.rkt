#lang racket/base

(require racket/list)
(require racket/string)

(module+ test
  (require rackunit)
  (define-simple-check (check-string=? a b) (string=? a b))
  ;; https://stackoverflow.com/questions/41081395/unit-testing-in-racket-with-multiple-outputs/
  (define-syntax check-values-equal?
    (syntax-rules ()
      [(_ a b) (check-equal? (call-with-values (lambda () a) list)
                             (call-with-values (lambda () b) list))])))


(define (drop-while pred lst)
  (if (null? lst) '()
      (if (pred (first lst))
          (drop-while pred (rest lst))
          lst)))

(define (seperate-while pred lst)
  (define (sep-while-iter a b)
    (if (null? b)
        (values (reverse a) '())
        (if (pred (first b))
            (sep-while-iter (cons (first b) a) (rest b))
            (values (reverse a) b))))
  (sep-while-iter '() lst))

;; Variation of seperate-while where pred is able to peek at the following elements
(define (seperate-while* pred lst)
  (define (sep-while-iter a b)
    (if (null? b)
        (values (reverse a) '())
        (if (pred b)
            (sep-while-iter (cons (first b) a) (rest b))
            (values (reverse a) b))))
  (sep-while-iter '() lst))

(module+ test
  (check-equal? (drop-while even? '(2 4 6 7 8 10)) '(7 8 10))
  (check-equal? (drop-while even? '(7 9 10)) '(7 9 10))
  (check-equal? (drop-while even? '(2 4 6)) '())
  (check-equal? (drop-while even? '()) '())
  (check-values-equal? (seperate-while even? '(2 4 6 7 8 10)) (values '(2 4 6) '(7 8 10)))
  (check-values-equal? (seperate-while even? '(7 9 10)) (values '() '(7 9 10)))
  (check-values-equal? (seperate-while even? '(2 4 6)) (values '(2 4 6) '()))
  (check-values-equal? (seperate-while even? '()) (values '() '())))

;; https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
(define (string-escape str)
  (apply string-append (map (lambda (char)
                              (case char
                                [(#\u07) "\\a"]
                                [(#\u08) "\\b"]
                                [(#\u1b) "\\e"]
                                [(#\u0c) "\\f"]
                                [(#\u0a) "\\n"]
                                [(#\u0d) "\\r"]
                                [(#\u09) "\\t"]
                                [(#\u0b) "\\v"]
                                [(#\u5c) "\\\\"]
                                [(#\u27) "\\\'"]
                                [(#\u22) "\\\""]
                                [(#\u3f) "\\?"]
                                [else (string char)]))
                            (string->list str))))

(define (string-unescape str)
  ; NOTE: Code assumes all escape sequences have a length of 2
  (if (< (string-length str) 2)
      str
      (let ([prefix-removed (substring str 2)])
        (cond
          [(string-prefix? str "\\a") (string-append "\u07" (string-unescape prefix-removed))]
          [(string-prefix? str "\\b") (string-append "\u08" (string-unescape prefix-removed))]
          [(string-prefix? str "\\e") (string-append "\u1b" (string-unescape prefix-removed))]
          [(string-prefix? str "\\f") (string-append "\u0c" (string-unescape prefix-removed))]
          [(string-prefix? str "\\n") (string-append "\u0a" (string-unescape prefix-removed))]
          [(string-prefix? str "\\r") (string-append "\u0d" (string-unescape prefix-removed))]
          [(string-prefix? str "\\t") (string-append "\u09" (string-unescape prefix-removed))]
          [(string-prefix? str "\\v") (string-append "\u0b" (string-unescape prefix-removed))]
          [(string-prefix? str "\\\\") (string-append "\u5c" (string-unescape prefix-removed))]
          [(string-prefix? str "\\\'") (string-append "\u27" (string-unescape prefix-removed))]
          [(string-prefix? str "\\\"") (string-append "\u22" (string-unescape prefix-removed))]
          [(string-prefix? str "\\?") (string-append "\u3f" (string-unescape prefix-removed))]
          [else (string-append (substring str 0 1) (string-unescape (substring str 1)))]))))

(module+ test
  (check-string=? (string-escape "Hello, World!") "Hello, World!")
  (check-string=? (string-unescape "Hello, World!") "Hello, World!")
  (check-string=? (string-escape "Hello, \nWorld!") "Hello, \\nWorld!")
  (check-string=? (string-unescape "Hello, \\nWorld!") "Hello, \nWorld!")
  (check-string=? (string-escape "\\") "\\\\")
  (check-string=? (string-unescape "\\\\") "\\")
  (check-string=? (string-escape "\"") "\\\"")
  (check-string=? (string-unescape "\\\"") "\"")
  (check-string=? (string-unescape "\\") "\\"))

(provide drop-while seperate-while seperate-while* string-escape string-unescape)
