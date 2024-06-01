#lang racket/base

(require racket/list)

(require (prefix-in ast: "ast.rkt"))
(require "scanner.rkt")
(require "exn.rkt")

(define (parse ltokens)
  (define tokens (list->vector ltokens))
  (define pos 0)

  (define (previous) (vector-ref tokens (- pos 1)))
  (define (peek) (vector-ref tokens pos))

  (define (is-at-end)
    (eq? (token-type (peek)) 'eof))

  (define (check type)
    (if (is-at-end) #f
        (eq? (token-type (peek)) type)))

  (define (advance)
    (when (not (is-at-end)) (set! pos (+ pos 1)))
    (previous))

  (define (consume type err-msg)
    (if (check type)
        (advance)
        (raise-lox-error (token-line (peek)) err-msg)))

  (define (match . types)
    (let loop ([t types])
      (cond
        [(empty? t) #f]
        [(check (first t))
         (begin
           (advance)
           #t)]
        [else (loop (rest t))])))

  ;; equality := comparison ( ( '==' | '!=' ) comparison )*
  (define (equality)
    (define expr (comparison))
    (let loop ()
      (if (match 'bang-equal 'equal-equal)
          (let ()
            (define op (previous))
            (define right (comparison))
            (set! expr (ast:binary op expr right))
            (loop))
          expr)))

  ;; comparison := term ( ( '>' | '>=' | '<' | '<=' ) term )*
  (define (comparison)
    (define expr (term))
    (let loop ()
      (if (match 'greater 'greater-equal 'less 'less-equal)
          (let ()
            (define op (previous))
            (define right (term))
            (set! expr (ast:binary op expr right))
            (loop))
          expr)))

  ;; term := factor ( ( '-' | '+' ) factor )*
  (define (term)
    (define expr (factor))
    (let loop ()
      (if (match 'minus 'plus)
          (let ()
            (define op (previous))
            (define right (factor))
            (set! expr (ast:binary op expr right))
            (loop))
          expr)))

  ;; factor := unary ( ( '/' | '*' ) unary )*
  (define (factor)
    (define expr (unary))
    (let loop ()
      (if (match 'slash 'star)
          (let ()
            (define op (previous))
            (define right (unary))
            (set! expr (ast:binary op expr right))
            (loop))
          expr)))

  ;; unary := ( '!' | '-' ) unary 
  ;;        | primary
  (define (unary)
    (if (match 'bang 'minus)
        (let ()
          (define op (previous))
          (define right (unary))
          (ast:unary op right))
        (primary)))

  ;; primary := NUMBER | STRING | 'true' | 'false' | 'nil'
  ;;          | '(' expression ')'
  (define (primary)
    (cond
      [(match 'false) (ast:literal 'false)]
      [(match 'true) (ast:literal 'true)]
      [(match 'nil) (ast:literal 'null)]
      [(match 'number 'string) (ast:literal (token-literal (previous)))]
      [(match 'left-paren)
       (let ()
         (define expr (expression))
         (consume 'right-paren "Expect ')' after expression")
         (ast:grouping expr))]
      [else (error "Expect expression")]))
  
  (define (expression) (equality))

  (expression))

(provide parse)
