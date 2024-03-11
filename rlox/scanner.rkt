#lang racket/base

(require "exn.rkt")
(require "util.rkt")
(require racket/match)
(require racket/list)

;;; While the pattern matching is messy and unextensible, one can always have a better using FSM
;;; and stuff, but its well beyond the scope of this project. So I'll just leave it here.

;;; Also for anyone who is curious, grab the tiger book and read about lexing and parsing.

(struct token (type lexeme literal line) #:transparent)

(define (format-token t)
  (format "Token ~a (~a): ~a at line ~a"
          (token-type t)
          (token-lexeme t)
          (token-literal t)
          (token-line t)))

;; NOTE: Numbers as the first character of an identifier will be treated as a separate number token
(define (char-id? ch)
  (or (char-alphabetic? ch) (char=? ch #\_) (char-numeric? ch)))

(define (scan-tokens source)
  (define (scan-token-iter consumed-src line)
    (define (add-token type lexeme literal line rest-source)
      (cons (token type lexeme literal line)
            (scan-token-iter rest-source line)))
    (match consumed-src
      ;; EOF
      ['() (token 'eof "" "" line)]
      ;; Newline -> ignore and +1 line
      [(cons #\newline r) (scan-token-iter r (+ 1 line))]
      ;; Single character tokens
      [(cons #\( r) (add-token 'left-paren "(" "" line r)]
      [(cons #\) r) (add-token 'right-paren ")" "" line r)]
      [(cons #\{ r) (add-token 'left-brace "{" "" line r)]
      [(cons #\} r) (add-token 'right-brace "}" "" line r)]
      [(cons #\, r) (add-token 'comma "," "" line r)]
      [(cons #\. r) (add-token 'dot "." "" line r)]
      [(cons #\- r) (add-token 'minus "-" "" line r)]
      [(cons #\+ r) (add-token 'plus "+" "" line r)]
      [(cons #\; r) (add-token 'semicolon ";" "" line r)]
      [(cons #\* r) (add-token 'star "*" "" line r)]
      ;; Multiple charactor tokens with common prefix
      ;; NOTE: Order matters here
      [(cons #\! (cons #\= r)) (add-token 'bang-equal "!=" "" line r)]
      [(cons #\! r) (add-token 'bang "!" "" line r)]
      [(cons #\= (cons #\= r)) (add-token 'equal-equal "==" "" line r)]
      [(cons #\= r) (add-token 'equal "=" "" line r)]
      [(cons #\< (cons #\= r)) (add-token 'less-equal "<=" "" line r)]
      [(cons #\< r) (add-token 'less "<" "" line r)]
      [(cons #\> (cons #\= r)) (add-token 'greater-equal ">=" "" line r)]
      [(cons #\> r) (add-token 'greater ">" "" line r)]
      ; Slash or comment?
      [(cons #\/ (cons #\/ r)) (scan-token-iter
                                (drop-while (lambda (c) (not (char=? c #\newline))) r)
                                (+ 1 line))]
      [(cons #\/ r) (add-token 'slash "/" "" line r)]
      ;; Ignore whitespaces
      [(cons w r) #:when (char-whitespace? w) (scan-token-iter r line)]
      ;; String literal
      [(cons #\" r)
       (let-values ([(str rst) (seperate-while (lambda (c) (not (char=? c #\"))) r)])
         (if (null? rst)
             (raise-lox-error line "Unterminated string")
             (let ([strstr (list->string str)])
               (add-token 'string
                          (string-append "\"" strstr "\"")
                          (string-unescape strstr)
                          ; Count new lines & Remove extra quotation mark
                          (+ line (count (lambda (c) (char=? c #\newline)) str))
                          (rest rst)))))]
      ;; Number literal
      [(cons d r)
       #:when (char-numeric? d)
       ; For simplicity, numbers are treated as a sequence of digits and dots (while multiple dots are illegial)
       (let-values ([(num rst) (seperate-while (lambda (c) (or (char-numeric? c) (char=? #\. c))) (cons d r))])
         (let ([converted (string->number (list->string num))])
           (unless converted  ; string->number gives #f when the string is not a valid number
             (raise-lox-error line (format "Invalid number: ~a" (list->string num))))
           (add-token 'number (list->string num) converted line rst)))]
      ;; Identifier or keyword
      [(cons d r) 
       #:when (char-id? d)
       (let-values ([(id rst) (seperate-while char-id? (cons d r))])
         (let ([idstr (list->string id)])
           (add-token (match idstr
                        ["and" 'and]
                        ["class" 'class]
                        ["else" 'else]
                        ["false" 'false]
                        ["for" 'for]
                        ["fun" 'fun]
                        ["if" 'if]
                        ["nil" 'nil]
                        ["or" 'or]
                        ["print" 'print]
                        ["return" 'return]
                        ["super" 'super]
                        ["this" 'this]
                        ["true" 'true]
                        ["var" 'var]
                        ["while" 'while]
                        [else 'identifier])
                      idstr
                      idstr
                      line
                      rst)))]
      ;; Error handling
      [else (raise-lox-error line (format "Unexpected character: '~a'" (first consumed-src)))]))
  (scan-token-iter (string->list source) 1))

(provide (struct-out token)
         format-token
         scan-tokens)
