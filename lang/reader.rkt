#lang racket/base

;; A little joke to see how to use make-meta-reader to create nested languages.
;; This one will make your programs look like they're shouting: all string
;; literals will be uppercased.
(require syntax/module-reader)

(provide (rename-out [UPPERCASE-read read]
                     [UPPERCASE-read-syntax read-syntax]
                     [UPPERCASE-get-info get-info]))

(define (wrap-read p)
  (lambda args
    (define datum (apply p args))
    datum))

(define (wrap-read-syntax p)
  (lambda args
    (define stx (apply p args))
    (walk stx)))

;; walk: syntax -> syntax
;; Walk through the syntax object, replacing the string literals with their UPPERCASE.
(define (walk stx)
  (printf "Walking on ~s\n" stx)
  (syntax-case stx ()
    [id
     (identifier? #'stx)
     stx]
    [(pattern ...)
     (with-syntax ([(transformed-pattern ...)
                    (map walk (syntax->list #'(pattern ...)))])
       (syntax/loc stx
         (transformed-pattern ...)))]
    [(pattern ... . tail)
     (with-syntax ([(transformed-pattern ...)
                    (map walk (syntax->list #'(pattern ...)))]
                   [transformed-tail (walk #'tail)])
       (syntax/loc stx
         (transformed-pattern ... . transformed-tail)))]
    [#(pattern ...)
     (with-syntax ([(transformed-pattern ...)
                    (map walk (syntax->list #'(pattern ...)))])
       (syntax/loc stx
         #(transformed-pattern ...)))]
    [#s(key-datum pattern ...)
     (with-syntax ([transformed-pattern
                    (map walk (syntax->list #'(pattern ...)))])
       (syntax/loc stx
         #s(key-datum transformed-pattern)))]
    [const
     (begin
       (printf "I'm here with ~s\n" #'const)
       (cond
         [(string? (syntax-e #'const))
          (datum->syntax #'const (string-upcase (syntax-e #'const)) #'const)]
         [else
          #'const]))]))





(define-values (UPPERCASE-read UPPERCASE-read-syntax UPPERCASE-get-info)
  (make-meta-reader
   'UPPERCASE
   "language path"
   (lambda (bstr)
     (let* ([str (bytes->string/latin-1 bstr)]
            [sym (string->symbol str)])
       (and (module-path? sym)
            (vector
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader"))))))
   wrap-read
   wrap-read-syntax
   (lambda (proc)
     (lambda (key defval)
       (if proc (proc key defval) defval)))))