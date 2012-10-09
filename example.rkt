#lang planet dyoo/UPPERCASE at-exp racket

(define (f x)
  (* x x))

; You should see "HI" here:
"hi"


;; The composition of languages should work with
;; tools like at-exp.
@string-append{
hi, this is a test!
Hello world!
The square of 42 is @(number->string @f[42]).
This acts like a regular language,
but with all the
string literals turned uppercase.}