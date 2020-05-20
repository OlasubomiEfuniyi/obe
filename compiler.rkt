#lang racket

(provide compile)
(module+ test
  (require rackunit))

#|
type Expr =
| integer
|#

;;Entry compile function
;;Expr -> ASM
(define (compile expr)
  `(
    ,@(compile-e expr)
    ret))

;;The toplevel compile function from which the compilation of an expression begins
;; Expr -> Listof(ASM)
(define (compile-e expr)
  (match expr
    [(? integer? expr) (compile-integer expr)]
    [_ (error "Invalid Program")]))

;;Compile an integer into a assembly, placing the value in rax
;;Expr -> ASM
(define (compile-integer i)
  `((mov rax ,i)))


;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (compile 5) '((mov rax 5)))
  (check-equal? (compile 9223372036854775807) '((mov rax 9223372036854775807)))
  (check-equal? (compile -5) '((mov rax -5)))
  (check-equal? (compile -9223372036854775807) '((mov rax -9223372036854775807))))
    