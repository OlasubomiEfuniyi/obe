#lang racket

(provide compile)
(module+ test
  (require rackunit))

(define type-mask 1)
(define type-integer 0)
(define type-bignum 1)

#|
type Expr =
| integer
|`(bignum ,integer)
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
    [`(bignum ,num) (compile-bignum num)]
    [_ (error "Invalid Program")]))

;;Compile an integer into assembly, placing the value in rax
;;Expr -> ASM
(define (compile-integer i)
  ;;Integers are tagged with a 0 as the least significant bit. This means the biggest integer that can be represented is
  ;;2^62 - 1 without using bignums
  `((mov rax ,(arithmetic-shift i 1))))

;:Compile an integer into assebmly, placing the value on the heap alongside how many bytes it takes up
;;Note: Racket allows arbitrary precision for its integer values
;;Expr -> ASM
;;
(define (compile-bignum num) ;;This needs to be changed to take an arbitrary expression
  (let* (
         (num-string (number->string num))
         (len (+ (string-length num-string) 1)) ;;The number of bytes of characters to be used since each character is one byte
         (mod (modulo len 8))
         (padding (build-string (if (zero? mod) 0 (- 8 mod)) (λ (i) #\0)))
         (num-string-padded (string-append padding num-string))
         ;;Using the padded num-string ensures that rdi remains at an 8 byte boundary
         (num-list (string->list num-string-padded)))
    `(
      ;;Save the pointer to the beginning of the list
      (mov rax rdi)
      ;;Compile the list of characters representing the number. Each character will take up one byte
      ;;The first byte will containt the length of the list of characters
      ,@(compile-char-list num-list)
      ;;Null terminate the string
      (mov rbx 0)
      (mov (offset rdi 0) rbx)
      (add rdi 8)
      ;;Tag the result as a bignum
      (or rax ,type-bignum))))

;;Compile a list of characters by placing each of them on the heap one byte at a time
;;No result is returned in rax.
;;listof(characters) -> ASM
(define (compile-char-list lst)
  (match lst
    ['() '()]
    [(cons c lst)
     ;;Place the character on the heap
    `(
      (mov rbx ,(char->integer c))
      (mov (offset rdi 0) rbx)
      ;;Advance to the next free position on the heap
      (add rdi 1)
      ;;Compile the rest of the list
      ,@(compile-char-list lst))]))

;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (compile 5) `((mov rax ,(* 2 5)) ret))
  (check-equal? (compile 9223372036854775807) `((mov rax ,(* 2 9223372036854775807)) ret))
  (check-equal? (compile -5) `((mov rax ,(* 2 -5)) ret))
  (check-equal? (compile -9223372036854775807) `((mov rax ,(* 2 -9223372036854775807)) ret)))
    