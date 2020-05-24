#lang racket

(provide compile)
(require "program-executor.rkt")
(module+ test
  (require rackunit))

(define type-mask 1)
(define type-integer 0)
(define type-bignum 1)

#|
type Expr =
| integer
|`(bignum ,integer)
|`(add ,integer ,integer)
|`(add-bn ,bignum ,bignum)
|#

;;Entry compile function
;;Expr -> ASM
(define (compile expr)
  `(,@(compile-e expr)
    ;;Since rsp is never moved beyond the initial stack fram setup, this sould revert the setup
    (pop rsi)
    (pop rdi)
    (pop rbx)
    (pop rbp)
    ret
    err
    (push rbp)
    (call error)
    ret))

;;The toplevel compile function from which the compilation of an expression begins
;; Expr -> Listof(ASM)
(define (compile-e expr)
  (match expr
    [(? integer? expr) (compile-integer expr)]
    [`(bignum ,num) (compile-bignum num)]
    [`(add ,e1 ,e2) (compile-add e1 e2)]
    [`(add-bn ,e1 ,e2) (compile-add-bn e1 e2)]
    [_ (error "Invalid Program")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Values;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         (padding (get-padding len))
         ;;Pad the string representation of the number with enough null characters to keep the
         ;;heap at a 64 bit boundry
         (num-string-padded (string-append num-string padding))
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
      (add rdi 1) ;;So that rdi points to the next free position on the heap, not the last character
      ;;Tag the result as a bignum
      (or rax ,type-bignum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::::::::Compile Arithmetic Operations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile the addition of two expressions
;;Expr Expr -> ASM
(define (compile-add e1 e2)
  (let ((c1 (compile-e e1))
        (c2 (compile-e e2)))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp -1) rax) ;;Save the result of evaluating the first expression on the stack
      ,@c2
      ,@assert-integer
      ;;Add the result of evaluating the first expression to the result
      ;;of evaluating the second expression
      (add rax (offset rsp -1)))))


;;Compile the addition of two big nums
;;Expr Expr->ASM
(define (compile-add-bn e1 e2)
  (let ((c1 (compile-e e1))
        (c2 (compile-e e2)))
    `(,@c1
      ,@assert-bignum
      (mov (offset rsp -1) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering
      
      ,@c2
      ,@assert-bignum
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp -2) rdi)  ;;Store a pointer to the beginning of the result on the stack
      (mov (offset rsp -3) rsi)
      (mov (offset rsp -4) rdx)
      
      ;;Call add-bignum rdi already contains a pointer to the next free position on the heap
      ;;This will be our first iargument
      (mov rdi (offset rsp -2))
      (mov rsi (offset rsp -1)) ;;First bignum
      (mov rdx rax);;Second bignum


      
      (sub rsp 32);;Make rsp point to the top of the stack
      (call addBignum)

  
      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15)
      (sub rbx rsp)
      (cmp rbx 32)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp -2))
      (mov rsi (offset rsp -3))
      (mov rdx (offset rsp -4))

      ;;Set rdi to point to the next free position on the heap
      ;;rax contains the length of the bignum string, including the null character. The addition
      ;;should keep rdi at a 64 bit boundry
      (add rdi rax)
      
      ;;Return a tagged pointer to the result on the heap
      (mov rax (offset rsp -2))
      (or rax ,type-bignum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Assertions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A variable that holds ASM for confirming that the value in rax is an integer
(define assert-integer
  `((mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-integer)
    (jne err)))

;;A variable that hods ASM for confirming that the value in rax is a bignum
(define assert-bignum
  `((mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-bignum)
    (jne err)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Helper Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;Determine how much space needs to be left/filled to keep rdi
;;at a 64bit offset
(define (get-padding len)
  (let* ((mod (modulo len 8))
        (padding (build-string (if (zero? mod) 0 (- 8 mod)) (λ (i) (integer->char 0)))))
    padding))
  
;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  ;;Test integers
  (check-equal? (execute 5) 5)
  (check-equal? (execute -5) -5)
  
  ;;Test bignum
  (check-equal? (execute `(bignum 9223372036854775807)) 9223372036854775807)
  (check-equal? (execute `(bignum -9223372036854775807)) -9223372036854775807)
  
  ;;Test add
  (check-equal? (execute `(add 5 8)) 13)
  (check-equal? (execute `(add 4611686018427387902 1)) 4611686018427387903)
  (check-equal? (execute `(add (bignum 1) 5)) 'err)
  (check-equal? (execute `(add 5 (bignum 1))) 'err)
  (check-equal? (execute `(add (bignum 1) (bignum 6))) 'err)

  ;;Test add-bn
  (check-equal? (execute `(add-bn (bignum 2) (bignum 3))) 5)
  (check-equal? (execute `(add-bn (bignum 9223372036854775807) (bignum 9223372036854775807))) 18446744073709551614)
  (check-equal? (execute `(add-bn 1 (bignum 1))) 'err)
  (check-equal? (execute `(add-bn (bignum 2) 9)) 'err)
  (check-equal? (execute `(add-bn 10 11)) 'err))
    
