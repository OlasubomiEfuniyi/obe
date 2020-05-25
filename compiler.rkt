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
|`(sub ,integer ,integer)
|`(add-bn ,bignum ,bignum)
|`(sub-bn ,bignum ,bignum)
|#

;;Entry compile function
;;Expr -> ASM
(define (compile expr)
  `(,@(compile-e expr '()) ;;Start with empty environment
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
(define (compile-e expr env)
  (match expr
    [(? integer? expr) (compile-integer expr)]
    [`(bignum ,e1) (compile-bignum e1 env)]
    [`(add ,e1 ,e2) (compile-add e1 e2 env)]
    [`(sub ,e1 ,e2) (compile-sub e1 e2 env)]
    [`(add-bn ,e1 ,e2) (compile-add-bn e1 e2 env)]
    [`(sub-bn ,e1 ,e2) (compile-sub-bn e1 e2 env)]
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
(define (compile-bignum num env)
  #|(let ((c1 (compile-e e1 env))
        (loop (gensym "loop"))
        (loop2 (gensym "loop"))
        (end (gensym "end"))
        (end2 (gensym "end")))
    `(,@c1
      ,@assert-integer ;;Only integers can be made into bignums
      ;;Save a pointer to the beginning of the bignum on the stack
      (mov ,(- (add1 (length env))) rdi)
      
      ;;Initialize the counter
      (mov rbx 0)
      
      ;;Repeatedly divide by 10 to get quotient and remainder until quotient is 0
      ,loop
      (cmp rax 0)
      (je end)
      (mov rdx 0)
      ;;Move the character representation of the remainder onto the heap
      (div 10)
      (add rdx 30)
      (mov (offset rdi 0) rdx)
      (add rdi 1)
      (add rbx 1)
      (jmp ,loop)
      ,end
      
      ;;Null terminate the string
      (mov (offset rdi 0) 0)
      (add rdi 1)
      (add rbx 1)
      
      ;;Continue to add null characters until rbx is a multiple of 8. We assume rdi was a multiple of 8 to begin with
      ,loop2
      (mov rax rbx)
      (mov rdx 0)
      (div 8)
      (cmp rdx 0) ;;No remainder means that rdi which was incremented rbx times is a multiple of 8
      (je ,end2)
      (mov (offset rdi 0) 0) ;;Pad with extra null character
      (add rdi 1)
      (add rbx 1)
      (jmp loop2)
      
      ,end2
      (mov rax ,(- (add1 (length env))))
      (or rax ,type-bignum)
      )))|#
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
(define (compile-add e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack
      ,@c2
      ,@assert-integer
      ;;Add the result of evaluating the first expression to the result
      ;;of evaluating the second expression
      (add rax (offset rsp ,(- (add1 (length env))))))))

;;Compile the subtraction of two expressions
;;Expr Expr -> ASM
(define (compile-sub e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env))))
    `(,@c1
      ,@assert-integer
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)
      
      ,@c2
      ,@assert-integer

      ;;Get the result of evaluating the first expression
      (mov rbx (offset rsp ,(- (add1 (length env)))))
      (sub rbx rax)
      (mov rax rbx)
      ;;Tag the result as an integer
      (or rax ,type-integer))))

;;Compile the addition of two big nums
;;Expr Expr -> ASM
(define (compile-add-bn e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env))))
    `(,@c1
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering
      
      ,@c2
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)  ;;Store a pointer to the beginning of the result on the stack
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rdx)
      
      ;;Call add-bignum rdi already contains a pointer to the next free position on the heap
      ;;This will be our first iargument
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (add1 (length env))))) ;;First bignum
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
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))
      (mov rdx (offset rsp ,(- (+ 4 (length env)))))

      ;;Set rdi to point to the next free position on the heap
      ;;rax contains the length of the bignum string, including the null character. The addition
      ;;should keep rdi at a 64 bit boundry
      (add rdi rax)
      
      ;;Return a tagged pointer to the result on the heap
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum))))


;;Compile the subtraction of two bignums
;;Expr Expr -> ASM
(define (compile-sub-bn e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env))))
    `(,@c1
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering
      
      ,@c2
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)  ;;Store a pointer to the beginning of the result on the stack
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rdx)
      
      ;;Call add-bignum rdi already contains a pointer to the next free position on the heap
      ;;This will be our first iargument
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (add1 (length env))))) ;;First bignum
      (mov rdx rax);;Second bignum


      
      (sub rsp 32);;Make rsp point to the top of the stack
      (call subBignum)

  
      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15)
      (sub rbx rsp)
      (cmp rbx 32)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))
      (mov rdx (offset rsp ,(- (+ 4 (length env)))))

      ;;Set rdi to point to the next free position on the heap
      ;;rax contains the length of the bignum string, including the null character. The addition
      ;;should keep rdi at a 64 bit boundry
      (add rdi rax)
      
      ;;Return a tagged pointer to the result on the heap
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
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

;;Append v to the beginning of env
;;Symbol Listof(Symbol) -> Listof(Sybmol)
(define (extend v env)
  (cons v env))
  
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
  (check-equal? (execute `(add-bn 1 (bignum 1))) 2)
  (check-equal? (execute `(add-bn 10 (bignum 1))) 11)
  (check-equal? (execute `(add-bn (bignum 2) 9)) 11)
  (check-equal? (execute `(add-bn 10 11)) 'err)

  ;;Test sub
  (check-equal? (execute `(sub 2 1)) 1)
  (check-equal? (execute `(sub 110 10)) 100)
  (check-equal? (execute `(sub (add 2 3) (add 200 300))) -495)
  (check-equal? (execute `(sub (bignum 2) 1)) 'err)
  (check-equal? (execute `(sub 2 (bignum 1))) 'err)
  (check-equal? (execute `(sub (bignum 2) (bignum 1))) 'err)
  (check-equal? (execute `(sub (add-bn (bignum 2) (bignum 3)) 1)) 'err)
  (check-equal? (execute `(sub 1 (add-bn (bignum 2) (bignum 3)))) 'err)
  (check-equal? (execute `(sub (add-bn (bignum 2) (bignum 3)) 1)) 'err)
  (check-equal? (execute `(add (sub 2 1) 9)) 10)
  
  ;;Test sub-bn
  (check-equal? (execute `(sub-bn (bignum 1237940039285380274899124224) (bignum 1208925819614629174706176))) 1236731113465765645724418048)
  (check-equal? (execute `(sub-bn (bignum 204840021458546589812482594366668142542429986589197318528619143395036962199099876801) (bignum 204840021458546589812482594366668142542429986589197318528619143395036962199099876801))) 0)
  (check-equal? (execute `(sub-bn (bignum 1237940039285380274899124224) 1)) 1237940039285380274899124223)
  (check-equal? (execute `(sub-bn 1 (bignum 1237940039285380274899124224))) 1237940039285380274899124223)
  (check-equal? (execute `(sub-bn 2 1)) 'err)
  (check-equal? (execute `(sub-bn (bignum 1237940039285380274899124224) (add 1 2))) 1237940039285380274899124221)
  (check-equal? (execute `(add-bn (sub-bn (bignum 1) (bignum 10)) 19)) 10)
  (check-equal? (execute `(sub-bn (sub 6 1) (bignum 5))) 0))
    
