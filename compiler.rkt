#lang racket

(provide compile)
(require "program-executor.rkt")
(module+ test
  (require rackunit))

(define shift 3)
(define type-mask #b111)
(define type-integer #b000)
(define type-bignum #b001)
(define type-true #b010)
(define type-false #b011)
(define type-empty-list #b111)
(define type-list #b100)
(define type-pair #b101)


#|
type CEnv = Listof(Symbol)

type Expr =
|Value
|Arithmetic
|Decision
|LetBinding
|Variable
|ListOp


type ListOp =
|`(head List)
|`(tail List)

type PairOp =
|`(first Pair)
|`(second Pair)

type Value =
|Number
|Boolean
|'()
|List

type List =
|``(Expr*)

type Pair =
|(cons Expr Expr)

type Arithmetic =
|`(add ,Number ,Number)
|`(sub ,Number ,Number)
|`(add-bn (bignum ,integer) Number) | `(add-bn Number (bignum ,integer))
|`(sub-bn (bignum ,integer) Number) | `(sub-bn Number (bignum ,integer))

type Decision =
|`(if Expr Expr Expr)

type Boolean =
|`#t
|`#f

type Number =
| integer
|`(bignum ,integer)

type LetBinding =
| `(let (Binding*) Expr+)

type Binracketding =
|`(Variable Expr)

type Variable =
|Symbol

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
;; Expr CEnv -> Listof(ASM)
(define (compile-e expr env)
  (match expr
    [(? value? expr) (compile-value expr env)]
    [(? arithmetic? expr) (compile-arithmetic expr env)]
    [(? let-binding? expr) (compile-let-binding expr env)]
    [(? variable? expr) (compile-variable expr env)]
    [(? decision? expr) (compile-decision expr env)]
    [(? list-op? expr) (compile-list-op expr env)]
    [(? pair-op? expr) (compile-pair-op expr env)]
    [_ (error "Invalid Program")]))


;;Compile an value into ASM
;;Value CEnv-> ASM
(define (compile-value expr env)
  (match expr
    [(? num? expr) (compile-number expr env)]
    [(? boolean? expr) (compile-boolean expr)]
    [''() `((mov rax ,type-empty-list))]
    [`(quote ,expr) (compile-list expr env)]
    [`(cons ,e1 ,e2) (compile-pair e1 e2 env)]))

;;Compile an arithmetic expression into ASM
;;Number CEnv -> ASM
(define (compile-arithmetic expr env)
  (match expr
    [`(add ,e1 ,e2) (compile-add e1 e2 env)]
    [`(sub ,e1 ,e2) (compile-sub e1 e2 env)]
    [`(add-bn ,e1 ,e2) (compile-add-bn e1 e2 env)]
    [`(sub-bn ,e1 ,e2) (compile-sub-bn e1 e2 env)]))

;;Compile a Number value into ASM
;;Number CEnv -> ASM
(define (compile-number expr env)
  (match expr
    [(? integer? expr) (compile-integer expr)]
    [`(bignum ,e1) (compile-bignum e1 env)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::Compile LetBinding;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a let binding
;;LetBinding CEnv -> ASM
(define (compile-let-binding expr env)
  (match expr
    [`(let ((,(? symbol? xs) ,es) ...) ,exps ..1) (compile-let xs es exps env)]))

;;Compile a let binding going from left to right
(define (compile-let xs es exps env)
  (let ((ces (compile-es es env)))
    `(,@ces
      ;;xs is reversed so that the order of the variable names corresponds to
      ;;how they were placed on the stack
      ,@(compile-es exps (append (reverse xs) env)) 
      )))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Variable;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a variable
;;Variable CEnv -> ASM
(define (compile-variable v env)
  `(
    ;;Lookup the value of the variable based on its compile time position
    (mov rax (offset rsp ,(- (add1 (lookup v env)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Decision;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a decision
;;Decision CEnv -> ASM
(define (compile-decision d env)
  (match d
    [`(if ,e1 ,e2 ,e3) (compile-if e1 e2 e3 env)]))


;;Compile an if expression
;;Expr Expr Expr CEnv -> ASM
(define (compile-if e1 e2 e3 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 env))
        (c3 (compile-e e3 env))
        (true (gensym "true"))
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      (cmp rax 0) ;;Only 0 and #f is considered false. A bignum with the value of 0 will not pass this check
      (je ,false)
      (cmp rax ,type-false)
      (je ,false)
      ,true
      ,@c2
      (jmp ,end)
      ,false
      ,@c3
      ,end
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile List;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a list value into ASM. A list compiled this way is considered a different value from a list created with cons
;;List CEnv -> ASM
(define (compile-list lst env)
  `(;;Save a pointer to the beginning of the list
    (mov (offset rsp ,(- (add1 (length env)))) rdi)
    
    ;;The previous two positions on the heap have been taken by the first value of the list
    ;;and a pointer to the rest of the list. Subsequent
    ;;expressions must use the next free position
    (add rdi 16) 
    ,@(compile-list-helper lst env)
    (mov rax (offset rsp ,(- (add1 (length env)))));;Get the pointer to the beginning of the list
    (or rax ,type-list) ;;Tag the pointer
    ))

(define (compile-list-helper lst env)
  (match lst
    ['()
     `((mov rax ,type-empty-list)
       (mov rbx (offset rsp ,(- (add1 (length env)))))
       (mov (offset rbx 0) rax))]
    [(cons h lst)
     `(,@(compile-e h (extend #f env)) ;;Compile the head of the list
       ;;Get the pointer to the beginning of the list that was saved on the stack
       (mov rbx (offset rsp ,(- (add1 (length env)))))
       ;;Store the list element in the first 8 bytes that was reserved
       (mov (offset rbx  0) rax)
       ;;Store a pointer to the rest of the list in the remaining 8 bytes
       ;;that was reserved. Tag it as a list
       (mov r15 rdi)
       (or r15 ,type-list)
       (mov (offset rbx 1) r15)
       ;;Save the pointer to the beginning of the rest of the list on the stack
       (mov (offset rsp ,(- (add1 (length (extend #f env))))) rdi)
       ;;Reserve the space required to save the next element and a pointer to the rest of the list
       (add rdi 16)
       ,@(compile-list-helper lst (extend #f env)) ;;Compile the rest of the list
       )]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile ListOp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a list operation
;;Expr CEnv -> ASM
(define (compile-list-op expr env)
  (match expr
    [`(head ,lst) (compile-head lst env)]
    [`(tail ,lst) (compile-tail lst env)]))

;;Compile the head operation
(define (compile-head lst env)
  (let ((c1 (compile-e lst env)))
    `(,@c1
      ,@assert-list
      (xor rax ,type-list);;Untag the pointer
      (mov rax (offset rax 0)))))


;;Compile the tail operation
(define (compile-tail lst env)
  (let ((c1 (compile-e lst env)))
    `(,@c1
      ,@assert-list
      (xor rax ,type-list);;Untag the pointer
      (mov rax (offset rax 1)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Pair;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a cons operation
;;Expr Expr CEnv -> ASM
(define (compile-pair e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env))))
    `(,@c1
      (mov (offset rsp ,(- (add1 (length env)))) rax);;Save the result of evaluating the first expression on the stack
      ,@c2
      (mov rbx (offset rsp ,(- (add1 (length env)))))
      (mov (offset rdi 0) rbx) ;;Move the first value on the heap
      (mov (offset rdi 1) rax) ;;Move the second value on the heap
      (mov rax rdi) ;;Save a pointer to the beginning of the pair
      (or rax ,type-pair);;Tag the pointer
      (add rdi 16)))) ;;Make rdi contain the address of the next free location on the heap



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:Compile Pair-op;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a pair operation
;;Pair-Op CEnv -> ASM
(define (compile-pair-op expr env)
  (match expr
    [`(first ,expr) (compile-first expr env)]
    [`(second ,expr) (compile-second expr env)]))

;;Compile first operation on a pair
;;Expr CEnv -> ASM
(define (compile-first expr env)
  `(,@(compile-e expr env)
    ,@assert-pair
    ;;Untag the address
    (xor rax ,type-pair)
    (mov rax (offset rax 0))))

;;Compile second operation on a pair
;;Expr CEnv -> ASM
(define (compile-second expr env)
  `(,@(compile-e expr env)
    ,@assert-pair
    ;;Untag the address
    (xor rax ,type-pair)
    (mov rax (offset rax 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:Compile Boolean;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a boolean into assembly, placing the value in rax
;;Boolean -> ASM
(define (compile-boolean b)
  `((mov rax ,(if b type-true type-false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Number;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile an integer into assembly, placing the value in rax
;;Expr -> ASM
(define (compile-integer i)
  ;;Integers are tagged with a 0 as the least significant bit. This means the biggest integer that can be represented is
  ;;2^62 - 1 without using bignums
  `((mov rax ,(arithmetic-shift i shift))))

;:Compile an integer into assebmly, placing the value on the heap alongside how many bytes it takes up
;;Note: Racket allows arbitrary precision for its integer values
;;Expr CEnv -> ASM
;;
(define (compile-bignum num env)
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
;;Expr Expr CEnv -> ASM
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
;;Expr Expr CEnv -> ASM
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
;;Expr Expr CEnv-> ASM
(define (compile-add-bn e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 4 (length env)))))
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


      
      (sub rsp ,stack-size);;Make rsp point to the top of the stack
      (call addBignum)

  
      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15)
      (sub rbx rsp)
      (cmp rbx ,stack-size)
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
;;Expr Expr CEnv-> ASM
(define (compile-sub-bn e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 4 (length env)))))
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


      
      (sub rsp ,stack-size);;Make rsp point to the top of the stack
      (call subBignum)

  
      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15)
      (sub rbx rsp)
      (cmp rbx ,stack-size)
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

;;A variable that holds ASM for confirming that the value in rax is a list
(define assert-list
  `((mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-list)
    (jne err)))

;;A variable that holds ASM for confirming that the value in rax is a pair
(define assert-pair
  `((mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-pair)
    (jne err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Helper Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a list of expressions
;;Listof(Expr) CEnv -> ASM
(define (compile-es es env)
  (match es
    ['() '()]
    [(cons e es)
     `(,@(compile-e e env)
       (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the value on the top of the stack 
       ,@(compile-es es (extend #f env))
       )]))

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

;;Find the index of the first occurence of v in env
;;Variable CEnv -> number
(define (lookup v env)
  (let ((t (member v env)))
    (if t
        (sub1 (length t))
        (error (string-append "Variable " (symbol->string v) "not in scope")))))

                      

;;Determine if the expression is a number
;;Expr -> boolean
(define (num? expr)
  (match expr
    [(? integer?) #t]
    [`(bignum ,n) #t]
    [_ #f]))

;;Determine if the expression is an arithmetic operation
;;Expr -> boolean
(define (arithmetic? expr)
  (match expr
    [(list (or 'add 'sub 'add-bn 'sub-bn) e1 e2) #t]
    [_ #f]))

;;Determine if the expression is a let binding
;;Expr -> boolean
(define (let-binding? expr)
  (match expr
    [`(let ((,(? symbol?) ,r1) ...) ,e ..1) #t]
    [_ #f]))

;;Determine if the expression is a variable
;;Expr -> boolean
(define (variable? expr)
  (symbol? expr))

;;Determine if the expression is a decision
;;Expr -> boolean
(define (decision? expr)
  (match expr
    [`(if ,a ,b ,c) #t]
    [_ #f]))

;;Determine if the expression is a value
;;Expr -> boolean
(define (value? expr)
  (or (num? expr) (boolean? expr) (equal? expr '())
      (match expr
        [`(quote ,expr ...) #t]
        [`(cons ,e1 ,e2) #t]
        [_ #f])))

;;Determine if the expression is a list-op
;;Expr -> boolean
(define (list-op? expr)
  (match expr
    [`(head ,lst) #t]
    [`(tail ,lst) #t]
    [_ #f]))

;;Determine if the expression is a pair-op
;;Expr -> boolean
(define (pair-op? expr)
  (match expr
    [`(first ,pair) #t]
    [`(second ,pair) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  #|;;Test integers
  (check-equal? (execute 5) 5)
  (check-equal? (execute -5) -5)
  
  ;;Test bignum
  (check-equal? (execute `(bignum 9223372036854775807)) 9223372036854775807)
  (check-equal? (execute `(bignum -9223372036854775807)) -9223372036854775807)
  
  ;;Test add
  (check-equal? (execute `(add 5 8)) 13)
  (check-equal? (execute `(add 1152921504606846974 1)) 1152921504606846975)
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
  (check-equal? (execute `(sub-bn 1 (bignum 1237940039285380274899124224))) -1237940039285380274899124223)
  (check-equal? (execute `(sub-bn 2 1)) 'err)
  (check-equal? (execute `(sub-bn (bignum 1237940039285380274899124224) (add 1 2))) 1237940039285380274899124221)
  (check-equal? (execute `(add-bn (sub-bn (bignum 1) (bignum 10)) 19)) 10)
  (check-equal? (execute `(sub-bn (sub 6 1) (bignum 5))) 0)

  ;;Test let
  (check-equal? (execute `(let () 5)) 5)
  (check-equal? (execute `(let ((x 5)) x)) 5)
  (check-equal? (execute `(let ((a 1) (b (bignum 2))) b)) 2)
  (check-equal? (execute `(let ((a 1) (b (bignum 2))) (add-bn a b))) 3)
  (check-equal? (execute `(let () (add-bn 5 5))) 'err)
  (check-equal? (execute `(let ((x (add-bn 5 5))) 5)) 'err)
  (check-equal? (execute `(let ((x 10)) (let ((x (let ((x 4) (y 6)) (sub x y)))) x))) -2)
  (check-equal? (execute `(let ((x 10)) (let ((x (let ((x 4) (y 6)) (sub x y)))) (add x 2)))) 0)
  (check-equal? (execute `(let ((x 10)) (let ((x (let ((x 4) (y 6)) (sub x y)))) (add-bn x (bignum 2))))) 0)
  (check-equal? (execute `(let ((var1 6)  (var2 7) (var3 (bignum 8))) (add-bn 3 (bignum 8)))) 11)
  (check-equal? (execute `(let ((var1 6) (var2 7) (var3 (bignum 8))) (add var1 var2) (sub-bn (add-bn var2 var3) 5))) 10)
  (check-equal? (execute `(let ((var1 6) (var2 7) (var3 (bignum 8))) (add var1 var2) (sub (add-bn var2 var3) 5))) 'err)
  (check-equal? (execute `(let ((var1 6) (var2 7) (var3 (bignum 8))) (add-bn var1 var2) (sub-bn (add-bn var2 var3) 5))) 'err)
  (check-equal? (execute `(let ((length (add 2 3)) (breadth (sub-bn (add-bn 1 (bignum 10)) (sub-bn (bignum 8) 2))) (height (sub 6 1))) (add-bn (add-bn length breadth) height)
                            (add length length) (sub-bn length breadth) (let ((f-dim (add-bn length breadth))) length f-dim))) 10)

  ;;Test booleans
  (check-equal? (execute `#t) #t)
  (check-equal? (execute `#f) #f)
  
  ;;Test if expression
  (check-equal? (execute `(if (add 1 2) (add 1 3) (sub 1 2))) 4)
  (check-equal? (execute `(if (add -1 1) (add 1 3) (sub 1 2))) -1)
  (check-equal? (execute `(if #t 5 6)) 5)
  (check-equal? (execute `(if #f 5 6)) 6)
  (check-equal? (execute `(if 5 #t #f)) #t)
  (check-equal? (execute `(if 0 #t #f)) #f)
  (check-equal? (execute `(if (bignum 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890) (add 1 3) (sub 1 2))) 4)
  (check-equal? (execute `(if (sub-bn (bignum 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890) (bignum 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890)) (add 1 3) (sub 1 2))) 4)
  (check-equal? (execute `(if (let ((var1 1) (var2 (let ((var1 2) (var2 3)) (add var1 var2)))) (sub var2 var1)) 1 2)) 1)
  (check-equal? (execute `(if 0 1 (let ((var1 1) (var2 (let ((var1 2) (var2 3)) (add var1 var2)))) (sub var2 var1)))) 4)
  (check-equal? (execute `(let ((x (if 1 2 3)) (y (if (add 0 1) 5 6))) (if (sub x y) x y))) 2)

  ;;Test list value
  (check-equal? (execute ''(1 2 3)) '(1 2 3))
  (check-equal? (execute ''(#t #f #t #f)) '(#t #f #t #f))
  (check-equal? (execute ''((add 2 3) (sub 1 2))) '(5 -1))
  (check-equal? (execute ''((bignum 12345678901234567890) (if #t 1 2))) '(12345678901234567890 1))
  (check-equal? (execute ''('(#t #f) 1 2 '(3 4))) '((#t #f) 1 2 (3 4)))
  (check-equal? (execute ''('('(1) '(2)))) '(((1) (2))))
  
  ;;Test head
  (check-equal? (execute '(head '(1 2 3))) 1)
  (check-equal? (execute '(head '())) 'err)
  (check-equal? (execute '(head (head '('(1 5) 2 3)))) 1)
  (check-equal? (execute '(head '('('(1) '(2))))) '((1) (2)))
  (check-equal? (execute '(head (if #t '((bignum 1) (add 1 2)) #f))) 1)
  
  ;;Test Tail
  (check-equal? (execute '(tail '(1 2 3))) '(2 3))
  (check-equal? (execute '(tail '())) 'err)
  (check-equal? (execute '(tail (tail '('(1 5) 2 3)))) '(3))
  (check-equal? (execute '(tail '('('(1) '(2))))) '())
  (check-equal? (execute '(tail (if #t '((bignum 1) (add 1 2)) #f))) '(3))|#

  ;;Test Pair
  (check-equal? (execute '(cons #t #f)) '(#t . #f))
  (check-equal? (execute '(cons #t (cons #f #t))) '(#t #f . #t))
  (check-equal? (execute '(cons #t (cons #f '(1)))) '(#t #f . '(1))) ;;Mixing a list and a pair does not morph into a list
  (check-equal? (execute '(cons 1 (cons #t (cons 3 (cons #f '()))))) '(1 #t 3 #f . '())) ;;The empty list is just another value in the pair
                


  ;;Test Pair operations
  (check-equal? (execute '(first (cons #t #f))) #t)
  (check-equal? (execute '(second (cons #t #f))) #f)

  (let ((lst '(cons 2 (cons 1 '()))))
    (check-equal? (execute `(first ,lst)) 2)
    (check-equal? (execute `(first (second ,lst))) 1)
    (check-equal? (execute `(second (second ,lst))) '())
    (check-equal? (execute `(second (second (second ,lst)))) 'err))

  (let ((pair '(cons (add 1 2) (cons (add 1 1) (sub 1 0)))))
    (check-equal? (execute `(first ,pair)) 3)
    (check-equal? (execute `(first (first ,pair))) 'err)
    (check-equal? (execute `(first (second ,pair))) '2)
    (check-equal? (execute `(first (second (second ,pair)))) 'err))

  (let ((lp '(cons #t '(1 2 3))))
    (check-equal? (execute `(first ,lp)) #t)
    (check-equal? (execute `(second ,lp)) '(1 2 3))
    (check-equal? (execute `(first (second ,lp))) 'err) ;;List and pairs are different types of values with different operations
    (check-equal? (execute `(head (second ,lp))) 1)
    (check-equal? (execute `(tail (second ,lp))) '(2 3))))
  

