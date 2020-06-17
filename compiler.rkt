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
|Logic
|Arithmetic
|Decision
|LetBinding
|Variable
|ListOp
|PairOp

type Logic =
|`(>  Expr Expr) where both arguments evaluate to a Number
|`(< Number Number) where both arguments evaluate to a Number
|`(>= Number Number) where both arguments evaluate to a Number
|`(<= Number Number) where both arguments evaluate to a Number
|`(= Expr Expr) ;;Structural equality
|`(and Expr Expr) where both arguments evaluate to a Boolean
|`(or Expr Expr) where both arguments evaluate to a Boolean

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
|`(add integer integer)
|`(sub integer integer)

type Decision =
|`(if Expr Expr Expr)

type Boolean =
|`#t
|`#f

type Number =
| integer

type LetBinding =
| `(let (Binding*) Expr+)

type Binding =
|`(Variable Expr)
|`(Variable Variable Pair)

type Variable =
|Symbol

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Desugar;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Change a list literal to a sequence of cons ending with the empty list. E.g '(1 2) <=> (cons 1 (cons 2 '()))
;;Expr -> Expr
(define (desugar expr)
  (match expr
    [(? value? expr) (desugar-value expr)]
    [(? arithmetic? expr) (desugar-arithmetic expr)]
    [(? let-binding? expr) (desugar-let-binding expr)]
    [(? variable? expr) expr]
    [(? decision? expr) (desugar-decision expr)]
    [(? list-op? expr) (desugar-list-op expr)]
    [(? pair-op? expr) (desugar-pair-op expr)]
    [(? logic? expr) (desugar-logic expr)]
    [_ (error "Invalid Program")]))

;;Desugar a value
;;Value -> Value
(define (desugar-value expr)
  (match expr
    [(? num? n) n]
    [(? boolean? b) b]
    [''() ''()]
    [`(quote ,lst) (list-literal-to-cons `,lst)]
    [`(cons ,e1 ,e2) `(cons ,(desugar e1) ,(desugar e2))]))

;;Desugar an arithmetic operation
;;Arithmetic -> Arithmetic
(define (desugar-arithmetic expr)
   (match expr
    [`(add ,e1 ,e2) `(add ,(desugar e1) ,(desugar e2))]
    [`(sub ,e1 ,e2) `(sub ,(desugar e1) ,(desugar e2))]))

;;Desugar a let binding
;;LetBinding -> LetBinding
(define (desugar-let-binding expr)
  (match expr
    [`(let (,(? binding? bs) ...) ,exps ..1) `(let (,@(map (match-lambda 
                                                             [`(,(? symbol? s) ,r1) `(,s ,(desugar r1))]
                                                             [`(,(? symbol? s1) ,(? symbol? s2) ,r1) `(,s1 ,s2 ,(desugar r1))]
                                                             [_ (error "Illegal binding")]) bs)) ,@(map desugar exps))]))
;;Desugar a pair operation
;;PairOp- -> PairOp
(define (desugar-pair-op expr)
  (match expr
    [`(first ,expr) `(first ,(desugar expr))]
    [`(second ,expr) `(second ,(desugar expr))]))

;;Desugar a list operation
;;ListOp -> ListOp
(define (desugar-list-op expr)
  (match expr
    [`(head ,lst) `(head ,(desugar lst))]
    [`(tail ,lst) `(tail ,(desugar lst))]))

;;Desugar a decision
;;Decision -> Decision
(define (desugar-decision expr)
  (match expr
    [`(if ,e1 ,e2 ,e3) `(if ,(desugar e1) ,(desugar e2) ,(desugar e3))]))

;;Desugar a logical expression
;;Logic -> Logic
(define (desugar-logic expr)
  (match expr
    [`(> ,e1 ,e2) `(> ,(desugar e1) ,(desugar e2))]
    [`(< ,e1 ,e2) `(< ,(desugar e1) ,(desugar e2))]
    [`(>= ,e1 ,e2) `(>= ,(desugar e1) ,(desugar e2))]
    [`(<= ,e1 ,e2) `(<= ,(desugar e1) ,(desugar e2))]
    [`(= ,e1 ,e2) `(= ,(desugar e1) ,(desugar e2))]))

(define (list-literal-to-cons lst)
  (match lst
    ['() ''()]
    [(cons h t) `(cons ,(desugar h) ,(list-literal-to-cons t))]
    [_ (display lst)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Top level compile;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Entry compile function
;;Expr -> ASM
(define (compile expr)
  `(,@(compile-e (desugar expr) '()) ;;Start with empty environment
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
    [(? logic? expr) (compile-logic expr env)]
    [_ (error "Invalid Program")]))


;;Compile an value into ASM
;;Value CEnv-> ASM
(define (compile-value expr env)
  (match expr
    [(? num? expr) (compile-number expr env)]
    [(? boolean? expr) (compile-boolean expr)]
    [''() `((mov rax ,type-empty-list))]
    [`(cons ,e1 ,e2) (compile-pair e1 e2 env)]))

;;Compile an arithmetic expression into ASM
;;Number CEnv -> ASM
(define (compile-arithmetic expr env)
  (match expr
    [`(add ,e1 ,e2) (compile-add e1 e2 env)]
    [`(sub ,e1 ,e2) (compile-sub e1 e2 env)]))

;;Compile a Number value into ASM
;;Number CEnv -> ASM
(define (compile-number expr env)
  (match expr
    ;;Every integer is treated as a bignum
    [(? integer? num) (compile-bignum num env)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::Compile LetBinding;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a let binding
;;LetBinding CEnv -> ASM
(define (compile-let-binding expr env)
  (match expr
    [`(let (,(? binding? bs) ...) ,exps ..1) (compile-let bs exps env '())]))


;;Compile a let binding going from left to right
;;ListOf(Bindings) ListOf(Expr) CEnv -> ASM

(define (compile-let bs exps env new-env)
  (match bs
    ;;Dealing with a simple binding of a value to a variable name
    [(cons `(,(? symbol? x) ,expr) '()) ;;When we reach the last binding, no need to reserve space on the stack
     `(;;Compile the head of the list of bindings. We do not want the expression
       ;;to have a notion of the bindings that have been compiled as part of the
       ;;current let, yet we want it to take the space they will fill up
       ;;into account. So, we append enough #f values to block off the
       ;;space required. This is also done in the similar cases below.
       ,@(compile-e expr (append (make-list (length new-env) #f) env)) 
       (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rax) ;;Save the result on the stack
       ,@(compile-let '() exps env (cons x new-env)) ;;Compile the rest of the list of bindings
       )]
    [(cons `(,(? symbol? x) ,expr) rest)
     `(,@(compile-e expr (append (make-list (length new-env) #f) env)) ;;Compile the head of the list of bindings
       (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rax) ;;Save the result on the stack
       ,@(compile-let rest exps env (cons x new-env)) ;;Compile the rest of the list of bindings
       )]
    [(cons `(,(? symbol? x1) ,(? symbol? x2) ,expr) '())
     (let ((untag-pair (gensym "untag"))
           (continue (gensym "continue")))
     `(,@(compile-e expr (append (make-list (length new-env) #f) env))
      ,@(assert-pair-list) ;;This binding syntax is only valid for pairs
      (mov rbx rax)
      (and rbx ,type-mask)
      (cmp rbx ,type-pair)
      (je ,untag-pair)
      ;;If we reached here, assert-pair-list passed, so since it is not a pair, it must be a list
      (xor rax ,type-list)
      (jmp ,continue)
      ,untag-pair
      (xor rax ,type-pair) ;;Untag the value
      ,continue
      (mov rbx (offset rax 0))
      (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rbx) ;;Place the head value on the stack first
      (mov rbx (offset rax 1))
      (mov (offset rsp ,(- (+ 2 (length env) (length new-env)))) rbx) ;;Place the tail value on the stack
      ,@(compile-let '() exps env (cons x2 (cons x1 new-env)))))] ;;Compile the rest of the list of bindings
    [(cons `(,(? symbol? x1) ,(? symbol? x2) ,expr) rest)
     (let ((untag-pair (gensym "untagPair"))
           (continue (gensym "continue")))
     `(,@(compile-e expr (append (make-list (length new-env) #f) env))
      ,@(assert-pair-list) ;;This binding syntax is only valid for pairs
      (mov rbx rax)
      (and rbx ,type-mask)
      (cmp rbx ,type-pair)
      (je ,untag-pair)
      (xor rax ,type-list)
      (jmp ,continue)
      ,untag-pair
      (xor rax ,type-pair) ;;Untag the value
      ,continue
      (mov rbx (offset rax 0))
      (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rbx) ;;Place the head value on the stack first
      (mov rbx (offset rax 1))
      (mov (offset rsp ,(- (+ 2 (length env) (length new-env)))) rbx) ;;Place the tail value on the stack
      ,@(compile-let rest exps env (cons x2 (cons x1 new-env)))))] ;;Compile the rest of the list of bindings
    ['()
     `(,@(compile-es exps (append new-env env)))] ;;Compile each expression that makes up the body of the let expression under the environment created by the bindings
     ))

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
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      ;;Only  #f is considered false. A bignum with the value of 0 will not pass this check
      (cmp rax ,type-false)
      (je ,false)
      ,@c2
      (jmp ,end)
      ,false
      ,@c3
      ,end
      )))

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
        (c2 (compile-e e2 (extend #f env)))
        (list (gensym "list"))
        (end (gensym "end")))
    `(,@c1
      (mov (offset rsp ,(- (add1 (length env)))) rax);;Save the result of evaluating the first expression on the stack
      ,@c2
      (mov rbx (offset rsp ,(- (add1 (length env)))))
      (mov (offset rdi 0) rbx) ;;Move the first value on the heap
      (mov (offset rdi 1) rax) ;;Move the second value on the heap
      (mov rbx rax) ;;Save the second value for use later. Make sure it is not overwritten before it is used
      (mov rax rdi) ;;Save a pointer to the beginning of the pair
      ;;Determine if this is a list or just a pair. It is a list of the second operand is a list or the empty list
      (cmp rbx ,type-empty-list)
      (je ,list)
      (and rbx ,type-mask)
      (cmp rbx ,type-list)
      (je ,list)
      (or rax ,type-pair);;Tag the pointer as a pair
      (jmp ,end)
      ,list
      (or rax ,type-list);;Tag the pointer as a list
      ,end
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
  (let ((untag-pair (gensym "untag"))
        (continue (gensym "continue")))
  `(,@(compile-e expr env)
    ,@(assert-pair-list)
    ;;Untag the address
    (mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-pair)
    (je ,untag-pair)
    (xor rax ,type-list)
    (jmp ,continue)
    ,untag-pair
    (xor rax ,type-pair)
    ,continue
    (mov rax (offset rax 0)))))

;;Compile second operation on a pair
;;Expr CEnv -> ASM
(define (compile-second expr env)
  (let ((untag-pair (gensym "untag"))
        (continue (gensym "continue")))
  `(,@(compile-e expr env)
    ,@(assert-pair-list)
    ;;Untag the address
    (mov rbx rax)
    (and rbx ,type-mask)
    (cmp rbx ,type-pair)
    (je ,untag-pair)
    (xor rax ,type-list)
    (jmp ,continue)
    ,untag-pair
    (xor rax ,type-pair)
    ,continue
    (mov rax (offset rax 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:Compile Boolean;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a boolean into assembly, placing the value in rax
;;Boolean -> ASM
(define (compile-boolean b)
  `((mov rax ,(if b type-true type-false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Number;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile a logical operation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a logical operation
;;Expr CEnv -> ASM
(define (compile-logic expr env)
  (match expr
    [`(> ,e1 ,e2) (compile-greater e1 e2 env)]
    [`(< ,e1 ,e2) (compile-less e1 e2 env)]
    [`(>= ,e1 ,e2) (compile-greater-equal e1 e2 env)]
    [`(<= ,e1 ,e2) (compile-less-equal e1 e2 env)]
    [`(= ,e1 ,e2) (compile-equal e1 e2 env)]))

;;Compile the greater than logical operation
;;Expr Expr CEnv -> ASM
(define (compile-greater e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 3 (length env))))
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      ,@assert-bignum
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Call compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi rax) ;;Pass the second argument

      (sub rsp ,stack-size)
      (call compBignum)

      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15) ;;Get the previous base of the stack
      (sub rbx rsp) ;;Subtract it from the top of the stack. This should give us the stack size if everything went well
      (cmp rbx ,stack-size)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (cmp rax rbx)
      (jle ,false)
      (mov rax ,type-true)
      (jmp ,end)
      ,false
      (mov rax ,type-false)
      ,end)))

;;Compile the less than logical operation
;;Expr Expr CEnv -> ASM
(define (compile-less e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 3 (length env))))
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      ,@assert-bignum
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Call compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi rax) ;;Pass the second argument

      (sub rsp ,stack-size)
      (call compBignum)

      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15) ;;Get the previous base of the stack
      (sub rbx rsp) ;;Subtract it from the top of the stack. This should give us the stack size if everything went well
      (cmp rbx ,stack-size)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (cmp rax rbx)
      (jge ,false)
      (mov rax ,type-true)
      (jmp ,end)
      ,false
      (mov rax ,type-false)
      ,end)))

;;Compile the greater than or equal logical operation
;;Expr  Expr CEnv -> ASM
(define (compile-greater-equal e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 3 (length env))))
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      ,@assert-bignum
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Call compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi rax) ;;Pass the second argument

      (sub rsp ,stack-size)
      (call compBignum)

      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15) ;;Get the previous base of the stack
      (sub rbx rsp) ;;Subtract it from the top of the stack. This should give us the stack size if everything went well
      (cmp rbx ,stack-size)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (cmp rax rbx)
      (jl ,false)
      (mov rax ,type-true)
      (jmp ,end)
      ,false
      (mov rax ,type-false)
      ,end)))

;;Compile the less than or equal logical operation
;;Expr Expr CEnv -> ASM
(define (compile-less-equal e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 3 (length env))))
        (false (gensym "false"))
        (end (gensym "end")))
    `(,@c1
      ,@assert-bignum
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Call compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi rax) ;;Pass the second argument

      (sub rsp ,stack-size)
      (call compBignum)

      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15) ;;Get the previous base of the stack
      (sub rbx rsp) ;;Subtract it from the top of the stack. This should give us the stack size if everything went well
      (cmp rbx ,stack-size)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (cmp rax rbx)
      (jg ,false)
      (mov rax ,type-true)
      (jmp ,end)
      ,false
      (mov rax ,type-false)
      ,end)))

;;Compile the equal logical operation
;;Expr Expr CEnv -> ASM
(define (compile-equal e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 3 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (bignum (gensym "bignum"))
        (list (gensym "list"))
        (pair (gensym "pair"))
        (simple (gensym "simple"))
        (empty-list (gensym "empty"))
        (continue (gensym "continue")))
    `(,@c1
  
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2

      (mov r10 (offset rsp ,(- (add1 (length env)))))
      (mov r11 rax)

      (and r10 ,type-mask)
      (and r11 ,type-mask)
      (cmp r10 r11)
      (jne ,false)
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Call compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi rax) ;;Pass the second argument

      (sub rsp ,stack-size)

      (cmp r10 ,type-bignum)
      (je ,bignum)
      (cmp r10 ,type-list)
      (je ,list)
      (cmp r10 ,type-pair)
      (je ,pair)
      (cmp r10 ,type-false)
      (je ,simple)
      (cmp r10 ,type-true)
      (je ,simple)
      (cmp r10 ,type-empty-list)
      (je ,simple)
      
      ,bignum
      (call compBignum)
      (jmp ,continue)
      
      ,list
      (call listEqual)
      (jmp ,continue)
      
      ,pair
      (call pairEqual)
      (jmp ,continue)
      
      ,simple
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (mov rbx (offset rsp ,(- (+ 2 (length env)))))
      (cmp rax rbx)
      (jne ,false)
      (mov rax ,type-true)
      (jmp ,end)
      
      ,continue
      ;;Make sure the stack is as expected and then restore the stack
      ;;stack pointer to where it was before the setup for the function
      ;;call
      (mov rbx r15) ;;Get the previous base of the stack
      (sub rbx rsp) ;;Subtract it from the top of the stack. This should give us the stack size if everything went well
      (cmp rbx ,stack-size)
      (jne err)
      (mov rsp r15)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 2 (length env)))))
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (cmp rax rbx)
      (jne ,false)
      (mov rax ,type-true)
      (jmp ,end)
      ,false
      (mov rax ,type-false)
      ,end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::::::::Compile Arithmetic Operations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile the addition of two big nums
;;Expr Expr CEnv-> ASM
(define (compile-add e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 4 (length env)))))
    `(,@c1
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering
      
      ,@c2
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi) 
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
(define (compile-sub e1 e2 env)
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

;;A variable that holds ASM for confirming that the value in rax is a pair or a list
(define (assert-pair-list)
  (let ((end (gensym "end")))
    `((mov rbx rax)
      (and rbx ,type-mask)
      (cmp rbx ,type-pair)
      (je ,end)
      (cmp rbx ,type-list)
      (jne err)
      ,end)))

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
        (error (string-append "Variable " (symbol->string v) " not in scope")))))

                      

;;Determine if the expression is a number
;;Expr -> boolean
(define (num? expr)
  (match expr
    [(? integer?) #t]
    [_ #f]))

;;Determine if the expression is an arithmetic operation
;;Expr -> boolean
(define (arithmetic? expr)
  (match expr
    [(list (or 'add 'sub) e1 e2) #t]
    [_ #f]))

;;Determine if the expression is a let binding
;;Expr -> boolean
(define (let-binding? expr)
  (match expr
    [`(let (,(? binding?) ...) ,e ..1) #t]
    [_ #f]))

;;Determine if a subexpression is a binding
(define (binding? expr)
  (match expr
    [`(,(? symbol?) ,r1) #t]
    [`(,(? symbol?) ,(? symbol?) ,r1) #t]
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

;;Determine if the expression is a logical expression
;;Expr -> boolean
(define (logic? expr)
  (match expr
    [(or `(> ,e1 ,e2)`(< ,e1 ,e2) `(>= ,e1 ,e2) `(<= ,e1 ,e2) `(= ,e1 ,e2)) #t]
    [_ #f]))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  ;;Test integers
  (check-equal? (execute (compile 5)) 5)
  (check-equal? (execute (compile -5)) -5)
  
  ;;Test bignum
  (check-equal? (execute (compile 9223372036854775807)) 9223372036854775807)
  (check-equal? (execute (compile -9223372036854775807)) -9223372036854775807)
  
  ;;Test add
  (check-equal? (execute (compile `(add 5 8))) 13)
  (check-equal? (execute (compile `(add 1152921504606846974 1))) 1152921504606846975)
  (check-equal? (execute (compile `(add 1 5))) 6)
  (check-equal? (execute (compile `(add 2852854835955294957474594945204067443045840403248 193847594393848575739028485758594939383838383))) 2853048683549688806050333973689826037985224241631)
  (check-equal? (execute (compile `(add -9 -8))) -17)
  (check-equal? (execute (compile `(add -100 20))) -80)
  (check-equal? (execute (compile `(add 1 (add 2 (add 3 (add 4 (add 5 0))))))) 15)
  (check-equal? (execute (compile `(add 2853048683549688806050333973689826037985224241631
                                        (add 2853048683549688806050333973689826037985224241631
                                             (add 2853048683549688806050333973689826037985224241631
                                                  (add 2853048683549688806050333973689826037985224241631
                                                       (add 2853048683549688806050333973689826037985224241631 0))))))) 14265243417748444030251669868449130189926121208155)
  (check-equal? (execute (compile `(add 5 #t))) 'err)
  (check-equal? (execute (compile `(add #t #f))) 'err)
  (check-equal? (execute (compile `(add 2853048683549688806050333973689826037985224241631
                                        (add 2853048683549688806050333973689826037985224241631
                                             (add 2853048683549688806050333973689826037985224241631
                                                  (add 2853048683549688806050333973689826037985224241631
                                                       (add 2853048683549688806050333973689826037985224241631 #f))))))) 'err)

  ;;Test sub
  (check-equal? (execute (compile `(sub 2 1))) 1)
  (check-equal? (execute (compile `(sub 110 10))) 100)
  (check-equal? (execute (compile `(sub (add 2 3) (add 200 300)))) -495)
  (check-equal? (execute (compile `(sub 10 (sub 9 (sub 8 (sub 7 (sub 6 (sub 5 (sub 4 (sub 3 (sub 2 (sub 1 0)))))))))))) 5)
  (check-equal? (execute (compile `(sub 1 (sub 2 (sub 3 (sub 4 (sub 5 (sub 6 (sub 7 (sub 8 (sub 9 (sub 10 0)))))))))))) -5)
  (check-equal? (execute (compile `(sub #t 1))) 'err)
  (check-equal? (execute (compile `(sub 2 #f))) 'err)
  (check-equal? (execute (compile `(sub 384859598294957493938859599484949383838399858559594849393928290919848475757738383892928484757583939484959583939488575839485751983
                                        922228383999999999928177718384849958585858574722234532))) 384859598294957493938859599484949383838399858559594849393928290919848475756816155508928484757655761766574733980902717264763517451)
  (check-equal? (execute (compile `(add (sub 2 1) 9))) 10)

  ;;Test let without pattern matching
  (check-equal? (execute (compile `(let () 5))) 5)
  (check-equal? (execute (compile `(let ((x 5)) x))) 5)
  (check-equal? (execute (compile `(let ((a 1) (b 2)) b))) 2)
  (check-equal? (execute (compile `(let ((a 1) (b 2)) (add a b)))) 3)
  (check-equal? (execute (compile `(let () (add 5 5)))) 10)
  (check-equal? (execute (compile `(let ((x (add 5 5))) 5))) 5)
  (check-equal? (execute (compile `(let ((x 10)) (let ((x (let ((x 4) (y 6))(sub x y)))) x)))) -2)
  (check-equal? (execute (compile `(let ((x 10)) (let ((x (let ((x 4) (y 6)) (sub x y)))) (add x 2))))) 0)
  (check-equal? (execute (compile `(let ((x 10)) (let ((x (let ((x 4) (y 6)) (sub x y)))) (add x 2))))) 0)
  (check-equal? (execute (compile `(let ((var1 6)  (var2 7) (var3 8)) (add 3 8)))) 11)
  (check-equal? (execute (compile `(let ((var1 6) (var2 7) (var3 8)) (add var1 var2) (sub (add var2 var3) 5)))) 10)
  (check-equal? (execute (compile `(let ((var1 6) (var2 7) (var3 8)) (add var1 var2) (sub (add var2 var3) 5)))) 10)
  (check-equal? (execute (compile `(let ((var1 6) (var2 7) (var3 8)) (add var1 var2) (sub (add var2 var3) 5)))) 10)
  (check-equal? (execute (compile `(let ((length (add 2 3)) (breadth (sub (add 1 10) (sub 8 2))) (height (sub 6 1))) (add (add length breadth) height)
                            (add length length) (sub length breadth) (let ((f-dim (add length breadth))) length f-dim)))) 10)

  ;;Test let with pattern matching
  (check-equal? (execute (compile `(let ((x y (cons 1 2))) (add x y)))) 3)
  (check-equal? (execute (compile `(let ((x y (cons 1 '()))) (cons 2 y)))) ''(2))
  (check-equal? (execute (compile `(let ((lst '(1 2 3 4 5))) (let ((h t lst)) t)))) ''(2 3 4 5))
  (check-equal? (execute (compile `(let ((h t (add 1 2))) h))) 'err)
  (check-equal? (execute (compile `(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 '())))))) (let ((h t (tail lst))) t)))) ''(3 4))
  (check-equal? (execute (compile `(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 '())))))) (let ((h t (second lst))) t)))) ''(3 4))
  (check-equal? (execute (compile `(let ((var1 1234) (x y (cons 1 2))) (add var1 x)))) 1235)
  (check-equal? (execute (compile `(let ((x y (cons 1 2)) (var1 1234)) (add var1 x)))) 1235)
  (check-equal? (execute (compile `(let ((var1 1) (var2 #t) (var3 var4 (cons 1 (cons 2 (cons 3 4))))) (if var2 (add (add var1 var3) (first var4)) 0)))) 4)
  (check-equal? (execute (compile `(let ((a b '(1 2 3 4))) (let ((c d b)) (let ((e f d)) (add (add a c) e)))))) 6)
  (check-equal? (execute (compile `(let ((myList (cons 1 (cons 2 '()))))
                                     (if (head myList)
                                         (let ((h t myList))
                                           t)
                                           #f)))) ''(2))
  
  ;;Test booleans
  (check-equal? (execute (compile `#t)) #t)
  (check-equal? (execute (compile `#f)) #f)
  
  ;;Test if expression
  (check-equal? (execute (compile `(if (add 1 2) (add 1 3) (sub 1 2)))) 4)
  (check-equal? (execute (compile `(if (add -1 1) (add 1 3) (sub 1 2)))) 4)
  (check-equal? (execute (compile `(if #t 5 6))) 5)
  (check-equal? (execute (compile `(if #f 5 6))) 6)
  (check-equal? (execute (compile `(if 5 #t #f))) #t)
  (check-equal? (execute (compile `(if 0 #t #f))) #t)
  (check-equal? (execute (compile `(if 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890 (add 1 3) (sub 1 2)))) 4)
  (check-equal? (execute (compile `(if (sub 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890 12345678912345678912345678912345678912345678901234567890123456789123456789012345678901234567890) (add 1 3) (sub 1 2)))) 4)
  (check-equal? (execute (compile `(if (let ((var1 1) (var2 (let ((var1 2) (var2 3)) (add var1 var2)))) (sub var2 var1)) 1 2))) 1)
  (check-equal? (execute (compile `(if 0 1 (let ((var1 1) (var2 (let ((var1 2) (var2 3)) (add var1 var2)))) (sub var2 var1))))) 1)
  (check-equal? (execute (compile `(let ((x (if 1 2 3)) (y (if (add 0 1) 5 6))) (if (sub x y) x y)))) 2)

  ;;Test list value
  (check-equal? (execute (compile ''(1 2 3))) ''(1 2 3))
  (check-equal? (execute (compile ''(#t #f #t #f))) ''(#t #f #t #f))
  (check-equal? (execute (compile ''((add 2 3) (sub 1 2)))) ''(5 -1))
  (check-equal? (execute (compile ''(12345678901234567890 (if #t 1 2)))) ''(12345678901234567890 1))
  (check-equal? (execute (compile ''('(#t #f) 1 2 '(3 4)))) ''('(#t #f) 1 2 '(3 4)))
  (check-equal? (execute (compile ''('('(1) '(2))))) ''('('(1) '(2))))
  
  ;;Test head
  (check-equal? (execute (compile '(head '(1 2 3)))) 1)
  (check-equal? (execute (compile '(head '()))) 'err)
  (check-equal? (execute (compile '(head (head '('(1 5) 2 3))))) 1)
  (check-equal? (execute (compile '(head '('('(1) '(2)))))) ''('(1) '(2)))
  (check-equal? (execute (compile '(head (if #t '( 1 (add 1 2)) #f)))) 1)
  
  ;;Test Tail
  (check-equal? (execute (compile '(tail '(1 2 3)))) ''(2 3))
  (check-equal? (execute (compile '(tail '()))) 'err)
  (check-equal? (execute (compile '(tail (tail '('(1 5) 2 3))))) ''(3))
  (check-equal? (execute (compile '(tail '('('(1) '(2)))))) ''())
  (check-equal? (execute (compile '(tail (if #t '(1 (add 1 2)) #f)))) ''(3))

  ;;Test Pair
  (check-equal? (execute (compile '(cons #t #f))) ''(#t . #f))
  (check-equal? (execute (compile '(cons #t (cons #f #t)))) ''(#t #f . #t))
  (check-equal? (execute (compile '(cons #t (cons #f '(1))))) ''(#t #f 1))
  (check-equal? (execute (compile '(cons 1 (cons #t (cons 3 (cons #f '())))))) ''(1 #t 3 #f))
                


  ;;Test Pair operations
  (check-equal? (execute (compile '(first (cons #t #f)))) #t)
  (check-equal? (execute (compile '(second (cons #t #f)))) #f)

  (let ((lst '(cons 2 (cons 1 '()))))
    (check-equal? (execute (compile `(first ,lst))) 2)
    (check-equal? (execute (compile `(first (second ,lst)))) 1)
    (check-equal? (execute (compile `(second (second ,lst)))) ''())
    (check-equal? (execute (compile `(second (second (second ,lst))))) 'err))

  (let ((pair '(cons (add 1 2) (cons (add 1 1) (sub 1 0)))))
    (check-equal? (execute (compile `(first ,pair))) 3)
    (check-equal? (execute (compile `(first (first ,pair)))) 'err)
    (check-equal? (execute (compile `(first (second ,pair)))) 2)
    (check-equal? (execute (compile `(first (second (second ,pair))))) 'err))
 
  (let ((lp '(cons #t '(1 2 3))))
    (check-equal? (execute (compile `(first ,lp))) #t)
    (check-equal? (execute (compile `(second ,lp))) ''(1 2 3))
    (check-equal? (execute (compile `(first (second ,lp)))) 1) 
    (check-equal? (execute (compile `(head (second ,lp)))) 1)
    (check-equal? (execute (compile `(tail (second ,lp)))) ''(2 3)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Test Logical operators;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Test >
  (check-equal? (execute (compile `(> 6 5))) #t)
  (check-equal? (execute (compile `(> 5 6))) #f)
  (check-equal? (execute (compile `(> -5 -2))) #f)
  (check-equal? (execute (compile `(> -2 -5))) #t)
  (check-equal? (execute (compile `(> 123456789123456789123456789123456789123456789123456789123456789123456789123456789 123456789123456789123456789123456789123456789))) #t)
  (check-equal? (execute (compile `(> 5 #t))) 'err)
  (check-equal? (execute (compile `(> #t 5))) 'err)
  (check-equal? (execute (compile `(> #t #f))) 'err)
  (check-equal? (execute (compile `(> (add 70 80) (sub 30 50)))) #t)
  (check-equal? (execute (compile `(> (let ((x 5) (y 6) (z 7)) (add x (add y (add z 0)))) (let ((x 1) (y 2) (z 3)) (sub x (sub y (sub z 0))))))) #t)
  (check-equal? (execute (compile `(let ((x (> 5 3))) x))) #t)
  (check-equal? (execute (compile `(let ((x 1) (y 2)) (> y x)))) #t)

  ;;Test <
  (check-equal? (execute (compile `(< 6 5))) #f)
  (check-equal? (execute (compile `(< 5 6))) #t)
  (check-equal? (execute (compile `(< -5 -2))) #t)
  (check-equal? (execute (compile `(< -2 -5))) #f)
  (check-equal? (execute (compile `(< 123456789123456789123456789123456789123456789123456789123456789123456789123456789 123456789123456789123456789123456789123456789))) #f)
  (check-equal? (execute (compile `(< 5 #t))) 'err)
  (check-equal? (execute (compile `(< #t 5))) 'err)
  (check-equal? (execute (compile `(< #t #f))) 'err)
  (check-equal? (execute (compile `(< (add 70 80) (sub 30 50)))) #f)
  (check-equal? (execute (compile `(< (let ((x 5) (y 6) (z 7)) (add x (add y (add z 0)))) (let ((x 1) (y 2) (z 3)) (sub x (sub y (sub z 0))))))) #f)
  (check-equal? (execute (compile `(let ((x (< 5 3))) x))) #f)
  (check-equal? (execute (compile `(let ((x 1) (y 2)) (< x y)))) #t)

    ;;Test >=
  (check-equal? (execute (compile `(>= 6 5))) #t)
  (check-equal? (execute (compile `(>= 5 6))) #f)
  (check-equal? (execute (compile `(>= 6 6))) #t)
  (check-equal? (execute (compile `(>= -5 -2))) #f)
  (check-equal? (execute (compile `(>= -2 -5))) #t)
  (check-equal? (execute (compile `(>= 123456789123456789123456789123456789123456789123456789123456789123456789123456789 123456789123456789123456789123456789123456789))) #t)
  (check-equal? (execute (compile `(>= 5 #t))) 'err)
  (check-equal? (execute (compile `(>= #t 5))) 'err)
  (check-equal? (execute (compile `(>= #t #f))) 'err)
  (check-equal? (execute (compile `(>= (add 70 80) (sub 30 50)))) #t)
  (check-equal? (execute (compile `(>= (add 70 80) (sub 200 50)))) #t)
  (check-equal? (execute (compile `(>= (let ((x 5) (y 6) (z 7)) (add x (add y (add z 0)))) (let ((x 1) (y 2) (z 3)) (sub x (sub y (sub z 0))))))) #t)
  (check-equal? (execute (compile `(let ((x (>= 5 3))) x))) #t)
  (check-equal? (execute (compile `(let ((x 1) (y 2)) (>= y x)))) #t)

  ;;Test <=
  (check-equal? (execute (compile `(<= 6 5))) #f)
  (check-equal? (execute (compile `(<= 5 6))) #t)
  (check-equal? (execute (compile `(<= 6 6))) #t)
  (check-equal? (execute (compile `(<= -5 -2))) #t)
  (check-equal? (execute (compile `(<= -2 -5))) #f)
  (check-equal? (execute (compile `(<= 123456789123456789123456789123456789123456789123456789123456789123456789123456789 123456789123456789123456789123456789123456789))) #f)
  (check-equal? (execute (compile `(<= 5 #t))) 'err)
  (check-equal? (execute (compile `(<= #t 5))) 'err)
  (check-equal? (execute (compile `(<= #t #f))) 'err)
  (check-equal? (execute (compile `(<= (add 70 80) (sub 30 50)))) #f)
  (check-equal? (execute (compile `(<= (add 70 80) (sub 200 50)))) #t)
  (check-equal? (execute (compile `(<= (let ((x 5) (y 6) (z 7)) (add x (add y (add z 0)))) (let ((x 1) (y 2) (z 3)) (sub x (sub y (sub z 0))))))) #f)
  (check-equal? (execute (compile `(let ((x (<= 5 3))) x))) #f)
  (check-equal? (execute (compile `(let ((x 1) (y 2)) (<= x y)))) #t)

  
  ;;Test =
  (check-equal? (execute (compile `(= 5 5))) #t)
  (check-equal? (execute (compile `(= #t #t))) #t)
  (check-equal? (execute (compile `(= #t #f))) #f)
  ;;(check-equal? (execute (compile `(= '(1 2 3 4 5) '(1 2 3 4 5)))) #t)
  ;;(check-equal? (execute (compile `(= '(1 2) '(1)))) #f)
  ;;(check-equal? (execute (compile `(= (cons 1 (cons 2 (cons 3 '())))) (cons 1 (cons 2 (cons 3 '()))))) #t)
  ;;(check-equal? (execute (compile `(= (head '(1 2 3)) 1))) #t)
  (check-equal? (execute (compile `(= (head '(1 2 3)) (tail '(1 2 3))))) #f)
  ;;(check-equal? (execute (compile `(= (tail '(1 2 3) '(2 3))))) #t)
  (check-equal? (execute (compile `(= '() '()))) #t)
  ;;(check-equal? (execute (compile `(= '() (tail '(1))))) #t)
  (check-equal? (execute (compile `(= (add 1 2) (add 3 0)))) #t)
  ;;(check-equal? (execute (compile `(= (cons 1 2) (cons 1 2)))) #t)
  ;;(check-equal? (execute (compile `(= (cons 1 2) (cons 2 1)))) #f)
  (check-equal? (execute (compile `(= (cons 1 (cons 2 (cons 3 4))) '(1 2 3 4)))) #f)
  (check-equal? (execute (compile `(= (first (cons 1 2)) (add 0 1)))) #t)
  (check-equal? (execute (compile `(= (first (cons 1 2)) (second (cons 1 2))))) #f)
  (check-equal? (execute (compile `(let ((x 1) (y 1) (z 2)) (= x y)))) #t)
  (check-equal? (execute (compile `(let ((x 1) (y 1) (z 2)) (= x z)))) #f))
  

