#lang racket

(provide compile)
(require "program-executor.rkt")
(module+ test
  (require rackunit))

;;When it comes to pointers to chunks on the heap, this shift is not useful since the fact that pointers are already 8 byte aligned
;;means we can get away without shifting the address
(define result-shift 3)
;;Immmediate values are tagged with #b000 at their beginning. After that, the next 3 bits tell you what specific immediat value you
;;are dealing with.
(define imm-shift (+ 3 result-shift))
;;A mask to figure out what type of result we have at hand (is it an immediate or a pointer to a chunk?)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
;;A mask to figure out what type of immediate value we have at hand.
(define imm-type-mask (sub1 (arithmetic-shift 1 imm-shift)))
;;A value that can clear the tag to any pointer. It is useful when attempting to change the reference count
;;of a chunk since knowing the type of the chunk is not necessary as they will all have their reference count
;;as their first 8 bytes.
(define clear-tag #xfffffffffffffff8)
;;These first 3 bits indicate a value is an immediate value. 
(define type-imm #b000);
;;The remaining 7 possible combinations are used to indicate the type of different chunks on the heap
(define type-box #b001)
(define type-bignum #b010)
(define type-list #b011)
(define type-pair #b100)
(define type-range #b101)

;;Immadiate Values
(define type-true  (arithmetic-shift #b000 result-shift))
(define type-false (arithmetic-shift #b001 result-shift))
(define type-empty-list (arithmetic-shift #b010 result-shift))




#|
type CEnv = Listof(Symbol)

type Expr =
|Display
|Loop
|Value
|Logic
|Arithmetic
|Decision
|LetBinding
|Variable
|ListOp
|PairOp
|BoxOp

type Display =
|`(println Expr)

type Loop =
|`(for Variable in Expr do Expr+) where the Expr after in is expected to evaluate to a range

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
|Range
|(box Expr)

type BoxOp =
|`(unbox ,Expr) Where Expr evaluates to a value of type box
|`(set ,Expr ,Expr) Where the first Expr evaluates to a value of type box and the second Expr evaluates to some value to be placed in the box

type Range =
|`(.. Expr Expr) starting from first integer, not including the last integer, stepping by 1. Both expressions must evaluate to an integer
|`(..= Expr Expr) starting from first integer, including the last integer, stepping 1. Both expressions must evaluate to an integer
|`(.. Expr Expr Expr) starting from first integer, not including the last integer, stepping by the third integer. All three expressions must evaluate to integers
|`(..= Expr Expr Expr) starting from first integer, including the last integer, stepping by the third integer. All three expressions must evaluate to integers

type List =
|``(Expr*)

type Pair =
|(cons Expr Expr)

type Arithmetic =
|`(add Expr Expr) Both expressions must evaluate to an integer
|`(sub Expr Expr) Both expressions must evaluate to an integer

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
    [(? box-op? expr) (desugar-box-op expr)]
    [(? logic? expr) (desugar-logic expr)]
    [(? display? expr) (desugar-display expr)]
    [(? loop? expr) (desugar-loop expr)]
    ;;;;;;;;;;;;;;;;For testing purposes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    [`(ref-eq ,chunk ,(? integer? i)) `(ref-eq ,(desugar chunk) ,i)] ;;num must be an integer in its literal sense.
    [_ (error "Invalid Program")]))

;;Desugar a value
;;Value -> Value
(define (desugar-value expr)
  (match expr
    [(? num? n) n]
    [(? boolean? b) b]
    [''() ''()]
    [`(quote ,lst) (list-literal-to-cons `,lst)]
    [`(cons ,e1 ,e2) `(cons ,(desugar e1) ,(desugar e2))]
    [`(.. ,e1 ,e2) `(.. ,(desugar e1) ,(desugar e2) 1)]
    [`(..= ,e1 ,e2) `(..= ,(desugar e1) ,(desugar e2) 1)]
    [`(.. ,e1 ,e2 ,e3) `(.. ,(desugar e1) ,(desugar e2) ,(desugar e3))]
    [`(..= ,e1 ,e2 ,e3) `(..= ,(desugar e1) ,(desugar e2) ,(desugar e3))]
    [`(box ,e1) `(box ,(desugar e1))]))

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

;;Desugar a box operation
;;BoxOp -> BoxOp
(define (desugar-box-op expr)
  (match expr
    [`(unbox ,v) `(unbox ,(desugar v))]
    [`(set ,e1 ,e2) `(set ,(desugar e1) ,(desugar e2))]))

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

;;Desugar a display expression
;;Display -> Display
(define (desugar-display expr)
  (match expr
    [`(println ,e1) `(println ,(desugar e1))]))

;;Desugar a loop expression
;;Loop -> Loop
(define (desugar-loop expr)
  (match expr
    [`(for ,v in ,expr do ,exprs ..1) `(for ,v in ,(desugar expr) do ,@(map desugar exprs))]))

(define (list-literal-to-cons lst)
  (match lst
    ['() ''()]
    [(cons h t) `(cons ,(desugar h) ,(list-literal-to-cons t))]
    [_ (display lst)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Top level compile;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Entry compile function
;;Expr -> ASM
(define (compile expr)
  `(
    (mov (offset rsp -1) rsi)
    (mov (offset rsp -2) rdx)
    ,@(compile-e (desugar expr) '(#f #f)) ;;Start with empty environment
    ;;Since rsp is never moved beyond the initial stack fram setup, this sould revert the setup
    (pop r15)
    (pop r14)
    (pop r13)
    (pop r12)
    (pop rsi)
    (pop rdi)
    (pop rbx)
    (pop rbp)
    ret
    err
    (push rbp)
    (call error)
    mem
    (push rbp)
    (call nomem)
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
    [(? box-op? expr) (compile-box-op expr env)]
    [(? logic? expr) (compile-logic expr env)]
    [(? display? expr) (compile-display expr env)]
    [(? loop? expr) (compile-loop expr env)]
    ;;;;;;;;;;;;;;;;;;;;For testing purposes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    [`(ref-eq ,chunk ,(? integer? i)) (compile-ref-eq chunk i env)];;num must be an integer in the literal sense
    [_ (error "Invalid Program")]))


;;Compile an value into ASM
;;Value CEnv-> ASM
(define (compile-value expr env)
  (match expr
    [(? num? expr) (compile-number expr env)]
    [(? boolean? expr) (compile-boolean expr)]
    [''() `((mov rax ,type-empty-list))]
    [`(cons ,e1 ,e2) (compile-pair e1 e2 env)]
    [`(.. ,e1 ,e2 ,e3) (compile-range-not-inclusive e1 e2 e3 env)]
    [`(..= ,e1 ,e2 ,e3) (compile-range-inclusive e1 e2 e3 env)]
    [`(box ,e1) (compile-box e1 env)]))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Box;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a box expression
;;Expr CEnv -> ASM
(define (compile-box e1 env)
  (let ((continue (gensym "continue")))
    `(
      ,@(compile-e e1 env)
      ;;If the result of compiling e1 is a pointer to a chunk on the heap, this box now has a reference to the chunk
      ;;which it will hold indefinitely. Therefore, the reference count of the chunk should be increased by 1.
      ,@(increment-ref-count continue)

      ,continue
      ;;Initialize the reference count of the box to 0 since we do not know whether the box is being refernced by variable
      ;;or larger expression
      ,@(assert-heap-offset 0)
      (mov rbx 0)
      (mov (offset rdi 0) rbx)

      ;;Place the value to be boxed on the heap
      ,@(assert-heap-offset 1)
      (mov (offset rdi 1) rax)

      ;;Get the pointer to the box. The first 8 bytes of the box is its
      ;;reference count, while its last 8 bytes is the boxed value
      (mov rax rdi) 
      (or rax ,type-box) ;;Tag the value as type box
      (add rdi 16))))

;;Compile a box operation
;;BoxOp CEnv -> ASM
(define (compile-box-op e1 env)
  (match e1
    [`(unbox ,e1) (compile-unbox e1 env)]
    [`(set ,e1 ,e2) (compile-set e1 e2 env)]))

;;Compile the unbox operation
;;Expr CEnv -> ASM
(define (compile-unbox e1 env)
  (let ((continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue"))
        (stack-size (* 8 (+ 2 (length env)))))
    `(
      ,@(compile-e e1 env)
      ,@assert-box
      ;;Increment the reference count of the box
      ,@(increment-ref-count continue1)
      ,continue1
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the box on the stack
      
      (xor rax ,type-box)
      (mov rax (offset rax 1))
      ;;unbox should increment the ref count of the value within the box. That way, if the box is being
      ;;garbage collected and it is the only one with a reference to the value, this value will not be
      ;;garbage collected.
      ,@(increment-ref-count continue2)
      ,continue2
      
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;temporarily save the return value on the stack
      ;;Decrement the reference count of the box
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      ;;Decrement the reference count of the unboxed value, but do not attempt to garbage collect it since it is the
      ;;return value of unbox
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #f)
      ,continue4
      ;;Return the unboxed value
      )))


(define (compile-set e1 e2 env)
  (let ((continue (gensym "continue"))
        (post-decr (gensym "postDecrement"))
        (stack-size (* 8 (+ 2 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue"))
        (continue5 (gensym "continue")))
    `(
      ;;Compile the box
      ,@(compile-e e1 env)
      ,@assert-box
      ;;Increment the reference count of the box
      ,@(increment-ref-count continue1)
      ,continue1
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ;;Compile the value
      ,@(compile-e e2 (extend #f env))
      ;;Increment the reference count of the value
      ,@(increment-ref-count continue2)
      ,continue2
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;save the new value to be placed in the box

      ;;Decrement the reference count of the value that is to be replaced if it is a pointer to a chunk on the heap
      ;;since the box will no longer hold a reference to it
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (xor rax ,type-box)
      (mov rax (offset rax 1))
      ,@(decrement-ref-count post-decr stack-size #t)

      ,post-decr
      ;;Untag the pointer to the box and replace the value in the box
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (xor rax ,type-box)
      (mov rbx (offset rsp ,(- (+ 2 (length env)))))
      (mov (offset rax 1) rbx)
      ;;Increment the reference to the value that was just placed in the box. The box now has a reference to it
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(increment-ref-count continue3)
      ,continue3
      ;;Decrement the reference count of the box. If ref count is 0 do not garbage collect it since it is the return value
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #f)
      ,continue4
      ;;Decrement the reference count of the value placed in the box. set is losing the reference it temporarily held to the value
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue5 stack-size #t)
      ,continue5
      ;;Return a tagged pointer to the box
      (mov rax (offset rsp ,(- (add1 (length env))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Loop;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a loop expression
;;Loop CEnv -> ASM
(define (compile-loop expr env)
  (match expr
    [`(for ,v in ,e-rng do ,exprs ..1) (compile-for v e-rng exprs env)]))

;;Compile a for expression
;;Variable Expr ListOf(Expr) CEnv -> ASM
(define (compile-for v e-rng exprs env)
  (let ((c-rng (compile-e e-rng env))
        ;;reserve five spots on the stack where the beginning of the range, end of the range, rdi, rsi and range will reside.
        (c-exprs (compile-es exprs (extend #f (extend #f (extend #f (extend #f (extend v env)))))))
        (end (gensym "end"))
        (loop (gensym "loop"))
        (stack-size (* 8 (+ 5 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))

    `(,@c-rng ;;Compile the range
      ;;This for loop will hold a reference count to the range during its execution. It will lose the reference when it is done.
      ;;I increment and then decrement the reference count because in a case where the range expression was only created to be used by the for loop, the decrement
      ;;step, in undoing the increment will also cause a garbage collection of the range. However, it is possible that the for loop is referencing an already exisiting
      ;;range.
      ;;NOTE: The for loop cannot pass along a reference to the range. It can however pass along a reference to the beginning of the range because this implementation
      ;;takes it on as the value associated with the variable initially. It can also pass on a reference to 
      ;;any of the successive bignums created while iterating over the range. This implementation never takes on a reference to the end of the range.
      ,@(increment-ref-count continue1)
      ,continue1
      ,@assert-range
      (mov (offset rsp ,(- (+ 5 (length env)))) rax) ;;Save a tagged reference to the range so that the reference count can be decremented at the end of the for loop
      
      (xor rax ,type-range) ;;Untag the pointer to the range for the work to be done
      
      ;;;;;;;;;;;;;;;Loop over the range, making v represent each successive value in the range;;;;;;;;;;;;;;;;;;;;;;;
      ;;v will represent each successive value in the range because we place each successive value on the stack where a lookup
      ;;for v will find it.

      ;;NOTE: The for loop temporarily holds a reference to the beginning of the range before incrementing. The reference count is only incremented
      ;;to maintian the pattern of incrementing the reference count of whatever v points to and decrementing it before v takes on a new value.
      ;;NOTE: The for loop never holds a reference to the end of the range in v. It uses it for comparisons though. Since the range still holds
      ;;a reference to it, there is no risk of having the end of the range garbage collected from underneath the for loop.
      (mov r15 rax) ;;r15 now holds an untagged reference to the range chunk
      (mov rax (offset r15 1)) ;;rax now holds the beginning of the range
      ,@(increment-ref-count continue3)
      ,continue3
      (mov rbx (offset r15 1)) ;;Move the beginning of the range into rbx
      (mov rax (offset r15 2)) ;;Move the end of the range into rax.

      ;;Always make sure that the current value of v is at offset 1 from the top of the stack and
      ;;the end of the range is at offset 2 at the top of the stack.
      ;;Save rdi and rsi at the top of the stack
      (mov (offset rsp ,(- (add1 (length env)))) rbx)
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Iterate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ,loop
      
      ,@c-exprs ;;Execute the body of the loop
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi) ;;The body of the loop may have updated rdi
      
      ;;Increment the bignum in rbx and compare it with the bignum in rax to determine when to stop

      ;;Make sure that there is space on the heap for the reference count and the GMP struct that make up the bignum.
      ;;At this point, rdi points to the next free position on the heap
      ,@(assert-heap-offset 0) ;;For ref count
      ,@(assert-heap-offset 1) ;;For first 8 bytes of GMP struct
      ,@(assert-heap-offset 2) ;;For last 8 bytes of GMP struct
      
      ;;Initialize the reference count of the bignum that will be placed on the stack by increment to 1 (not 0 since we are going to increment it anyways).
      ;;It is initialized to 1 because the for loop has a reference to it which it will loose and therefore
      ;;cause it to be decremented. If at the point it is decremented, only the for loop has a reference to it,
      ;;then it will be garbage collected as desired. But it is possible that a reference to it has been passed on
      ;;within the body of the for loop.
      (mov r15 1)
      (mov (offset rdi 0) r15)
      
      (mov rsi rdi) ;;The second argument is an untagged pointer to a position on the heap where the resulting
                    ;;GMP struct should ge placed
      (add rsi 8) ;;make rsi point to the next free position i.e after the reference count
      
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;The first argument is the bignum to be incremented
      
      (xor rdi ,type-bignum)
      (sub rsp ,stack-size)
      (call increment)
      (add rsp ,stack-size) ;;Restore the stack
      
      ;;Update the value of rdi as saved on the stack
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (add rdi 24)
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      
      ;;Decrement the reference count of the old value of v, possibly causing it to be garbage collected
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue2 stack-size #t)
      ,continue2

      ;;Place the new value of v on the stack
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      (sub rax 24)
      (or rax ,type-bignum)
      (mov (offset rsp ,(- (add1 (length env)))) rax)
      
      ;;Compare the new value of v to the end of the range
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;make the incremented value the first argument to the comparison of the current value and the end of the range
      (xor rdi ,type-bignum)
      (mov rsi (offset rsp ,(- (+ 2 (length env))))) ;;pass the end of the range
      (xor rsi ,type-bignum)
      (sub rsp ,stack-size)
      (call compBignum)
      (add rsp ,stack-size)
      
      (mov rdi (offset rsp ,(- (+ 3 (length env)))));;Restore rdi
      (mov rsi (offset rsp ,(- (+ 4 (length env)))));;Restore rsi
      
      (cmp rax 0) ;;If the return value of compBignum is <= 0, then loop
      (jle ,loop)

      ;;Decrement the reference count of the range
      (mov rax (offset rsp ,(- (+ 5 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      (mov rax ,type-true)
      ,end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Display;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a display expression
;;Display CEnv -> ASM
(define (compile-display expr env)
  (match expr
    [`(println ,e1) (compile-println e1 env)]))

;;Compile a println expression
;;Expr CEnv -> ASM
(define (compile-println e1 env)
  (let ((c1 (compile-e e1 env))
        (stack-size (* 8 (+ 1 (length env)))))
    `(,@c1

      ;;;;;;;;;;;;;;;::Call printResult to display the result of evaluating e1;;;;;;;;;;;;;;;;;;;;;;;;

      ;;Save the value in rdi before using it to pass argument to printResult
      (mov (offset rsp ,(- (add1 (length env)))) rdi)

      ;;Pass argument in rdi
      (mov rdi rax)

      ;;Setup the stack
      (sub rsp ,stack-size)

      (call printResult)

      ;;Restore the stack
      (add rsp ,stack-size)

      ;;Restore rdi
      (mov rdi (offset rsp ,(- (add1 (length env)))))

      ;;Return #t
      (mov rax ,type-true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile LetBinding;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a let binding
;;LetBinding CEnv -> ASM
(define (compile-let-binding expr env)
  (match expr
    [`(let (,(? binding? bs) ...) ,exps ..1) (compile-let bs exps env '())]))


;;Compile a let binding going from left to right
;;ListOf(Bindings) ListOf(Expr) CEnv -> ASM

(define (compile-let bs exps env new-env)
  (let ((continue (gensym "continue")))
    (match bs
      ;;Dealing with a simple binding of a value to a variable name
      [(cons `(,(? symbol? x) ,expr) rest) ;;When we reach the last binding, no need to reserve space on the stack
       `(;;Compile the head of the list of bindings. We do not want the expression
         ;;to have a notion of the bindings that have been compiled as part of the
         ;;current let, yet we want it to take the space they will fill up
         ;;into account. So, we append enough #f values to block off the
         ;;space required. This is also done in the similar cases below.
         ,@(compile-e expr (append (make-list (length new-env) #f) env))
         ;;If rax is a pointer to a chunk on the heap, the chunk's reference count should now be
         ;;bumped up by 1 since we have a new reference to it.
         ,@(increment-ref-count continue)
         ,continue
         (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rax) ;;Save the result on the stack
         ,@(compile-let rest exps env (cons x new-env)) ;;Compile the rest of the list of bindings
         )]
      [(cons `(,(? symbol? x1) ,(? symbol? x2) ,expr) rest)
       (let ((untag-pair (gensym "untag"))
             (continue (gensym "continue"))
             (post-incr-1 (gensym "postIncrement"))
             (post-incr-2 (gensym "postIncrement")))
         `(,@(compile-e expr (append (make-list (length new-env) #f) env))
           ,@(assert-pair-list) ;;This binding syntax is only valid for pairs and lists
           (mov rbx rax)
           (and rbx ,result-type-mask)
           (cmp rbx ,type-pair)
           (je ,untag-pair)
           ;;If we reached here, assert-pair-list passed, so since it is not a pair, it must be a list
           (xor rax ,type-list)
           (jmp ,continue)
           ,untag-pair
           (xor rax ,type-pair) ;;Untag the value
           ,continue
           ;;If the head value is a pointer to a chunk on the heap, the chunk's reference count should be incremented
           ;;because a new reference to it is being created. Same goes for the tail value
           (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rax) ;;Save a pointer to the structure
           (mov rax (offset rax 1))
           ,@(increment-ref-count post-incr-1)
           
           ,post-incr-1
           (mov rbx (offset rsp ,(- (+ 1 (length env) (length new-env)))))
           (mov (offset rsp ,(- (+ 2 (length env) (length new-env)))) rbx) ;;Transfer the pointer to the structure before it is overwritten
           (mov (offset rsp ,(- (+ 1 (length env) (length new-env)))) rax) ;;Place the head value on the stack first

           (mov rax (offset rsp ,(- (+ 2 (length env) (length new-env))))) ;;Get the pointer to the structure
           (mov rax (offset rax 2))
           ,@(increment-ref-count post-incr-2)

           ,post-incr-2
           (mov (offset rsp ,(- (+ 2 (length env) (length new-env)))) rax) ;;Place the tail value on the stack
           ,@(compile-let rest exps env (cons x2 (cons x1 new-env)))))] ;;Compile the rest of the list of bindings
      ['()
       (let* ((l-env (length env))
             (l-new-env (length new-env))
             (len (+ l-env l-new-env))
             (stack-size (* 8 (+ 1 l-env l-new-env))))
         `(
           ,@(compile-es-let exps (append new-env env))
           (mov (offset rsp ,(- (+ 1 len))) rax) ;;Save the return value of the let expression
           ;;Decrement the reference count of every chunk pointed to by a bound variable from this let. If the reference count of a chunk
           ;;drops to 0 but is the return value, do not garbage collect it. The code generated by decrement-ref-counts-in-let assumes that
           ;;the return value of the let expression has been placed at the top of the stack.
           ,@(decrement-ref-counts-in-let new-env (append new-env env) stack-size)
           (mov rax (offset rsp ,(- (+ 1 len))))))] ;;Compile each expression that makes up the body of the let expression under the environment created by the bindings
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
  (let ((c1 (compile-e lst env))
        (stack-size (* 8 (+ 2 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-list
      ;;Increment the ref count of the list
      ,@(increment-ref-count continue1)
      ,continue1
      ;;Save the list on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)
      (xor rax ,type-list);;Untag the pointer
      (mov rax (offset rax 1))
      ;;Temporarily increment the ref count of the head element
      ;;to prevent possible garbage collection if the list is
      ;;garbage collected
      ,@(increment-ref-count continue2)
      ,continue2
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;Save the return value on the stack
      ;;Decrement the ref count of the list, possibly garbage collecting it
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      ;;Decrment the ref count of the head, do not attempt to garbage collect
      ;;since it is the return value
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #f)
      ,continue4)))


;;Compile the tail operation
;;Expr CEnv -> ASM
(define (compile-tail lst env)
  (let ((c1 (compile-e lst env))
        (stack-size (* 8 (+ 2 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-list
      ;;Increment the ref count of the list
      ,@(increment-ref-count continue1)
      ,continue1
      ;;Save the list on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)
      (xor rax ,type-list);;Untag the pointer
      (mov rax (offset rax 2))
      ;;Temporarily increment the ref count of the tail
      ;;to prevent possible garbage collection if the list is
      ;;garbage collected
      ,@(increment-ref-count continue2)
      ,continue2
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;Save the return value on the stack
      ;;Decrement the ref count of the list, possibly garbage collecting it
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      ;;Decrement the ref count of the tail, do not attempt to garbage collect
      ;;since it is the return value
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #f)
      ,continue4)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Pair;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a cons operation
;;Expr Expr CEnv -> ASM
(define (compile-pair e1 e2 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (list (gensym "list"))
        (end (gensym "end"))
        (stack-size (* 8 (+ 2 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue")))
    `(,@c1
      ;;The list/pair holds a reference to value e1 evaluates to. It is the head/first of the structure
      ,@(increment-ref-count continue1)
      ,continue1
      (mov (offset rsp ,(- (add1 (length env)))) rax);;Save the result of evaluating the first expression on the stack
      
      ,@c2
      ;;The list/pair holds a reference to the value e2 evaluates to. It is the tail/second of the structure
      ,@(increment-ref-count continue2)
      ,continue2
      (mov (offset rsp ,(- (+ 2 (length env)))) rax);;Save the result of evaluating the second expression on the stack

      ,@(assert-heap-offset 0) ;;Make sure there is enough heap space for the structure's ref count
      (mov rbx 0)
      (mov (offset rdi 0) rbx) ;;ref count of the structure is initialized to 0
      
      ,@(assert-heap-offset 1) ;;Make sure there is enough heap space for the structure's head/first
      (mov rbx (offset rsp ,(- (add1 (length env)))))
      (mov (offset rdi 1) rbx) ;;Move the first value on the heap
      
      ,@(assert-heap-offset 2) ;;Make sure there is enough heap space for the structure's tail/second
      (mov rbx (offset rsp ,(- (+ 2 (length env)))))
      (mov (offset rdi 2) rbx) ;;Move the second value on the heap
      
      (mov rax rdi) ;;get the address to the beginning of the list/pair
      
      ;;Determine if this is a list or just a pair. It is a list if the second operand is a list or the empty list
      (mov rbx (offset rsp ,(- (+ 2 (length env)))))
      (cmp rbx ,type-empty-list)
      (je ,list)
      (and rbx ,result-type-mask)
      (cmp rbx ,type-list)
      (je ,list)
      (or rax ,type-pair);;Tag the pointer as a pair
      (jmp ,end)
      ,list
      (or rax ,type-list);;Tag the pointer as a list
      ,end
      (add rdi 24)))) ;;Make rdi contain the address of the next free location on the heap
      



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
        (stack-size (* 8 (+ 2 (length env))))
        (continue (gensym "continue"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
  `(,@(compile-e expr env)
    ,@(assert-pair-list)
    ;;Increment the ref count of the pair
    ,@(increment-ref-count continue1)
    ,continue1
    ;;Save a pointer to the pair on the stack
    (mov (offset rsp ,(- (add1 (length env)))) rax)
    
    ;;Untag the address
    (mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-pair)
    (je ,untag-pair)
    (xor rax ,type-list)
    (jmp ,continue)
    ,untag-pair
    (xor rax ,type-pair)
    ,continue
    (mov rax (offset rax 1))
    
    ;;Temporarily increment the reference count of the first item in the pair to avoid
    ;;its garbage collection if the list is garbage collected
    ,@(increment-ref-count continue2)
    ,continue2
    
    ;;Save the return value on the stack
    (mov (offset rsp ,(- (+ 2 (length env)))) rax)
    
    ;;Decrement the ref count of the list
    (mov rax (offset rsp ,(- (add1 (length env)))))
    ,@(decrement-ref-count continue3 stack-size #t)
    ,continue3

    (mov rax (offset rsp ,(- (+ 2 (length env)))))
    ;;Decrement the ref count of the first item in the pair, but do not garbage collect if it drops to 0 since it will be returned
    ,@(decrement-ref-count continue4 stack-size #f)
    ,continue4)))

;;Compile second operation on a pair
;;Expr CEnv -> ASM
(define (compile-second expr env)
  (let ((untag-pair (gensym "untag"))
        (stack-size (* 8 (+ 2 (length env))))
        (continue (gensym "continue"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
  `(,@(compile-e expr env)
    ,@(assert-pair-list)
    ;;Increment the ref count of the pair
    ,@(increment-ref-count continue1)
    ,continue1

    ;;Save the pair on the stack
    (mov (offset rsp ,(- (add1 (length env)))) rax)
    
    ;;Untag the address
    (mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-pair)
    (je ,untag-pair)
    (xor rax ,type-list)
    (jmp ,continue)
    ,untag-pair
    (xor rax ,type-pair)
    ,continue
    (mov rax (offset rax 2))
    ;;Temporarily increment the ref count of the rest of the pair to prevent its garbage collection
    ;;if the pair is garbage collected
    ,@(increment-ref-count continue2)
    ,continue2

    ;;Save the return value on the stack
    (mov (offset rsp ,(- (+ 2 (length env)))) rax)

    ;;Decrement the ref count of the pair
    (mov rax (offset rsp ,(- (add1 (length env)))))
    ,@(decrement-ref-count continue3 stack-size #t)
    ,continue3

    ;;Decrement the ref count of the rest of the pair but do not garbage collect since it is a return value
    (mov rax (offset rsp ,(- (+ 2 (length env)))))
    ,@(decrement-ref-count continue4 stack-size #f)
    ,continue4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Range;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Compile a range expression, starting from the first integer and not including the last integer
;;Expr Expr CEnv -> ASM
(define (compile-range-not-inclusive e1 e2 e3 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (c3 (compile-e e3 (extend #f (extend #f env))))
        (stack-size (* 8 (+ 5 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue"))
        (continue5 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;This range will have a reference to the bignum that inidicates the start of the range
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;save the starting point of the range on the stack

      ,@c2
      ,@assert-bignum
      ;;This range will not have a reference to this bignum but rather to its decremented value. However we want
      ;;to increment its reference count so that right after decrementing, we can decrement the reference count.
      ;;If the reference count falls to 0 in the process, the bignum can then be garbage collected
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;save the ending point of the range on the stack

      ,@c3
      ,@assert-bignum
      ,@(increment-ref-count continue3)
      ,continue3
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (+ 3 (length env)))) rax) ;;save the step value on the stack

      ;;;;;;;;;;;;;;;;Make sure the step value is positive;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (mov (offset rsp ,(- (+ 4 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 5 (length env)))) rsi)
      (mov rdi rax)
      (mov rsi 1)
      (sub rsp ,stack-size)
      (call compBignumSI)
      (add rsp ,stack-size)
      (cmp rax 0)
      (jl err)
      
      ;;;;;;;;;;;;;Make sure the starting point is strictly less than the ending point usig compBignum;;;;;;;;;;;;;;;

      ;;Prep for call to compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;pass the starting point of the range in rdi
      (mov rsi (offset rsp ,(- (+ 2 (length env))))) ;;pass the ending point of the range in rsi

      (sub rsp ,stack-size)
      (call compBignum)
      (add rsp ,stack-size)

      ;;Restore the registers used to pass arguments to compBignum
      (mov rdi (offset rsp ,(- (+ 4 (length env)))))
      (mov rsi (offset rsp ,(- (+ 5 (length env)))))

      ;;Use the result of compBignum to check that the starting point is strictly less than the ending point
      (cmp rax 0)
      (jge err) ;;If the return value is greater than or equal to 0, starting point is greater than or equal to ending point
      
      ;;;;;;;;;;;;;;;;;Decrement the end of the range before placing it on the heap;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      
      ;;Prep for/call decrement
      (mov (offset rsp ,(- (+ 4 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 5 (length env)))) rsi)
      ;;Make sure the space that will be used to store the reference count and GMP struct is within the bounds of the heap
      ,@(assert-heap-offset 0)
      ,@(assert-heap-offset 1)
      ,@(assert-heap-offset 2)
      (mov rdi rax)
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))
      ;;Set the reference count of the new bignum to be made to 0
      (mov rbx 0)
      (mov (offset rsi 0) rbx)
      (add rsi 8)
      (sub rsp ,stack-size)
      (call decrement)

      (add rsp ,stack-size) ;;Restore the stack
      (mov rdi (offset rsp ,(- (+ 4 (length env))))) ;;Restore rdi
      (mov rsi (offset rsp ,(- (+ 5 (length env))))) ;;Restore rsi

      (add rdi 24) ;;Skip over the reference count and gmp struct for the decremented value on the heap

      ;;Decrement the reference count of the initial ending bignum, possibly garbage collecting it
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Create the range on the heap and return a pointer to it. 
      ;;Place the beginning of the range on the heap
      ,@(assert-heap-offset 0)
      (mov rbx 0)
      (mov (offset rdi 0) rbx) ;;Set the reference count of this range to 0
      
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(assert-heap-offset 1)
      (mov (offset rdi 1) rax)
      
      (mov rax (offset rsp ,(- (+ 4 (length env))))) ;;Get the pointer to the decremented bignum
      (or rax ,type-bignum) ;;Tag the bignum
      ,@(increment-ref-count continue5)
      ,continue5
      ,@(assert-heap-offset 2)
      (mov (offset rdi 2) rax)

      ;;Place the step value after the start and end value on the heap
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      (or rax ,type-bignum)
      ,@(assert-heap-offset 3)
      (mov (offset rdi 3) rax)

      (mov rax rdi)
      (or rax ,type-range)
      (add rdi 32) ;;Make rdi point to the next free position on the heap
      ,@assert-heap))) 
      

;;Compile a range expression, starting from the first integer and including the last integer
;;Expr Expr CEnv -> ASM
(define (compile-range-inclusive e1 e2 e3 env)
  (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (c3 (compile-e e3 (extend #f (extend #f env))))
        (stack-size (* 8 (+ 5 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;This range will have a reference to the bignum representing the start of the range
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;save the starting point of the range on the stack

      ,@c2
      ,@assert-bignum
      ;;This range will have a reference to the bignum representing the end of the range
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;save the ending point of the range on the stack

      ,@c3
      ,@assert-bignum
      ;;This range will have a reference to the bignum representing the step value of the range
      ,@(increment-ref-count continue3)
      ,continue3
      (xor rax ,type-bignum) ;;Untag the pointer to the bignum
      (mov (offset rsp ,(- (+ 3 (length env)))) rax) ;;save the step value of the range on the stack

      ;;;;;;;;;;;;;;;;Make sure the step value is positive;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (mov (offset rsp ,(- (+ 4 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 5 (length env)))) rsi)
      (mov rdi rax)
      (mov rsi 1)
      (sub rsp ,stack-size)
      (call compBignumSI)
      (add rsp ,stack-size)
      (cmp rax 0)
      (jl err)
      
      ;;;;;;;;;;;;;Make sure the starting point is less than or equal to the ending point usig compBignum;;;;;;;;;;;;;;;

      ;;Prep for call to compBignum
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;pass the starting point of the range in rdi
      (mov rsi (offset rsp ,(- (+ 2 (length env))))) ;;pass the ending point of the range in rsi

      (sub rsp ,stack-size)
      (call compBignum)
      (add rsp ,stack-size)

      ;;Restore the registers used to pass arguments to compBignum
      (mov rdi (offset rsp ,(- (+ 4 (length env)))))
      (mov rsi (offset rsp ,(- (+ 5 (length env)))))

      ;;Use the result of compBignum to check that the starting point is less than or equal to the ending point
      (cmp rax 0)
      (jg err) ;;If the return value is greater than or equal to 0, starting point is greater than or equal to ending point


      ;;Create the range on the heap and return a pointer to it. Each pointer to a gmp struct is 16 bytes, keepind rdi a multiple of 8
      ,@(assert-heap-offset 0)
      (mov rbx 0)
      (mov (offset rdi 0) rbx);; Initialize the reference count of this range to 0
      
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(assert-heap-offset 1)
      (mov (offset rdi 1) rax)
      
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum)
      ,@(assert-heap-offset 2)
      (mov (offset rdi 2) rax)
      
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      (or rax ,type-bignum)
      ,@(assert-heap-offset 3)
      (mov (offset rdi 3) rax)

      (mov rax rdi)
      (or rax ,type-range)
      (add rdi 32)))) ;;Make rdi point to the next free position on the heap
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compile Boolean;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         (num-list (string->list num-string-padded))
         (stack-size (* 8 (+ 3 (length env)))))
    `(
      (mov (offset rsp ,(- (add1 (length env)))) rdi) ;;Save a pointer to the beginning of the string representation of the bignum
      ;;Compile the list of characters representing the number. Each character will take up one byte
      ;;The first byte will containt the length of the list of characters
      ,@(compile-char-list num-list)
      ,@(assert-heap-offset 0)
      ;;Null terminate the string
      (mov rbx 0)
      (mov (offset rdi 0) rbx)
      (add rdi 1) ;;So that rdi points to the next free position on the heap, not the last character.
                  ;;This finishes the addition to rdi started in compile-char-list, thereby maintaining
                  ;;rdi as a multiple of 8 bytes.
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Call the C function to compile a bignum into a GMP struct;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Save the values in the registers used to pass arguments. At this point, the last thing placed on the heap is the string representation of the
      ;;bignum
      (mov (offset rsp ,(- (+ 2 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 3 (length env)))) rsi)

      ;;Make sure that the addreses to be occupied by the GMP struct are within the heap's bounds
      ;;If the string representation of the bignum successfully took up 24 bytes or more, then we definitely
      ;;have enough space since the GMP struct and the reference count only need 24 bytes of space.
      ;;Otherwise, It must have either taken up 8 bytes of space or 16 bytes of space. If it took up
      ;;8 bytes of space, then we are sure of the first 8 bytes for the reference count. We still need
      ;;to confirm that the remaining 16 bytes is available. If it took up 16 bytes of space, then we
      ;;are sure of the first 8 bytes for the reference count and the first 8 bytes of the GMP struct. We
      ;;need to confirm that the last 8 bytes for the GMP struct is available.
      ,@(if (> len 16)
            ;;Then the padded string must be 24 bytes or more
            `()
            (if (> len 8)
                ;;Then the padded string is 16 bytes
                (assert-heap-offset 0)

                ;;Then the string is 8 bytes
                `(,@(assert-heap-offset 0)
                  ,@(assert-heap-offset 1))))
      
      ;;Store arguments in the registers used to pass arguments to the function
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pointer to the begining of the string rep of the bignum
      (mov rsi (offset rsp ,(- (add1 (length env)))));;Pointer to the next available space on the heap where the GPM struct will go.
                   ;;This is intentionally the same as the beginning of the string because once
                   ;;we have read from it, we no longer need it and can overwrite its values. We
                   ;;just have to make sure that the reading takes place before writing.
      (add rsi 8) ;;Skip 8 bytes to leave space for the reference count
      
      ;;Setup the stack for the call
      (sub rsp ,stack-size)
      (call compileBignum)


      ;;Restore the stack after the call
      (add rsp ,stack-size)

      ;;Set the reference count ot 0. Cannot do this before compiling the bignum because it will overwrite the string
      (mov rdi (offset rsp ,(- (add1 (length env)))))
      (mov rbx 0)
      (mov (offset rdi 0) rbx)
      
      ;;Restore the registers that were used to pass arguments
      (mov rdi (offset rsp ,(- (+ 1 (length env))))) ;;rdi has now been reset to its position before the string representation of the num was placed on the heap
      (mov rsi (offset rsp ,(- (+ 3 (length env)))))

      ;;Skip the reference count
      (add rdi 8)
      ;;make rdi point to the next avialable position on the heap by skipping the GMP struct
      (add rdi rax)
      
      ;;Tag the result as a gmp_struct
      (mov rax (offset rsp ,(- (+ 1 (length env)))))
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
        (stack-size (* 8 (+ 4 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of both arguments. Decrement the ref count at the end, forcing garbage collection if necessary
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum);;Untag the pointer
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum);;Untag the pointer
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

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
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))

      (mov (offset rsp ,(- (+ 3 (length env)))) rax);;Save the return value on the stack

      ;;Decrement the reference count of the arguments
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      
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
        (stack-size (* 8 (+ 4 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of both arguments. Decrement the ref count at the end, forcing garbage collection if necessary
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum);;Untag the pointer
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum);;Untag the pointer
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

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
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))

      (mov (offset rsp ,(- (+ 3 (length env)))) rax);;Save the return value on the stack

      ;;Decrement the reference count of the arguments
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      
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
        (stack-size (* 8 (+ 4 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of both arguments. Decrement the ref count at the end, forcing garbage collection if necessary
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum);;Untag the pointer
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum);;Untag the pointer
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

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
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))

      (mov (offset rsp ,(- (+ 3 (length env)))) rax);;Save the return value on the stack

      ;;Decrement the reference count of the arguments
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      
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
        (stack-size (* 8 (+ 4 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of both arguments. Decrement the ref count at the end, forcing garbage collection if necessary
      ,@(increment-ref-count continue1)
      ,continue1
      (xor rax ,type-bignum);;Untag the pointer
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      (xor rax ,type-bignum);;Untag the pointer
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      
      ;;Save the stack
      (mov r15 rsp) ;;The function being called will take care of setting and restoring rbp

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

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
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))

      (mov (offset rsp ,(- (+ 3 (length env)))) rax);;Save the return value on the stack

      ;;Decrement the reference count of the arguments
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum)
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Determine the appropriate boolean value to return
      (mov rbx 0)
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      
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
        (stack-size (* 8 (+ 4 (length env))))
        (false (gensym "false"))
        (end (gensym "end"))
        (bignum (gensym "bignum"))
        (list (gensym "list"))
        (pair (gensym "pair"))
        (simple (gensym "simple"))
        (empty-list (gensym "empty"))
        (continue (gensym "continue"))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ;;Increment the reference count of both arguments. Decrement them at the end,
      ;;possibly triggering garbage collection
      ,@(increment-ref-count continue1)
      ,continue1
      ;;Save the result of evaluating the first expression on the stack
      (mov (offset rsp ,(- (add1 (length env)))) rax)

      ,@c2
      ,@(increment-ref-count continue2)
      ,continue2
      ;;Save the result of evaluating the second expression on the stack
      (mov (offset rsp ,(- (+ 2 (length env)))) rax)
      
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (mov rbx (offset rsp ,(- (+ 2 (length env)))))

      ;;Make sure the two values being compared for equality are of the same type
      (and rax ,result-type-mask)
      (and rbx ,result-type-mask)
      (cmp rax rbx)
      (jne ,false)

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi)
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)

      ;;All the comp calls have the same argument structure
      (mov rdi (offset rsp ,(- (add1 (length env))))) ;;Pass the first argument
      (mov rsi (offset rsp ,(- (+ 2 (length env))))) ;;Pass the second argument


      (cmp rax ,type-bignum)
      (je ,bignum)
      (cmp rax ,type-list)
      (je ,list)
      (cmp rax ,type-pair)
      (je ,pair)
      (cmp rax ,type-imm)
      (je ,simple)
      (jne err)
      
      ,bignum
      ;;Untag the pointers to the bignums since this is what compBignum expects
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@assert-bignum
      (xor rax ,type-bignum)
      (mov rdi rax)

      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@assert-bignum
      (xor rax ,type-bignum)
      (mov rsi rax)

      ;;Call compBignum
      (sub rsp ,stack-size)
      (call compBignum)
      (jmp ,continue)
      
      ,list
      (sub rsp ,stack-size)
      (call listEqual)
      (jmp ,continue)
      
      ,pair
      (sub rsp ,stack-size)
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
      (add rsp ,stack-size) ;;Restore the stack

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))

      (mov (offset rsp ,(- (+ 3 (length env)))) rax) ;;Save the return value of the comp function

      ;;Decrement the reference count of the arguments
      (mov rax (offset rsp ,(- (add1 (length env)))))
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Determine the appropriate boolean value to return
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
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
        (stack-size (* 8 (+ 5 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of the first argument if its a chunk. The reference count will be decremented just before the end of add,
      ;;thereby triggering garbage collection if necessary e.g For an argument expression whose result only has scope within this add expression
      ,@(increment-ref-count continue1)
      ,continue1
      ;;Untag the address before placing it on the stack
      (xor rax ,type-bignum)
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      ;;Untag the address before placing it on the stack
      (xor rax ,type-bignum)
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;Save the result of evaluating the second expression on the stack to prevent loss when external functions are called

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi) 
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)
      (mov (offset rsp ,(- (+ 5 (length env)))) rdx)


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Create the new GMP struct;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Initialize the reference count of the new GMP struct to 0
      ;;Make sure there is enough space on the heap
      ,@(assert-heap-offset 0) ;;For reference count
      ,@(assert-heap-offset 1) ;;For first half of GMP struct
      ,@(assert-heap-offset 2) ;;For second half of GMP struct
      
      (mov rbx 0)
      (mov (offset rdi 0) rbx)
      (add rdi 8)
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi) ;;Update the saved value of rdi

      ;;Setup for the gmp function calls required to add two bignums.
      ;;No need to do anything with rdi since it already contains the address of
      ;;the next free position on the heap
      (sub rsp ,stack-size);;Make rsp point to the top of the stack
      ;;initialize the gpm struct in the next free position on the heap, i.e the address pointed to by rdi
      (call my_mpz_init)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Perform the addition;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;add the two bignums and save the result in the newly initialized struct
      (add rsp ,stack-size) ;;Restore the stack to access the arguments for calling the addition function
      (mov rdi (offset rsp ,(- (+ 3 (length env))))) ;;Untagged pointer to the new GMP struct
      (mov rsi (offset rsp ,(- (add1 (length env))))) ;;Untagged pointer to the first bignum
      (mov rdx (offset rsp ,(- (+ 2 (length env))))) ;;Untagged pointer to the second bignum

      (sub rsp ,stack-size) 
      (call my_mpz_add) ;;First arg is the address of the struct where the result should be placed, second arg is the first bignum, third arg is the second bignum

      ;;Restore the stack
      (add rsp ,stack-size)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))
      (mov rdx (offset rsp ,(- (+ 5 (length env)))))

      ;;Set rdi to the next free position on the heap. Add 16 because an mpz_t has a
      ;;size of 16 bytes. This may change in the future
      (add rdi 16)

      ;;Decrement the reference counts of the arguments to add
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum) ;;Because the argument was untagged before placing it on the stack
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum) ;;Because the argument was untagged before placing it on the stack
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Return a tagged pointer to the result on the heap
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      (sub rax 8) ;;Move 8 bytes down to include the reference count
      (or rax ,type-bignum))))

;;Compile the subtraction of two bignums
;;Expr Expr CEnv-> ASM
(define (compile-sub e1 e2 env)
    (let ((c1 (compile-e e1 env))
        (c2 (compile-e e2 (extend #f env)))
        (stack-size (* 8 (+ 5 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue"))
        (continue3 (gensym "continue"))
        (continue4 (gensym "continue")))
    `(,@c1
      ,@assert-bignum
      ;;Increment the reference count of the first argument if its a chunk. The reference count will be decremented just before the end of add,
      ;;thereby triggering garbage collection if necessary e.g For an argument expression whose result only has scope within this add expression
      ,@(increment-ref-count continue1)
      ,continue1
      ;;Untag the address before placing it on the stack
      (xor rax ,type-bignum)
      (mov (offset rsp ,(- (add1 (length env)))) rax) ;;Save the result of evaluating the first expression on the stack to prevent clobbering

      ,@c2
      ,@assert-bignum
      ,@(increment-ref-count continue2)
      ,continue2
      ;;Untag the address before placing it on the stack
      (xor rax ,type-bignum)
      (mov (offset rsp ,(- (+ 2 (length env)))) rax) ;;Save the result of evaluating the second expression on the stack to prevent loss when external functions are called

      ;;Save the registers used to pass in arguments
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi) 
      (mov (offset rsp ,(- (+ 4 (length env)))) rsi)
      (mov (offset rsp ,(- (+ 5 (length env)))) rdx)


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Create the new GMP struct;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;Initialize the reference count of the new GMP struct to 0
      ;;Make sure there is enough space on the heap
      ,@(assert-heap-offset 0) ;;For reference count
      ,@(assert-heap-offset 1) ;;For first half of GMP struct
      ,@(assert-heap-offset 2) ;;For second half of GMP struct
      
      (mov rbx 0)
      (mov (offset rdi 0) rbx)
      (add rdi 8)
      (mov (offset rsp ,(- (+ 3 (length env)))) rdi) ;;Update the saved value of rdi

      ;;Setup for the gmp function calls required to add two bignums.
      ;;No need to do anything with rdi since it already contains the address of
      ;;the next free position on the heap
      (sub rsp ,stack-size);;Make rsp point to the top of the stack
      ;;initialize the gpm struct in the next free position on the heap, i.e the address pointed to by rdi
      (call my_mpz_init)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Perform the addition;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;add the two bignums and save the result in the newly initialized struct
      (add rsp ,stack-size) ;;Restore the stack to access the arguments for calling the addition function
      (mov rdi (offset rsp ,(- (+ 3 (length env))))) ;;Untagged pointer to the new GMP struct
      (mov rsi (offset rsp ,(- (add1 (length env))))) ;;Untagged pointer to the first bignum
      (mov rdx (offset rsp ,(- (+ 2 (length env))))) ;;Untagged pointer to the second bignum

      (sub rsp ,stack-size) 
      (call my_mpz_sub) ;;First arg is the address of the struct where the result should be placed, second arg is the first bignum, third arg is the second bignum

      ;;Restore the stack
      (add rsp ,stack-size)

      ;;Restore the registers used to pass arguments
      (mov rdi (offset rsp ,(- (+ 3 (length env)))))
      (mov rsi (offset rsp ,(- (+ 4 (length env)))))
      (mov rdx (offset rsp ,(- (+ 5 (length env)))))

      ;;Set rdi to the next free position on the heap. Add 16 because an mpz_t has a
      ;;size of 16 bytes. This may change in the future
      (add rdi 16)

      ;;Decrement the reference counts of the arguments to add
      (mov rax (offset rsp ,(- (add1 (length env)))))
      (or rax ,type-bignum) ;;Because the argument was untagged before placing it on the stack
      ,@(decrement-ref-count continue3 stack-size #t)
      ,continue3
      (mov rax (offset rsp ,(- (+ 2 (length env)))))
      (or rax ,type-bignum) ;;Because the argument was untagged before placing it on the stack
      ,@(decrement-ref-count continue4 stack-size #t)
      ,continue4
      
      ;;Return a tagged pointer to the result on the heap
      (mov rax (offset rsp ,(- (+ 3 (length env)))))
      (sub rax 8) ;;Move 8 bytes down to include the reference count
      (or rax ,type-bignum))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Assertions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A variable that hods ASM for confirming that the value in rax is a bignum
(define assert-bignum
  `((mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-bignum)
    (jne err)))

;;A variable that holds ASM for confirming that the value in rax is a list
(define assert-list
  `((mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-list)
    (jne err)))

;;A variable that holds ASM for confirming that the value in rax is a box
(define assert-box
  `((mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-box)
    (jne err)))

;;A variable that holds ASM for confirming that the value in rax is a pair
(define assert-pair
  `((mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-pair)
    (jne err)))

;;A variable that holds ASM for confirming that the value in rax is a range
(define assert-range
  `((mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-range)
    (jne err)))
    

;;A function that holds ASM for confirming that the value in rax is a pair or a list
(define (assert-pair-list)
  (let ((end (gensym "end")))
    `((mov rbx rax)
      (and rbx ,result-type-mask)
      (cmp rbx ,type-pair)
      (je ,end)
      (cmp rbx ,type-list)
      (jne err)
      ,end)))

;;A variable that holds ASM for determining if the heap pointer has exceeded the last
;;available position on the heap
(define assert-heap
  `((mov rbx rdi)
    (sub rbx (offset rsp -1)) ;;Find the difference between the current position on the heap and the beginning of the heap
    (cmp rbx (offset rsp -2)) ;;Compare the difference to the size of the heap
    (jge mem)))

;;Determine if the heap pointer offseted by the offset value is still within the bounds of the heap
;;Integer -> ASM
(define (assert-heap-offset offset)
  `((mov rbx rdi)
    (add rbx ,(* 8 offset))
    (sub rbx (offset rsp -1)) ;;Find the difference between the current position on the heap and the beginning of the heap
    (cmp rbx (offset rsp -2)) ;;Compare the difference to the size of the heap
    (jge mem)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Reference counting ASM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A function that generates ASM for incrementing the reference count of the value
;;in rax if it is a pointer to a chunk on the heap. continue should be a label
;;to the point where the program should continue executing if the value in rax
;;is not a reference to a chunk on the heap.
;;rbx is overwritten
;;r14 is overwritten
;;Symbol -> ASM
(define (increment-ref-count continue)
  `(
    (mov rbx rax)
    (and rbx ,result-type-mask)
    (cmp rbx ,type-imm)
    (je ,continue) ;;We only want to increment the reference count if the value in rax is a pointer to a heap value
    (and rax ,clear-tag)
    (mov r14 (offset rax 0))
    (add r14 1)
    (mov (offset rax 0) r14)
    (or rax rbx)));;Tag rax again

;;A function that generate ASM for decrementing the reference count of the value
;;in rax if it is a pointer to a chunk on the heap
;;Symbol Integer -> ASM
(define (decrement-ref-count continue stack-size garbage-collect?)
  `(
    (mov rbx rax) 
    (and rbx ,result-type-mask)
    (cmp rbx ,type-imm) ;;Check if the value is a pointer to a value on the heap
    (je ,continue)
    (and rax ,clear-tag) ;;The value is a pointer, decrement its reference count
    (mov r14 (offset rax 0))
    (sub r14 1)
    (mov (offset rax 0) r14)
    (or rax rbx)
    ,@(if garbage-collect?
        ;;Possibly garbage collect the chunk
        (garbage-collect continue stack-size)
        ;;Do nothing
        '())
    )) ;;Tag rax as it was before

;;A function that generates ASM for decrementing the reference count of each chunk on the heap
;;pointed to by variables in the provided environment.
;;CEnv CEnv Integer -> ASM
(define (decrement-ref-counts-in-let new-env collective-env stack-size)
  (match new-env
    ['() `()]
    [(cons (? symbol? x) new-env)
     (let ((continue (gensym "continue"))
           (dec-gc (gensym "decGC")))
       `(
         (mov rax (offset rsp ,(- (add1 (lookup x collective-env)))))
         (mov rbx (offset rsp ,(- (+ (/ stack-size 8))))) ;;Get the return value of the let expression
         (cmp rbx rax) ;;Check if this let bound value is the same as the return value
         (jne ,dec-gc)
         ,@(decrement-ref-count continue stack-size #f) ;;Do not attempt to garbage collect
         (jmp ,continue)
         ,dec-gc
         ,@(decrement-ref-count continue stack-size #t) ;;Attempt to garbage collect
         ,continue
         ,@(decrement-ref-counts-in-let new-env collective-env stack-size)))]
    [_ (error "Invalid new environment")]))

;;A function that generates ASM for possibly garbage collecting a chunk on the heap if its
;;reference count is 0. The supposed pointer to the chunk is expected to be in rax.
;;Symbol Integer -> ASM
(define (garbage-collect continue stack-size)
  `(
    (mov rbx rax) 
    (and rbx ,result-type-mask)
    (cmp rbx ,type-imm) ;;Check if the value is a pointer to a value on the heap
    (je ,continue)
    (and rax ,clear-tag) ;;The value is a pointer, check its reference count
    (mov r14 (offset rax 0))
    (cmp r14 0)
    (jg ,continue)
    ;;Perform Garbage collection
    (or rax rbx) ;;tag rax again
    (mov r14 rdi)
    (mov (offset rsp ,(- (add1 (/ stack-size 8))))  rax)
    (mov rdi rax)
    (sub rsp ,(+ 8 stack-size))
    (call garbageCollect)
    (add rsp ,(+ 8 stack-size))
    (mov rdi r14)
    (mov rax (offset rsp ,(- (add1 (/ stack-size 8)))))
    (jmp ,continue)))

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

;;Compile a list of expressions for a let binding
;;Listof(Expr) CEnv -> ASM
(define (compile-es-let es env)
  (match es
    ['() '()]
    [(cons e '())
     ;;We ignore the fact that this expression may produce a fresh pointer to a chunk on the heap that is not already referenced, and we know will not be
     ;;reference by us because it is going to be our return value. Therefore, it is up to the larger expression to decide if a reference to it will be
     ;;kept, if it will be garbage collected, or if it will just pass the buck like we are doing now.
     `(,@(compile-e e env))] ;;Save the value on the top of the stack 
    [(cons e es)
     (let ((continue (gensym "continue"))
           (stack-size (* 8 (length env))))
       `(,@(compile-e e env)
         ;;If the result of evaluating this expression (which is not the last expression in the body of the let) is a pointer to the heap,
         ;;then it should be garbage collected if its reference count is 0. This cases is useful for chunks that are created by an expression
         ;;in the body of the let binding but are never referenced. 
         ,@(garbage-collect continue stack-size)
         ,continue
         ,@(compile-es-let es env))
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
      ,@(assert-heap-offset 0) ;;Ensure that rdi is within the bounds of the heap
      (mov rbx ,(char->integer c))
      (mov (offset rdi 0) rbx)
      
      ;;Advance to the next free position on the heap. No need to assert-heap
      ;;since an out of bounds heap pointer is only a problem if it is dereferenced
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
        [`(.. ,e1 ,e2) #t]
        [`(..= ,e1 ,e2) #t]
        [`(.. ,e1 ,e2 ,e3) #t]
        [`(..= ,e1 ,e2 ,e3) #t]
        [`(box ,e1) #t]
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

;;Determine if the expression is a box-op
;;Expr -> boolean
(define (box-op? expr)
  (match expr
    [`(unbox ,e1) #t]
    [`(set ,e1 ,e2) #t]
    [_ #f]))

;;Determine if the expression is a logical expression
;;Expr -> boolean
(define (logic? expr)
  (match expr
    [(or `(> ,e1 ,e2)`(< ,e1 ,e2) `(>= ,e1 ,e2) `(<= ,e1 ,e2) `(= ,e1 ,e2)) #t]
    [_ #f]))

;;Determine if the expression is a display expression
;;Expr -> boolean
(define (display? expr)
  (match expr
    [`(println ,expr) #t]
    [_ #f]))

;;Determine if the expression is a loop expression
;;Expr -> boolean
(define (loop? expr)
  (match expr
    [`(for ,v in ,expr do ,exprs ..1) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Language features used for testing purposes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Expr integer CEnv-> ASM
(define (compile-ref-eq chunk integer env)
  (let ((stack-size (* 8 (+ 1 (length env))))
        (continue1 (gensym "continue"))
        (continue2 (gensym "continue")))
    `(,@(compile-e chunk env)
      ;;Increment the reference count of the chunk
      ,@(increment-ref-count continue1)
      ,continue1
      (mov (offset rsp ,(- (add1 (length env)))) rax)
      
      ;;Make sure the result of evaluating the "chunk" is actually a chunk
      (mov rbx rax)
      (and rbx ,result-type-mask)
      (cmp rbx ,type-imm) 
      (je err) ;;The value is either an immediate or a chunk. If it is an immediate, this function raises an error.
      
      ;;The value is a chunk. Its integer ref count is at offset 0
      (and rax ,clear-tag) ;;Untag the chunk
      (mov rax (offset rax 0))
      
      ;;Determine which boolean should be returned
      (mov rbx ,integer)
      (sub rax 1) ;;Not considering the reference this function has to the chunk
      (cmp rax rbx)
      (jne err)
      (mov rax ,type-true)

      (mov rbx rax)
      (mov rax (offset rsp ,(- (add1 (length env))))) ;;Get the tagged chunk into rax
      (mov (offset rsp ,(- (add1 (length env)))) rbx) ;;Save the return value on the stack
      
      ;;Decrement the reference count of the chunk
      ,@(decrement-ref-count continue2 stack-size #t)
      ,continue2

      (mov rax (offset rsp ,(- (add1 (length env))))) ;:Get the return value from the stack
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  ;;Test bignum
  (check-equal? (execute (compile 5)) 5)
  (check-equal? (execute (compile -5)) -5)
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
  (check-equal? (execute (compile `(let ((x y (cons 1 '()))) (cons 2 y)))) '(2))
  (check-equal? (execute (compile `(let ((lst '(1 2 3 4 5))) (let ((h t lst)) t)))) '(2 3 4 5))
  (check-equal? (execute (compile `(let ((h t (add 1 2))) h))) 'err)
  (check-equal? (execute (compile `(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 '())))))) (let ((h t (tail lst))) t)))) '(3 4))
  (check-equal? (execute (compile `(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 '())))))) (let ((h t (second lst))) t)))) '(3 4))
  (check-equal? (execute (compile `(let ((var1 1234) (x y (cons 1 2))) (add var1 x)))) 1235)
  (check-equal? (execute (compile `(let ((x y (cons 1 2)) (var1 1234)) (add var1 x)))) 1235)
  (check-equal? (execute (compile `(let ((var1 1) (var2 #t) (var3 var4 (cons 1 (cons 2 (cons 3 4))))) (if var2 (add (add var1 var3) (first var4)) 0)))) 4)
  (check-equal? (execute (compile `(let ((a b '(1 2 3 4))) (let ((c d b)) (let ((e f d)) (add (add a c) e)))))) 6)
  (check-equal? (execute (compile `(let ((myList (cons 1 (cons 2 '()))))
                                     (if (head myList)
                                         (let ((h t myList))
                                           t)
                                           #f)))) '(2))
  
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
  (check-equal? (execute (compile ''(1 2 3))) '(1 2 3))
  (check-equal? (execute (compile ''(#t #f #t #f))) '(#t #f #t #f))
  (check-equal? (execute (compile ''((add 2 3) (sub 1 2)))) '(5 -1))
  (check-equal? (execute (compile ''(12345678901234567890 (if #t 1 2)))) '(12345678901234567890 1))
  (check-equal? (execute (compile ''('(#t #f) 1 2 '(3 4)))) '((#t #f) 1 2 (3 4)))
  (check-equal? (execute (compile ''('('(1) '(2))))) '(((1) (2))))
  
  ;;Test head
  (check-equal? (execute (compile '(head '(1 2 3)))) 1)
  (check-equal? (execute (compile '(head '()))) 'err)
  (check-equal? (execute (compile '(head (head '('(1 5) 2 3))))) 1)
  (check-equal? (execute (compile '(head '('('(1) '(2)))))) '((1) (2)))
  (check-equal? (execute (compile '(head (if #t '( 1 (add 1 2)) #f)))) 1)
  
  ;;Test Tail
  (check-equal? (execute (compile '(tail '(1 2 3)))) '(2 3))
  (check-equal? (execute (compile '(tail '()))) 'err)
  (check-equal? (execute (compile '(tail (tail '('(1 5) 2 3))))) '(3))
  (check-equal? (execute (compile '(tail '('('(1) '(2)))))) '())
  (check-equal? (execute (compile '(tail (if #t '(1 (add 1 2)) #f)))) '(3))

  ;;Test Pair
  (check-equal? (execute (compile '(cons #t #f))) '(#t . #f))
  (check-equal? (execute (compile '(cons #t (cons #f #t)))) '(#t #f . #t))
  (check-equal? (execute (compile '(cons #t (cons #f '(1))))) '(#t #f 1))
  (check-equal? (execute (compile '(cons 1 (cons #t (cons 3 (cons #f '())))))) '(1 #t 3 #f))
                


  ;;Test Pair operations
  (check-equal? (execute (compile '(first (cons #t #f)))) #t)
  (check-equal? (execute (compile '(second (cons #t #f)))) #f)

  (let ((lst '(cons 2 (cons 1 '()))))
    (check-equal? (execute (compile `(first ,lst))) 2)
    (check-equal? (execute (compile `(first (second ,lst)))) 1)
    (check-equal? (execute (compile `(second (second ,lst)))) '())
    (check-equal? (execute (compile `(second (second (second ,lst))))) 'err))

  (let ((pair '(cons (add 1 2) (cons (add 1 1) (sub 1 0)))))
    (check-equal? (execute (compile `(first ,pair))) 3)
    (check-equal? (execute (compile `(first (first ,pair)))) 'err)
    (check-equal? (execute (compile `(first (second ,pair)))) 2)
    (check-equal? (execute (compile `(first (second (second ,pair))))) 'err))
 
  (let ((lp '(cons #t '(1 2 3))))
    (check-equal? (execute (compile `(first ,lp))) #t)
    (check-equal? (execute (compile `(second ,lp))) '(1 2 3))
    (check-equal? (execute (compile `(first (second ,lp)))) 1) 
    (check-equal? (execute (compile `(head (second ,lp)))) 1)
    (check-equal? (execute (compile `(tail (second ,lp)))) '(2 3)))

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
  (check-equal? (execute (compile `(= '(1 2 3 4 5) '(1 2 3 4 5)))) #t)
  (check-equal? (execute (compile `(= '(1 2) '(1)))) #f)
  (check-equal? (execute (compile `(= (cons 1 (cons 2 (cons 3 '()))) (cons 1 (cons 2 (cons 3 '())))))) #t)
  (check-equal? (execute (compile `(= (head '(1 2 3)) 1))) #t)
  (check-equal? (execute (compile `(= (head '(1 2 3)) (tail '(1 2 3))))) #f)
  (check-equal? (execute (compile `(= (tail '(1 2 3)) '(2 3)))) #t)
  (check-equal? (execute (compile `(= '() '()))) #t)
  (check-equal? (execute (compile `(= '() (tail '(1))))) #t)
  (check-equal? (execute (compile `(= (add 1 2) (add 3 0)))) #t)
  (check-equal? (execute (compile `(= (cons 1 2) (cons 1 2)))) #t)
  (check-equal? (execute (compile `(= (cons 1 2) (cons 2 1)))) #f)
  (check-equal? (execute (compile `(= (cons 1 (cons 2 (cons 3 4))) '(1 2 3 4)))) #f)
  (check-equal? (execute (compile `(= (first (cons 1 2)) (add 0 1)))) #t)
  (check-equal? (execute (compile `(= (first (cons 1 2)) (second (cons 1 2))))) #f)
  (check-equal? (execute (compile `(let ((x 1) (y 1) (z 2)) (= x y)))) #t)
  (check-equal? (execute (compile `(let ((x 1) (y 1) (z 2)) (= x z)))) #f)

  ;;Test Range ..
  (check-equal? (execute (compile `(.. 5 6))) `(5..=5 1))
  (check-equal? (execute (compile `(.. (add 1 0) (add 1 2)))) `(1..=2 1))
  (check-equal? (execute (compile `(.. (add 1 (sub 5 6)) 8 2))) `(0..=7 2))
  (check-equal? (execute (compile `(.. '(1 2 3) 5))) 'err)
  (check-equal? (execute (compile `(.. 5 '(1 2 3)))) 'err)
  (check-equal? (execute (compile `(.. '(1 2 3) '()))) 'err)
  (check-equal? (execute (compile `(.. 5 4))) 'err)
  (check-equal? (execute (compile `(.. 5 5))) 'err)
  (check-equal? (execute (compile `(.. (add 1 (add 2 3)) (add 1 (add 2 2))))) 'err)
  (check-equal? (execute (compile `(.. (if #t 100 #f) (let ((x 7)) (add x 100)) (add 1 2)))) '(100..=106 3))
  (check-equal? (execute (compile `(.. (head '(1 2 3)) 5))) '(1..=4 1))

  ;;Test Range ..=
  (check-equal? (execute (compile `(..= 5 6))) `(5..=6 1))
  (check-equal? (execute (compile `(..= (add 1 0) (add 1 2) 20))) `(1..=3 20))
  (check-equal? (execute (compile `(..= (add 1 (sub 5 6)) 8 -1))) 'err)
  (check-equal? (execute (compile `(..= '(1 2 3) 5))) 'err)
  (check-equal? (execute (compile `(..= 5 '(1 2 3)))) 'err)
  (check-equal? (execute (compile `(..= '(1 2 3) '()))) 'err)
  (check-equal? (execute (compile `(..= 5 4))) 'err)
  (check-equal? (execute (compile `(..= 5 5))) '(5..=5 1))
  (check-equal? (execute (compile `(..= (add 1 (add 2 3)) (add 1 (add 2 2))))) 'err)
  (check-equal? (execute (compile `(..= (if #t 100 #f) (let ((x 7)) (add x 100)) (add 1 5)))) '(100..=107 6))
  (check-equal? (execute (compile `(..= (head '(1 2 3)) 5))) '(1..=5 1))

  ;;Test println
  (check-equal? (execute (compile `(println 5))) '(5 #t))
  (check-equal? (execute (compile `(println (add 1 3)))) '(4 #t))
  (check-equal? (execute (compile `(println (if #t '(1 2 3) (cons 1 2))))) '((1 2 3) #t))

  ;;Test for loop
  (check-equal? (execute (compile `(for value in (..= 1 5) do (let ((x (add 2 value))) (println x))))) '(3 4 5 6 7 #t))
  (check-equal? (execute (compile `(for x in (if #t (..= 1 5) (.. 1 4)) do (let ((y x)) y)))) #t)
    (check-equal? (execute (compile `(for x in (..= 1 3) do
                                  (println x)
                                  (for x in (..= 1 3) do (println x))))) '(1 1 2 3 2 1 2 3 3 1 2 3 #t))
  (check-equal? (execute (compile `(for x in (..= 1 3) do
                                  (println x)
                                  (for x in (..= x 3) do (println x))))) '(1 1 2 3 2 2 3 3 3 #t))
  (check-equal? (execute (compile `(for x in (.. 1 1) do 5))) 'err)
  (check-equal? (execute (compile `(for x in (.. 1 2) do 5 (add 1 #t)))) 'err)

  ;;Test box
  (check-equal? (execute (compile `(box 5))) '#&5)
  (check-equal? (execute (compile `(box (box 5)))) '#&#&5)
  (check-equal? (execute (compile `(box (cons 1 2)))) '#&(1 . 2))
  (check-equal? (execute (compile `(box (add 0 #t)))) 'err)
  (check-equal? (execute (compile `(box (.. 1 3)))) '#&(1..=2 1))

  ;; Test unbox
  (check-equal? (execute (compile `(unbox (box 5)))) '5)
  (check-equal? (execute (compile `(unbox (box (box 5))))) '#&5)
  (check-equal? (execute (compile `(unbox (box (cons 1 2))))) '(1 . 2))
  (check-equal? (execute (compile `(unbox (box (add 0 #t))))) 'err)
  (check-equal? (execute (compile `(unbox (box (.. 1 3))))) '(1..=2 1))

  ;; Test set
  (check-equal? (execute (compile `(set (box 5) 6))) '#&6)
  (check-equal? (execute (compile `(set (box 5) #t))) '#&#t)
  (check-equal? (execute (compile `(set 6 (box 5)))) 'err)
  (check-equal? (execute (compile `(set (set (box 5) 6) 7))) '#&7) 


  ;;Test reference count
  (check-equal? (execute (compile `(let ((x (box 10))) (ref-eq x 1)
                                     (let ((y x) (z x)) (ref-eq x 3) (ref-eq y 3) (ref-eq z 3))))) #t)
  (check-equal? (execute (compile `(let () (ref-eq (let ((y 5)) (ref-eq y 1) y) 0) (ref-eq (let ((y 5)) (ref-eq y 1) y) 0)))) #t)
  (check-equal? (execute (compile `(let ((x 10)) (add x (let () (ref-eq x 2) 2))))) 12)
  (check-equal? (execute (compile `(let ((x 10) (y 11)) (add x (add y (let ((z y)) (ref-eq y 3) (ref-eq x 2) 0)))))) 21)
  (check-equal? (execute (compile `(ref-eq (cons 1 '()) 0))) #t)
  (check-equal? (execute (compile `(let ((a 2) (x '())) (let ((y (cons a x))) (let ((z (cons a y))) (ref-eq y 2) (ref-eq z 1) (ref-eq (head y) 3) (ref-eq (head z) 3)))))) #t)
  (check-equal? (execute (compile `(let ((a 2) (x '())) (let ((y (cons a x))) (let ((z (cons a y))) (ref-eq y 2) (ref-eq z 1) (ref-eq (first y) 3) (ref-eq (first z) 3)))))) #t)
  (check-equal? (execute (compile `(let ((b (box '(1 2 3)))) (ref-eq b 1) (ref-eq (unbox b) 1) (let ((c (unbox b))) (ref-eq c 2)) (ref-eq b 1) (let ((d b)
                                                                                                                                        (e (unbox b))
                                                                                                                                        (f (unbox b))) (ref-eq b 2) (ref-eq d 2) (ref-eq (unbox b) 3) (ref-eq (unbox d) 3))))) #t)
  (check-equal? (execute (compile `(let ((b (box 3)) (a 1)) (ref-eq (unbox b) 1) (set b a) (ref-eq (unbox b) 2)))) #t)
  (check-equal? (execute (compile `(for x in (.. 1 5 1) do
                                     (if (= x 1)
                                         (ref-eq x 2)
                                         (ref-eq x 1))))) #t)
  (check-equal? (execute (compile `(for x in (..= 1 5 1) do
                                     (let ((v (..= x 5 1)))
                                       (ref-eq v 1)
                                       (for t in v do
                                         (if (= t 1)
                                             (ref-eq t 4)
                                             (if (= t x)
                                                 (ref-eq t 3)
                                                 (ref-eq t 1)))))))) #t)
  (check-equal? (execute (compile `(let ((lst '(1 2 3))) (let ((h t lst)) (ref-eq h 2) (ref-eq t 2) (let ((h t t)) (ref-eq h 2) (ref-eq t 2) (let ((h t t)) (ref-eq h 2) (= t '()))))))) #t)
  (check-equal? (execute (compile `(let ((pair (cons 1 (cons 2 (cons 3 4))))) (let ((f s pair)) (ref-eq f 2) (ref-eq s 2) (let ((f s s)) (ref-eq f 2) (ref-eq s 2) (let ((f s s)) (ref-eq f 2) (ref-eq s 2) (= s 4))))))) #t)

  ;;Test GC reduces ref count of inner chunks that are referenced by the chunk being garbage collected
  (check-equal? (execute (compile `(let ((x 3)) (ref-eq (let ((y x)) (ref-eq x 2) (ref-eq y 2) (for z in (.. y (add 1 y)) do 0) x) 1)))) #t)
  (check-equal? (execute (compile `(let ((x 1)) (let ((y (..= x 5))) (ref-eq x 2) y) (ref-eq x 1)))) #t)
  (check-equal? (execute (compile `(let ((a 5) (b 6) (c (cons 7 '()))) (let ((lst (cons a (cons b c)))) (ref-eq lst 1) (ref-eq (head lst) 2) (ref-eq a 2)
                                                                         (ref-eq (head (tail lst)) 2) (ref-eq b 2)
                                                                         (ref-eq (tail (tail lst)) 2) (ref-eq c 2))
                                     (ref-eq a 1) (ref-eq b 1) (ref-eq c 1) (ref-eq (head c) 1)))) #t)
  (check-equal? (execute (compile `(let ((a 5) (b 6) (c (cons 7 8))) (let ((lst (cons a (cons b c)))) (ref-eq lst 1) (ref-eq (first lst) 2) (ref-eq a 2)
                                                                         (ref-eq (first (second lst)) 2) (ref-eq b 2)
                                                                         (ref-eq (second (second lst)) 2) (ref-eq c 2))
                                     (ref-eq a 1) (ref-eq b 1) (ref-eq c 1) (ref-eq (first c) 1)))) #t)
  (check-equal? (execute (compile `(let ((a 6)) (let ((b (box a))) (ref-eq (unbox b) 2) (ref-eq a 2) 7) (ref-eq a 1)))) #t))

  


