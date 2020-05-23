#lang racket
#|

type ASM =
|Arithmetic
|Transfer
|ControlFlow
|Comparison
|Symbol
|StackOperation


type StackOperation =
|'push

type Arithmetic =
|'(add Arg Arg)
|'(or Arg Arg)
|'(and Arg Arg)

type Comparison =
|'cmp

type Access =
| '(offset Register ,integer)

type Transfer =
|'(mov Arg Arg)

type ControlFlow =
|'ret
|'jne

type Arg =
| Value
| Register
||Access

type Value =
| integer

type Register
| rax
|rdi

|#

(provide asm->assembly)

(module+ test
  (require rackunit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Assembly conversion functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Conver a list of ASM to assembly string
;;Listof(ASM) -> string
(define (asm->assembly lst)
  (match lst
    [(cons h lst) (string-append "\t" (asm->assembly-helper h) "\n" (asm->assembly lst))]
    ['() ""]
    [_ (error "Expected a list of ASM")]))

;;Convert ASM representation to executable assembly
;;Listof(ASM) -> string
(define (asm->assembly-helper asm)
  (match asm
    [(? transfer? asm) (transfer->assembly asm)]
    [(? control-flow? asm) (control-flow->assembly asm)]
    [(? arithmetic? asm) (arithmetic->assembly asm)]
    [(? comparison? asm) (comparison->assembly asm)]
    [(? symbol? asm) (string-append (symbol->string asm) ":")]
    [(? stack-operation? asm) (stack-operation->assembly asm)]
    [_ (let () (display asm) (error "Unrecognized ASM"))]))


;;Convert stack operation instruction to assembly
;;ASM -> string
(define (stack-operation->assembly asm)
  (match asm
    [`(push ,arg) (string-append "push " (arg->string arg))]
    [_ (error "Unsupported stack-operation")]))

;;Convert transfer instruction to executable assembly
;;ASM -> string
(define (transfer->assembly asm)
  (match asm
    [(list op arg1 arg2) (string-append (symbol->string op) " " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsupported transfer operation")]))

;:Convert a control flow instruction to executable assembly
(define (control-flow->assembly cf)
  (match cf
    ['ret "ret"]
    [`(jne ,lab) (string-append "jne " (symbol->string lab))]
    [`(call ,lab) (string-append "call " (symbol->string lab))]
    [_ (error "Unsupported control flow")]))

;;Convert a comparison instruction to executable assembly
(define (comparison->assembly asm)
  (match asm
    [`(cmp ,arg1 ,arg2) (string-append "cmp " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsupported comparison instruction")]))

;;Convert a arithmetic instruction to executable assembly
;;ASM -> string
(define (arithmetic->assembly asm)
  (match asm
    [`(add ,arg1 ,arg2) (string-append "add " (arg->string arg1) ", " (arg->string arg2))]
    [`(or ,arg1 ,arg2) (string-append "or " (arg->string arg1) ", " (arg->string arg2))]
    [`(and ,arg1 ,arg2) (string-append "and " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsopported arithmetic operation")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;String conversion funcitons;;;;;;;;;;;;;;;;;;;;;;;
;;Convert an Arg to a string
;;Arg -> string
(define (arg->string arg)
  (match arg
    [(? value? arg) (value->string arg)]
    [(? register? arg) (register->string arg)]
    [`(offset ,reg ,v) (string-append "[" (register->string reg) " + " (number->string (* 8 v)) "]")]
    [_ (error "Invalid Arg")]))

;;Covnvert a Value to a string
;;Value -> string
(define (value->string v)
  (match v
    [(? integer? i) (number->string i)]))

;;Convert a Register to a string
;;Register -> string
(define (register->string r)
  (symbol->string r))


;;;;;;;;;;;;;;;;;;;; Predicates;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Determine if an ASM representation is a transfer instruction
;;ASM -> boolean
(define (transfer? asm)
  (match asm
    [(list 'mov arg1 arg2) #t]
    [_ #f]))

;;Determine if an ASM representation is a control-flow instruction
(define (control-flow? asm)
  (match asm
    ['ret #t]
    [(list (or 'jne 'call) lab) #t]
    [_ #f]))

;;Determine if the argument is  a Value
;;Symbol -> boolean
(define (value? v)
  (integer? v))

;;Determine if the argument is a Register
;;Symbol -> boolean
(define (register? r)
  (match r
    [(or 'rax 'rdi 'rbx 'rsp 'rbp) #t]
    [_ #f]))

;;Determine if the argument is an arithmetic instruction
;;ASM -> string
(define (arithmetic? asm)
  (match asm
    [`(add ,a ,b) #t]
    [`(or ,a ,b) #t]
    [`(and ,a ,b) #t]
    [_ #f]))

;;Determine if the argument is a comparison instruction
;;ASM -> string
(define (comparison? asm)
  (match asm
    [`(cmp ,a ,b) #t]
    [_ #f]))

;;Determine if the argument is a stack operation
;;ASM -> boolean
(define (stack-operation? asm)
  (match asm
    [`(push ,arg) #t]
    [_ #f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (asm->assembly '((mov rax 9223372036854775807))) "\tmov rax, 9223372036854775807\n")
  (check-equal? (asm->assembly '((mov rax 5))) "\tmov rax, 5\n")
  (check-equal? (asm->assembly '((mov rax -9223372036854775807))) "\tmov rax, -9223372036854775807\n")
  (check-equal? (asm->assembly '((mov rax -5))) "\tmov rax, -5\n"))