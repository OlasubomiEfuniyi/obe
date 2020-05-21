#lang racket
#|

type ASM =
|Arithmetic
|Transfer
|ControlFlow

type Access =
| '(offset Register ,integer)

type Transfer =
|'(mov Arg Arg)

type ControlFlow =
|'ret

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
    [_ (error "Unrecognized ASM")]))


;;Convert transfer instruction to executable assembly
(define (transfer->assembly asm)
  (match asm
    [(list op arg1 arg2) (string-append (symbol->string op) " " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsupported transfer operation")]))

;:Convert a control flow instruction to executable assembly
(define (control-flow->assembly cf)
  (match cf
    ['ret (symbol->string 'ret)]
    [_ (error "Unsupported control flow")]))

;;Convert a arithmetic instruction to executable assembly
;;ASM -> string
(define (arithmetic->assembly asm)
  (match asm
    [`(add ,arg1 ,arg2) (string-append "add " (arg->string arg1) ", " (arg->string arg2))]
    [`(or ,arg1 ,arg2) (string-append "or " (arg->string arg1) ", " (arg->string arg2))]
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
    [_ #f]))

;;Determine if the argument is  a Value
;;Symbol -> boolean
(define (value? v)
  (integer? v))

;;Determine if the argument is a Register
;;Symbol -> boolean
(define (register? r)
  (match r
    [(or 'rax 'rdi 'rbx) #t]
    [_ #f]))

;;Determine if the argument is an arithmetic instruction
;;ASM -> string
(define (arithmetic? asm)
  (match asm
    [`(add ,a ,b) #t]
    [`(or ,a ,b) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (asm->assembly '(mov rax 9223372036854775807)) "mov rax, 9223372036854775807")
  (check-equal? (asm->assembly '(mov rax 5)) "mov rax, 5")
  (check-equal? (asm->assembly '(mov rax -9223372036854775807)) "mov rax, -9223372036854775807")
  (check-equal? (asm->assembly '(mov rax -5)) "mov rax, -5"))