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
|pop

type Arithmetic =
|'(add Arg Arg)
|'(sub Arg Arg)
|'(div Arg)
|'(or Arg Arg)
|'(and Arg Arg)
|'(xor Arg Arg)

type Comparison =
|'cmp

type Access =
| '(offset Register ,integer)

type Transfer =
|'(mov Arg Arg)

type ControlFlow =
|'ret
|'(jmp ,Symbol)
|'(jne ,Symbol)
|'(je ,Symbol)
|'(jle ,Symbol)
|'(jge ,Symbol)
|'(jl ,Symbol)
|'(jg ,Symbol)

type Arg =
| Value
| Register
||Access

type Value =
| integer

type Register
|'rax
|'rdi
|'rsp
|'rbp
|'rsi
|'rbx
|'r8
|'r9
|'r10
|'r11
|'r15
|'rdx
|'r14

|#

(provide assembly)

(module+ test
  (require rackunit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Assembly conversion functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (assembly compiled-prog)
  (string-append "\tglobal entry\n \textern nomem\n \textern error\n \textern compileBignum\n \textern addBignum\n \textern subBignum\n \textern rotateString\n \textern compBignum\n "
                 "\textern listEqual\n \textern pairEqual\n \textern my_mpz_init\n \textern my_mpz_add\n \textern my_mpz_sub\n \textern printResult\n \textern decrement\n \textern increment\n"
                 "\textern garbageCollect\n \textern compBignumSI\n \tsection .text\n entry:\n \tpush rbp\n \tmov rbp, rsp\n \tpush rbx\n \tpush rdi\n \tpush rsi\n \tpush r12\n "
                 "\tpush r13\n \tpush r14\n \tpush r15\n" (asm->assembly compiled-prog)))

;;Conver a list of ASM to assembly strin
;;Listof(ASM) -> string
(define (asm->assembly lst)
  (match lst
    [(cons h lst) (string-append (asm->assembly-helper h) "\n" (asm->assembly lst))]
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
    [_ (let () (error (string-append (symbol->string (car asm)) "Unrecognized ASM")))]))


;;Convert stack operation instruction to assembly
;;ASM -> string
(define (stack-operation->assembly asm)
  (match asm
    [`(push ,arg) (string-append "\tpush " (arg->string arg))]
    [`(pop ,arg) (string-append "\tpop " (arg->string arg))]
    [_ (error "Unsupported stack-operation")]))

;;Convert transfer instruction to executable assembly
;;ASM -> string
(define (transfer->assembly asm)
  (match asm
    [(list op arg1 arg2) (string-append "\t" (symbol->string op) " " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsupported transfer operation")]))

;:Convert a control flow instruction to executable assembly
(define (control-flow->assembly cf)
  (match cf
    ['ret "\tret"]
    [`(jmp ,lab) (string-append "\tjmp " (symbol->string lab))]
    [`(jne ,lab) (string-append "\tjne " (symbol->string lab))]
    [`(je ,lab) (string-append "\tje " (symbol->string lab))]
    [`(jle ,lab) (string-append "\tjle " (symbol->string lab))]
    [`(jge ,lab) (string-append "\tjge " (symbol->string lab))]
    [`(jl ,lab) (string-append "\tjl " (symbol->string lab))]
    [`(jg ,lab) (string-append "\tjg " (symbol->string lab))]
    [`(call ,lab) (string-append "\tcall " (symbol->string lab))]
    [_ (error "Unsupported control flow")]))

;;Convert a comparison instruction to executable assembly
(define (comparison->assembly asm)
  (match asm
    [`(cmp ,arg1 ,arg2) (string-append "\tcmp " (arg->string arg1) ", " (arg->string arg2))]
    [_ (error "Unsupported comparison instruction")]))

;;Convert a arithmetic instruction to executable assembly
;;ASM -> string
(define (arithmetic->assembly asm)
  (match asm
    [`(add ,arg1 ,arg2) (string-append "\tadd " (arg->string arg1) ", " (arg->string arg2))]
    [`(sub ,arg1 ,arg2) (string-append "\tsub " (arg->string arg1) ", " (arg->string arg2))]
    [`(or ,arg1 ,arg2) (string-append "\tor " (arg->string arg1) ", " (arg->string arg2))]
    [`(and ,arg1 ,arg2) (string-append "\tand " (arg->string arg1) ", " (arg->string arg2))]
    [`(xor ,arg1 ,arg2) (string-append "\txor " (arg->string arg1) ", " (arg->string arg2))]
    [`(div ,arg1) (string-append "\tdiv " (arg->string arg1))]
    [_ (error "Unsopported arithmetic operation")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;String conversion funcitons;;;;;;;;;;;;;;;;;;;;;;;
;;Convert an Arg to a string
;;Arg -> string
(define (arg->string arg)
  (match arg
    [(? value? arg) (value->string arg)]
    [(? register? arg) (register->string arg)]
    [`(offset ,arg ,v) (string-append "[" (arg->string arg) " + " (number->string (* 8 v)) "]")]
    [_ (error "Invalid Arg: " (string-append (symbol->string arg)))]))

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
    [(list (or 'jmp 'jne 'je 'jle 'jge 'jl 'jg 'call) lab) #t]
    [_ #f]))

;;Determine if the argument is  a Value
;;Symbol -> boolean
(define (value? v)
  (integer? v))

;;Determine if the argument is a Register
;;Symbol -> boolean
(define (register? r)
  (match r
    [(or 'rax 'rdi 'rsi 'rbx 'rdx 'rsp 'rbp 'r9 'r8 'r10 'r11 'r12 'r13 'r15 'r14) #t]
    [_ #f]))

;;Determine if the argument is an arithmetic instruction
;;ASM -> string
(define (arithmetic? asm)
  (match asm
    [`(add ,a ,b) #t]
    [`(sub ,a ,b) #t]
    [`(or ,a ,b) #t]
    [`(and ,a ,b) #t]
    [`(xor ,a ,b) #t]
    [`(div ,a) #t]
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
    [`(pop ,arg) #t]
    [_ #f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (asm->assembly '((mov rax 9223372036854775807))) "\tmov rax, 9223372036854775807\n")
  (check-equal? (asm->assembly '((mov rax 5))) "\tmov rax, 5\n")
  (check-equal? (asm->assembly '((mov rax -9223372036854775807))) "\tmov rax, -9223372036854775807\n")
  (check-equal? (asm->assembly '((mov rax -5))) "\tmov rax, -5\n"))