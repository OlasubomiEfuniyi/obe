#lang racket
(require "compiler.rkt" "asm-printer.rkt")

(provide main)
;;This function reads a file, compiles its contents and outputs the assembly
;;into another file
(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((program (read)))
        (display (complete-assembly-file (asm->assembly (compile program))))))))

(define (complete-assembly-file prog)
  ;;Aside from the initial directives, also setup the stack frame by saving the base pointer of the caller
  ;;and setting the base pointer of the current function. Save the callee saved registers that may be used.
  ;;Now rbp and rsp start off by pointing to the base pointer of the caller. Right underneath this is the return
  ;;address of the caller.
  (string-append "\tglobal entry\n \textern error\n \textern addBignum\n \textern subBignum\n \textern rotateString\n \tsection .text\n entry:\n \tpush rbp\n \tmov rbp, rsp\n \tpush rbx\n \tpush rdi\n \tpush rsi\n" prog))


