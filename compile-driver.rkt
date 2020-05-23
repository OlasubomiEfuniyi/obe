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
  (string-append "\tglobal entry\n" "\textern error\n" "\tsection .text\n" "entry: \n" prog))


