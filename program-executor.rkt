#lang racket
(provide execute)
(require "asm-printer.rkt")

#|;;Execute a program written in the language and return the final result
(define (execute prog)
  (let ()
    ;;Output the program to a files so that you can use the same approach
    ;;of making the file through the shell to produce a runnable program
    (with-output-to-file "program.rkt" #:exists 'truncate
      (λ () (display prog)))
    ;;Try to clean previous files that resulted from assembly and compilation.
    (if (system "make clean >> make-output 2>> make-output")
        ;;Try to make a runnable program
        (if (system "make program.run >> make-output 2>> make-output")
            ;;Try to run the program and save the output to result, a temporary file
            (if (system "./program.run > result")
                (with-input-from-file "result"
                  (λ ()
                    ;;Read the result
                    (let ((res (read)))
                      ;;Try to delete the temporary result file
                      (if (system "rm result make-output")
                          res
                          (error "Could not delete the temporary file result")))))
                (error "Could not run the program"))
            (error "Could not make a runnable program"))
        (error "Could not clean the directory")))) |#

;;Read multiple lines of datums into one list
(define (read-multi-line)
  (let ((v (read)))
    (match v
      [(? eof-object?) '()]
      [_ (cons v (read-multi-line))])))

;;Executes the program
;;ASM -> value
(define (execute prog)
  (let* ((f.s (make-temporary-file "nasm~a.s"))
         (f.run (path-replace-extension f.s ".run")))
    ;;write the assembly code into the temporary file
    (with-output-to-file f.s
      #:exists 'replace
      (λ ()
        (display (assembly prog))))
    (system (format "(make -s ~a) 2>&1 >/dev/null" f.run))
    (delete-file f.s) ;;Delete the assembly file
    ;;Execute the runnable program, saving its output as a string and using
    ;;read to read in the output as input
    (let ((res (with-output-to-string
                 (λ ()
                   (system (path->string f.run))
                   (delete-file f.run)))))
      (with-input-from-string
          res
         (λ ()
           (let ((v (read-multi-line)))
             (if (= (length v) 1) (car v) v))))))) ;;Only return a list if there are multiple lines of output
  