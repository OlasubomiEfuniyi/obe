#lang racket
(provide execute)
(require racket/system)

;;Execute a program written in the language and return the final result
(define (execute prog)
  (let ()
    ;;Output the program to a files so that you can use the same approach
    ;;of making the file through the shell to produce a runnable program
    (with-output-to-file "program.rkt" #:exists 'replace
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
        (error "Could not clean the directory"))))