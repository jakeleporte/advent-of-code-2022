#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (read-sum)
  "Reads newline-separated numbers from standard input and,
when a blank line or EOF is encountered, returns their sum.
If the first encountered line is EOF, returns null."
  (define first-line (read-line))
  (if (eof-object? first-line)
      '()
      (let loop ((line first-line) (sum 0))
	(if (or (eof-object? line) (string-null? line))
	    sum
	    (loop (read-line) (+ sum (string->number line)))))))

(define (insert-into-max-list! max-list new-sum)
  (call/cc
   (lambda (break)
     (for-each (lambda (e i)
		 (if (>= new-sum e)
		     (begin
		       (list-set! max-list i new-sum)
		       (break list))))
	       max-list
	       (iota (length max-list))))))

(define (max-n n)
  (define max-list (iota n 0 0))
  (let loop ((maxes max-list) (new-sum (read-sum)))
    (if (null? new-sum)
	max-list
	(loop (insert-into-max-list! max-list new-sum) (read-sum)))))

(define (main)
  (display (apply + (max-n 3)))
  (newline))

(main)
