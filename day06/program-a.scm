#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (find-first-header packet n)
  (let loop ((remaining packet)
	     (char-num 0))
    (if (null? remaining)
	'()
	(let ((window (take remaining n)))
	  (if (= (char-set-size
		  (list->char-set window))
		 n)
	      (+ char-num n)
	      (loop (cdr remaining) (1+ char-num)))))))

(define (main)
  (let ((packet (string->list (read-line))))
    (display (find-first-header packet 4))
    (newline)))

(main)
