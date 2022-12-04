#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (read-ranges)
  (let ((line (read-line)))
    (if (eof-object? line)
	'()
	(let* ((split-line (string-split line #\,))
	       (left (map string->number
			  (string-split (car split-line) #\-)))
	       (right (map string->number
			   (string-split (cadr split-line) #\-))))
	  (cons (cons (car left) (cadr left))
		(cons (car right) (cadr right)))))))

(define (overlap? ranges)
  (define range-a (car ranges))
  (define range-b (cdr ranges))
  (and (>= (cdr range-a) (car range-b))
       (<= (car range-a) (cdr range-b))))

(define (main)
  (let loop ((ranges (read-ranges))
	     (total 0))
    (if (null? ranges)
	(begin
	  (display total) (newline))
	(loop (read-ranges) (+ total (if (overlap? ranges)
					 1 0))))))

(main)
