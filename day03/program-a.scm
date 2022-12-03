#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (read-rucksack)
  (let ((line (read-line)))
    (if (eof-object? line)
	'()
	(let* ((line-len (string-length line))
	       (midpoint (/ line-len 2)))
	  (cons (string->char-set
		 (substring line 0 midpoint))
		(string->char-set
		 (substring line midpoint line-len)))))))

(define (rucksack-shared-letter rucksack)
  (define shared-char-set
    (char-set-intersection
     (car rucksack)
     (cdr rucksack)))
  (char-set-ref shared-char-set
		(char-set-cursor shared-char-set)))

(define (char-priority char)
  (define codepoint (char->integer char))
  (cond ((char-lower-case? char)
	 (+ 1 (- codepoint (char->integer #\a))))
	((char-upper-case? char)
	 (+ 27 (- codepoint (char->integer #\A))))))

(define (main)
  (let loop ((rucksack (read-rucksack))
	     (priority-sum 0))
    (if (null? rucksack)
	(begin (display priority-sum)
	       (newline))
	(loop (read-rucksack)
	      (+ priority-sum
		 (char-priority
		  (rucksack-shared-letter rucksack)))))))

(main)
