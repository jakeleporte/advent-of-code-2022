#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (read-rucksack-group n)
  (let loop ((line (read-line))
	     (ruck-group '())
	     (num-read 1))
    (cond ((eof-object? line) '())
	  ((= num-read n) (cons (string->char-set line) ruck-group))
	  (else (loop (read-line)
		      (cons (string->char-set line) ruck-group)
		      (1+ num-read))))))

(define (ruck-list-shared-letter ruck-list)
  (define shared-char-set
    (reduce char-set-intersection char-set:empty ruck-list))
  (char-set-ref shared-char-set
		(char-set-cursor shared-char-set)))

(define (char-priority char)
  (define codepoint (char->integer char))
  (cond ((char-lower-case? char)
	 (+ 1 (- codepoint (char->integer #\a))))
	((char-upper-case? char)
	 (+ 27 (- codepoint (char->integer #\A))))))

(define (main)
  (let loop ((ruck-list (read-rucksack-group 3))
	     (priority-sum 0))
    (if (null? ruck-list)
	(begin (display priority-sum)
	       (newline))
	(loop (read-rucksack-group 3)
	      (+ priority-sum
		 (char-priority
		  (ruck-list-shared-letter ruck-list)))))))

(main)
