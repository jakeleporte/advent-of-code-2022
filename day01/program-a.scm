#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

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

(define (max-sum)
  (let loop ((current-max 0) (new-sum (read-sum)))
    (if (null? new-sum)
	current-max
	(loop (max current-max new-sum) (read-sum)))))

(define (main)
  (display (max-sum))
  (newline))

(main)
