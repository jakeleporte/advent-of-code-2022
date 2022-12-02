#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define rps-letters '((#\A . rock)
		      (#\X . rock)
		      (#\B . paper)
		      (#\Y . paper)
		      (#\C . scissors)
		      (#\Z . scissors)))

(define rps-values '((rock . 1)
		     (paper . 2)
		     (scissors . 3)))

(define (read-round)
  (define line (read-line))
  (if (eof-object? line)
      '()
      (cons (assoc-ref rps-letters (string-ref line 0))
	    (assoc-ref rps-letters (string-ref line 2)))))

(define (score-round round)
  (define rps-outcome
    (modulo (- (assoc-ref rps-values (cdr round))
               (assoc-ref rps-values (car round)))
	    3))
  (cond ((= rps-outcome 1) 6)
	((= rps-outcome 0) 3)
	((= rps-outcome 2) 0)))

(define (round-total round)
  (+ (score-round round)
     (assoc-ref rps-values (cdr round))))

(define (add-scores)
  (let loop ((round (read-round))
	     (total-score 0))
    (if (null? round)
	total-score
	(loop (read-round)
	      (+ total-score (round-total round))))))

(define (main)
  (display (add-scores))
  (newline))

(main)
