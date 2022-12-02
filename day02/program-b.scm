#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define rps-letters '((#\A . rock)
		      (#\B . paper)
		      (#\C . scissors)))

(define rps-intent '((#\X . lose)
		     (#\Y . draw)
		     (#\Z . win)))

(define rps-intent-values '((lose . -1)
			    (draw . 0)
			    (win . 1)))

(define rps-values '((rock . 1)
		     (paper . 2)
		     (scissors . 3)))

(define rps-moves '((1 . rock)
		    (2 . paper)
		    (0 . scissors)))

(define (read-round)
  (define line (read-line))
  (if (eof-object? line)
      '()
      (cons (assoc-ref rps-letters (string-ref line 0))
	    (assoc-ref rps-intent (string-ref line 2)))))

(define (build-round round)
  (define opponent (car round))
  (define intent (cdr round))
  (define you (assoc-ref
	       rps-moves
	       (modulo
		(+ (assoc-ref rps-values opponent)
		   (assoc-ref rps-intent-values intent))
		3)))
  (cons opponent you))

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
	      (+ total-score (round-total (build-round round)))))))

(define (main)
  (display (add-scores))
  (newline))

(main)
