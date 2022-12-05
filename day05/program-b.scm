#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     (ice-9 regex))

(define move-command-regexp
  (make-regexp "^move ([0-9]+) from ([0-9]+) to ([0-9]+)$"))

(define (read-drawing)
  (define lines '())
  (let loop ((line (read-line))
	     (lines '()))
    (if (string-null? line)
	lines
	(loop (read-line)
	      (cons line lines)))))

(define (ith-box-char line i)
  (define str-i (+ 1 (* 4 i)))
  (if (>= str-i (string-length line))
      #\space
      (string-ref line str-i)))

(define (add-boxes stacks line)
  (define num-stacks (vector-length stacks))
  (for-each (lambda (i)
	      (let ((box-char (ith-box-char line i)))
		(unless (eqv? box-char #\space)
		  (vector-set!
		   stacks i
		   (cons box-char (vector-ref stacks i))))))
	    (iota num-stacks)))

(define (drawing->stacks drawing)
  (define labels (car drawing))
  (define boxes (cdr drawing))
  (define num-stacks (string->number
		      (last (filter
			     (negate string-null?)
			     (string-split labels char-set:whitespace)))))
  (define stacks (make-vector num-stacks '()))
  (let loop ((curr-boxes boxes))
    (if (null? curr-boxes)
	stacks
	(begin
	  (add-boxes stacks (car curr-boxes))
	  (loop (cdr curr-boxes))))))

(define (read-move-command)
  (define line (read-line))
  (if (eof-object? line)
      '()
      (let ((command-match (regexp-exec move-command-regexp line)))
	(map string->number
	     (list (match:substring command-match 1)
		   (match:substring command-match 2)
		   (match:substring command-match 3))))))

(define (execute-move-command stacks command)
  (define number (car command))
  (define source (1- (cadr command)))
  (define dest (1- (caddr command)))
  (define boxes (take (vector-ref stacks source) number))
  (vector-set! stacks source (drop (vector-ref stacks source) number))
  (vector-set! stacks dest (append boxes (vector-ref stacks dest))))

(define (execute-move-commands stacks)
  (let loop ((command (read-move-command)))
    (unless (null? command)
      (execute-move-command stacks command)
      (loop (read-move-command)))))

(define (main)
  (define stacks (drawing->stacks (read-drawing)))
  (execute-move-commands stacks)
  (for-each (lambda (i)
	      (display (car (vector-ref stacks i))))
	    (iota (vector-length stacks)))
  (newline))

(main)
