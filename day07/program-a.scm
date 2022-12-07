#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (execute-command tokens current-dirs)
  (define current-dir
    (if (null? current-dirs)
	""
	(car current-dirs)))
  (if (string=? (car tokens) "cd")
      (let ((target (cadr tokens)))
	(cond ((string=? target "..")
	       (cdr current-dirs))
	      ((string=? target "/")
	       (cons target current-dirs))
	      ((string=? current-dir "/")
	       (cons (string-append current-dir target) current-dirs))
	      (else
	       (cons (string-append current-dir "/" target) current-dirs))))
      current-dirs))

(define (read-dir-sizes)
  (define dir-sizes '())
  (let loop ((line (read-line))
	     (current-dirs '()))
    (if (eof-object? line)
	dir-sizes
	(let ((tokens (string-split line #\space)))
	  (cond ((string=? (car tokens) "$")
		 (loop (read-line)
		       (execute-command (cdr tokens) current-dirs)))
		((string=? (car tokens) "dir")
		 (loop (read-line) current-dirs))
		(else (for-each
		       (lambda (dir)
			 (set! dir-sizes
			       (assoc-set!
				dir-sizes
				dir
				(+ (let ((cur-size (assoc-ref dir-sizes dir)))
				     (if (number? cur-size) cur-size 0))
				   (string->number (car tokens))))))
		       current-dirs)
		      (loop (read-line) current-dirs)))))))

(define (main)
  (define dir-sizes-f
    (filter (lambda (e) (<= (cdr e) 100000))
	    (read-dir-sizes)))
  (display (apply + (map (lambda (e) (cdr e)) dir-sizes-f)))
  (newline))

(main)
