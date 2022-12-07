#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define space-total 70000000)
(define space-needed 30000000)

(define (execute-cd-command tokens current-dirs)
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
  (define read-ls-output #f)
  (let loop ((line (read-line))
	     (current-dirs '()))
    (if (eof-object? line)
	dir-sizes
	(let ((tokens (string-split line #\space)))
	  (cond ((string=? (car tokens) "$")
		 (cond ((string=? (cadr tokens) "ls")
			(set! read-ls-output
			      (if (assoc-ref dir-sizes (car current-dirs))
				  #f
				  #t))
			(loop (read-line) current-dirs))
		       (else
			(loop (read-line)
			      (execute-cd-command (cdr tokens) current-dirs)))))
		((string=? (car tokens) "dir")
		 (loop (read-line) current-dirs))
		(else (if read-ls-output
			  (for-each
			   (lambda (dir)
			     (set! dir-sizes
				   (assoc-set!
				    dir-sizes
				    dir
				    (+ (let ((cur-size (assoc-ref dir-sizes dir)))
					 (if (number? cur-size) cur-size 0))
				       (string->number (car tokens))))))
			   current-dirs))
		      (loop (read-line) current-dirs)))))))

(define (main)
  (define dir-sizes (read-dir-sizes))
  (define space-used (assoc-ref dir-sizes "/"))
  (define space-free (- space-total space-used))
  (define space-to-free (- space-needed space-free))
  (define big-enough
    (map cdr
	 (filter (lambda (e) (>= (cdr e) space-to-free))
		 dir-sizes)))
  (display (apply min big-enough))
  (newline))

(main)
