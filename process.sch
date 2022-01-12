;; -*- fill-column: 100 -*-
;;
;; Postprocess the output from the benchmarks in this repo
;; lhansen@mozilla.com / January 2022
;;
;; Output is in result-file.txt.  When running process.sch on result-file.txt, it prints the median
;; and/or mean values for all the benchmarks, sorted lexicographically by benchmark name.
;; 
;; The code should be clean r5rs Scheme but as usual there are incompatible system interfaces
;; and command line setups.  Instructions for various Scheme systems:
;;  - `larceny -r5 larceny.sch -- result-file.txt`
;;  - `guile2.2 --no-auto-compile -l guile.sch -- result-file.txt`

;; Configuration
(define *print-names* #t)		; #f to print only numbers listed alpha by key
(define *median* #t)			; print median
(define *mean* #t)			; print mean

(define (median nums)
  (let ((nums (sort nums <)))
    (list-ref nums (quotient (length nums) 2))))

(define (mean nums)
  (/ (apply + nums) (length nums)))

(define (process filename)

  (define input
    (with-input-from-file filename
      (lambda ()
	(let loop ((xs '()) (item (read)))
	  (if (eof-object? item)
	      (reverse xs)
	      (let ((other-item (read)))
		(loop (cons (list item other-item) xs)
		      (read))))))))

  ;; ((key value ...) ...)
  (define keys '())

  ;; Group data by name
  (for-each (lambda (x)
	      (let* ((name (car x))
		     (value (cadr x))
		     (probe (assq name keys)))
		(if probe
		    (set-cdr! probe (cons value (cdr probe)))
		    (set! keys (cons (cons name (list value)) keys)))))
	    input)

  ;; Process each group
  (for-each (lambda (x)
	      (if *print-names*
		  (display (car x)))
	      (if *median*
		  (begin
		    (display #\tab)
		    (display (inexact->exact (round (cadr x))))))
	      (if *mean*
		  (begin
		    (display #\tab)
		    (display (inexact->exact (round (caddr x))))))
	      (newline))
	    (map (lambda (x)
		   (list (car x) (median (cdr x)) (mean (cdr x))))
		 (sort keys (lambda (x y)
			      (string<? (symbol->string (car x)) (symbol->string (car y))))))))	 
