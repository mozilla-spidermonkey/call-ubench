;; Run `larceny -r5 process.sch -- input-file.txt`, this prints the median values for all the
;; benchmarks, sorted lexicographically by benchmark name.  Uncomment name printing below to check
;; that things work.

(define input
  (with-input-from-file (vector-ref (command-line-arguments) 0)
    (lambda ()
      (let loop ((xs '()) (item (read)))
	(if (eof-object? item)
	    (reverse xs)
	    (let ((other-item (read)))
	      (loop (cons (list item other-item) xs)
		    (read))))))))

(define keys '())

(for-each (lambda (x)
	    (let* ((name (car x))
		   (value (cadr x))
		   (probe (assq name keys)))
	      (if probe
		  (set-cdr! probe (cons value (cdr probe)))
		  (set! keys (cons (cons name (list value)) keys)))))
	  input)

(define (median nums)
  (let ((nums (sort nums <)))
    (list-ref nums (quotient (length nums) 2))))

(for-each (lambda (x)
	    ;;(display (car x)) (display #\tab)
	    (display (inexact->exact (round (cdr x)))) (newline))
	  (map (lambda (x)
		 (cons (car x) (median (cdr x))))
	       (sort keys (lambda (x y)
			    (string<? (symbol->string (car x)) (symbol->string (car y)))))))



	     

