(library-directories (cons "." (library-directories)))
(optimize-level 3)

(import (gobble)
	(matchable)
        (euler)
	(prefix (dawg) d:)
	(srfi :13)
	(srfi :14))

(define (no-cr s)
  (define n (1- (string-length s)))
  (if (eqv? #\return (string-ref s n))
      (substring s 0 n)
      s))

(define collins-words
  (with-input-from-file "share/definitions.txt"
    (lambda ()
      (let lp ((xs '()))
	(match (get-line (current-input-port))
	  (#!eof xs)
	  (ln
	   (lp (cons (string-tokenize (no-cr ln)
				      (char-set-complement
				       (char-set #\tab)))
		     xs))))))))

(define collins
  (d:word-list->dawg
   (map car collins-words)))

(define (main)
  (d:store-dawg collins "share/collins.fasl"))

(main)
