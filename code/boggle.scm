(define-record-type path
  (fields letters spot mask tree))

(define (tree-step x T)
  (and T (d:lookup-char T x)))

(define (extend path board j)
  (let ((x (board-ref board j)))
    (if (char=? x #\Q)
	(make-path (cons* #\U #\Q (path-letters path))
		   j
		   (fxlogbit1 j (path-mask path))
		   (tree-step #\U (tree-step #\Q (path-tree path))))
	(make-path (cons x (path-letters path))
		   j
		   (fxlogbit1 j (path-mask path))
		   (tree-step x (path-tree path))))))

(define (extract-word-list word-list)
  (sort (lambda (S1 S2)
	  (or (> (string-length S1) (string-length S2))
	      (and (= (string-length S1) (string-length S2))
		   (string<? S1 S2))))
	(filter (lambda (word)
		  (fx< 2 (string-length word)))
		(vector->list (hashtable-keys word-list)))))

(define (tree-match? tree)
  (d:eow? (d:dawg-byte tree)))

;; currently uses ints as bit sets. means maximum board size is 7x7
;; until i switch to intsets or bitvectors or something. assumes board
;; is string with perfect square length.
(define (boggle-search board dictionary)
  (define G
    (board-graph (isqrt (board-length board))))
  (define word-list
    (make-hashtable string-hash string=?))
  (define (walk path)
    (when (path-tree path)
      (when (tree-match? (path-tree path))
	(let ((word (list->string (reverse (path-letters path)))))
	  (unless (hashtable-ref word-list word #f)
	    (hashtable-set! word-list word #t))))
      (for-each (lambda (j)
		  (unless (fxbit-set? (path-mask path) j)
		    (walk (extend path board j))))
		(vector-ref G (path-spot path)))))
  (map (lambda (j)
	 (walk (extend (make-path '() 0 0 dictionary) board j)))
       (iota (board-length board)))
  (extract-word-list word-list))

(define (gobble board dawg)
  (boggle-search (make-board board) dawg))

(define score-vector
  '#vfx(0 0 0 1 1 2 3 5 11))

(define (score-word word)
  (fxvector-ref score-vector (fxmin 8 (string-length word))))

(define (score-word-list word-list)
  (fold-left (lambda (score word)
	       (fx+ score (score-word word)))
	     0
	     (map car word-list)))
