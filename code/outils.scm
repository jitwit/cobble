;;;; Support code
(define vector-swap!
  (lambda (V i j)
    (let ((t (vector-ref V i)))
      (vector-set! V i (vector-ref V j))
      (vector-set! V j t))))
(define shuffle!
  (lambda (V)
    (let loop ((i (1- (vector-length V))))
      (unless (< i 1)
	(let ((j (random (1+ i))))
	  (vector-swap! V i j))
	(loop (1- i))))
    'done))
(define shuffle
  (lambda (X)
    (let ((V (list->vector X)))
      (shuffle! V)
      (vector->list V))))
(define square
  (lambda (x)
    (* x x)))
(define-syntax compose
  (lambda (x)
    (syntax-case x ()
      ((_) #'(lambda x x))
      ((_ g) #'g)
      ((_ f g ...)
       #'(lambda (x)
	   (f ((compose g ...) x)))))))
(define filter-map
  (lambda (p xs)
    (fold-right (lambda (x xs)
		  (cond ((p x) => (lambda (y) (cons y xs)))
			(else xs)))
		'()
		xs)))
