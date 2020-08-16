(import (gobble)
        (only (euler) shuffle square? compose)
        (matchable))

(random-seed (time-nanosecond (current-time)))
(random-seed (random (+ (time-nanosecond (current-time))
                        (time-nanosecond (current-time))
                        (time-nanosecond (current-time)))))

(define (dump-solution board solution)
  (display board)
  (for-each (lambda (word)
	      (display #\space)
	      (display word))
	    solution)
  (newline))

(define (interesting-board? solution)
  (let ((longest-word (if (null? solution)
                          0
                          (string-length (car solution)))))
    ;; still accept "tough" boards but mostly make sure there are words to find.
    (or (<= 8 longest-word)
        (cond
         ((<= 7 longest-word) (< 15/20 (random 1.0)))
         ((<= 6 longest-word) (< 1/2   (random 1.0)))
         ((<= 5 longest-word) (< 1/2   (random 1.0)))
         (else #f)))))

;; fill directory to have n boards
(define (generate-in-directory n dir)
  (define n* (length (directory-list dir)))
  (define N (- n n*))
  (format #t "there are ~a board(s), making ~a more~%" n* N)
  (let make ((i 1) (j 1))
    (unless (> i N)
      (let* ((board (substring (roll dice-5x5) 0 16))
             (board-file (string-append dir "/" board))
             (solution (gobble board)))
        (cond
         ((file-exists? board-file)
          (error 'generate "time to make better randoms, eh?"))
         ((interesting-board? solution)
          (format #t "saving    ~a ~5d/~d/~d~%" board i j N)
          (with-output-to-file board-file
            (lambda ()
              (dump-solution board solution)))
          (make (fx1+ i) (fx1+ j)))
         (else
	  (format #t "rejecting ~a ~5d/~d/~d~%" board i j N)
          (make i (fx1+ j))))))))

(define (generate-stdout n)
  (define made 0)
  (let make ()
    (let* ((board (substring (roll dice-5x5) 0 16))
	     (solution (gobble board)))
	(when (interesting-board? solution)
	  (set! made (fx1+ made))
	  (dump-solution board solution)))
    (unless (fx= made n)
      (make))))

(define generate
  (case-lambda
    ((n dir) (generate-in-directory n dir))
    ((n)     (generate-stdout n))))

(define (solve-single board)
  (unless (square? (string-length board))
    (format #t
	    "Expecting square size board but ~s has ~a characters~%"
	    board (string-length board))
    (exit 1))
  (let* ((words (gobble board))
	 (n (apply max (cons 0 (map (compose string-length) words))))
	 (fmt (format "~~~aa~~%" (+ 2 n))))
    (for-each (lambda (word)
		(format #t fmt word))
	      words)))

(define random-board
  (case-lambda
    (() (display-board (roll dice-4x4)))
    ((flat) (display (roll dice-4x4)) (newline))))

(define help-message
  (case-lambda
    (() (format #t "gobbler solves and generates boggle boards.
options:

    -n <n> -d <dir>    solve n random boards and save to files in given directory
    -n <n> -stdout     solve n random boards, dumping solutions to stdout
    -b <board>         output solutions to board
    (-r | -rn)         output a random board. use -rn to get flat chars or -r for square
    -h                 self
"))
    ((args)
     (help-message)
     (format #t "~%got arguments: ~a~%" args))))

(define (main)
  (match (command-line)
    ((_ "-n" n "-d" dir)  (generate (string->number n) dir))
    ((_ "-n" n "-stdout") (generate (string->number n)))
    ((_ "-b" board)       (solve-single board))
    ((_ "-r")             (random-board))
    ((_ "-rn")            (random-board 'flat))
    ((_ "-h")             (help-message))
    (else                 (help-message (command-line)))))

(main)
