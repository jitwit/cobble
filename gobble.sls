#!chezscheme
(eval-when (load compile) (optimize-level 3))
(library (gobble)
  (export gobble
	  ;; boards
          display-board
          display-ln
          roll
          dice-4x4
          dice-5x5
	  score-word
	  score-word-list
	  board-graph
	  board-rectangle
	  make-board)
  (import (chezscheme)
	  (prefix (dawg) d:))
  (include "code/outils.scm")
  (include "code/board.scm")
  (include "code/boggle.scm"))
