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
	  board-rectangle)
  (import (chezscheme)
          (only (euler) shuffle compose square)
          (only (srfi :1) append-map filter-map)
	  (prefix (dawg) d:)
	  (prefix (patricia) t:))
  (include "code/board.scm")
  (include "code/boggle.scm"))
