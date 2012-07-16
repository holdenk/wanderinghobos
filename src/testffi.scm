;; -*- indent-tabs-mode: nil -*- ;;
(declare (uses parse-input simulate heuristic search failsafe mario))
(use sequences vector-lib posix test)
(display "making point")
;;Make a point
(define 1x2 (make-point 1 2))
(test-group "point"
	    (test 1 (point-x 1x2))
	    (test 2 (point-y 1x2))
	    )
;;Make a board
(define test-board
        (make-board-fuck 1 2 (board->c_board (world-board (file->world "tests/contest1.map")))))
(test-group "board"
      (test 1 (cboard-width test-board))
      (test 2 (cboard-height test-board))
)
(display "yay")
