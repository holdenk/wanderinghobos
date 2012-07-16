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
(define test-board (world-board (file->world "tests/contest1.map")))
(define test-cboard
        (make-board-fuck (board-width test-board) (board-height test-board) (board->c_board test-board)))
(test-group "board"
      (test (board-width test-board) (cboard-width test-cboard))
      (test (board-height test-board) (cboard-height test-cboard))
      (test "#######. *R##  \\.##\\ * #L  .\\#######" (cboard-board test-cboard))
      (test  test-board (c_board->board (cboard-board test-cboard) 6 4))
)
(display "yay")
