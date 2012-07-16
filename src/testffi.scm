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
(display "yay")