;; -*- indent-tabs-mode: nil -*- ;;
(declare (uses parse-input simulate heuristic search failsafe))
(use sequences vector-lib posix)
;;Setup the failsaif
;;Trigger when we have 10 seconds
(register-failsafe)
;;main business
(let* ((world (parse-input))
       (wheight (vector-length (world-board world)))
       (wwidth (car (vector->list
                     (vector-map (lambda (i e) (vector-length e))
                                 (world-board world)))))
       (moves (reverse (vector-ref 
                        ;;If we have command line arguments run "quickly"
                        (if (> (length (command-line-arguments)) 0)
                            (best-move world (+ 100 (* 2 (* wheight wwidth))))
                            ;;Otherwise run forver
                            (fuckerquest-prod world (+ 250 (* 3 (* wheight wwidth))) +inf.0)
                            )
                        2))))
  (display (output-moves moves)))

