(declare (unit fail-safe))
(use posix)
;;Trigger when we have 10 seconds
(define (failsafe)
 (display "A")
)

(set-signal-handler! signal/int failsafe)