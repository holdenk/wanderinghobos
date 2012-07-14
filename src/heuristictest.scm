(declare (unit heuristictest))
(require-library parse-input)
(require-library simulate)
(use test)

(define (dry-world board) (make-world board +inf.0 +inf.0 +inf.0 0 0 '()))
(define heuristictestworld1
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall hug robot empty wall)
     #(wall wall wall wall wall))))

(define heuristictestworld2
 (dry-world
  '#(#(wall rock earth empty wall)
     #(wall robot hug empty wall)
     #(wall wall wall wall wall))))

(test-group "find-the-robot"
	    ;;It is not Zoidbergs turn
	    (test (list 2 1) (find-robot heuristictestworld1))
	    (test (list 1 1) (find-robot heuristictestworld2))
)
(test-group "find-the-hug"
	    (test (list (list 1 1)) (find-hugs heuristictestworld1))
	    (test (list (list 2 1)) (find-hugs heuristictestworld2))
)

