; -*- indent-tabs-mode: nil -*- ;
;;MARIO
;;Its a me, mario! I do the plumbing between here and the C-world
(declare (unit mario))
(require-extension bind coops)
(bind* "#include \"./csimulate.h\"")

(define make-board-fuck (foreign-lambda c-pointer "make_board" integer integer s8vector))