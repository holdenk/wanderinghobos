(declare (uses parse-input))
(declare (uses fail-safe))
(declare (uses simulate simulatetest))
(use test)
(use list-utils)
(use srfi-13)
(test-group "single-step"
						(foldl (lambda (acc name)
										 (let ((pre (file->world (string-append name ".map")))
													 (post (file->world (string-append name ".res"))))
											 (test name
														 (world-board post)
														 (world-board (simulate pre)))))
									 '()
									 (command-line-arguments)))

