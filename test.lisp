(in-package :texticl)

;;; test cases are selected on a fairly ad-hoc basis, consisting in
;;; the most part of whatever I typed into the repl to test whichever
;;; token I was adding

(defvar *test-cases* nil)
(when (probe-file "test-cases.lisp")
  (load "test-cases.lisp"))

(defun add-test (text)
  (let* ((got (with-output-to-string (o) (subst-inline text o)))
	 (el (assoc text *test-cases* :test #'string=)))
    (cond (el
	   (setf (cadr el) got))
	  (t
	   (push (list text got) *test-cases*)))
    (list text got)))

(defun save-tests ()
  (with-open-file (o "test-cases.lisp"
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (print '(in-package :texticl) o)
    (print `(let ((c ',*test-cases* )) (if (> (length c) (length *test-cases*)) (setf *test-cases* c) (warn "not overwriting, fewer tests"))) o)
    (terpri o)))

(defun run-tests ()  
  (loop for (text expected) in *test-cases*
	for actual = (with-output-to-string (o) (subst-inline text o))
	unless (string= actual expected)
	do (warn "test failed ~S" 
		 (list :text text :expected expected :got actual))))
(run-tests)
