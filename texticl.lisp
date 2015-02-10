(in-package :texticl)

(defmacro while (test &body body)
  `(loop (unless ,test (return)) ,@body))

(defvar *texticl-out* nil)
(defun out (&rest strings)
  (dolist (n strings) (write-sequence n *texticl-out* )))

(defparameter *block-elements*
  '("p" "h1" "h2" "h3" "h4" "h5" "h6"
    ("bq" "blockquote")
    "pre"))

(defun maybe-car (x) (if (consp x) (car x) x))
(defun translate-block-element (x) 
  (dolist (e *block-elements*)
    (if (and (consp e) (string-equal (car e) x))
	(return-from translate-block-element (cadr e))))
  x)

(defun texticl (text &optional (stream *standard-output*))
  (let ((block-re (format nil "^(~{~A~^|~})(?=\\.)"
			  (mapcar #'maybe-car *block-elements*)))
	(*texticl-out* stream))
    (dolist (para (cl-ppcre:split "\\n\\n" text))
      (let ((tagname
	     (cl-ppcre:scan-to-strings block-re para)))
	(cond (tagname
	       (let ((skip (1+ (length tagname)))
		     (tagname (translate-block-element tagname)))
		 (out "<" tagname ">") 
		 (parse-lists (subseq para skip))
		 (out "</" tagname ">")))
	      (t 
	       (out "<p>") (parse-lists para) (out "</p>")))))))

(defun read-paragraph (stream)
  (let ((string 
	 (with-output-to-string (o)
	   (loop 
	    (let ((line (read-line stream nil nil)))
	      (unless line (return))
	      (write-line line o)
	      (unless (> (length line) 0) (return)))))))
    (and (> (length string) 0) string)))

(defun compile-regexps (tokens)
  (loop for (re fn) in tokens
	collect (list (cl-ppcre:create-scanner re) fn)))
	
(defun texticl-stream (in &optional (out *standard-output*))
  (let ((block-re (cl-ppcre:create-scanner
		   (format nil "^(~{~A~^|~})(?=\\.)"
			   (mapcar #'maybe-car *block-elements*))))
	(*inline-tokens* (compile-regexps *inline-tokens*))
	(*texticl-out* out))
    (loop 
     (let ((para (read-paragraph in)))
       (unless para (return))
       (let ((tagname
	      (cl-ppcre:scan-to-strings block-re para)))
	 (cond (tagname
		(let ((skip (1+ (length tagname)))
		      (tagname (translate-block-element tagname)))
		  (out "<" tagname ">") 
		  (parse-lists (subseq para skip))
		  (out "</" tagname ">")))
	       (t 
		(out "<p>") (parse-lists para) (out "</p>"))))))))

(defun parse-lists (text)
  (let ((boundaries  (cl-ppcre:all-matches "(?m)^[#*]+" text))
	(open nil))
    (labels ((seen-p (marker) (position marker open :test 'string=))
	     (close-tags (marker)
	       (let ((p (if marker 
			    (position marker open :test 'string=)
			    -1)))
		 (unless p (return-from close-tags nil))
		 (let ((tags (reverse (subseq open (1+ p)))))
		   (dolist (i tags)
		     (out (if (eql (elt i 0) #\*) "</ul>" "</ol>") )))
		 (setf open (subseq open 0 (1+ p))))))
      (subst-inline (subseq text 0 (first boundaries)) *texticl-out*)
      (loop for (start p end) on boundaries by #'cddr
	    for el = (subseq text start p)
	    for tx  =  (subseq text (or p start) end)
	    do (progn
		 (unless (seen-p el) 
		   (out (if (eql (elt el 0) #\*) "<ul>" "<ol>"))
		   (setf open (nconc open (list el))))
		 (close-tags el)
		 (out "<li>")
		 (subst-inline tx *texticl-out*)))
      (close-tags nil))))

(macrolet ((r (args &body body)
	     `(lambda (text out match-start match-end ,@args)
	       (declare (ignorable text match-start match-end))
	       ,@body))
	   (=> (&rest args)
	     (cons 'progn
		   (loop for a in args
			 if (stringp a) collect `(write-sequence ,a out)
			 else collect `(subst-inline ,a out)))))
  (labels ((decorated-words (char)
	     (setf char (cl-ppcre:quote-meta-chars (string char)))
	     (format nil 
		     "(?ms)(\\W|^)~A(?=\\S)(.*?)(?<=\\S)~A(\\W|$)" 
		     char char)))
    (defparameter *inline-tokens* 
      `(("\"([^\"]*)\":(https?:\\/\\/[^ ]+)" ; link with label
	 ,(r (label url)
	     (format out "<a href=\"~A\" class=external>~A</a>" url label)))
	("((ftp|https?):\\/\\/[^ ]+)"  	;url, no label
	 ,(r (url scheme)
	     (declare (ignore scheme))
	     (format out "<a href=\"~A\" class=external>~A</a>" url url)))
	("(?<!\\w)@(.*?)@" 		;"computer" text
	 ,(r (body)
	     (format out "<tt>~A</tt>" (araneida:html-escape body))))
	("(?<!\\w)!([<>]?)(.*?)!"  
	 ,(r (align url)
	     (let ((align (cond ((equal align ">") "align=right")
				((equal align "<") "align=left")
				(t "" ))))
	       (format out "<img ~A src=\"~A\">" align url))))
	;; try not to maul stuff in html element attributes
	("(<[A-Za-z/][^>]*>)" ,(r (tag) (write-sequence tag out)))
	
	(,(decorated-words #\_) 
	 ,(r (s1 body s2)  (=> s1 "<i>" body "</i>" s2)))
	(,(decorated-words #\-) 
	 ,(r (s1 body s2)  (=> s1 "<s>" body "</s>" s2)))
	(,(decorated-words #\*)
	 ,(r (s1 body s2)  (=> s1 "<b>" body "</b>" s2)))))))
  
(defun subst-inline (text out) 
  (declare (optimize (speed 3))
	   (type string text))
  (let ((start 0))
    (loop
     (let ((first-match nil)
	   (first-re nil))
       (dolist (re *inline-tokens*)
	 (let ((match (multiple-value-list
		       (cl-ppcre:scan (car re) text :start start))))
	   (when (car match)
	     (when (or (not first-match)
		       (< (car match) (car first-match))
		       (and (= (car match) (car first-match))
			    (> (cadr match) (cadr first-match))))
	       (setf first-match match
		     first-re re)))))
       (cond (first-re
	      (destructuring-bind (m-s m-e r-s r-e) first-match
		(write-sequence text out :start start :end m-s)
		(let ((args (loop for s across r-s
				  and e across r-e
				  collect (and s e (subseq text s e)))))
		  (apply (second first-re) text out m-s m-e args)
		  (setf start m-e))))
	     (t
	      (write-sequence text out :start start)
	      (setf start nil)))
       (when (or (not start) (> start (length text)))
	 (return))))))

