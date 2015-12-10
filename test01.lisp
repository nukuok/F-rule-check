;;;;
(ql:quickload :cl-ppcre)

;;;;
(load "test01-html.lisp")
(load "color-rule.lisp")

;;;;
(defvar *index-format*)
(setf *index-format* ".list")
(defvar *file-format*)
(setf *file-format* ".article")
(defvar *html-format*)
(setf *html-format* ".html")

(defvar *file-list*)
(setf *file-list* (list "TS29165"))

;;;;
(defun remove-last-char (text)
  (subseq text 0 (- (length text) 1)))

(defun equal-initial-part (short long &optional additional-function)
  (and (>= (length long) (length short))
       (equal short (subseq long 0 (length short)))
       (if additional-function
	   (funcall additional-function short long)
	   1)))

(defun one-more-char (short long)
  (let ((temp-position (length short))
	(temp-long (coerce long 'list)))
    (or (equal (nth temp-position temp-long) #\.)
	(equal (nth temp-position temp-long) #\Tab)
	(equal (nth temp-position temp-long) #\ ))))

(defun show-color (line-to-be-colored color)
  (lambda (line-to-be-replaced)
    (cl-ppcre:regex-replace-all
     line-to-be-colored
     line-to-be-replaced
     (list
      (concatenate 'string "<strong><font color=" color ">")
      :match
      "</font></strong>"))))


(defun show-color-for-a-line (initial-line color-rule)
  (if (car color-rule)
      (show-color-for-a-line
       (funcall (show-color (caar color-rule)
			    (coerce (cadar color-rule) 'sequence))
		initial-line)
       (cdr color-rule))
      initial-line))

;;(print (show-color-for-a-line "TrGW MSC" *color-rule*))
;;(print (show-color-for-a-line "abc" *color-rule* 1))

;;;;
(defun create-index-list (filename)
  (with-open-file
      (instream (concatenate 'string filename *index-format*) :direction :input)
    (labels ((iter (in result)
	       (let ((temp (read-line in nil nil)))
		 (if temp
		     (iter in (cons (remove-last-char temp) result))
		     (reverse result)))))
      (iter instream nil))))

;;(print (create-index-list "TS29165"))


;;;;#### bug cannot output the article for the last item in index-list
(defun create-table-file (filename)
  (let ((index-list (create-index-list filename)))
    (with-open-file
	(instream (concatenate 'string filename *file-format*)
		  :direction :input)
      (with-open-file
	  (outstream (concatenate 'string filename *html-format*)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(output-html-header outstream)
	(output-tr-begin outstream)
	(output-td-begin outstream)
	(output-td-end outstream)
	(output-td-begin outstream)
	(loop for a-index in index-list do
	     (loop
		(let ((current-line (read-line instream nil "eofeofeof")))
		  (when (equal current-line "eofeofeof") (return))
		  (if (equal-initial-part a-index (remove-last-char current-line))
		      (progn
			(output-td-end outstream)
			(output-tr-end outstream)
			(output-tr-begin outstream)
			(output-td-begin outstream)
			(format outstream "~A<br>" (remove-last-char current-line))
			(output-td-end outstream)
			(output-td-begin outstream)
			(return))
		      (format outstream "~A<br>" (remove-last-char current-line))))))
	(output-td-end outstream)
	(output-tr-end outstream)
	(output-html-tail outstream)))))
      
;;;;#### recognize 4xx as 4: resolved by one-more-char
(defun create-table-file-old (filename)
  (let ((the-index-list (create-index-list filename)))
    (with-open-file
	(instream (concatenate 'string filename *file-format*)
		  :direction :input)
      (with-open-file
	  (outstream (concatenate 'string filename "-complete" *html-format*)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(output-html-header outstream)
	(labels ((iter (a-index index-list instream)
		   (let ((current-line (read-line instream nil "eofeofeof")))
		     (unless (equal current-line "eofeofeof")
		       (if (equal-initial-part
			    (car index-list)
			    (remove-last-char current-line)
			    #'one-more-char)
			 (progn
			   (when a-index
			     (output-td-end outstream) (output-tr-end outstream))
			   (output-tr-begin outstream) (output-td-begin outstream)
			   (format outstream "~A"
				   (show-color-for-a-line
				    (remove-last-char current-line)
				    *color-rule*))
				   
			   (output-td-end outstream) (output-td-begin outstream)
			   (iter (car index-list) (cdr index-list) instream))
			 (progn
			   (format outstream "~A"
				   (show-color-for-a-line
				    (remove-last-char current-line)
				    *color-rule*))
			   (iter a-index index-list instream)))))))
	  (iter nil the-index-list instream))
	(output-td-end outstream) (output-tr-end outstream)
	(output-html-tail outstream)))))

;;(create-table-file "TS29165")

;;;;#### remove needlss line
(defun create-table-file (filename)
  (let ((the-index-list (create-index-list filename)))
    (with-open-file
	(instream (concatenate 'string filename *file-format*)
		  :direction :input)
      (with-open-file
	  (outstream (concatenate 'string filename *html-format*)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(output-html-header outstream)
	(labels ((iter (a-index index-list instream)
		   (let ((current-line (read-line instream nil "eofeofeof")))
		     (unless (equal current-line "eofeofeof")
		       (if (equal-initial-part
			    (car index-list)
			    (remove-last-char current-line)
			    #'one-more-char)
			 (progn
			   (when a-index
			     (output-td-end outstream) (output-tr-end outstream))
			   (output-tr-begin outstream) (output-td-begin outstream)
			   (output-filter current-line *color-rule* outstream)
			   (output-td-end outstream) (output-td-begin outstream)
			   (iter (car index-list) (cdr index-list) instream))
			 (progn
			   (output-filter current-line *color-rule* outstream)
			   (iter a-index index-list instream)))))))
	  (iter nil the-index-list instream))
	(output-td-end outstream) (output-tr-end outstream)
	(output-html-tail outstream)))))

(defun output-filter (line color-rule outstream)
  (let ((result
	 (show-color-for-a-line
	  (remove-last-char line)
	  color-rule)))
    (when (changed-p result)
      (format outstream "~A" result))))
    

(defun changed-p (inarray)
  (let ((templength (length inarray)))
    (member '(#\/ #\s #\t #\r #\o #\n #\g)
	    (loop for x from 0 to (- templength 7)
	       collect (list (aref inarray x)
			     (aref inarray (+ x 1))
			     (aref inarray (+ x 2))
			     (aref inarray (+ x 3))
			     (aref inarray (+ x 4))
			     (aref inarray (+ x 5))
			     (aref inarray (+ x 6))))
	    :test #'equal)))
    
(create-table-file-old "TS29165")
(create-table-file "TS29165")
(create-table-file-old "IR95")
(create-table-file "IR95")
(create-table-file-old "IR65")
(create-table-file "IR65")
(create-table-file-old "IR67")
(create-table-file "IR67")
(create-table-file-old "JJ9030")
(create-table-file "JJ9030")
(create-table-file-old "JJ9031")
(create-table-file "JJ9031")




(defun remove-bad-lines (filename)
  (with-open-file (instream filename :direction :input)
    (let ((right-line-initial-char (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "a" "b" "c" "d" "e" "f" "i" "v")))
      (loop
	 (let* ((current-line (read-line instream nil "eofeofeof"))
	       (line-length (length current-line)))
	     (if (equal current-line "eofeofeof")
		 (return)
		 (when (member (subseq current-line 0 1) right-line-initial-char
			       :test #'equal)
		   (format t "~A~%" (subseq current-line 0 (- line-length 1))))))))))
