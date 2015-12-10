(defun output-html-header (outstream)
  (format outstream "~{~A~%~}"
	  '("<html>"
	    "  <head>"
	    "    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">"	    "  </head>"
	    "  <body>"
	    "    <div id=\"contents\">"
	    "      <table class=\"style\">")))
(defun output-html-tail (outstream)
  (format outstream "~{~A~%~}"
	  '("      </table>"
	    "    </div>"
	    "  </body>"
	    "</html>")))

(defun output-tr-begin (outstream)
  (format outstream "~A~%" "        <tr>"))
(defun output-tr-end (outstream)
  (format outstream "~A~%" "        </tr>"))
(defun output-td-begin (outstream)
  (format outstream "~A~%" "          <td>"))
(defun output-td-end (outstream)
  (format outstream "~A~%" "          </td>"))
