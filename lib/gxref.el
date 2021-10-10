(require 'xref)
(require 'cl-lib)

(defvar gxref-global-program "global")

(defun gxref--line-to-xref (line)
  (when (string-match "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)" line)
    (xref-make (match-string 4 line)
               (xref-make-file-location (match-string 3 line)
                                        (string-to-number (match-string 2 line))
                                        0))))

(defun gxref--find-symbol (symbol arg)
  (remove nil
          (mapcar 'gxref--line-to-xref
                  (process-lines gxref-global-program
                                 arg
                                 (shell-quote-argument symbol)))))

(defun gxref-xref-backend () 'gxref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gxref)))
  (let ((symbol (symbol-at-point)))
    (when symbol
      (symbol-name symbol))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gxref)))
  (process-lines gxref-global-program "-c"))

(cl-defmethod xref-backend-definitions ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-dxa"))

(cl-defmethod xref-backend-references ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-rxa"))

(cl-defmethod xref-backend-apropos ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-gixa"))

;;;###autoload
(defun gxref-find-definitions ()
  (interactive)
  (let ((xref-backend-functions '(gxref-xref-backend)))
    (call-interactively 'xref-find-definitions)))

;;;###autoload
(defun gxref-find-references ()
  (interactive)
  (let ((xref-backend-functions '(gxref-xref-backend)))
    (call-interactively 'xref-find-references)))

;;;###autoload
(defun gxref-find-apropos ()
  (interactive)
  (let ((xref-backend-functions '(gxref-xref-backend)))
    (call-interactively 'xref-find-apropos)))

(provide 'gxref)
