(require 'xref)
(require 'cl-lib)

(defvar gtags-global-program "global")

(defun gtags--line-to-xref (line)
  (when (string-match "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)" line)
    (xref-make (match-string 4 line)
               (xref-make-file-location (match-string 3 line)
                                        (string-to-number (match-string 2 line))
                                        0))))

(defun gtags--find-symbol (symbol arg)
  (remove nil
          (mapcar 'gtags--line-to-xref
                  (process-lines gtags-global-program
                                 arg
                                 (shell-quote-argument symbol)))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gtags)))
  (let ((symbol (symbol-at-point)))
    (when symbol
      (symbol-name symbol))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gtags)))
  (process-lines gtags-global-program "-c"))

(cl-defmethod xref-backend-definitions ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-dxa"))

(cl-defmethod xref-backend-references ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-rxa"))

(cl-defmethod xref-backend-apropos ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-gixa"))

;;;###autoload
(define-minor-mode gtags-mode
  "Gtags xref backends enabled mode."
  :global t
  :lighter " gtags"
  :group 'etags)

(advice-add 'etags--xref-backend
            :around (lambda (_func)
                      (if (and gtags-mode
                               (eq 0 (shell-command (concat gtags-global-program " -p"))))
                          'gtags
                        'etags)))

(provide 'gtags)
