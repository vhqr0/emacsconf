(require 'xref)
(require 'cl-lib)

;;;###autoload
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
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gtags)))
  (process-lines gtags-global-program "-c"))

(cl-defmethod xref-backend-definitions ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-dxa"))

(cl-defmethod xref-backend-references ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-rxa"))

(cl-defmethod xref-backend-apropos ((_backend (eql gtags)) symbol)
  (gtags--find-symbol symbol "-gixa"))

(defun gtags--completion-at-point-function ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring beg end))
             (flag (if completion-ignore-case "-ci" "-c")))
        `(,beg ,end ,(process-lines gtags-global-program flag prefix))))))

;;;###autoload
(define-minor-mode gtags-mode
  "Gtags completion at point backend and xref backend enabled mode."
  :global t
  :lighter " gtags"
  :group 'etags)

(defun gtags--enable-p ()
  (and gtags-mode
       (locate-dominating-file default-directory "GTAGS")))

(advice-add 'etags--xref-backend
            :override (lambda ()
                        (if (gtags--enable-p)
                            'gtags
                          'etags)))

(advice-add 'tags-completion-at-point-function
            :around (lambda (func)
                      (if (gtags--enable-p)
                          (gtags--completion-at-point-function)
                        (funcall func))))

(provide 'gtags)
