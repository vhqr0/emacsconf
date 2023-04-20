;;; -*- lexical-binding: t -*-



(defvar gtags-global-program "global")

(defconst gtags-line-re "^[^ \t]+[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)")

(defvar gtags-compute-completion-table t)

(defvar gtags-completion-tables (make-hash-table :test 'equal))

(defvar-local gtags-directory nil)

(defvar-local gtags-completion-table nil)



(defun gtags-refresh ()
  (interactive)
  (clrhash gtags-completion-tables)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq gtags-directory nil
            gtags-completion-table nil))))

(defun gtags-directory (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or gtags-directory
        (let ((directory (locate-dominating-file default-directory "GTAGS")))
          (setq gtags-directory (or directory ""))))))

(defun gtags-completion-table (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or gtags-completion-table
        (let ((key (gtags-directory)))
          (unless (string-empty-p key)
            (let ((table (gethash key gtags-completion-tables)))
              (unless table
                (message (concat "gtags: compute completion table from " key))
                (setq table (process-lines gtags-global-program "-c"))
                (puthash key table gtags-completion-tables))
              (setq gtags-completion-table table)))))))

(defun gtags-lazy-completion-table ()
  (lambda (string pred action)
    (and gtags-compute-completion-table
         (complete-with-action action (gtags-completion-table) string pred))))

(defun gtags-line-to-xref (line)
  (when (string-match gtags-line-re line)
    (let* ((summary (match-string 3 line))
           (file (match-string 2 line))
           (linum (string-to-number (match-string 1 line)))
           (location (xref-make-file-location file linum 0)))
      (xref-make summary location))))

(defun gtags-find-symbol (symbol arg)
  (let* ((lines (process-lines gtags-global-program arg symbol))
         (xrefs (mapcar 'gtags-line-to-xref lines)))
    (remove nil xrefs)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gtags)))
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gtags)))
  (gtags-lazy-completion-table))

(cl-defmethod xref-backend-definitions ((_backend (eql gtags)) symbol)
  (gtags-find-symbol symbol "-dxa"))

(cl-defmethod xref-backend-references ((_backend (eql gtags)) symbol)
  (gtags-find-symbol symbol "-rxa"))

(cl-defmethod xref-backend-apropos ((_backend (eql gtags)) symbol)
  (gtags-find-symbol symbol "-gixa"))

(defun gtags-completion-at-point-function ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds) (gtags-lazy-completion-table)))))



(defun gtags-override-xref ()
  (if (not (string-empty-p (gtags-directory)))
      'gtags
    'etags))

(defun gtags-around-capf (func)
  (if (not (string-empty-p (gtags-directory)))
      (gtags-completion-at-point-function)
    (funcall func)))

;;;###autoload
(define-minor-mode gtags-mode
  "Gtags completion at point backend and xref backend enabled mode."
  :global t
  :lighter " gtags"
  :group 'etags
  (if gtags-mode
      (progn
        (advice-add 'etags--xref-backend :override 'gtags-override-xref)
        (advice-add 'tags-completion-at-point-function :around 'gtags-around-capf))
    (advice-remove 'etags--xref-backend 'gtags-override-xref)
    (advice-remove 'tags-completion-at-point-function 'gtags-around-capf)))



(provide 'gtags)
