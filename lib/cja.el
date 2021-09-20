(defun cja-expand ()
  (when (eq last-command-event ?\C-j)
    (delete-char (- (length last-abbrev-text)))
    (let* ((value (symbol-value last-abbrev))
           (text (car value))
           (count (cdr value)))
      (insert text)
      (when count
        (backward-char count)))
    t))

(put 'cja-expand 'no-self-insert t)

(defun cja-add-abbrevs (table abbrevs)
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cdr abbrev) 'cja-expand :system t)))

(defun cja-add-normal-abbrevs (table abbrevs)
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cdr abbrev) nil :system t)))

(provide 'cja)
