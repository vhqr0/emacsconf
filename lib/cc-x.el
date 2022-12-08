(require 'xref)
(require 'cl-lib)



(defvar cc-command '("clang"))

;;;###autoload
(defun cc-x-flymake-cc-command ()
  `(,@cc-command "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                 "-fsyntax-only"
                 "-"))

;;;###autoload
(defun cc-x-help ()
  (interactive)
  (let ((buffer (get-buffer-create "*cc-help*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (save-excursion
      (when (looking-at-p "\\sw")
        (re-search-forward "\\sw\\_>"))
      (save-window-excursion
        (let ((min (point-min)) (max (point-max)))
          (widen)
          (unwind-protect
              (apply 'call-process-region
                     `(,(point-min) ,(point-max)
                       ,(car cc-command) nil ,buffer nil
                       ,@(cdr cc-command)
                       "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                       "-fsyntax-only"
                       "-Xclang"
                       ,(format "-code-completion-at=-:%d:%d"
                                (line-number-at-pos)
                                (1+ (current-column)))
                       "-"))
            (narrow-to-region min max)))))
    (with-current-buffer buffer
      (setq truncate-lines t
            buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(provide 'cc-x)
