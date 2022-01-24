(defvar cc-util-cc-command "clang")

(defvar cc-util-cc-flags nil)

(defvar cc-util-format-command "clang-format")

(defvar cc-util-completion-in-region-function 'listify-completion-in-region)

;;;###autoload
(defun cc-util-flymake-cc-command ()
  `(,cc-util-cc-command ,@cc-util-cc-flags
                        "-x"
                        ,(if (derived-mode-p 'c++-mode) "c++" "c")
                        "-fsyntax-only"
                        "-"))

(defun cc-util-completion-to-buffer (buffer)
  (save-window-excursion
    (let ((min (point-min)) (max (point-max)))
      (widen)
      (unwind-protect
          (let ((args `(,(point-min) ,(point-max)
                        ,cc-util-cc-command nil ,buffer nil
                        ,@cc-util-cc-flags
                        "-x"
                        ,(if (derived-mode-p 'c++-mode) "c++" "c")
                        "-fsyntax-only"
                        "-Xclang"
                        ,(format "-code-completion-at=-:%d:%d"
                                 (line-number-at-pos)
                                 (1+ (- (point) (line-beginning-position))))
                        "-")))
            (apply 'call-process-region args))
        (narrow-to-region min max)))))

(defun cc-util-completion-at-point-function ()
  (let ((buffer (generate-new-buffer "*cc-output*"))
        choices)
    (cc-util-completion-to-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward
              "^COMPLETION: \\(\\(?:[[:word:]]\\|_\\)+\\) : .+$" nil t)
        (push (match-string 1) choices)))
    (kill-buffer buffer)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          `(,(car bounds) ,(cdr bounds) ,choices)
        `(,(point) ,(point) ,choices)))))

;;;###autoload
(defun cc-util-complete ()
  (interactive)
  (let ((completion-in-region-function cc-util-completion-in-region-function)
        (completion-at-point-functions '(cc-util-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun cc-util-help ()
  (interactive)
  (let ((buffer (get-buffer-create "*cc-help*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (save-excursion
      (when (looking-at-p "\\sw")
        (re-search-forward "\\sw\\_>"))
      (cc-util-completion-to-buffer buffer))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

;;;###autoload
(defun cc-util-format ()
  (interactive)
  (let ((row (line-number-at-pos))
        (col (- (point) (line-beginning-position))))
    (shell-command-on-region (point-min) (point-max) cc-util-format-command nil t)
    (forward-line (1- row))
    (forward-char (min col (- (line-end-position) (line-beginning-position))))))

(provide 'cc-util)
