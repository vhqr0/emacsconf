(defvar cc-util-cc-command "clang")

(defvar cc-util-cc-flags nil)

(defvar cc-util-completion-in-region-function 'listify-completion-in-region)

;;;###autoload
(defun cc-util-flymake-cc-command ()
  `(,cc-util-cc-command ,@cc-util-cc-flags
                        "-x"
                        ,(if (derived-mode-p 'c++-mode) "c++" "c")
                        "-fsyntax-only"
                        "-"))

(defun cc-util-completion-at-point-function ()
  (let ((buffer (generate-new-buffer "*cc-output*"))
        choices)
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
          (narrow-to-region min max))))
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

(provide 'cc-util)
