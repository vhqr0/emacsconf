(require 'xref)
(require 'cl-lib)



(defvar cc-command '("clang"))

;;;###autoload
(defun cc-x-flymake-cc-command ()
  `(,@cc-command "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                 "-fsyntax-only"
                 "-"))

;;;###autoload
(defun cc-help ()
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



(defvar global-program "global")

(defvar global-compute-completion-table t)

(defun global-line-to-xref (line)
  (when (string-match "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)" line)
    (xref-make (match-string 4 line)
               (xref-make-file-location (match-string 3 line)
                                        (string-to-number (match-string 2 line))
                                        0))))

(defun global-find-symbol (symbol arg)
  (remove nil
          (mapcar 'global-line-to-xref
                  (process-lines global-program
                                 arg
                                 (shell-quote-argument symbol)))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gtags)))
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gtags)))
  (when global-compute-completion-table
    (process-lines global-program "-c")))

(cl-defmethod xref-backend-definitions ((_backend (eql gtags)) symbol)
  (global-find-symbol symbol "-dxa"))

(cl-defmethod xref-backend-references ((_backend (eql gtags)) symbol)
  (global-find-symbol symbol "-rxa"))

(cl-defmethod xref-backend-apropos ((_backend (eql gtags)) symbol)
  (global-find-symbol symbol "-gixa"))

(defun gtags-completion-at-point-function ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring beg end))
             (flag (if completion-ignore-case "-ci" "-c")))
        `(,beg ,end ,(process-lines global-program flag prefix))))))



;;;###autoload
(define-minor-mode gtags-mode
  "Gtags completion at point backend and xref backend enabled mode."
  :global t
  :lighter " gtags"
  :group 'etags)

;;;###autoload
(put 'gtags-mode 'safe-local-variable 'booleanp)

(defun gtags-enable-p ()
  (and gtags-mode
       (locate-dominating-file default-directory "GTAGS")))

(advice-add 'etags--xref-backend
            :override (lambda ()
                        (if (gtags-enable-p)
                            'gtags
                          'etags)))

(advice-add 'tags-completion-at-point-function
            :around (lambda (func)
                      (if (gtags-enable-p)
                          (gtags-completion-at-point-function)
                        (funcall func))))



;;;###autoload
(defun tags-xref-find-definitions ()
  (interactive)
  (let ((global-compute-completion-table t)
        (xref-backend-functions '(etags--xref-backend)))
    (call-interactively 'xref-find-definitions)))

;;;###autoload
(defun tags-xref-find-references ()
  (interactive)
  (let ((global-compute-completion-table t)
        (xref-backend-functions '(etags--xref-backend)))
    (call-interactively 'xref-find-references)))
