;;; -*- lexical-binding: t -*-



;;* cc
(defvar +flymake-cc-program "clang")
(defvar +flymake-cc-args nil)
(defun +flymake-cc-command ()
  `(,+flymake-cc-program "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                         "-fsyntax-only"
                         "-fno-color-diagnostics"
                         "-fno-caret-diagnostics"
                         "-fno-diagnostics-show-option"
                         ,@+flymake-cc-args
                         "-"))
(setq flymake-cc-command '+flymake-cc-command)

;;* python
(defvar +ipython-program "ipython")
(setq python-indent-guess-indent-offset nil ; inhibit verbose
      python-shell-interpreter +ipython-program
      python-shell-interpreter-args "--simple-prompt")
(defun +python-comment-inline-offset-setup ()
  (setq-local comment-inline-offset 2))
(add-hook 'python-mode-hook '+python-comment-inline-offset-setup)
(unless (eq system-type 'windows-nt)
  (add-hook 'inferior-python-mode-hook '+maybe-enable-company-mode)
  (add-hook 'inferior-python-mode-hook 'python-mls-mode) ; python-mls
  )

;;* web
(setq js-indent-level 2
      css-indent-offset 2)

;;* markdown
(setq markdown-fontify-code-blocks-natively t)
