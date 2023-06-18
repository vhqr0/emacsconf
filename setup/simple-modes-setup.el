;;; -*- lexical-binding: t -*-



;;* elisp
(use-package macrostep
  :bind (:map emacs-lisp-mode-map ("C-c e" . macrostep-expand)))

;;* cc
(use-package flymake-cc
  :ensure nil
  :defer t
  :init
  (defvar +flymake-cc-program "clang")
  (defvar +flymake-cc-args nil)
  :config
  (defun +flymake-cc-command ()
    `(,+flymake-cc-program "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                           "-fsyntax-only"
                           "-fno-color-diagnostics"
                           "-fno-caret-diagnostics"
                           "-fno-diagnostics-show-option"
                           ,@+flymake-cc-args
                           "-"))
  (setq flymake-cc-command '+flymake-cc-command))

;;* python
(use-package python
  :init
  (defvar python-shell-interpreter "ipython")
  (defvar python-shell-interpreter-args "--simple-prompt")
  (setq python-indent-guess-indent-offset nil)
  :config
  (defun +python-comment-inline-offset-setup ()
    (setq-local comment-inline-offset 2))
  (add-hook 'python-mode-hook '+python-comment-inline-offset-setup)
  (unless (eq system-type 'windows-nt)
    (add-hook 'inferior-python-mode-hook '+maybe-enable-company-mode)))

(unless (eq system-type 'windows-nt)
  (use-package python-mls
    :hook (inferior-python-mode . python-mls-mode)))


;;* web
(use-package js
  :ensure nil
  :defer t
  :init
  (setq js-indent-level 2))

(use-package css-mode
  :ensure nil
  :defer t
  :init
  (setq css-indent-offset 2))

;;* markdown
(use-package markdown-mode
  :defer t
  :init
  (setq markdown-fontify-code-blocks-natively t))

(use-package edit-indirect :defer t)
