;;; -*- lexical-binding: t -*-



;;* lisp
(use-package paredit
  :hook (lisp-data-mode . paredit-mode))

(use-package macrostep
  :bind (:map lisp-data-mode-map ("C-c e" . macrostep-expand)))

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
  :ensure nil
  :init
  (defvar python-shell-interpreter "ipython")
  (defvar python-shell-interpreter-args "--simple-prompt")
  (setq python-indent-guess-indent-offset nil)
  :config
  (defun +python-comment-inline-offset-setup ()
    (setq-local comment-inline-offset 2))
  (add-hook 'python-mode-hook '+python-comment-inline-offset-setup)

  ;; fix: `comint-last-prompt' was disappeared while completing
  (defun +python-shell-completion-at-point-around (func &rest args)
    (let ((saved-comint-last-prompt comint-last-prompt)
          (completion (apply func args)))
      (setq comint-last-prompt saved-comint-last-prompt)
      completion))
  (advice-add 'python-shell-completion-at-point
              :around '+python-shell-completion-at-point-around)

  (add-hook 'inferior-python-mode-hook '+maybe-enable-company-mode))

(use-package python-mls
  ;; `python-mls' didn't work on Windows,
  ;; see: https://github.com/jdtsmith/python-mls/issues/8
  :unless (eq system-type 'windows-nt)
  :hook (inferior-python-mode . python-mls-mode))

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
