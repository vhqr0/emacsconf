;;; -*- lexical-binding: t -*-



;;* basic

;;** display-line-numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;** compile
(defalias 'make 'compile)

;;** project
(projectile-mode 1)

(define-key projectile-command-map "\e" nil)
(define-key projectile-command-map "x" 'project-execute-extended-command)
(define-key projectile-command-map "m" 'projectile-compile-project)
(define-key projectile-command-map "e" 'projectile-run-eshell)
(define-key projectile-command-map "s" 'projectile-run-shell)
(define-key evil-leader-map "p" projectile-command-map)

;;** xref
(setq xref-search-program 'ripgrep)

;;** flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))
(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;;** eglot
(setq eglot-extend-to-xref t
      eglot-events-buffer-size 0
      eglot-stay-out-of '(company)
      eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))

;;** yasnippet
(setq yas-alias-to-yas/prefix-p nil)
(yas-global-mode 1)
(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))
(define-key abbrev-map "s" 'yas-insert-snippet)
(define-key abbrev-map "v" 'yas-visit-snippet-file)



;;* company

(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      company-backends
      '(company-files
        (company-capf :with company-yasnippet)
        (company-dabbrev-code company-keywords :with company-yasnippet)
        (company-dabbrev company-yasnippet)))

(defun +company-set-backends (hook backends)
  (add-hook hook `(lambda ()
                    (setq-local company-backends ',backends)
                    (when global-company-mode
                      (company-mode 1)))))

(+company-set-backends 'eshell-mode-hook '(company-files))
(+company-set-backends 'sh-mode-hook '(company-files company-dabbrev))

(defun +company-mode-on-override ()
  (when (derived-mode-p 'prog-mode)
    (company-mode 1)))

(advice-add 'company-mode-on :override '+company-mode-on-override)

(global-company-mode 1)

(define-key company-mode-map (kbd "<f2>") 'company-complete)
