;;; -*- lexical-binding: t -*-



;;* basic

;;** project
(use-package project :ensure nil)
(use-package project-x
  :ensure nil
  :config
  (project-x-mode 1))

;;** xref
(use-package xref
  :ensure nil
  :defer t
  :init
  (setq xref-search-program 'ripgrep))

;;** flymake
(use-package flymake
  :ensure nil
  :defer t
  :config
  (bind-keys :map flymake-mode-map
             ("M-n" . flymake-goto-next-error)
             ("M-p" . flymake-goto-prev-error)))
(use-package flymake-proc
  :ensure nil
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;;** eglot
(use-package eglot
  :ensure nil
  :defer t
  :init
  (setq eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-stay-out-of '(company)
        eglot-ignored-server-capabilities
        '(:hoverProvider :documentHighlightProvider)))

;;** yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-alias-to-yas/prefix-p nil)
  :config
  (yas-global-mode 1)
  (defun +auto-save-visited-predicate-yasnippet ()
    (and yas-minor-mode yas--active-snippets))
  (add-hook '+auto-save-visited-predicate-hook
            '+auto-save-visited-predicate-yasnippet))




;;* company

(use-package company
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-quick-access t
        company-tooltip-width-grow-only t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-transformers
        '(company-sort-prefer-same-case-prefix)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend)
        company-backends
        '(company-files
          (company-capf :with company-yasnippet)
          (company-dabbrev-code company-keywords :with company-yasnippet)
          (company-dabbrev company-yasnippet)))
  :config
  (defun +company-mode-on-override ()
    (when (derived-mode-p 'prog-mode)
      (company-mode 1)))
  (advice-add 'company-mode-on :override '+company-mode-on-override)
  (global-company-mode 1)
  (bind-key "<f2>" 'company-complete company-mode-map)
  (defun +auto-save-visited-predicate-company ()
    (and company-mode company-candidates))
  (add-hook '+auto-save-visited-predicate-hook
            '+auto-save-visited-predicate-company))

(defun +maybe-enable-company-mode ()
  (when global-company-mode
    (company-mode 1)))

(defun +company-set-backends (hook backends)
  (add-hook hook `(lambda ()
                    (setq-local company-backends ',backends)
                    (+maybe-enable-company-mode))))

(+company-set-backends 'eshell-mode-hook '(company-files))
(+company-set-backends 'sh-mode-hook '(company-files company-dabbrev))
