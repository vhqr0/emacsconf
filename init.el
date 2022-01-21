;;; -*- lexical-binding: t -*-



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib-tp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "theme" user-emacs-directory))

(require '+autoload)
(require '+autoload-tp)

(when (file-exists-p package-quickstart-file)
  (defvar package-activated-list nil)
  (load package-quickstart-file))



(setq visible-bell t)

(setq inhibit-splash-screen t)

(tool-bar-mode -1)

(blink-cursor-mode -1)

(pixel-scroll-precision-mode 1)

(load-theme 'zenburn)



(defalias 'w 'save-buffer)

(setq confirm-kill-emacs 'y-or-n-p
      auto-revert-check-vc-info t
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 10
      kept-new-versions 10
      delete-old-versions t
      backup-directory-alist '(("." . "~/.bak"))
      auto-save-file-name-transforms '((".*" "~/.bak/" t))
      lock-file-name-transforms '((".*" "~/.bak/" t)))

(setq auto-save-visited-interval 2)

(auto-save-visited-mode 1)

(setq recentf-max-saved-items 200)

(recentf-mode 1)

(save-place-mode 1)



(define-key minibuffer-local-completion-map "\s" "-")

(define-key minibuffer-local-map "\M-." 'terminalize-yank-symbol)

(setq icomplete-compute-delay 2
      icomplete-max-delay-chars 4
      icomplete-delay-completions-threshold 10)

(icomplete-mode 1)

(savehist-mode 1)



(setq-default indent-tabs-mode nil)

(setq show-paren-context-when-offscreen t)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-x M-r") 'raise-sexp)
(global-set-key (kbd "C-x M-d") 'delete-pair)



(setq disabled-command-function nil)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map "U" 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(winner-mode 1)

(setq tab-bar-select-tab-modifiers '(meta))

(tab-bar-mode 1)

(terminalize-mode 1)



(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)

(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)

(global-set-key (kbd "C-x l") list-misc-prefix-map)

(require 'eve)



(global-set-key (kbd "<f2>") 'listify-tab-completion)
(global-set-key (kbd "<f5>") 'listify-open)

(with-eval-after-load 'eve
  (define-key eve-vi-mode-map "\C-p" 'listify-open)
  (define-key eve-jk-mode-map "\C-p" 'listify-open))

(define-key minibuffer-local-map "\C-r" 'listify-history)

(with-eval-after-load 'comint
  (define-key comint-mode-map "\C-r" 'listify-history))

(with-eval-after-load 'em-hist
  (define-key eshell-hist-mode-map "\C-r" 'listify-history))



(defalias 'make 'compile)

(setq xref-search-program 'ripgrep)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(advice-add 'eglot--snippet-expansion-fn :override (lambda () 'yas-expand-snippet))

(add-hook 'eglot-managed-mode-hook 'yas-minor-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)



(setq company-idle-delay 0.15
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      company-frontends
      '(company-pseudo-tooltip-frontend)
      company-backends
      '(company-files company-dabbrev-code company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        `(company-capf ,@company-backends))))

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(global-set-key (kbd "C-x w") 'xclip)

(define-key ctl-x-x-map "o" 'xdg-open)

(setq dired-listing-switches "-alh")

(with-eval-after-load 'dired
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (define-key dired-mode-map "V" 'dired-do-xdg-open))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")

(setq default-input-method "pyim")
