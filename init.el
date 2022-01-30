;;; -*- lexical-binding: t -*-



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib-tp" user-emacs-directory))

(require '+autoload)
(require '+autoload-tp)



(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)



(defalias 'w 'save-buffer)

(setq confirm-kill-emacs 'y-or-n-p
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



(setq completion-ignore-case t)

(define-key minibuffer-local-map "\M-." 'minibuffer-yank-symbol)

(define-key minibuffer-local-completion-map "\s" "-")

(setq icomplete-compute-delay 2
      icomplete-max-delay-chars 4
      icomplete-delay-completions-threshold 10)

(icomplete-mode 1)



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

(global-set-key (kbd "C-x 9") 'rotate-window)

(global-set-key "\M-E" 'eshell-dwim)



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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-x l") list-misc-prefix-map)

(require 'eve)



(global-set-key (kbd "C-M-/") 'listify-dabbrev-completion)
(global-set-key (kbd "C-M-_") 'listify-dabbrev-completion)

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

(setq flymake-cc-command 'cc-util-flymake-cc-command)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c m") 'flymake-mode)
  (define-key c-mode-base-map (kbd "C-c c") 'cc-util-complete)
  (define-key c-mode-base-map (kbd "C-c h") 'cc-util-help)
  (define-key c-mode-base-map (kbd "C-c f") 'cc-util-format))

(setq eglot-extend-to-xref t)

(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-backends '(company-capf))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'eglot--managed-mode-hook 'company-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)



(define-key ctl-x-x-map "o" 'xdg-open)

(setq dired-listing-switches "-alh")

(with-eval-after-load 'dired
  (define-key dired-mode-map "V" 'dired-do-xdg-open))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")



(when (getenv "WSLENV")
  (setq xclip-program "clip.exe"
        xdg-open-program (expand-file-name "bin/wsl-xdg-open" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
