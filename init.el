;;; -*- lexical-binding: t -*-



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require '+autoload)

(when (file-exists-p package-quickstart-file)
  (defvar package-activated-list nil)
  (load package-quickstart-file))



(setq visible-bell t)

(setq inhibit-splash-screen t)

(setq describe-bindings-outline nil)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)



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

(savehist-mode 1)

(defalias 'w 'save-buffer)
(defalias 'make 'compile)

(define-key minibuffer-local-completion-map "\s" "-")

(define-key minibuffer-local-map "\C-r" 'insert-register)

(defun +minibuffer-yank-symbol ()
  (interactive)
  (when (window-minibuffer-p)
    (let ((symbol (with-selected-window (minibuffer-selected-window)
                    (thing-at-point 'symbol))))
      (when symbol
        (insert symbol)))))

(define-key minibuffer-local-map "\M-." '+minibuffer-yank-symbol)

(setq icomplete-compute-delay 2
      icomplete-max-delay-chars 4
      icomplete-delay-completions-threshold 10)

(icomplete-mode 1)



(setq-default indent-tabs-mode nil)

(setq show-paren-context-when-offscreen t)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-x M-r")   'raise-sexp)
(global-set-key (kbd "C-x M-d")   'delete-pair)
(global-set-key (kbd "C-x M-DEL") 'backward-kill-sexp)



(setq disabled-command-function nil)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map "U" 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(define-key tab-bar-switch-repeat-map "t" 'tab-next)
(define-key tab-bar-switch-repeat-map "T" 'tab-previous)

(winner-mode 1)



(setq view-read-only t)

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(setq lazy-highlight-buffer t
      lazy-highlight-cleanup nil)

(defalias 'hls 'lazy-highlight-cleanup)

(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)

(global-set-key (kbd "C-x m") 'list-imenu)
(global-set-key (kbd "C-x y") 'list-kill-ring)

(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)

(require 'eve)



(global-set-key (kbd "<f2>") 'listify-tab-completion)
(global-set-key (kbd "<f5>") 'listify-open)

(with-eval-after-load 'eve
  (define-key eve-vi-mode-map "\C-p" 'listify-open)
  (define-key eve-jk-mode-map "\C-p" 'listify-open))

(with-eval-after-load 'view
  (define-key view-mode-map "\C-p" 'listify-open))

(define-key minibuffer-local-map (kbd "<f5>") 'listify-minibuffer-history)



(setq xref-search-program 'ripgrep)

(global-set-key "\M-\"" 'xref-find-apropos)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(setq emmet-mode-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-RET") 'emmet-expand-line)
        (define-key map (kbd "C-c n") 'emmet-next-edit-point)
        (define-key map (kbd "C-c p") 'emmet-prev-edit-point)
        (define-key map (kbd "C-c C-c w") 'emmet-wrap-with-markup)
        map))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)



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



(defvar +xclip-program "xclip -selection clip")

(defun +xclip (beg end)
  (interactive "r")
  (call-shell-region beg end +xclip-program))

(global-set-key (kbd "C-x w") '+xclip)

(defvar +xdg-open-program "xdg-open")

(defun +xdg-open (&optional file)
  (interactive `(,(or buffer-file-name default-directory)))
  (when file
    (call-process-shell-command (concat +xdg-open-program " " file))))

(define-key ctl-x-x-map "o" '+xdg-open)

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (+xdg-open file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-kill-line)
  (define-key dired-mode-map "V" '+dired-do-xdg-open))

(defun +grep-rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: " "rg --no-heading " 'grep-history)
   'grep-mode))

(defalias 'rg '+grep-rg)

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")

(when (getenv "WSLENV")
  (setq +xclip-program "clip.exe"
        +xdg-open-program (expand-file-name "bin/wsl-xdg-open" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
