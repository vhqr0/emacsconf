;;; -*- lexical-binding: t -*-



(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require '+autoload)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))



(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)



(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)

(setq recentf-max-saved-items 100)

(recentf-mode 1)

(defalias 'w 'save-buffer)
(defalias 'make 'compile)

(define-key minibuffer-local-completion-map "\s" "-")



(setq-default indent-tabs-mode nil)

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



(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)

(defun +project-switch ()
  (interactive)
  (let ((default-directory (project-prompt-project-dir))
        (command (lookup-key project-prefix-map
                             `[,(read-event "switch project: ")])))
    (when command
      (call-interactively command))))

(define-key project-prefix-map "p" '+project-switch)



(defun +kill-ring-display ()
  (interactive)
  (switch-to-buffer-other-window "*kill-ring-display*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (text kill-ring)
      (insert text "\n\n\C-l\n\n")))
  (set-buffer-modified-p nil)
  (beginning-of-buffer)
  (view-mode 1))

(global-set-key (kbd "C-x y") '+kill-ring-display)



(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key special-mode-map "n" 'next-line)
(define-key special-mode-map "p" 'previous-line)

(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map "g" nil)
  (dolist (key '("j" "k" "h" "l" "w" "W" "b" "B" "e" "E" "U"
                 "/" "?" "n" "N" "f" "F" "t" "T" ";" ":"
                 "gg" "G" "'" "`" "0" "$" "^"))
    (define-key view-mode-map key (intern (concat "eve-" key))))
  (define-key view-mode-map "gn" "\C-c\C-n")
  (define-key view-mode-map "gp" "\C-c\C-p")
  (define-key view-mode-map "."  'repeat)
  (define-key view-mode-map "y"  'eve-command-arg)
  (define-key view-mode-map "m"  'point-to-register)
  (define-key view-mode-map "v"  'set-mark-command)
  (define-key view-mode-map ":"  'execute-extended-command))

(global-set-key "\C-z" 'eve-change-mode-to-vi)

(defun +eve-setup ()
  (cond ((derived-mode-p 'special-mode 'compilation-mode 'dired-mode)
         (eve-jk-mode 1))
        ((derived-mode-p 'prog-mode 'text-mode 'fundamental-mode)
         (eve-change-mode-to-vi))
        ((derived-mode-p 'comint-mode)
         (eve-change-mode-to-insert))))

(defun +eve-view-setup ()
  (if view-mode
      (when eve-current-mode
        (eve-change-mode-to-emacs))
    (+eve-setup)))

(add-hook 'after-change-major-mode-hook '+eve-setup)
(add-hook 'view-mode-hook '+eve-view-setup)



(global-set-key (kbd "<f2>") 'listify-tab-completion)

(with-eval-after-load 'eve
  (define-key eve-jk-mode-map "\C-p" 'listify-switch-to-buffer)
  (define-key eve-vi-mode-map "\C-p" 'listify-switch-to-buffer))

(with-eval-after-load 'view
  (define-key view-mode-map "\C-p" 'listify-switch-to-buffer))



(with-eval-after-load 'eve
  (define-key eve-vi-mode-map "g." 'avy-resume)
  (define-key eve-vi-mode-map "gf" 'avy-goto-char)
  (define-key eve-vi-mode-map "gj" 'avy-goto-line)
  (define-key eve-vi-mode-map "gw" 'avy-goto-word-0)
  (define-key eve-vi-mode-map "go" 'avy-goto-symbol-1))

(with-eval-after-load 'view
  (define-key view-mode-map "g." 'avy-resume)
  (define-key view-mode-map "gf" 'avy-goto-char)
  (define-key view-mode-map "gj" 'avy-goto-line)
  (define-key view-mode-map "gw" 'avy-goto-word-0)
  (define-key view-mode-map "go" 'avy-goto-symbol-1))



(global-set-key (kbd "C-c j") 'imenu)

(define-key prog-mode-map (kbd "C-c f") 'flymake-mode)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)))

(setq eglot-ignored-server-capabilites '(:hoverProvider))



(setq company-idle-delay 0.15
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      company-frontends
      '(company-pseudo-tooltip-frontend)
      company-backends
      '(company-files
        (company-dabbrev-code
         company-keywords)
        company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        `(company-capf ,@company-backends))))

(with-eval-after-load 'company
  (define-key company-mode-map "\M-o" 'company-complete)
  (define-key company-active-map "\M-o" 'listify-company))

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(when (getenv "WSLENV")
  (setq +xclip-program "clip.exe"
        +xdg-open-program (expand-file-name "bin/xdg-open" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))

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

(setq xref-search-program 'ripgrep)

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
