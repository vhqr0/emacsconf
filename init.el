;;; -*- lexical-binding: t -*-



(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require '+autoload)



;; (setq text-quoting-style 'grave)
;; (startup--setup-quote-display)

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



(define-key minibuffer-local-completion-map " " "-")

(defalias 'w    'save-buffer)
(defalias 'mak  'compile)
(defalias 'make 'compile)



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



(defvar +kill-ring-display-buffer "*kill-ring-display*")

(defun +kill-ring-display ()
  (interactive)
  (switch-to-buffer-other-window +kill-ring-display-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (text kill-ring)
      (insert text "\n\n\C-l\n\n")))
  (set-buffer-modified-p nil)
  (beginning-of-buffer)
  (view-mode 1))

(global-set-key (kbd "C-x y") '+kill-ring-display)



(define-key special-mode-map "n" 'next-line)
(define-key special-mode-map "p" 'previous-line)

(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map "j"    'next-line)
  (define-key view-mode-map "k"    'previous-line)
  (define-key view-mode-map "h"    'backward-char)
  (define-key view-mode-map "l"    'forward-char)
  (define-key view-mode-map "\C-p" 'listify-switch-to-buffer))

(global-set-key "\C-z" 'eve-change-mode-to-vi)

(defun +eve-setup ()
  (cond ((derived-mode-p 'special-mode 'compilation-mode 'dired-mode)
         (eve-jk-mode 1))
        ((derived-mode-p 'prog-mode 'text-mode 'fundamental-mode)
         (eve-change-mode-to-vi))))

(defun +eve-view-setup ()
  (if view-mode
      (when eve-current-mode
        (eve-change-mode-to-emacs))
    (+eve-setup)))

(add-hook 'after-change-major-mode-hook '+eve-setup)
(add-hook 'view-mode-hook '+eve-view-setup)

(with-eval-after-load 'eve
  (define-key eve-jk-mode-map "\C-p" 'listify-switch-to-buffer)
  (define-key eve-vi-mode-map "\C-p" 'listify-switch-to-buffer))

(global-set-key (kbd "<f2>") 'listify-tab-completion)
(global-set-key (kbd "<f5>") 'listify-switch-to-buffer)

(global-set-key (kbd "M-g r") 'avy-resume)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-0)
(global-set-key (kbd "M-g j") 'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "M-g j") 'avy-isearch)



(with-eval-after-load 'cc-mode
  (require 'cja-cc))

(global-set-key (kbd "C-c C-j") 'imenu)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)))

(setq eglot-ignored-server-capabilites '(:hoverProvider))

(setq company-idle-delay 0.15
      company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-if-just-one-frontend)
      company-backends
      '(company-files
        (company-dabbrev-code
         company-etags
         company-keywords)
        company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        `(company-capf ,@company-backends))))

(add-hook 'prog-mode-hook 'company-mode)



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

(defun +xdg-open (file)
  (call-process-shell-command (concat +xdg-open-program " " file)))

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (+xdg-open file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map "J" 'dired-goto-file)
  (define-key dired-mode-map "K" 'dired-kill-line)
  (define-key dired-mode-map "V" '+dired-do-xdg-open))

(with-eval-after-load 'project
  (setq project-switch-commands
        '((project-find-file "find file")
          (project-dired "dired")
          (project-vc-dir "vc-dir")
          (project-shell "shell"))))

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



(defun +org-xdg-open (file _link)
  (+xdg-open file))

(setq org-modules '(org-tempo)
      org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.pdf\\'\\|\\.x?html?\\'" . +org-xdg-open))
      org-export-backends '(html reveal latex)
      org-html-postamble nil
      org-special-ctrl-a/e t)

(with-eval-after-load 'org
  (define-key org-mode-map "<" "\C-q<"))
