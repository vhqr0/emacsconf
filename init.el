;;; -*- lexical-binding: t -*-


(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require '+autoload)


(setq text-quoting-style 'grave)
(startup--setup-quote-display 'grave)

(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)

(recentf-mode 1)

(global-set-key (kbd "C-x j") 'recentf-open-files)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-x M-r") 'raise-sexp)
(global-set-key (kbd "C-x M-d") 'delete-pair)


(setq disabled-command-function nil)

(setq view-read-only t)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map (kbd "U") 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(winner-mode 1)

(global-set-key (kbd "M-g r") 'avy-resume)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g j") 'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "M-g j") 'avy-isearch)

(global-set-key (kbd "<f2>") 'listify-tab-completion)


(global-set-key (kbd "C-c C-j") 'imenu)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

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
      company-backends '(company-files company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-capf company-files company-dabbrev))))

(add-hook 'prog-mode-hook 'company-mode)


(when (getenv "WSLENV")
  (setq +xclip-program "clip.exe"
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))

(defvar +xclip-program "xclip -selection clip")

(defun +xclip (beg end)
  (interactive "r")
  (call-shell-region beg end +xclip-program))

(global-set-key (kbd "C-x w") '+xclip)

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") '+dired-do-xdg-open))

(with-eval-after-load 'project
  (setq project-switch-commands
        '((project-find-file "find file")
          (project-dired "dired")
          (project-vc-dir "vc-dir")
          (project-shell "shell"))))

(defun rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: " "rg --no-heading " 'grep-history)
   'grep-mode))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")


(defun +org-xdg-open (file _link)
  (call-process-shell-command (concat "xdg-open " file)))

(setq org-modules '(org-tempo)
      org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.pdf\\'\\|\\.x?html?\\'" . +org-xdg-open))
      org-export-backends '(html reveal latex)
      org-html-postamble nil
      org-special-ctrl-a/e t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") "\C-q<"))
