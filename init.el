;;; -*- lexical-binding: t -*-



(defvar +package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defvar +package
  '(evil
    evil-surround
    evil-snipe
    avy
    ace-window
    ivy
    swiper
    counsel
    hydra
    ivy-hydra
    amx
    wgrep
    magit
    forge
    yasnippet
    company
    eglot
    markdown-mode
    htmlize
    org-roam
    org-roam-ui
    cmake-mode
    emmet-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require '+autoload)

(setq package-quickstart t
      package-archives +package-archives)

(require 'package)

(let (package-refreshed-p)
  (dolist (pkg +package)
    (unless (package-installed-p pkg)
      (unless package-refreshed-p
        (setq package-refreshed-p t)
        (package-refresh-contents))
      (package-install pkg)))
  (when package-refreshed-p
    (package-quickstart-refresh)))



(setq visible-bell t)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil
      confirm-kill-emacs 'y-or-n-p)

(setq text-quoting-style 'grave)

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(unless (eq system-type 'windows-nt)
  (xterm-mouse-mode 1))



(setq auto-revert-check-vc-info t
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 10
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.bak"))
      auto-save-file-name-transforms '((".*" "~/.bak/" t))
      lock-file-name-transforms '((".*" "~/.bak/" t)))

(setq auto-save-visited-interval 1)

(auto-save-visited-mode 1)

(setq recentf-max-saved-items 200)

(recentf-mode 1)



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

(windmove-default-keybindings)



(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)
(define-key isearch-mode-map "\M-." 'isearch-forward-symbol-at-point)

(define-key goto-map "m" 'pop-to-mark-command)

(define-key help-map "B" 'describe-keymap)
(define-key help-map "j" 'find-library)
(define-key help-map "J" 'load-theme)

(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)
(define-key ctl-x-x-map "a" 'auto-save-visited-mode)

(define-key ctl-x-4-map "j" 'dired-jump-other-window)

(setq avy-single-candidate-jump nil)

(define-key goto-map "." 'avy-resume)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "k" 'avy-goto-char-timer)
(define-key goto-map "o" 'avy-goto-symbol-1)
(define-key isearch-mode-map "\M-g" 'avy-isearch)

(setq aw-dispatch-when-more-than 1)

(global-set-key "\M-o" 'ace-window)

(simple-x-default-keybindings)

(require 'evil-setup)
(require 'ivy-setup)



(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq completion-ignore-case t)

(setq xref-search-program 'ripgrep)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(setq flymake-cc-command 'cc-x-flymake-cc-command)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-h") 'cc-help))

(defvar +python-skip-set-ipython nil)

(unless +python-skip-set-ipython
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt"))

(setq js-indent-level 2)

(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(setq eglot-extend-to-xref t
      eglot-events-buffer-size 0
      eglot-stay-out-of '(company))

(yas-global-mode 1)
(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(define-key abbrev-map "s" 'yas-insert-snippet)
(define-key abbrev-map "v" 'yas-visit-snippet-file)

(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      company-backends
      '(company-capf
        company-files
        (company-dabbrev-code company-keywords)
        company-dabbrev))

(global-company-mode 1)



(defun set-company-backends (hook backends)
  (add-hook hook `(lambda ()
                    (setq-local company-backends ',backends))))

(defun add-company-backends (hook backends)
  (set-company-backends hook (append backends company-backends)))

(set-company-backends 'eshell-mode-hook '(company-files))
(add-company-backends 'cmake-mode-hook '(company-cmake))

(defun company-mode-on-override ()
  (when (or (derived-mode-p 'prog-mode 'text-mode 'eshell-mode))
    (company-mode 1)))

(advice-add 'company-mode-on :override 'company-mode-on-override)

(defun yas-maybe-expand-company-filter (_cmd)
  (cond ((not yas-minor-mode)
         'company-complete-common)
        ((yas-active-snippets)
         'yas-next-field-or-maybe-expand)
        ((let ((yas--condition-cache-timestamp (current-time)))
           (yas--templates-for-key-at-point))
         'yas-expand)
        (t
         'company-complete-common)))

(defun yas-maybe-prev-company-filter (_cmd)
  (when (and yas-minor-mode
             (yas-active-snippets))
    'yas-prev-field))

(defvar yas-maybe-expand-company
  '(menu-item "" nil :filter yas-maybe-expand-company-filter))

(defvar yas-maybe-prev-company
  '(menu-item "" nil :filter yas-maybe-prev-company-filter))

(define-key company-active-map "\t" yas-maybe-expand-company)
(define-key company-active-map [tab] yas-maybe-expand-company)
(define-key company-active-map [backtab] yas-maybe-prev-company)



(require 'org-setup)



(setq dired-listing-switches "-lha")

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq magit-section-visibility-indicator '("..." . t))

(define-key vc-prefix-map "j" 'magit-status)
(define-key vc-prefix-map "f" 'magit-file-dispatch)

(setq ispell-dictionary "american")
