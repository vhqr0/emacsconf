;;; -*- lexical-binding: t -*-



(defvar +package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defvar +package
  '(evil
    evil-surround
    evil-snipe
    expand-region
    ivy
    swiper
    counsel
    avy
    ivy-avy
    ace-window
    hydra
    ivy-hydra
    amx
    wgrep
    magit
    yasnippet
    auto-yasnippet
    company
    eglot
    markdown-mode))

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
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(xterm-mouse-mode 1)



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

(setq avy-background t)

(define-key goto-map "." 'avy-resume)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "k" 'avy-goto-char-timer)
(define-key goto-map "o" 'avy-goto-symbol-1)
(define-key isearch-mode-map "\M-g" 'avy-isearch)

(setq aw-dispatch-always t)

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

(setq eglot-extend-to-xref t
      eglot-events-buffer-size 0
      eglot-stay-out-of '(company))

(yas-global-mode 1)
(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(global-set-key "\C-o" 'aya-open-line)
(define-key evil-insert-state-map "\C-o" 'aya-open-line)

(setq company-idle-delay 0
      company-minimum-prefix-length 3
      company-tooltip-align-annotations t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t
      company-backends
      '(company-capf company-files (company-dabbrev-code company-keywords) company-dabbrev)
      company-global-modes
      '(lisp-interaction-mode emacs-lisp-mode c-mode c++-mode python-mode))

(global-company-mode 1)



(setq dired-listing-switches "-lha")

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq magit-auto-revert-mode nil
      magit-section-visibility-indicator '("..." . t))

(setq ispell-dictionary "american")
