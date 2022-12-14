;;; -*- lexical-binding: t -*-



(defvar +package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defvar +package
  '(evil
    evil-surround
    ivy
    swiper
    counsel
    hydra
    ivy-hydra
    helpful
    amx
    wgrep
    magit
    projectile
    counsel-projectile
    yasnippet
    company
    eglot
    markdown-mode
    edit-indirect
    htmlize
    org-roam
    emmet-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(let* ((lib-directory (expand-file-name "lib/" user-emacs-directory))
       (lib-autoload (expand-file-name "lib-autoload.el" lib-directory)))
  (add-to-list 'load-path lib-directory)
  (unless (file-exists-p lib-autoload)
    (make-directory-autoloads lib-directory lib-autoload))
  (load-file lib-autoload))

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
      delete-by-moving-to-trash t)

(let ((save-files-directory (expand-file-name "save/" user-emacs-directory)))
  (setq backup-directory-alist `((".*" . ,(expand-file-name "backup/" save-files-directory)))
        auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" save-files-directory) t))
        lock-file-name-transforms `((".*" ,(expand-file-name "lock/" save-files-directory) t))
        trash-directory (expand-file-name "trash/" save-files-directory)))

(setq auto-save-visited-interval 1)

(auto-save-visited-mode 1)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASV"))

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

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(control shift))

(dotimes (i 10)
  (global-set-key (kbd (format "C-c C-%d" i))
                  (kbd (format "C-S-%d" i))))

(define-key tab-prefix-map "`" 'toggle-frame-tab-bar)

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



(define-key help-map "j" 'find-library)
(define-key help-map "t" 'load-theme)

(define-key ctl-x-4-map "j" 'dired-jump-other-window)

(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)
(define-key ctl-x-x-map "a" 'auto-save-visited-mode)

(defvar ctl-x-l-map (make-sparse-keymap))
(define-key ctl-x-map "l" ctl-x-l-map)

(define-key ctl-x-l-map "b" 'ibuffer)



(define-key ctl-x-x-map "H" 'symbol-overlay-mode)

(defvar symbol-at-point-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'symbol-overlay-put)
    (define-key map "u" 'symbol-overlay-remove-all)
    (define-key map "n" 'symbol-overlay-jump-next)
    (define-key map "p" 'symbol-overlay-jump-prev)
    (define-key map "N" 'symbol-overlay-jump-prev)
    (define-key map "<" 'symbol-overlay-jump-first)
    (define-key map ">" 'symbol-overlay-jump-prev)
    (define-key map "d" 'symbol-overlay-jump-to-definition)
    (define-key map "H" 'symbol-overlay-switch-backward)
    (define-key map "L" 'symbol-overlay-switch-forward)
    (define-key map "t" 'symbol-overlay-toggle-in-scope)
    (define-key map "r" 'symbol-overlay-rename)
    (define-key map "q" 'symbol-overlay-query-replace)
    (define-key map "s" 'symbol-overlay-isearch-literally)
    (define-key map "o" 'occur-at-point)
    (define-key map "." 'swiper-thing-at-point)
    map))

(setq symbol-overlay-map symbol-at-point-map)



(setq avy-single-candidate-jump nil
      avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")

(define-key goto-map "." 'avy-resume)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "f" 'avy-goto-char)
(define-key goto-map "w" 'avy-goto-word-0)

(define-key isearch-mode-map "\M-g" 'avy-isearch)

(setq aw-dispatch-when-more-than 1)

(define-key goto-map "o" 'ace-window)

(define-key goto-map "l" 'link-hint-open-link)



(simple-x-default-keybindings)

(require 'evil-setup)
(require 'ivy-setup)

(projectile-mode 1)
(counsel-projectile-mode 1)

(define-key projectile-command-map "\e" nil)
(define-key projectile-command-map "x" 'project-execute-extended-command)
(define-key projectile-command-map "m" 'projectile-compile-project)
(define-key projectile-command-map "e" 'projectile-run-eshell)
(define-key projectile-command-map "s" 'projectile-run-shell)
(define-key evil-leader-map "p" projectile-command-map)



(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq completion-ignore-case t)

(global-set-key (kbd "C-M-_") 'dabbrev-completion)

(setq xref-search-program 'ripgrep)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(setq flymake-cc-command 'cc-x-flymake-cc-command)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c h") 'cc-x-help))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt")

(setq js-indent-level 2
      css-indent-offset 2)

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
      '(company-files
        (company-capf :with company-yasnippet)
        (company-dabbrev-code company-keywords :with company-yasnippet)
        (company-dabbrev company-yasnippet)))

(defun company-set-backends (hook backends)
  (add-hook hook `(lambda ()
                    (setq-local company-backends ',backends)
                    (when global-company-mode
                      (company-mode 1)))))

(company-set-backends 'eshell-mode-hook '(company-files))
(company-set-backends 'sh-mode-hook '(company-files company-dabbrev))

(defun company-mode-on-override ()
  (when (derived-mode-p 'prog-mode)
    (company-mode 1)))

(advice-add 'company-mode-on :override 'company-mode-on-override)

(global-company-mode 1)



(require 'org-setup)



(setq dired-listing-switches "-lha")

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq magit-section-visibility-indicator '("..." . t))

(define-key vc-prefix-map "j" 'magit-status)
(define-key vc-prefix-map "f" 'magit-file-dispatch)
(define-key vc-prefix-map "?" 'magit-dispatch)

(setq ispell-dictionary "american")
