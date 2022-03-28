;;; -*- lexical-binding: t -*-


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(require '+autoload)

(defvar +package '(counsel magit company eglot elpy))

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

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
      initial-scratch-message ";;; -*- lexical-binding: t -*-\n\n")

(blink-cursor-mode -1)

(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (pixel-scroll-precision-mode 1))

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1))



(defalias 'w 'save-buffer)

(setq confirm-kill-emacs 'y-or-n-p
      auto-revert-check-vc-info t
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

(add-hook 'hexl-mode-hook
          (lambda ()
            (setq-local auto-save-visited-mode nil)))

(setq recentf-max-saved-items 200)

(recentf-mode 1)



(setq completion-ignore-case t)

(define-key minibuffer-local-completion-map "\s" "-")

(setq icomplete-compute-delay 2
      icomplete-max-delay-chars 4
      icomplete-delay-completions-threshold 10)

(icomplete-mode 1)

(advice-add 'ivy-read :around
            (lambda (func &rest args)
              (let (icomplete-mode)
                (apply func args))))

(global-set-key (kbd "C-M-_") 'dabbrev-completion)

(global-set-key (kbd "<f2>") 'listify-tab-completion)



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

(simple-x-default-keybindings)

(require 'eve)

(define-key goto-map "f" 'eve-gf)
(define-key goto-map "t" 'eve-gt)
(define-key goto-map "w" 'eve-gw)
(define-key goto-map "e" 'eve-ge)
(define-key goto-map "j" 'eve-gj)

(require 'eve-leader)



(defalias 'make 'compile)

(setq xref-search-program 'ripgrep)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(setq flymake-cc-command 'cc-x-flymake-cc-command)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "M-H") 'cc-help))

(setq eglot-extend-to-xref t)

(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-backends
      '(company-capf company-files (company-dabbrev-code company-keywords) company-dabbrev)
      company-global-modes
      '(lisp-interaction-mode emacs-lisp-mode c-mode c++-mode python-mode))

(global-company-mode 1)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)



(setq dired-listing-switches "-alh")

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")

(with-eval-after-load 'erc-backend
  (require 'erc-sasl))
