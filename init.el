;;; -*- lexical-binding: t -*-



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib-tp" user-emacs-directory))

(require '+autoload)
(require '+autoload-tp)



(setq visible-bell t)

(setq inhibit-startup-screen t
      initial-scratch-message ";;; -*- lexical-binding: t -*-\n\n")

(blink-cursor-mode -1)

(menu-bar-mode -1)

(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

(load-theme 'modus-vivendi)



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

(setq auto-save-visited-interval 2)

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

(global-set-key (kbd "<f2>") 'listify-tab-completion)

(advice-add 'ivy-read :around
            (lambda (func &rest args)
              (let (icomplete-mode)
                (apply func args))))

(setq ivy-display-functions-alist '((t)))

(setq completion-in-region-function 'ivy-completion-in-region)



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



(setq ivy-use-virtual-buffers t)

(global-set-key (kbd "C-M-_") 'dabbrev-completion)

(define-key eve-leader-map "\s" 'execute-extended-command)

(define-key eve-leader-map "1" 'delete-other-windows)
(define-key eve-leader-map "2" 'split-window-below)
(define-key eve-leader-map "3" 'split-window-right)
(define-key eve-leader-map "0" 'delete-window)
(define-key eve-leader-map "o" 'other-window)
(define-key eve-leader-map "j" 'dired-jump)
(define-key eve-leader-map "g" 'magit)

(define-key eve-leader-map "." 'ivy-resume)
(define-key eve-leader-map "b" 'ivy-switch-buffer)
(define-key eve-leader-map "f" 'counsel-find-file)
(define-key eve-leader-map "p" 'counsel-git)
(define-key eve-leader-map "s" 'swiper)
(define-key eve-leader-map "S" 'counsel-rg)
(define-key eve-leader-map "i" 'counsel-imenu)
(define-key eve-leader-map "x" 'counsel-M-x)
(define-key eve-leader-map "y" 'counsel-yank-pop)
(define-key eve-leader-map "m" 'counsel-mark-ring)
(define-key eve-leader-map "r" 'counsel-register)

(define-key eve-leader-map "hb" 'counsel-descbinds)
(define-key eve-leader-map "hf" 'counsel-describe-function)
(define-key eve-leader-map "hv" 'counsel-describe-variable)
(define-key eve-leader-map "ho" 'counsel-describe-symbol)
(define-key eve-leader-map "hs" 'counsel-info-lookup-symbol)
(define-key eve-leader-map "hl" 'counsel-find-library)
(define-key eve-leader-map "ht" 'counsel-load-theme)



(defalias 'make 'compile)

(setq xref-search-program 'ripgrep)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-n" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-p" 'flymake-goto-prev-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(setq flymake-cc-command 'cc-x-flymake-cc-command)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c m") 'flymake-mode)
  (define-key c-mode-base-map (kbd "C-c h") 'cc-help))

(setq eglot-extend-to-xref t)

(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-backends '(company-capf))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'eglot--managed-mode-hook 'company-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)



(setq dired-listing-switches "-alh")

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")



(when (getenv "WSLENV")
  (setq xclip-program "clip.exe"
        xdg-open-program (expand-file-name "bin/wsl-xdg-open" user-emacs-directory)
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))
