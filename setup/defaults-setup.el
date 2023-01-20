;;; -*- lexical-binding: t -*-



;;* ui

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

(setq frame-title-format
      '((:eval (let ((buffer (if (window-minibuffer-p)
                                 (window-buffer (minibuffer-selected-window))
                               (current-buffer))))
                 (or (buffer-file-name buffer)
                     (buffer-name buffer))))))



;;* files

(setq auto-revert-check-vc-info t
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 10
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t
      delete-by-moving-to-trash t)

(setq backup-directory-alist         `((".*" . ,(expand-file-name "backup/"    user-emacs-directory)  ))
      auto-save-file-name-transforms `((".*"   ,(expand-file-name "auto-save/" user-emacs-directory) t))
      lock-file-name-transforms      `((".*"   ,(expand-file-name "lock/"      user-emacs-directory) t))
      trash-directory                           (expand-file-name "trash/"     user-emacs-directory)    )

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)
(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASV"))

(setq recentf-max-saved-items 200)
(recentf-mode 1)



;;* indent and pair

(setq-default indent-tabs-mode nil)

(setq show-paren-context-when-offscreen t)
(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-c r") 'raise-sexp)
(global-set-key (kbd "C-c d") 'delete-pair)



;;* repeat

(setq disabled-command-function nil)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map "U" 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(with-eval-after-load 'winner
  (define-key winner-repeat-map "u" 'winner-undo)
  (define-key winner-repeat-map "U" 'winner-redo))

(with-eval-after-load 'dired
  (put 'dired-jump 'repeat-map nil))



;;* layout

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(super))

(dotimes (i 10)
  (define-key goto-map (format "t%d" i) (kbd (format "s-%d" i))))

(define-key tab-prefix-map "`" 'toggle-frame-tab-bar)

(winner-mode 1)



;;* minibuffer

(define-key minibuffer-local-completion-map "\s" "-")

(setq icomplete-compute-delay 2
      icomplete-max-delay-chars 4
      icomplete-delay-completions-threshold 10)

(icomplete-mode 1)



;;* completion

(setq completion-ignore-case t)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        yas-hippie-try-expand           ; Notice: yasnippet
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key "\M-/" 'hippie-expand)
(global-set-key (kbd "C-M-_") 'dabbrev-completion) ; for terminal



;;* match

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key search-map (kbd "<f2>") 'occur-at-point)
(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)
(define-key isearch-mode-map "\M-." 'isearch-forward-symbol-at-point)



;;* maps

;;** help
(define-key help-map "t"  nil)
(define-key help-map "tt" 'load-theme)
(define-key help-map "tf" 'load-file)
(define-key help-map "tl" 'load-library)
(define-key help-map "j"  'find-library)
(define-key help-map "4j" 'find-library-other-window)

;;** ctl-x-4-map
(define-key ctl-x-4-map "j" 'dired-jump-other-window)

;;** ctl-x-x-map
(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)
(define-key ctl-x-x-map "a" 'auto-save-visited-mode)

;;** ctl-x-l-map
(defvar ctl-x-l-map (make-sparse-keymap))
(define-key ctl-x-map "l" ctl-x-l-map)
(define-key ctl-x-l-map "b" 'ibuffer)



;;* aliases
(defalias 'oc 'occur)
(defalias 'qr 'query-replace-regexp)
(defalias 'make 'compile)
(defalias 'remake 'recompile)



;;* extension

;;** simple-x
(simple-x-default-keybindings)

;;** avy
(setq avy-single-candidate-jump nil
      avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")
(setq aw-dispatch-when-more-than 1)
(define-key isearch-mode-map "\M-g" 'avy-isearch)
(define-key goto-map "." 'avy-resume)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "f" 'avy-goto-char)
(define-key goto-map "w" 'avy-goto-word-0)
(define-key goto-map "o" 'ace-window)
(define-key goto-map "l" 'link-hint-open-link)

;;** projectile
(projectile-mode 1)
(define-key projectile-command-map "\e" nil)
(define-key projectile-command-map "x" 'project-execute-extended-command)
(define-key projectile-command-map "e" 'projectile-run-eshell)
(define-key projectile-command-map "s" 'projectile-run-shell)
