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
      trash-directory                           (expand-file-name "trash/"     user-emacs-directory)
      )

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

(with-eval-after-load 'dired
  (put 'dired-jump 'repeat-map nil))



;;* layout

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(control shift))

(dotimes (i 10)
  (global-set-key (kbd (format "C-c C-%d" i))
                  (kbd (format "C-S-%d" i))))

(define-key tab-prefix-map "`" 'toggle-frame-tab-bar)

(winner-mode 1)

(windmove-default-keybindings)



;;* completion, search, match

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

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)
(define-key isearch-mode-map "\M-." 'isearch-forward-symbol-at-point)
