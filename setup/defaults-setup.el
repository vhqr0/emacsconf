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
      delete-by-moving-to-trash t
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-delete-by-moving-to-trash t)

(setq backup-directory-alist         `((".*" . ,(expand-file-name "backup/"    user-emacs-directory)  ))
      auto-save-file-name-transforms `((".*"   ,(expand-file-name "auto-save/" user-emacs-directory) t))
      lock-file-name-transforms      `((".*"   ,(expand-file-name "lock/"      user-emacs-directory) t))
      trash-directory                           (expand-file-name "trash/"     user-emacs-directory)    )

(defvar +auto-save-visited-predicate-hook nil)

(defun +auto-save-visited-predicate ()
  (not (run-hook-with-args-until-success '+auto-save-visited-predicate-hook)))

(setq auto-save-visited-interval 1
      auto-save-visited-predicate '+auto-save-visited-predicate
      remote-file-name-inhibit-auto-save-visited t)
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

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq global-hl-line-sticky-flag t)



;;* wrap

(setq-default truncate-lines t)

(setq word-wrap-by-category t)          ; for cjk wrap

(defun +fixup-whitespace-override ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context.

Override: fix join lines leave space between CJK chars."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (unless (or (looking-at "^\\|$\\|\\s)")
                (save-excursion
                  (forward-char -1)
                  (looking-at "$\\|\\s(\\|\\s'"))
                (and (looking-at "[[:multibyte:]]")
                     (save-excursion
                       (forward-char -1)
                       (looking-at "[[:multibyte:]]"))))
      (insert ?\s))))
(advice-add 'fixup-whitespace :override '+fixup-whitespace-override)



;;* repeat

(setq disabled-command-function nil)

(repeat-mode 1)

(with-eval-after-load 'dired
  (put 'dired-jump 'repeat-map nil))



;;* layout

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(meta))

(define-key tab-prefix-map "`" 'toggle-frame-tab-bar)

(winner-mode 1)



;;* completion

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq orderless-component-separator 'orderless-escapable-split-on-space)
(defun +minibuffer-setup-orderless ()   ; orderless
  (setq-local completion-styles '(orderless basic)))
(add-hook 'minibuffer-setup-hook '+minibuffer-setup-orderless)

(global-set-key "\C-\M-_" 'dabbrev-completion) ; for terminal

(global-set-key "\C-@"        'toggle-input-method)
(global-set-key (kbd "C-SPC") 'toggle-input-method)



;;* match

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(define-key isearch-mode-map (kbd "<f2>") 'isearch-occur)
(define-key isearch-mode-map "\M-." 'isearch-forward-symbol-at-point)



;;* maps

;;** help-map
(define-key help-map "t"  nil)
(define-key help-map "tt" 'load-theme)
(define-key help-map "tf" 'load-file)
(define-key help-map "tl" 'load-library)
(define-key help-map "j"  'find-library)
(define-key help-map "4j" 'find-library-other-window)
(define-key help-map "B"  'describe-keymap)

;;** ctl-x-4-map
(define-key ctl-x-4-map "j" 'dired-jump-other-window)

;;** ctl-x-x-map
(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "a" 'auto-save-visited-mode)

;;** ctl-x-l-map
(defvar ctl-x-l-map (make-sparse-keymap))
(define-key ctl-x-map "l" ctl-x-l-map)
(define-key ctl-x-l-map "b" 'ibuffer)
(define-key ctl-x-l-map "u" 'undo-tree-visualize) ; undo-tree



;;* extension

;;** simple-x
(simple-x-default-keybindings)

;;** project-x
(project-x-mode 1)

;;** avy
(setq avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")
(define-key goto-map ";" 'avy-resume)
(define-key goto-map "f" 'avy-goto-char)
(define-key goto-map "j" 'avy-goto-line)
(define-key goto-map "w" 'avy-goto-word-0)
