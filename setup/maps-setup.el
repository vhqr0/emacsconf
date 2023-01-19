;;; -*- lexical-binding: t -*-



;;* basic
(define-key help-map "t"  nil)
(define-key help-map "tt" 'load-theme)
(define-key help-map "tf" 'load-file)
(define-key help-map "tl" 'load-library)
(define-key help-map "j"  'find-library)
(define-key help-map "4j" 'find-library-other-window)

(define-key ctl-x-4-map "j" 'dired-jump-other-window)

(define-key ctl-x-x-map "h" 'hl-line-mode)
(define-key ctl-x-x-map "s" 'whitespace-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "l" 'display-line-numbers-mode)
(define-key ctl-x-x-map "a" 'auto-save-visited-mode)

;;* avy
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

;;* ctl-x-l-map
(defvar ctl-x-l-map (make-sparse-keymap))
(define-key ctl-x-map "l" ctl-x-l-map)
(define-key ctl-x-l-map "b" 'ibuffer)

;;** projectile
(projectile-mode 1)
(define-key projectile-command-map "\e" nil)
(define-key projectile-command-map "x" 'project-execute-extended-command)
(define-key projectile-command-map "e" 'projectile-run-eshell)
(define-key projectile-command-map "s" 'projectile-run-shell)

;;* evil-leader
(defvar evil-leader-map (make-sparse-keymap))
