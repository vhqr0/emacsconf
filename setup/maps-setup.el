;;; -*- lexical-binding: t -*-



;;* basic
(find-function-setup-keys)

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
(define-key ctl-x-x-map "H" 'symbol-overlay-mode) ; symbol-overlay

(defvar ctl-x-l-map (make-sparse-keymap))
(define-key ctl-x-map "l" ctl-x-l-map)
(define-key ctl-x-l-map "b" 'ibuffer)

;;* symbol-overlay
(defvar symbol-at-point-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i"       'symbol-overlay-put)
    (define-key map "u"       'symbol-overlay-remove-all)
    (define-key map "n"       'symbol-overlay-jump-next)
    (define-key map "p"       'symbol-overlay-jump-prev)
    (define-key map "N"       'symbol-overlay-jump-prev)
    (define-key map "<"       'symbol-overlay-jump-first)
    (define-key map ">"       'symbol-overlay-jump-prev)
    (define-key map "d"       'symbol-overlay-jump-to-definition)
    (define-key map "\t"      'symbol-overlay-switch-forward)
    (define-key map [tab]     'symbol-overlay-switch-forward)
    (define-key map [backtab] 'symbol-overlay-switch-backward)
    (define-key map "t"       'symbol-overlay-toggle-in-scope)
    (define-key map "r"       'symbol-overlay-rename)
    (define-key map "q"       'symbol-overlay-query-replace)
    (define-key map "s"       'symbol-overlay-isearch-literally)
    (define-key map "o"       'occur-at-point)
    map))
(setq symbol-overlay-map symbol-at-point-map)

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

;;* simple-x
(simple-x-default-keybindings)

;;* evil-leader
(defvar evil-leader-map (make-sparse-keymap))
