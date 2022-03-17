(define-key eve-leader-map "\s" 'execute-extended-command)

(define-key eve-leader-map "h" help-map)
(define-key eve-leader-map "s" search-map)
(define-key eve-leader-map "g" goto-map)
(define-key eve-leader-map "r" ctl-x-r-map)
(define-key eve-leader-map "x" ctl-x-x-map)
(define-key eve-leader-map "n" narrow-map)
(define-key eve-leader-map "v" vc-prefix-map)
(define-key eve-leader-map "p" project-prefix-map)

(define-key eve-leader-map "1" 'delete-other-windows)
(define-key eve-leader-map "2" 'split-window-below)
(define-key eve-leader-map "3" 'split-window-right)
(define-key eve-leader-map "0" 'delete-window)
(define-key eve-leader-map "o" 'other-window)
(define-key eve-leader-map "9" 'rotate-window) ; simple-x
(define-key eve-leader-map "u" 'winner-undo)   ; winner
(define-key eve-leader-map "U" 'winner-redo)   ; winner
(define-key eve-leader-map "4" ctl-x-4-map)
(define-key eve-leader-map "t" tab-prefix-map)

(define-key eve-leader-map "k" 'kill-buffer)
(define-key eve-leader-map "j" 'dired-jump)
(define-key eve-leader-map "B" 'ibuffer)
(define-key eve-leader-map "5" 'query-replace-regexp)
(define-key eve-leader-map "c" 'compile)
(define-key eve-leader-map ";" 'eval-expression)

;; replaced by counsel
;; (define-key eve-leader-map "f" 'find-file)
;; (define-key eve-leader-map "b" 'switch-to-buffer)

;;; magit
(define-key eve-leader-map "V" 'magit)

;;; counsel
(setq ivy-use-virtual-buffers t)

(define-key eve-leader-map "b" 'ivy-switch-buffer)
(define-key eve-leader-map "f" 'counsel-find-file)

(define-key search-map "s" 'swiper)

(defvar counsel-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "." 'ivy-resume)
    (define-key map "x" 'counsel-M-x)
    (define-key map "g" 'counsel-rg)
    (define-key map "i" 'counsel-imenu)
    (define-key map "y" 'counsel-yank-pop)
    (define-key map "m" 'counsel-mark-ring)
    (define-key map "r" 'counsel-register)
    (define-key map "b" 'counsel-descbinds)
    (define-key map "f" 'counsel-describe-function)
    (define-key map "v" 'counsel-describe-variable)
    (define-key map "o" 'counsel-describe-symbol)
    (define-key map "s" 'counsel-info-lookup-symbol)
    (define-key map "t" 'counsel-load-library)
    (define-key map "T" 'counsel-load-theme)
    map))

(define-key eve-leader-map "l" counsel-prefix-map)

(provide 'eve-leader)
