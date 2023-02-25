;;; -*- lexical-binding: t -*-

(defvar +leader-map (make-sparse-keymap))

(define-key +leader-map "h" help-map)
(define-key +leader-map "g" goto-map)
(define-key +leader-map "s" search-map)
(define-key +leader-map "n" narrow-map)
(define-key +leader-map "v" vc-prefix-map)
(define-key +leader-map "p" project-prefix-map)
(with-eval-after-load 'projectile       ; projectile
  (define-key +leader-map "p" projectile-command-map))
(define-key +leader-map "l" ctl-x-l-map) ; default-setup
(define-key +leader-map "r" ctl-x-r-map)
(define-key +leader-map "x" ctl-x-x-map)
(define-key +leader-map "4" ctl-x-4-map)
(define-key +leader-map "5" ctl-x-5-map)
(define-key +leader-map "t" tab-prefix-map)
(with-eval-after-load 'evil             ; evil
  (define-key +leader-map "w" evil-window-map))



;;* layout

(define-key +leader-map "1"     'delete-other-windows)
(define-key +leader-map "2"     'split-window-below)
(define-key +leader-map "3"     'split-window-right)
(define-key +leader-map "q"     'quit-window)
(define-key +leader-map "o"     'other-window)
(define-key +leader-map "0"     'delete-window)
(define-key +leader-map "9"     'rotate-window) ; simple-x
(define-key +leader-map [left]  'winner-undo)   ; winner
(define-key +leader-map [right] 'winner-redo)   ; winner



;;* others

(define-key +leader-map "\s" 'execute-extended-command)
(define-key +leader-map "b"  'switch-to-buffer)
(define-key +leader-map "k"  'kill-buffer)
(define-key +leader-map "f"  'find-file)
(define-key +leader-map "j"  'dired-jump)
(define-key +leader-map "e"  'eshell-dwim) ; simple-x
(define-key +leader-map "="  'format-dwim) ; simple-x
(define-key +leader-map ";"  'eval-expression)
(define-key +leader-map "!"  'shell-command)
(define-key +leader-map "&"  'async-shell-command)
(define-key +leader-map "$"  'ispell-word)
(define-key +leader-map "%"  'query-replace-regexp)
(define-key +leader-map ","  'xref-pop-marker-stack)
(define-key +leader-map "."  'xref-find-definitions)
(define-key +leader-map "?"  'xref-find-references)
