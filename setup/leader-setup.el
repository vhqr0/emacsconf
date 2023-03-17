;;; -*- lexical-binding: t -*-

(defvar +leader-prefix-map (make-sparse-keymap))

(define-key +leader-prefix-map "h" help-map)
(define-key +leader-prefix-map "g" goto-map)
(define-key +leader-prefix-map "s" search-map)
(define-key +leader-prefix-map "n" narrow-map)
(define-key +leader-prefix-map "v" vc-prefix-map)
;; (define-key +leader-prefix-map "p" project-prefix-map)
(define-key +leader-prefix-map "p" projectile-command-map) ; projectile
(define-key +leader-prefix-map "l" ctl-x-l-map) ; default-setup
(define-key +leader-prefix-map "r" ctl-x-r-map)
(define-key +leader-prefix-map "x" ctl-x-x-map)
(define-key +leader-prefix-map "4" ctl-x-4-map)
(define-key +leader-prefix-map "5" ctl-x-5-map)
(define-key +leader-prefix-map "t" tab-prefix-map)
(with-eval-after-load 'evil             ; evil
  (define-key +leader-prefix-map "w" evil-window-map))



;;* layout

(define-key +leader-prefix-map "1"     'delete-other-windows)
(define-key +leader-prefix-map "2"     'split-window-below)
(define-key +leader-prefix-map "3"     'split-window-right)
(define-key +leader-prefix-map "q"     'quit-window)
(define-key +leader-prefix-map "o"     'other-window)
(define-key +leader-prefix-map "0"     'delete-window)
(define-key +leader-prefix-map "9"     'rotate-window) ; simple-x
(define-key +leader-prefix-map [left]  'winner-undo)   ; winner
(define-key +leader-prefix-map [right] 'winner-redo)   ; winner



;;* others

(define-key +leader-prefix-map "\s" 'execute-extended-command)
(define-key +leader-prefix-map "u"  'universal-argument)
(define-key +leader-prefix-map "z"  'repeat)
(define-key +leader-prefix-map "b"  'switch-to-buffer)
(define-key +leader-prefix-map "k"  'kill-buffer)
(define-key +leader-prefix-map "f"  'find-file)
(define-key +leader-prefix-map "j"  'dired-jump)
(define-key +leader-prefix-map "e"  'eshell-dwim) ; simple-x
(define-key +leader-prefix-map "="  'format-dwim) ; simple-x
(define-key +leader-prefix-map ";"  'eval-expression)
(define-key +leader-prefix-map "!"  'shell-command)
(define-key +leader-prefix-map "&"  'async-shell-command)
(define-key +leader-prefix-map "$"  'ispell-word)
(define-key +leader-prefix-map "%"  'query-replace-regexp)
(define-key +leader-prefix-map ","  'xref-pop-marker-stack)
(define-key +leader-prefix-map "."  'xref-find-definitions)
(define-key +leader-prefix-map "?"  'xref-find-references)
