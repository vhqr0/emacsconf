;;; -*- lexical-binding: t -*-



;;* help-map

(unbind-key "t" help-map)
(bind-keys :map help-map
           ("tt" . load-theme)
           ("tf" . load-file)
           ("tl" . load-library)
           ("j"  . find-library)
           ("4j" . find-library-other-window))

;;* ctl-x-4-map

(bind-key "j" 'dired-jump-other-window ctl-x-4-map)

;;* +minor-prefix-map

(defvar-keymap +minor-prefix-map)
(bind-keys :map +minor-prefix-map
           ("t" . toggle-truncate-lines)
           ("a" . auto-save-visited-mode)
           ("h" . hl-line-mode)
           ("l" . display-line-numbers-mode)
           ("s" . whitespace-mode)
           ("v" . visual-line-mode)
           ("q" . auto-fill-mode)
           ("r" . read-only-mode))

;;* +file-prefix-map

(defvar-keymap +file-prefix-map)
(bind-keys :map +file-prefix-map
           ("f" . find-file)
           ("r" . find-file-read-only)
           ("v" . find-alternate-file)
           ("d" . dired)
           ("j" . dired-jump)
           ("o" . xdg-open)            ; simple-x
           ("i" . insert-file)
           ("w" . write-file))

;;* +buffer-prefix-map

(defvar-keymap +buffer-prefix-map)
(bind-keys :map +buffer-prefix-map
           ("f"     . font-lock-update)
           ("g"     . revert-buffer-quick)
           ("r"     . rename-buffer)
           ("u"     . rename-uniquely)
           ("n"     . clone-buffer)
           ("i"     . insert-buffer)
           ("q"     . bury-buffer)
           ("Q"     . unbury-buffer)
           ("b"     . switch-to-buffer)
           ("s"     . save-buffer)
           ("k"     . kill-buffer-dwim) ; simple-x
           ([left]  . previous-buffer)
           ([right] . next-buffer))

;;* list-prefix-map

(defvar +list-prefix-map (make-sparse-keymap))
(bind-keys :map +list-prefix-map
           ("b" . ibuffer)
           ("n" . notes)                ; notes
           ("u" . undo-tree-visualize)  ; undo-tree
           )



;;* leader-map

(defun +leader-C-u ()
  (interactive)
  (let ((arg (if current-prefix-arg
                 (prefix-numeric-value current-prefix-arg)
               1)))
    (setq prefix-arg (list (* 4 arg))))
  (set-transient-map +leader-prefix-map))

(defun +leader--god-execute (bind)
  (cond ((commandp bind)
         (setq this-command bind
               real-this-command bind)
         (if (commandp bind t)
             (call-interactively bind)
           (execute-kbd-macro bind))
         bind)
        ((keymapp bind)
         (set-transient-map bind))))

(defun +leader--god (prefix)
  (let* ((char (read-char (concat prefix " C-")))
         (kbd1 (format "%s C-%c" prefix char))
         (kbd2 (format "%s %c" prefix char)))
    (cond ((+leader--god-execute (key-binding (kbd kbd1))))
          ((+leader--god-execute (key-binding (kbd kbd2))))
          (t
           (user-error "no key binding on '%s' or '%s'" kbd1 kbd2)))))

(defun +leader-C-c ()
  (interactive)
  (+leader--god "C-c"))

(defun +leader-C-x ()
  (interactive)
  (+leader--god "C-x"))

(defvar-keymap +leader-prefix-map
  "h" help-map
  "g" goto-map
  "s" search-map
  "n" narrow-map
  "v" vc-prefix-map
  "p" project-prefix-map
  "r" ctl-x-r-map
  "4" ctl-x-4-map
  "5" ctl-x-5-map
  "t" tab-prefix-map
  "f" +file-prefix-map
  "b" +buffer-prefix-map
  "l" +list-prefix-map
  "m" +minor-prefix-map)

(with-eval-after-load 'evil
  (bind-keys :map evil-window-map
             ("0"     . evil-window-delete)
             ("1"     . delete-other-windows)
             ("2"     . evil-window-split)
             ("3"     . evil-window-vsplit)
             ([left]  . winner-undo)
             ([right] . winner-redo))
  (bind-keys :repeat-map evil-other-window-repeat-map
             ("w" . evil-window-next)
             ("W" . evil-window-prev))
  (bind-key "w" evil-window-map +leader-prefix-map))

(bind-keys :map +leader-prefix-map
           ("u"   . +leader-C-u)
           ("c"   . +leader-C-c)
           ("x"   . +leader-C-x)
           ("`"   . tmm-menubar)
           ("SPC" . execute-extended-command)
           (":"   . eval-expression)
           (";"   . evil-ex)            ; evil
           ("["   . sp-wrap-square)     ; smartparens
           ("("   . sp-wrap-round)      ; smartparens
           ("{"   . sp-wrap-curly)      ; smartparens
           ("e"   . eshell-dwim)        ; eshell-dwim
           ("="   . format-buffer)      ; evil-format
           ("#"   . server-edit)
           ("!"   . shell-command)
           ("&"   . async-shell-command)
           ("$"   . ispell-word)
           ("%"   . query-replace-regexp)
           (","   . xref-go-back)
           ("."   . xref-find-definitions)
           ("?"   . xref-find-references))
