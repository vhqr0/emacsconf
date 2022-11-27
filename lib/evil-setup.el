(setq evil-want-keybinding nil
      evil-disable-insert-state-bindings t
      evil-want-C-i-jump nil
      evil-want-C-d-scroll nil
      evil-want-C-w-delete nil
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t
      evil-undo-system 'undo-redo
      evil-symbol-word-search t
      evil-respect-visual-line-mode t)

(require 'evil)
(require 'evil-surround)

(setq-default evil-surround-pairs-alist
              (append evil-surround-pairs-alist
                      '((?a . ("<" . ">"))
                        (?r . ("[" . "]")))))

(evil-mode 1)
(global-evil-surround-mode 1)

(global-set-key "\M-z" [escape])

(define-key evil-motion-state-map "\M-j" 'evil-scroll-down)
(define-key evil-motion-state-map "\M-k" 'evil-scroll-up)

(define-key evil-insert-state-map "\C-r" 'evil-paste-from-register)

(setq evil-ex-completion-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-r"   'evil-paste-from-register)
        (define-key map "\d"     'evil-ex-delete-backward-char)
        (define-key map "\t"     'evil-ex-completion)
        (define-key map [tab]    'evil-ex-completion)
        (define-key map [escape] 'abort-recursive-edit)
        map))
(set-keymap-parent evil-ex-completion-map minibuffer-local-map)



;; initial state

(evil-define-state special
  "Special state."
  :tag " <SP> "
  :enable (emacs))

(define-key evil-special-state-map "\C-w" evil-window-map)
(define-key evil-special-state-map "\C-z" 'evil-motion-state)
(define-key evil-special-state-map "\M-j" 'evil-scroll-down)
(define-key evil-special-state-map "\M-k" 'evil-scroll-up)
(define-key evil-special-state-map ":"    'evil-ex)
(define-key evil-special-state-map "\\"   'evil-execute-in-emacs-state)
(define-key evil-special-state-map "j"    "\C-n")
(define-key evil-special-state-map "k"    "\C-p")
(define-key evil-special-state-map "h"    "\C-b")
(define-key evil-special-state-map "l"    "\C-f")

(defun evil-initial-state-for-buffer-override (&optional buffer default)
  (with-current-buffer (or buffer (current-buffer))
    (cond ((derived-mode-p 'dired-mode 'magit-section-mode 'image-mode 'doc-view-mode 'pdf-view-mode)
           'special)
          ((derived-mode-p 'comint-mode 'eshell-mode)
           'insert)
          ((derived-mode-p 'special-mode 'compilation-mode)
           'motion)
          (t
           'normal))))

(advice-add 'evil-initial-state-for-buffer
            :override 'evil-initial-state-for-buffer-override)

;; jk

(defun evil-jk-j ()
  (call-interactively
   (lookup-key `(,(current-local-map) ,(current-global-map)) [?k])))

(defun evil-jk ()
  (interactive)
  (if (and (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.2 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (push 'escape unread-command-events)
          (evil-jk-j)
          (push next-char unread-command-events)))
    (evil-jk-j)))

(define-key evil-insert-state-map  "j" 'evil-jk)
(define-key evil-replace-state-map "j" 'evil-jk)

(with-eval-after-load 'company
  (add-to-list 'company-begin-commands 'evil-jk))



;; operator

(defvar evil-operator-eval-alist
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region)))

(evil-define-operator evil-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (let ((func (cdr (assq major-mode evil-operator-eval-alist))))
    (if func
        (funcall func beg end)
      (user-error "Major mode doesn't support"))))

(evil-define-operator evil-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator evil-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

;;; simple-x.el
(evil-define-operator evil-operator-external-format (beg end)
  :move-point nil
  (interactive "<r>")
  (external-format beg end))

(define-key evil-motion-state-map "gy" 'evil-operator-eval)
(define-key evil-normal-state-map "gc" 'evil-operator-comment)
(define-key evil-motion-state-map "g-" 'evil-operator-narrow)
(define-key evil-motion-state-map "g=" 'evil-operator-external-format)

;; textobject

(evil-define-text-object evil-tobj-filename (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'filename)
    (evil-range beg end)))

(evil-define-text-object evil-tobj-defun (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end 'line)))

(evil-define-text-object evil-tobj-page (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'page)
    (evil-range beg end 'line)))

(evil-define-text-object evil-tobj-entire (const &optional beg end type)
  (evil-range (point-min) (point-max) 'line))

(define-key evil-inner-text-objects-map "r" 'evil-inner-bracket)
(define-key evil-outer-text-objects-map "r" 'evil-a-bracket)
(define-key evil-inner-text-objects-map "a" 'evil-inner-angle)
(define-key evil-outer-text-objects-map "a" 'evil-an-angle)
(define-key evil-inner-text-objects-map "F" 'evil-tobj-filename)
(define-key evil-outer-text-objects-map "F" 'evil-tobj-filename)
(define-key evil-inner-text-objects-map "f" 'evil-tobj-defun)
(define-key evil-outer-text-objects-map "f" 'evil-tobj-defun)
(define-key evil-inner-text-objects-map "P" 'evil-tobj-page)
(define-key evil-outer-text-objects-map "P" 'evil-tobj-page)
(define-key evil-inner-text-objects-map "h" 'evil-tobj-entire)
(define-key evil-outer-text-objects-map "h" 'evil-tobj-entire)

(evil-tobj-plus-default-keybindings)



;; leader

(defvar evil-leader-map (make-sparse-keymap))

(define-key evil-motion-state-map  "\s" evil-leader-map)
(define-key evil-special-state-map "\s" evil-leader-map)

(defun god-C-c ()
  (interactive)
  (let ((bind (key-binding (kbd (format "C-c C-%c" (read-char "C-c C-"))))))
    (when (commandp bind)
      (setq this-command bind
            real-this-command bind)
      (if (commandp bind t)
          (call-interactively bind)
        (execute-kbd-macro bind)))))

(define-key evil-leader-map "c" 'god-C-c)
(define-key evil-leader-map "h" help-map)
(define-key evil-leader-map "s" search-map)
(define-key evil-leader-map "g" goto-map)
(define-key evil-leader-map "a" abbrev-map)
(define-key evil-leader-map "r" ctl-x-r-map)
(define-key evil-leader-map "x" ctl-x-x-map)
(define-key evil-leader-map "l" ctl-x-l-map) ; init.el
(define-key evil-leader-map "i" symbol-at-point-map) ; init.el
(define-key evil-leader-map "n" narrow-map)
(define-key evil-leader-map "v" vc-prefix-map)
;;; use projectile
;; (define-key evil-leader-map "p" project-prefix-map)
(define-key evil-leader-map "m" kmacro-keymap)
(define-key evil-leader-map "4" ctl-x-4-map)
(define-key evil-leader-map "5" ctl-x-5-map)
(define-key evil-leader-map "t" tab-prefix-map)
(define-key evil-leader-map "w" workspace-prefix-map) ; workspace.el
(define-key evil-leader-map "\r" mule-keymap)

(define-key evil-leader-map "1" 'delete-other-windows)
(define-key evil-leader-map "2" 'split-window-below)
(define-key evil-leader-map "3" 'split-window-right)
(define-key evil-leader-map "q" 'quit-window)
(define-key evil-leader-map "o" 'other-window)
(define-key evil-leader-map "0" 'delete-window)
(define-key evil-leader-map "9" 'rotate-window) ; simple-x
(define-key evil-leader-map "u" 'winner-undo)   ; winner
(define-key evil-leader-map "U" 'winner-redo)   ; winner
(define-key evil-leader-map "k" 'kill-buffer)
(define-key evil-leader-map [left] 'previous-buffer)
(define-key evil-leader-map [right] 'next-buffer)
(define-key evil-leader-map "\t" 'evil-jump-backward)
(define-key evil-leader-map [tab] 'evil-jump-backward)
(define-key evil-leader-map [backtab] 'evil-jump-forward)

(dolist (pair insert-pair-alist)
  (define-key evil-leader-map `[,(car pair)] 'insert-pair))

(define-key evil-leader-map "\s" 'execute-extended-command)
(define-key evil-leader-map "b" 'switch-to-buffer)
(define-key evil-leader-map "f" 'find-file)
(define-key evil-leader-map "d" 'dired)
(define-key evil-leader-map "j" 'dired-jump)
(define-key evil-leader-map "z" 'repeat)
(define-key evil-leader-map ";" 'eval-expression)
(define-key evil-leader-map "e" 'eshell-dwim) ; simple-x
(define-key evil-leader-map "!" 'shell-command)
(define-key evil-leader-map "&" 'async-shell-command)
(define-key evil-leader-map "$" 'ispell-word)
(define-key evil-leader-map "^" 'delete-indentation)
(define-key evil-leader-map "%" 'query-replace-regexp)
(define-key evil-leader-map "," 'xref-pop-marker-stack)
(define-key evil-leader-map "." 'xref-find-definitions)
(define-key evil-leader-map "?" 'xref-find-references)

;;; counsel
(define-key evil-leader-map "y" 'counsel-yank-pop)

(provide 'evil-setup)
