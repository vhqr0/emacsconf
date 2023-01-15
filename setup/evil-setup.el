;; basic

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
(require 'evil-collection)

(setq-default evil-surround-pairs-alist
              (append evil-surround-pairs-alist
                      '((?a . ("<" . ">"))
                        (?r . ("[" . "]")))))

(evil-mode 1)
(global-evil-surround-mode 1)
(evil-collection-init)

(global-set-key "\M-z" [escape])

(define-key evil-motion-state-map "\M-j" 'evil-scroll-down)
(define-key evil-motion-state-map "\M-k" 'evil-scroll-up)

(define-key evil-normal-state-map [remap yank-pop] nil)

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

(defun +evil-initial-state-for-buffer-around (oldfun &optional buffer)
  (let ((state (funcall oldfun buffer)))
    (if (eq state 'emacs) 'special state)))

(advice-add 'evil-initial-state-for-buffer
            :around '+evil-initial-state-for-buffer-around)

;; jk

(defun +evil-jk-j ()
  (call-interactively
   (lookup-key `(,(current-local-map) ,(current-global-map)) [?k])))

(defun +evil-jk ()
  (interactive)
  (if (and (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.2 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (push 'escape unread-command-events)
          (+evil-jk-j)
          (push next-char unread-command-events)))
    (+evil-jk-j)))

(define-key evil-insert-state-map  "j" '+evil-jk)
(define-key evil-replace-state-map "j" '+evil-jk)

(with-eval-after-load 'company
  (add-to-list 'company-begin-commands '+evil-jk))



;; operator and textobject

(defvar +evil-operator-eval-alist
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)
    (python-mode . python-shell-send-region)))

(evil-define-operator +evil-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (let ((func (cdr (assq major-mode +evil-operator-eval-alist))))
    (if func
        (funcall func beg end)
      (user-error "Major mode doesn't support"))))

(evil-define-operator +evil-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator +evil-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

(evil-define-text-object +evil-tobj-filename (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'filename)
    (evil-range beg end)))

(evil-define-text-object +evil-tobj-defun (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end 'line)))

(evil-define-text-object +evil-tobj-page (const &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'page)
    (evil-range beg end 'line)))

(evil-define-text-object +evil-tobj-entire (const &optional beg end type)
  (evil-range (point-min) (point-max) 'line))

(define-key evil-motion-state-map       "gy" '+evil-operator-eval)
(define-key evil-normal-state-map       "gc" '+evil-operator-comment)
(define-key evil-motion-state-map       "g-" '+evil-operator-narrow)
(define-key evil-inner-text-objects-map "r"  'evil-inner-bracket)
(define-key evil-outer-text-objects-map "r"  'evil-a-bracket)
(define-key evil-inner-text-objects-map "a"  'evil-inner-angle)
(define-key evil-outer-text-objects-map "a"  'evil-an-angle)
(define-key evil-inner-text-objects-map "F"  '+evil-tobj-filename)
(define-key evil-outer-text-objects-map "F"  '+evil-tobj-filename)
(define-key evil-inner-text-objects-map "f"  '+evil-tobj-defun)
(define-key evil-outer-text-objects-map "f"  '+evil-tobj-defun)
(define-key evil-inner-text-objects-map "P"  '+evil-tobj-page)
(define-key evil-outer-text-objects-map "P"  '+evil-tobj-page)
(define-key evil-inner-text-objects-map "h"  '+evil-tobj-entire)
(define-key evil-outer-text-objects-map "h"  '+evil-tobj-entire)

(evil-tobj-x-default-keybindings)



;; leader

(define-key evil-motion-state-map  "\s" evil-leader-map)
(define-key evil-special-state-map "\s" evil-leader-map)

(general-define-key
 :states '(motion normal visual operator)
 :keymaps 'override
 "SPC" evil-leader-map)
