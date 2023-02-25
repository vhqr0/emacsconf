;;; -*- lexical-binding: t -*-



;;* basic

(setq undo-tree-mode-lighter nil
      undo-tree-auto-save-history nil)

(setq evil-want-keybinding nil
      evil-want-C-w-delete t
      evil-want-C-u-delete t
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t
      evil-undo-system 'undo-tree
      evil-search-module 'evil-search
      evil-ex-search-persistent-highlight nil
      evil-symbol-word-search t
      evil-respect-visual-line-mode t)

(global-undo-tree-mode 1)

(evil-mode 1)
(global-evil-surround-mode 1)

(evil-collection-init)
(setcdr (assq 'evil-collection-unimpaired-mode minor-mode-alist) '(""))

(global-set-key "\M-z" [escape])
(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)

(set-keymap-parent evil-ex-completion-map minibuffer-local-map)

(define-key evil-normal-state-map [remap yank-pop] nil)

;;* workaround

;; https://github.com/emacs-evil/evil/pull/1128
(defun +evil-ex-search-after (&optional count)
  (unless evil-ex-search-persistent-highlight
    (sit-for 0.3)
    (evil-ex-delete-hl 'evil-ex-search)))
(advice-add 'evil-ex-search :after '+evil-ex-search-after)

;;* initial state

(evil-define-state special
  "Special state."
  :tag " <SP> "
  :enable (emacs))

(define-key evil-special-state-map "\C-w" evil-window-map)
(define-key evil-special-state-map "\C-z" 'evil-motion-state)
(define-key evil-special-state-map "\\"   'evil-execute-in-emacs-state)
(define-key evil-special-state-map ":"    'evil-ex)
(define-key evil-special-state-map "\C-d" 'evil-scroll-down)
(define-key evil-special-state-map "\C-u" 'evil-scroll-up)
(define-key evil-special-state-map "\C-f" 'evil-scroll-page-down)
(define-key evil-special-state-map "\C-b" 'evil-scroll-page-up)
(define-key evil-special-state-map "\C-e" 'evil-scroll-line-down)
(define-key evil-special-state-map "\C-y" 'evil-scroll-line-up)
(define-key evil-special-state-map "j"    "\C-n")
(define-key evil-special-state-map "k"    "\C-p")
(define-key evil-special-state-map "h"    "\C-b")
(define-key evil-special-state-map "l"    "\C-f")

(defun +evil-initial-state-for-buffer-around (func &rest args)
  (let ((state (apply func args)))
    (if (eq state 'emacs) 'special state)))

(advice-add 'evil-initial-state-for-buffer
            :around '+evil-initial-state-for-buffer-around)

;;* jk

(defun +evil-jk-j ()
  (let* ((binding (key-binding [?k]))
         (binding (if (commandp binding t)
                      binding
                    'self-insert-command)))
    (setq this-command binding
          real-this-command binding)
    (call-interactively binding)))

(defun +evil-jk ()
  (interactive)
  (if (and (not executing-kbd-macro)
           (not defining-kbd-macro)
           (not (sit-for 0.2 'no-redisplay)))
      (let ((next-char (read-event)))
        (if (eq next-char ?k)
            (progn
              (setq this-command 'ignore
                    real-this-command 'ignore)
              (push 'escape unread-command-events))
          (+evil-jk-j)
          (push next-char unread-command-events)))
    (+evil-jk-j)))

(define-key evil-insert-state-map  "j" '+evil-jk)
(define-key evil-replace-state-map "j" '+evil-jk)

(with-eval-after-load 'company
  (add-to-list 'company-begin-commands '+evil-jk))



;;* operator and textobject

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

(define-key evil-motion-state-map "gy" '+evil-operator-eval)
(define-key evil-normal-state-map "gc" '+evil-operator-comment)
(define-key evil-motion-state-map "g-" '+evil-operator-narrow)

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

(define-key evil-inner-text-objects-map "F" '+evil-tobj-filename)
(define-key evil-outer-text-objects-map "F" '+evil-tobj-filename)
(define-key evil-inner-text-objects-map "f" '+evil-tobj-defun)
(define-key evil-outer-text-objects-map "f" '+evil-tobj-defun)
(define-key evil-inner-text-objects-map "P" '+evil-tobj-page)
(define-key evil-outer-text-objects-map "P" '+evil-tobj-page)
(define-key evil-inner-text-objects-map "h" '+evil-tobj-entire)
(define-key evil-outer-text-objects-map "h" '+evil-tobj-entire)



;;* leader

(defvar +shift-translation-alist
  '((?a  . ?A) (?b  . ?B) (?c  . ?C ) (?d  . ?D )
    (?e  . ?E) (?f  . ?F) (?g  . ?G ) (?h  . ?H )
    (?i  . ?I) (?j  . ?J) (?k  . ?K ) (?l  . ?L )
    (?m  . ?M) (?n  . ?N) (?o  . ?O ) (?p  . ?P )
    (?q  . ?Q) (?r  . ?R) (?s  . ?S ) (?t  . ?T )
    (?u  . ?U) (?v  . ?V) (?w  . ?W ) (?x  . ?X )
    (?y  . ?Y) (?z  . ?Z) (?1  . ?! ) (?2  . ?@ )
    (?3  . ?#) (?4  . ?$) (?5  . ?% ) (?6  . ?^ )
    (?7  . ?&) (?8  . ?*) (?9  . ?\() (?0  . ?\))
    (?`  . ?~) (?-  . ?_) (?=  . ?+ ) (?\[ . ?{ )
    (?\] . ?}) (?\\ . ?|) (?\; . ?: ) (?'  . ?\")
    (?,  . ?<) (?.  . ?>) (?/  . ?? )            ))

(defvar +shift-prefix-map (make-sparse-keymap))

(dolist (cons +shift-translation-alist)
  (define-key +shift-prefix-map
              (vector (car cons))
              `(menu-item
                "" nil :filter
                (lambda (cmd)
                  (key-binding (vector (or (assq ,(cdr cons) +shift-translation-alist)
                                           ,(cdr cons))))))))

(define-key evil-motion-state-map ","  +shift-prefix-map)
(define-key evil-motion-state-map "z," 'evil-repeat-find-char-reverse)

(define-key evil-motion-state-map  "\s" +leader-prefix-map)
(define-key evil-special-state-map "\s" +leader-prefix-map)

(general-define-key
 :states '(motion normal visual operator)
 :keymaps 'override
 "SPC" +leader-prefix-map
 ","   +shift-prefix-map)
