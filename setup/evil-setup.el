;;; -*- lexical-binding: t -*-



;;* basic

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-minibuffer t
        evil-want-C-w-delete t
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-C-g-bindings t
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
        evil-undo-system 'undo-tree
        evil-search-module 'evil-search
        evil-ex-search-persistent-highlight nil
        evil-symbol-word-search t
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(bind-key [escape] "M-z")

(bind-key [escape] 'keyboard-escape-quit minibuffer-local-map)
(set-keymap-parent evil-ex-completion-map minibuffer-local-map)

(unbind-key [remap yank-pop] evil-normal-state-map)

(unbind-key "C-@" evil-insert-state-map) ; for terminial input method

;;* workaround

;; https://github.com/emacs-evil/evil/pull/1128
(defun +evil-ex-search-after (&optional count)
  (unless evil-ex-search-persistent-highlight
    (sit-for 0.3)
    (evil-ex-delete-hl 'evil-ex-search)))
(advice-add 'evil-ex-search :after '+evil-ex-search-after)

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

(bind-keys
 :map evil-insert-state-map  ("j" . +evil-jk)
 :map evil-replace-state-map ("j" . +evil-jk))

(with-eval-after-load 'company
  (add-to-list 'company-begin-commands '+evil-jk))



;;* extra

(use-package evil-x :ensure nil)

(use-package evil-xclip
  :ensure nil
  :bind (:map evil-motion-state-map ("gx" . evil-operator-xclip))
  :init
  (unbind-key "gx" evil-normal-state-map))

(use-package evil-eval
  :ensure nil
  :bind (:map evil-motion-state-map ("gy" . evil-operator-eval)))

(use-package evil-format
  :ensure nil
  :bind (:map evil-normal-state-map ("g=" . evil-operator-format)))

(use-package evil-grep
  :ensure nil
  :commands evil-grep
  :init
  (evil-ex-define-cmd "grep" 'evil-grep)
  (evil-ex-define-cmd "rg" 'evil-grep))



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

(defvar +shift-prefix-map
  (let ((map (make-sparse-keymap)))
    (dolist (cons +shift-translation-alist)
      (let ((from (car cons))
            (to (cdr cons)))
        (define-key map (vector from)
                    `(menu-item
                      "" nil :filter
                      (lambda (cmd) (key-binding (vector ,to)))))))
    map))

(bind-key "," 'evil-repeat-find-char-reverse +shift-prefix-map)

(defvar +evil-override-mode-map (make-sparse-keymap))

(define-minor-mode +evil-override-mode
  "Evil global override minor mode."
  :global t
  :keymap +evil-override-mode-map)

(evil-define-key '(motion normal visual operator) +evil-override-mode-map
  ","  +shift-prefix-map
  "\s" +leader-prefix-map)

(+evil-override-mode 1)
