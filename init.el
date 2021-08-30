;;; -*- lexical-binding: t -*-


(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require '+autoload)


(setq text-quoting-style 'grave)
(startup--setup-quote-display 'grave)

(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-x M-r") 'raise-sexp)
(global-set-key (kbd "C-x M-d") 'delete-pair)


(setq disabled-command-function nil)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map (kbd "U") 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)


(setq completion-styles '(orderless)
      selectrum-refine-candidates-function #'orderless-filter
      selectrum-highlight-candidates-function #'orderless-highlight-matches)

(selectrum-mode 1)


(setq evil-undo-system 'undo-redo
      evil-search-module 'evil-search
      evil-want-fine-undo t
      evil-want-C-u-scroll t
      evil-want-keybinding nil
      evil-disable-insert-state-bindings t)

(evil-mode 1)
(global-evil-surround-mode 1)

(evil-global-set-key 'insert (kbd "C-r") 'evil-paste-from-register)

(global-set-key (kbd "M-z") [escape])

(defun +input-method-function (first-char)
  (if (and (eq first-char ?j)
           (evil-insert-state-p)
           (not executing-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char
             (let (input-method-function)
               (read-event))))
        (if (eq next-char ?k)
            '(escape)
          (push next-char unread-command-events)
          '(?j)))
    `(,first-char)))

(setq input-method-function '+input-method-function)

(evil-define-text-object +evil-textobj-defun (count &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end 'line)))

(evil-define-text-object +evil-textobj-entire (count &optional beg end type)
  (evil-range (point-min) (point-max) 'line))

(define-key evil-inner-text-objects-map (kbd "f") '+evil-textobj-defun)
(define-key evil-outer-text-objects-map (kbd "f") '+evil-textobj-defun)
(define-key evil-inner-text-objects-map (kbd "h") '+evil-textobj-entire)
(define-key evil-outer-text-objects-map (kbd "h") '+evil-textobj-entire)

(evil-define-operator +evil-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

(evil-define-operator +evil-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-global-set-key 'motion (kbd "g n") '+evil-operator-narrow)
(evil-global-set-key 'normal (kbd "g c") '+evil-operator-comment)

(defvar +evil-leader-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'find-file)
    (define-key map (kbd "b") 'switch-to-buffer)
    (define-key map (kbd "j") 'dired-jump)
    (define-key map (kbd "i") 'imenu)
    (define-key map (kbd "4") ctl-x-4-map)
    (define-key map (kbd "p") project-prefix-map)
    (define-key map (kbd "v") vc-prefix-map)
    (define-key map (kbd "r") (lookup-key ctl-x-map "r"))
    (define-key map (kbd "n") (lookup-key ctl-x-map "n"))
    map))

(evil-global-set-key 'motion (kbd "SPC") +evil-leader-map)


(defun +format ()
  (interactive)
  (let ((program (assq major-mode
                       '((c-mode . "clang-format -i ")
                         (c++-mode . "clang-format -i ")
                         (python-mode . "yapf -i ")))))
    (when (and program
               buffer-file-name
               (not buffer-read-only))
      (save-buffer)
      (shell-command (concat (cdr program) buffer-file-name))
      (revert-buffer nil t))))

(define-key prog-mode-map (kbd "C-c =") '+format)

(evil-define-operator +evil-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (let ((func (assq major-mode
                    '((emacs-lisp-mode . eval-region)
                      (lisp-interaction-mode . eval-region)
                      (python-mode . python-shell-send-region)))))
    (when func
      (funcall (cdr func) beg end))))

(evil-define-key 'motion prog-mode-map (kbd "g r") '+evil-operator-eval)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)))

(setq eglot-ignored-server-capabilites '(:hoverProvider))

(setq company-backends '(company-files company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-capf company-files company-dabbrev))))

(add-hook 'prog-mode-hook 'company-mode)


(when (getenv "WSLENV")
  (setq +xclip-program "clip.exe"
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function 'browse-url-generic))

(defvar +xclip-program "xclip -selection clip")

(defun +xclip (beg end)
  (interactive "r")
  (call-shell-region beg end +xclip-program))

(global-set-key (kbd "C-x w") '+xclip)

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") '+dired-do-xdg-open)
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-add-hjkl-bindings dired-mode-map 'normal
    " " +evil-leader-map
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    ";" (lookup-key dired-mode-map ":")))

(defun rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: " "rg --no-heading " 'grep-history)
   'grep-mode))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq ispell-dictionary "american")
