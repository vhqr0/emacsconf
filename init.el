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


(setq selectrum-refine-candidates-function #'orderless-filter
      selectrum-highlight-candidates-function #'orderless-highlight-matches)

(selectrum-mode 1)


(setq evil-search-module 'evil-search)

(evil-mode 1)
(global-evil-surround-mode 1)

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

(evil-define-text-object +evil-textobj-page (count &optional beg end type)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'page)
    (evil-range beg end 'line)))

(evil-define-text-object +evil-textobj-entire (count &optional beg end type)
  (evil-range (point-min) (point-max) 'line))

(define-key evil-inner-text-objects-map (kbd "f") '+evil-textobj-defun)
(define-key evil-outer-text-objects-map (kbd "f") '+evil-textobj-defun)
(define-key evil-inner-text-objects-map (kbd "l") '+evil-textobj-page)
(define-key evil-outer-text-objects-map (kbd "l") '+evil-textobj-page)
(define-key evil-inner-text-objects-map (kbd "h") '+evil-textobj-entire)
(define-key evil-outer-text-objects-map (kbd "h") '+evil-textobj-entire)

(evil-define-operator +evil-operator-narrow (beg end)
  :move-point nil
  (narrow-to-region beg end))

(evil-define-operator +evil-operator-comment (beg end)
  :move-point nil
  (comment-or-uncomment-region beg end))

(evil-global-set-key 'motion (kbd "g n") '+evil-operator-narrow)
(evil-global-set-key 'normal (kbd "g c") '+evil-operator-comment)

(defvar +evil-leader-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'find-file)
    (define-key map (kbd "b") 'switch-to-buffer)
    (define-key map (kbd "k") 'kill-buffer)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "h") 'previous-buffer)
    (define-key map (kbd "l") 'next-buffer)
    (define-key map (kbd "j") 'dired-jump)
    (define-key map (kbd "i") 'imenu)
    (define-key map (kbd "o") 'other-window)
    (define-key map (kbd "0") 'delete-window)
    (define-key map (kbd "1") 'delete-other-windows)
    (define-key map (kbd "2") 'split-window-below)
    (define-key map (kbd "3") 'split-window-right)
    (define-key map (kbd "4") ctl-x-4-map)
    (define-key map (kbd "t") tab-prefix-map)
    (define-key map (kbd "p") project-prefix-map)
    (define-key map (kbd "v") vc-prefix-map)
    (define-key map (kbd "n") narrow-map)
    (define-key map (kbd "r") ctl-x-r-map)
    (define-key map (kbd "x") ctl-x-x-map)
    map))

(evil-global-set-key 'motion (kbd "SPC") +evil-leader-map)


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

(with-eval-after-load 'project
  (setq project-switch-commands
        '((project-find-file "find file")
          (project-dired "dired")
          (project-vc-dir "vc-dir")
          (project-shell "shell"))))

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


(defun +org-xdg-open (file _link)
  (call-process-shell-command (concat "xdg-open " file)))

(setq org-modules '(org-tempo)
      org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.pdf\\'\\|\\.x?html?\\'" . +org-xdg-open))
      org-export-backends '(html reveal latex)
      org-html-postamble nil
      org-special-ctrl-a/e t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") "\C-q<")
  (define-key org-mode-map (kbd "M-o") 'org-meta-return)
  (evil-define-key '(normal visual) org-mode-map
    (kbd "TAB") 'org-cycle
    (kbd "M-h") 'org-metaleft
    (kbd "M-l") 'org-metaright
    (kbd "M-j") 'org-metadown
    (kbd "M-k") 'org-metaup
    (kbd "M-H") 'org-shiftmetaright
    (kbd "M-L") 'org-shiftmetaleft
    (kbd "M-J") 'org-shiftmetadown
    (kbd "M-K") 'org-shiftmetaup))
