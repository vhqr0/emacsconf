;;; -*- lexical-binding: t -*-



;;* basic

(setq ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      ivy-dispatching-done-columns 3    ; `ivy-hydra-read-action' columns
      ivy-read-action-function 'ivy-hydra-read-action)

(setq counsel-async-command-delay 0.25)

(setq counsel-describe-symbol-function   'helpful-symbol
      counsel-describe-variable-function 'helpful-variable
      counsel-describe-function-function 'helpful-callable
      counsel-descbinds-function         'helpful-callable)

(setq helpful-max-buffers nil)

(require 'ivy)
(require 'swiper)
(require 'counsel)
(autoload 'ivy-avy "ivy-avy" "" t)

(setq ivy--sources-list nil)            ; remove `ivy-switch-buffer' view source

(ivy-mode 1)
(counsel-mode 1)

(dolist (mode '(ivy-mode counsel-mode))
  (setcdr (assq mode minor-mode-alist) '("")))

(advice-add 'ivy-read :around           ; diminish `icomplete' while `ivy-read'
            (lambda (func &rest args)
              (let (icomplete-mode)
                (apply func args))))



;;* map

(define-key ivy-minibuffer-map (kbd "<f2>") 'ivy-occur)
(define-key ivy-minibuffer-map "\M-s" 'ivy-restrict-to-matches)
(define-key ivy-minibuffer-map "\M-." 'minibuffer-yank-symbol)
(define-key ivy-minibuffer-map "\M-g" 'ivy-avy)
(dolist (map (list swiper-map counsel-grep-map counsel-ag-map))
  (define-key map [remap ivy-avy] 'swiper-avy))

(define-key counsel-mode-map [remap describe-key] 'helpful-key)
(define-key counsel-mode-map [remap dired] 'counsel-dired)
(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] 'counsel-shell-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] 'counsel-esh-history)

(define-key counsel-mode-map [remap execute-extended-command] nil)
(define-key counsel-mode-map [remap yank-pop] nil)

(dolist (command '(execute-extended-command project-execute-extended-command))
  (add-to-list 'ivy-completing-read-handlers-alist (cons command 'completing-read-default)))



;;* bindings

(global-set-key (kbd "<f5>") 'ivy-resume)

(define-key search-map "s" 'swiper)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)

(define-key ctl-x-r-map "e" 'counsel-recentf)

(define-key ctl-x-l-map "." 'ivy-resume)
(define-key ctl-x-l-map "s" 'swiper)
(define-key ctl-x-l-map "g" 'counsel-rg)
(define-key ctl-x-l-map "f" 'counsel-fd) ; counsel-fd
(define-key ctl-x-l-map "c" 'counsel-locate)
(define-key ctl-x-l-map "e" 'counsel-recentf)
(define-key ctl-x-l-map "x" 'counsel-M-x)
(define-key ctl-x-l-map "i" 'counsel-imenu)
(define-key ctl-x-l-map "l" 'counsel-outline)
(define-key ctl-x-l-map "y" 'counsel-yank-pop)
(define-key ctl-x-l-map "m" 'counsel-mark-ring)
(define-key ctl-x-l-map "r" 'counsel-register)
(define-key ctl-x-l-map "k" 'counsel-kmacro)
(define-key ctl-x-l-map "n" 'counsel-notes) ; counsel-notes

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<f2>") 'counsel-company))



;;* workaround

;;** actions

(defun +ivy--action-append (x)
  (unless (eolp) (forward-char))
  (ivy--action-insert x))

(ivy-add-actions t '(("a" +ivy--action-append "append")))

(defun +counsel--set-variable (x)
  (counsel-set-variable (intern x)))

(ivy-add-actions 'counsel-describe-variable '(("s" +counsel--set-variable "set")))
(ivy-add-actions 'counsel-find-library '(("l" load-library "load")))

;;** find-file

(define-key counsel-find-file-map "\C-l" 'counsel-up-directory)

;;** grep

(defun +counsel-ag-around (func &rest args)
  (let ((current-prefix-arg (if (>= (prefix-numeric-value current-prefix-arg) 5)
                                16
                              current-prefix-arg)))
    (apply func args)))
(advice-add 'counsel-ag :around '+counsel-ag-around)



;;* projectile

(define-key counsel-mode-map [remap projectile-compile-project]  'counsel-compile)
(define-key counsel-mode-map [remap projectile-switch-project]   'counsel-projectile-switch-project)
(define-key counsel-mode-map [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer)
(define-key counsel-mode-map [remap projectile-find-file]        'counsel-projectile-find-file)
(define-key counsel-mode-map [remap projectile-find-file-dwim]   'counsel-projectile-find-file-dwim)
(define-key counsel-mode-map [remap projectile-find-dir]         'counsel-projectile-find-dir)



;;* tab-completion

(defun +ivy-tab-completion (arg &optional command)
  "Tab completion with `ivy-read'."
  (interactive "P")
  (let* ((completion-in-region-function 'ivy-completion-in-region)
         (command (or command
                      (lookup-key `(,(current-local-map) ,(current-global-map))
                                  (if arg
                                      (read-key-sequence-vector "command: ")
                                    (kbd "TAB")))))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (completion-in-region-mode -1)
    (call-interactively command)))

(global-set-key (kbd "<f2>") '+ivy-tab-completion)
