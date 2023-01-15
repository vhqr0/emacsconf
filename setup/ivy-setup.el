(setq ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      ivy-dispatching-done-columns 3    ; `ivy-hydra-read-action' columns
      ivy-read-action-function 'ivy-hydra-read-action)

(setq counsel-describe-symbol-function      'helpful-symbol
      counsel-describe-variable-function    'helpful-variable
      counsel-describe-function-function    'helpful-callable
      counsel-descbinds-function            'helpful-callable)

(setq helpful-max-buffers nil)

(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode 1)
(counsel-mode 1)

(dolist (mode '(ivy-mode counsel-mode))
  (setcdr (assq mode minor-mode-alist) '("")))



(define-key ivy-minibuffer-map "\M-\s" 'ivy-restrict-to-matches)

(define-key ivy-minibuffer-map (kbd "<f2>") 'ivy-occur)
(define-key ivy-minibuffer-map "\M-." 'minibuffer-yank-symbol)

(define-key counsel-mode-map [remap describe-key] 'helpful-key)
(define-key counsel-mode-map [remap dired] 'counsel-dired)
(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] 'counsel-shell-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] 'counsel-esh-history)
(define-key counsel-mode-map [remap projectile-compile-project] 'counsel-compile)



(autoload 'ivy-avy "ivy-avy" "ivy-avy" t)
(define-key ivy-minibuffer-map "\M-g" 'ivy-avy)
(dolist (map (list swiper-map counsel-grep-map counsel-ag-map))
  (define-key map [remap ivy-avy] 'swiper-avy))



(global-set-key (kbd "<f5>") 'ivy-resume)

(define-key ctl-x-r-map "v" 'ivy-push-view)
(define-key ctl-x-r-map "V" 'ivy-pop-view)

(define-key search-map "s" 'swiper)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)



(define-key ctl-x-l-map "." 'ivy-resume)
(define-key ctl-x-l-map "g" 'counsel-rg)
(define-key ctl-x-l-map "f" 'counsel-file-jump)
(define-key ctl-x-l-map "d" 'counsel-dired-jump)
(define-key ctl-x-l-map "e" 'counsel-recentf)
(define-key ctl-x-l-map "o" 'counsel-outline)
(define-key ctl-x-l-map "y" 'counsel-yank-pop)
(define-key ctl-x-l-map "m" 'counsel-mark-ring)
(define-key ctl-x-l-map "r" 'counsel-register)
(define-key ctl-x-l-map "k" 'counsel-kmacro)



(defun +ivy--action-append (x)
  (unless (eolp) (forward-char))
  (ivy--action-insert x))

(ivy-add-actions t '(("a" +ivy--action-append "append")))

(defun +counsel--set-variable (x)
  (counsel-set-variable (intern x)))

(ivy-add-actions 'counsel-describe-variable '(("s" +counsel--set-variable "set")))
(ivy-add-actions 'counsel-find-library '(("l" load-library "load")))



(defun +counsel-commands ()
  (interactive)
  (counsel-M-x "^counsel "))

(define-key ctl-x-l-map "x" '+counsel-commands)



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

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "<f2>") 'company-complete)
  (define-key company-active-map (kbd "<f2>") 'counsel-company))
