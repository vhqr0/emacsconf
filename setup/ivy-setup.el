;;; -*- lexical-binding: t -*-



;;* basic

(autoload 'ivy-avy "ivy-avy" "" t)

(setq ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      ivy-dispatching-done-columns 3    ; `ivy-hydra-read-action' columns
      ivy-read-action-function 'ivy-hydra-read-action)

(setq counsel-async-command-delay 0.25)

(ivy-mode 1)
(counsel-mode 1)

(dolist (mode '(ivy-mode counsel-mode))
  (setcdr (assq mode minor-mode-alist) '("")))



;;* map

(set-keymap-parent ivy-minibuffer-map minibuffer-local-map)

(define-key ivy-minibuffer-map (kbd "<f2>") 'ivy-occur)
(define-key ivy-minibuffer-map "\M-s" 'ivy-restrict-to-matches)
(define-key ivy-minibuffer-map "\M-g" 'ivy-avy)
(dolist (map (list swiper-map counsel-grep-map counsel-ag-map))
  (define-key map [remap ivy-avy] 'swiper-avy))

(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] 'counsel-shell-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] 'counsel-esh-history)

(with-eval-after-load 'evil-collection-ivy
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'normal 'ivy-minibuffer-map
      "\C-o" 'hydra-ivy/body
      "gg"   'ivy-beginning-of-buffer
      "G"    'ivy-end-of-buffer
      "m"    'ivy-mark
      "M"    'ivy-unmark
      "t"    'ivy-toggle-marks
      "\d"   'ivy-unmark-backward)))



;;* bindings

(global-set-key (kbd "<f5>") 'ivy-resume)

(define-key search-map "s" 'swiper)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)

(define-key ctl-x-r-map "v" 'ivy-push-view)
(define-key ctl-x-r-map "V" 'ivy-pop-view)
(define-key ctl-x-r-map "e" 'counsel-recentf)

(define-key ctl-x-l-map "." 'ivy-resume)
(define-key ctl-x-l-map "v" 'ivy-switch-view)
(define-key ctl-x-l-map "s" 'swiper)
(define-key ctl-x-l-map "g" 'counsel-rg)
(define-key ctl-x-l-map "f" 'counsel-fd) ; counsel-fd
(define-key ctl-x-l-map "c" 'counsel-locate)
(define-key ctl-x-l-map "e" 'counsel-recentf)
(define-key ctl-x-l-map "i" 'counsel-imenu)
(define-key ctl-x-l-map "l" 'counsel-outline)
(define-key ctl-x-l-map "y" 'counsel-yank-pop)
(define-key ctl-x-l-map "m" 'counsel-mark-ring)
(define-key ctl-x-l-map "r" 'counsel-register)
(define-key ctl-x-l-map "k" 'counsel-kmacro)
(define-key ctl-x-l-map "n" 'counsel-notes) ; counsel-notes



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

;;** rg

(ivy-configure 'counsel-rg :height 20)

;;** find-file

(define-key counsel-find-file-map "\C-l" 'counsel-up-directory)

;;** yank-pop

(defun +counsel-yank-pop-around (func &rest args)
  (let ((enable-recursive-minibuffers t)) ; enable in minibuffer
    (apply func args)))

(advice-add 'counsel-yank-pop :around '+counsel-yank-pop-around)



;;* projectile

(define-key counsel-mode-map [remap projectile-compile-project]  'counsel-compile)
(define-key counsel-mode-map [remap projectile-switch-project]   'counsel-projectile-switch-project)
(define-key counsel-mode-map [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer)
(define-key counsel-mode-map [remap projectile-find-file]        'counsel-projectile-find-file)
(define-key counsel-mode-map [remap projectile-find-file-dwim]   'counsel-projectile-find-file-dwim)
(define-key counsel-mode-map [remap projectile-find-dir]         'counsel-projectile-find-dir)



;;* tab-completion

(defun +ivy-tab-completion ()
  "Tab completion with `ivy-read'."
  (interactive)
  (let* ((completion-in-region-function 'ivy-completion-in-region)
         (command (key-binding [?\C-i]))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (completion-in-region-mode -1)
    (call-interactively command)))

(global-set-key (kbd "<f2>") '+ivy-tab-completion)
