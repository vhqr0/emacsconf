;;; -*- lexical-binding: t -*-



;;* embark
(global-set-key "\M-o" 'embark-act)

(defun avy-action-embark (pt)
  (unwind-protect (save-excursion (goto-char pt) (embark-act))
    (select-window (cdr (ring-ref avy-ring 0))) t))
(with-eval-after-load 'avy
  (add-to-list 'avy-dispatch-alist '(?o . avy-action-embark)))

;;* vertico
(vertico-mode 1)
(marginalia-mode 1)

(advice-add 'vertico--setup :after 'vertico-repeat-save)
(global-set-key (kbd "<f5>") 'vertico-repeat)

(define-key vertico-map (kbd "<f2>") 'embark-export)

(with-eval-after-load 'evil-collection-vertico
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'normal 'vertico-map
      "gg" 'vertico-first
      "G"  'vertico-last)))

;;* consult
(define-key search-map "s" 'consult-line)
(define-key search-map "g" 'consult-ripgrep)

(setq consult-preview-key '(:debounce 0.2 any))

(with-eval-after-load 'consult
  (consult-customize
   consult-grep
   consult-git-grep
   consult-ripgrep
   :preview-key '(:debounce 0.3 any)
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   consult-project-buffer
   consult-theme
   :preview-key '(:debounce 0.5 any)))

(defvar +consult-minor-mode-remap-bindings
  '((switch-to-buffer                       . consult-buffer)
    (switch-to-buffer-other-window          . consult-buffer-other-window)
    (switch-to-buffer-other-frame           . consult-buffer-other-frame)
    (project-switch-to-buffer               . consult-project-buffer)
    (goto-line                              . consult-goto-line)
    (imenu                                  . consult-imenu)
    (yank-pop                               . consult-yank-pop)
    (next-matching-history-element          . consult-history)
    (previous-matching-history-element      . consult-history)
    (comint-history-isearch-backward-regexp . consult-history)
    (eshell-next-matching-input             . consult-history)
    (eshell-previous-matching-input         . consult-history)
    (load-theme                             . consult-theme)))

(defvar +consult-minor-mode-override-functions
  '((completion--in-region . consult-completion-in-region)))

(defvar +consult-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding +consult-minor-mode-remap-bindings)
      (define-key map `[remap ,(car binding)] (cdr binding)))
    map))

(define-minor-mode +consult-minor-mode
  "Remap builtin commands to consult commands."
  :global t
  :keymap +consult-minor-mode-map
  (dolist (override +consult-minor-mode-override-functions)
    (if +consult-minor-mode
        (advice-add (car override) :override (cdr override))
      (advice-remove (car override) (cdr override)))))

(+consult-minor-mode 1)
