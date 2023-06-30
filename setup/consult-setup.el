;;; -*- lexical-binding: t -*-



;;* vertico
(use-package vertico
  :config
  (vertico-mode 1)
  (advice-add 'vertico--setup :after 'vertico-repeat-save)
  (bind-keys
   ("<f5>" . vertico-repeat)
   :map vertico-map
   ("<f2>" . embark-export)))

(use-package evil-collection-vertico
  :ensure nil
  :defer t
  :config
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'normal 'vertico-map
      "gg" 'vertico-first
      "G"  'vertico-last)))

(use-package marginalia
  :config
  (marginalia-mode 1))

;;* consult
(use-package consult
  :defer t
  :init
  (setq consult-preview-key '(:debounce 0.2 any))
  (bind-keys :map search-map
             ("s" . consult-line)
             ("g" . consult-ripgrep))
  :config
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

(use-package embark-consult :defer t)

(defun +around-enable-recursive (func &rest args)
  (let ((enable-recursive-minibuffers t))
    (apply func args)))

(advice-add 'consult-yank-pop :around '+around-enable-recursive)

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
