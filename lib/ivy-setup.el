(setq ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      ivy-read-action-function 'ivy-hydra-read-action)

(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode 1)
(counsel-mode 1)

(dolist (mode '(ivy-mode counsel-mode))
  (setcdr (assq mode minor-mode-alist) '("")))



(define-key ivy-minibuffer-map (kbd "<f2>") 'ivy-occur)
(define-key ivy-minibuffer-map "\M-." 'minibuffer-yank-symbol)

(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] 'counsel-shell-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] 'counsel-esh-history)
(define-key counsel-mode-map [remap insert-register] 'counsel-register)
(define-key counsel-mode-map [remap jump-to-register] 'counsel-register)
(define-key counsel-mode-map [remap evil-paste-from-register] 'counsel-evil-registers)
(define-key counsel-mode-map [remap evil-goto-mark] 'counsel-evil-marks)

(global-set-key (kbd "<f5>") 'ivy-resume)



(define-key search-map "s" 'swiper)
(define-key search-map "g" 'counsel-rg)
(define-key isearch-mode-map [remap swiper] 'swiper-from-isearch)

(define-key ctl-x-r-map "v" 'ivy-push-view)
(define-key ctl-x-r-map "V" 'ivy-pop-view)



(defun ivy--action-append (x)
  (forward-char)
  (ivy--action-insert x))

(ivy-set-actions t '(("a" ivy--action-append "append")))

(defun counsel--set-variable (x)
  (counsel-set-variable (intern x)))

(ivy-set-actions 'counsel-describe-variable '(("s" counsel--set-variable "set")))



(defun ivy-tab-completion (arg &optional command)
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

(global-set-key (kbd "<f2>") 'ivy-tab-completion)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<f2>") 'counsel-company))



(defun counsel-rg-file-jump (&optional initial-input initial-directory)
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  (let ((find-program rg-program)
        (counsel-file-jump-args '("--files")))
    (counsel-file-jump initial-input initial-directory)))

(defun counsel-rg-file-jump-from-find ()
  (interactive)
  (ivy-quit-and-run
    (counsel-rg-file-jump ivy-text (ivy-state-directory ivy-last))))

(define-key counsel-find-file-map "`" 'counsel-rg-file-jump-from-find)



(defun counsel--proced-get-processes ()
  (let ((oldbuf (get-buffer "*Proced*"))
        processes)
    (unless oldbuf
      (save-window-excursion
        (proced)))
    (with-current-buffer "*Proced*"
      (when oldbuf
        (proced-update t))
      (goto-char (point-min))
      (while (not (eolp))
        (push (cons
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))
               (proced-pid-at-point))
              processes)
        (forward-line)))
    (nreverse processes)))

(defun counsel--proced-kill-process (x)
  (proced-send-signal
   (completing-read "Send signal (default TERM): "
                    proced-signal-list
                    nil nil nil nil "TERM")
   `((,(cdr x) . ,(car x)))))

(defun counsel-proced ()
  (interactive)
  (require 'proced)
  (ivy-read "Processes: " (counsel--proced-get-processes)
            :action 'counsel--proced-kill-process
            :caller 'counsel-proced))



(provide 'ivy-setup)
