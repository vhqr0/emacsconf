(defvar terminalize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-D")   'terminalize-other-window)
    (define-key map (kbd "C-S-T") 'terminalize-other-tab)
    (define-key map (kbd "C-S-W") 'terminalize-close)
    map))

(defun terminalize-buffer ()
  (require 'eshell)
  (let ((ctn t)
        (directory default-directory)
        (buffer-list (buffer-list))
        (window-list (mapcar 'window-buffer (window-list)))
        buffer)
    (while (and ctn buffer-list)
      (setq buffer (car buffer-list)
            buffer-list (cdr buffer-list))
      (when (and (eq (with-current-buffer buffer
                       major-mode)
                     'eshell-mode)
                 (not (get-buffer-process buffer))
                 (not (member buffer window-list)))
        (setq ctn nil)))
    (when ctn
      (setq buffer (generate-new-buffer eshell-buffer-name)))
    (message (concat "Terminalize find buffer" (buffer-name buffer)))
    (with-current-buffer buffer
      (setq default-directory directory)
      (widen)
      (goto-char (point-max))
      (eshell-mode))
    buffer))

(defun terminalize-other-window ()
  (interactive)
  (switch-to-buffer-other-window (terminalize-buffer)))

(defun terminalize-other-tab ()
  (interactive)
  (if (eq last-command 'tab-bar-close-tab)
      (tab-bar-undo-close-tab)
    (switch-to-buffer-other-tab (terminalize-buffer))))

(defun terminalize-close ()
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (setq this-command 'tab-bar-close-tab)
        (tab-bar-close-tab))
    (delete-window)))

;;;###autoload
(define-minor-mode terminalize-mode
  "Terminalize emacs."
  :global t
  :keymap terminalize-mode-map)

(provide 'terminalize)
