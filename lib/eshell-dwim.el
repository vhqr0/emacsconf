(require 'eshell)

(defvar eshell-buffer-name)
(declare-function eshell-reset "esh-mode")
(declare-function eshell/cd "em-dirs")

;;;###autoload 
(defun eshell-dwim (arg)
  "Eshell in new window or other window if called with prefix ARG."
  (interactive "P")
  (let ((directory default-directory)
        (buffer-list (buffer-list))
        (window-list (mapcar 'window-buffer (window-list)))
        found buffer)
    (while (and (not found) buffer-list)
      (setq buffer (car buffer-list)
            buffer-list (cdr buffer-list))
      (when (and (eq (with-current-buffer buffer
                       major-mode)
                     'eshell-mode)
                 (string-prefix-p eshell-buffer-name (buffer-name buffer))
                 (not (get-buffer-process buffer))
                 (not (member buffer window-list)))
        (setq found t)))
    (if found
        (with-current-buffer buffer
          (eshell/cd directory)
          (eshell-reset))
      (setq buffer (generate-new-buffer eshell-buffer-name))
      (with-current-buffer buffer
        (eshell-mode)))
    (cond ((and (consp arg) (> (prefix-numeric-value arg) 4))
           (switch-to-buffer buffer))
          (arg
           (switch-to-buffer-other-window buffer))
          (t
           (let ((parent (window-parent (selected-window))))
             (cond ((window-left-child parent)
                    (select-window (split-window-vertically))
                    (switch-to-buffer buffer))
                   ((window-top-child parent)
                    (select-window (split-window-horizontally))
                    (switch-to-buffer buffer))
                   (t
                    (switch-to-buffer-other-window buffer))))))))

(provide 'eshell-dwim)
