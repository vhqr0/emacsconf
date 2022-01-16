(defvar eshell-buffer-name)

(defvar terminalize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-R")     'terminalize-rotate)
    (define-key map (kbd "M-D")     'terminalize-other-window)
    (define-key map (kbd "C-S-T")   'terminalize-other-tab)
    (define-key map (kbd "C-S-W")   'terminalize-close)
    (define-key map (kbd "C-c M-.") 'terminalize-yank-symbol)
    map))

(defun terminalize-yank-symbol ()
  (interactive)
  (let ((symbol (with-selected-window (if (window-minibuffer-p)
                                          (minibuffer-selected-window)
                                        (next-window))
                  (thing-at-point 'symbol))))
    (when symbol
      (insert symbol))))

(defun terminalize-rotate (arg)
  (interactive "P")
  (if arg
      (let* ((window (selected-window))
             (next-window (next-window))
             (buffer (window-buffer window))
             (next-buffer (window-buffer next-window)))
        (unless (eq window next-window)
          (set-window-buffer window next-buffer)
          (set-window-buffer next-window buffer)
          (select-window next-window)))
    (let* ((window (selected-window))
           (parent (window-parent window))
           (top-child (window-top-child parent))
           (next-window (window-next-sibling window))
           (prev-window (window-prev-sibling window)))
      (cond ((not parent))
            (next-window
             (let ((next-window-buffer (window-buffer next-window)))
               (delete-window next-window)
               (set-window-buffer (if top-child
                                      (split-window-horizontally)
                                    (split-window-vertically))
                                  next-window-buffer)))
            (prev-window
             (let ((prev-window-buffer (window-buffer prev-window)))
               (delete-window prev-window)
               (select-window (if top-child
                                  (split-window-horizontally)
                                (split-window-vertically)))
               (set-window-buffer window prev-window-buffer)))))))

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

(defun terminalize-other-window (arg)
  (interactive "P")
  (let ((buffer (terminalize-buffer)))
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

(defun terminalize-other-tab (arg)
  (interactive "P")
  (if (or arg (eq last-command 'tab-bar-close-tab))
      (tab-bar-undo-close-tab)
    (switch-to-buffer-other-tab (terminalize-buffer))))

(defun terminalize-close (arg)
  (interactive "P")
  (if (or arg (= (length (window-list)) 1))
      (progn
        (setq this-command 'tab-bar-close-tab)
        (tab-bar-close-tab))
    (delete-window)))

;;;###autoload
(define-minor-mode terminalize-mode
  "Terminalize emacs."
  :global t
  :keymap terminalize-mode-map
  :group 'tab-bar)

(provide 'terminalize)
