;;;###autoload
(defvar xclip-program "xclip -selection clip")

;;;###autoload
(defun xclip (beg end)
  (interactive "r")
  (call-shell-region beg end xclip-program)
  (deactivate-mark))

;;;###autoload
(defvar xdg-open-program "xdg-open")

;;;###autoload
(defun xdg-open (&optional file)
  (interactive `(,(or buffer-file-name default-directory)))
  (when (and file (not (file-remote-p file)))
    (call-process-shell-command (concat xdg-open-program " " file))))

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (xdg-open file)))

(declare-function grep--save-buffers "grep")

;;;###autoload
(defvar rg-program "rg")

;;;###autoload
(defun rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: "
                       (concat rg-program " --no-heading ")
                       'grep-history)
   'grep-mode))

;;;###autoload
(defun minibuffer-yank-symbol ()
  (interactive)
  (when (window-minibuffer-p)
    (let ((symbol (with-selected-window (minibuffer-selected-window)
                    (thing-at-point 'symbol))))
      (when symbol
        (insert symbol)))))

;;;###autoload
(defun rotate-window (arg)
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

(defvar eshell-buffer-name)

(defun sp-eshell-buffer ()
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
    (with-current-buffer buffer
      (setq default-directory directory)
      (widen)
      (goto-char (point-max))
      (eshell-mode))
    buffer))

;;;###autoload
(defun sp-eshell (arg)
  (interactive "P")
  (let ((buffer (sp-eshell-buffer)))
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

;;;###autoload
(defun sp-eshell-other-tab ()
  (interactive)
  (switch-to-buffer-other-tab (sp-eshell-buffer)))

(provide 'simple-plus)
