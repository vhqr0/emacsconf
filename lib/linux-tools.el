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
  (when file
    (call-process-shell-command (concat xdg-open-program " " file))))

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

(provide 'linux-tools)
