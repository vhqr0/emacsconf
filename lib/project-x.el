;;; -*- lexical-binding: t -*-



(defvar-local project-x-lighter nil)

(defvar project-x-lighter-format " Prj<%s>")

(defun project-x-lighter-update ()
  (let ((project (project-current)))
    (setq project-x-lighter nil)
    (when project
      (project-remember-project project)
      (let* ((root (project-root project))
             (name (file-name-nondirectory (directory-file-name root)))
             (lighter (format project-x-lighter-format name)))
        (setq project-x-lighter lighter)))))

;;;###autoload
(define-minor-mode project-x-mode
  "Display current project name in mode line lighter."
  :global t
  :lighter project-x-lighter
  (dolist (hook '(find-file-hook dired-mode-hook))
    (if project-x-mode
        (add-hook hook 'project-x-lighter-update)
      (remove-hook hook 'project-x-lighter-update))))



(provide 'project-x)
