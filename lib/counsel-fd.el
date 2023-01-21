;;; counsel-fd.el --- Ivy interface for fdfind. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ivy)
(require 'counsel)

(defvar counsel-fd-command "fdfind")
(defvar counsel-fd-options "-c never -t f")

(defvar counsel-fd-history nil)

(defun counsel-fd-cmd (input)
  "Return a `fdfind' shell command based on regexp INPUT.
This uses the user options `counsel-fd-command' and `counsel-fd-options'."
  (counsel-require-program counsel-fd-command)
  (format "%s %s %s"
          counsel-fd-command
          counsel-fd-options
          (shell-quote-argument
           (counsel--elisp-to-pcre
            (ivy--regex input)))))

(defun counsel-fd-function (input)
  "Call a \"fdfind\" style shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (counsel-fd-cmd input))
     '("" "working..."))))

;;;###autoload
(defun counsel-fd (&optional initial-input initial-directory fd-options)
  "Call a \"fdfind\" style shell command.

By default, the root directory is the first directory containing
a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FD-OPTIONS, if non-nil, is replaced to `counsel-fd-options'.

With `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY and FD-OPTIONS."
  (interactive)
  (if (>= (prefix-numeric-value current-prefix-arg) 16)
      (counsel-locate initial-input)
    (let ((default-directory (or initial-directory
                                 (and current-prefix-arg
                                      (counsel-read-directory-name "fd in directory: "))
                                 (counsel--git-root)
                                 default-directory))
          (counsel-fd-options (or fd-options
                                  (and current-prefix-arg
                                       (read-from-minibuffer "fd options: " counsel-fd-options))
                                  counsel-fd-options)))
      (ivy-read "fd: " 'counsel-fd-function
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-fd-history
                :action (lambda (file)
                          (when file
                            (with-ivy-window
                              (find-file
                               (concat (file-remote-p default-directory) file)))))
                :caller 'counsel-fd))))

(ivy-configure 'counsel-locate
  :unwind-fn #'counsel-delete-process
  :exit-codes '(1 "Nothing found"))

(ivy-set-actions
 'counsel-fd
 '(("j" find-file-other-window "other window")
   ("r" counsel-find-file-as-root "open as root")
   ("d" counsel-locate-action-dired "dired")
   ("x" counsel-locate-action-extern "xdg-open")))

(provide 'counsel-fd)
;;; counsel-fd.el ends here
