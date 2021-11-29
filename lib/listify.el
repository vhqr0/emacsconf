;;; listify.el --- Yet another completion api implementation. -*- lexical-binding: t -*-

;;; Commentary:
;; Add this code to your init file:
;; (global-set-key (kbd "<f2>") 'listify-tab-completion)
;; (global-set-key (kbd "<f5>") 'listify-switch-to-buffer)
;; (global-set-key (kbd "<f6>") 'listify-project-find-file)
;; (define-key company-active-map "\M-o" 'listify-company)

;;; Code:
(require 'subr-x)
(require 'hl-line)

(defgroup listify nil
  "Completing read UI."
  :prefix "listify-"
  :group 'listify)

(defcustom listify-idle-delay 0.15
  "The idle delay in seconds to update `listify-window'."
  :type 'number)

(defvar listify-collection nil)
(defvar listify-window nil)
(defvar listify-timer nil)

(defvar listify-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'listify-next)
    (define-key map (kbd "C-p") 'listify-prev)
    (define-key map (kbd "RET") 'listify-exit-minibuffer)
    (make-composed-keymap map minibuffer-local-map)))

(defun listify-update (&optional query)
  "Update completion lists, initial query can specified by QUERY."
  (let ((query (or query (minibuffer-contents))))
    (if listify-window
        (with-selected-window listify-window
          (unless (equal query header-line-format)
            (setq header-line-format query)
            (erase-buffer)
            (if (string-empty-p query)
                (let ((count 0)
                      (current listify-collection))
                  (while (and (< count 50) current)
                    (insert (car current) "\n")
                    (setq count (1+ count)
                          current (cdr current))))
              (let ((regexp (string-join (split-string query) ".*")))
                (condition-case nil
                    (let ((count 0)
                          (current listify-collection))
                      (while (and (< count 50) current)
                        (when (string-match regexp (car current))
                          (let ((match-data (match-data))
                                (copyed (copy-sequence (car current))))
                            (font-lock-prepend-text-property
                             (car match-data) (cadr match-data) 'face 'match copyed)
                            (insert copyed "\n"))
                          (setq count (1+ count)))
                        (setq current (cdr current))))
                  (invalid-regexp nil))))
            (goto-char (point-min))
            (hl-line-highlight)))
      (cancel-timer listify-timer))))

(defun listify-next ()
  "Listify next line."
  (interactive)
  (with-selected-window listify-window
    (forward-line)
    (hl-line-highlight)))

(defun listify-prev ()
  "Listify previous line."
  (interactive)
  (with-selected-window listify-window
    (forward-line -1)
    (hl-line-highlight)))

(defun listify-exit-minibuffer ()
  "Select current candidate."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (insert (with-selected-window listify-window
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))))
  (exit-minibuffer))

(defun listify-read (prompt collection)
  "Read from minibuffer and select with listify PROMPT COLLECTION."
  (save-window-excursion
    (let* ((listify-collection collection)
           (listify-window
            (with-selected-window (if (window-minibuffer-p)
                                      (minibuffer-selected-window)
                                    (selected-window))
              (switch-to-buffer-other-window "*listify*")
              (setq truncate-lines t)
              (hl-line-mode 1)
              (selected-window))))
      (listify-update "")
      (setq listify-timer (run-with-idle-timer listify-idle-delay t 'listify-update))
      (unwind-protect
          (read-from-minibuffer prompt nil listify-map)
        (kill-buffer (window-buffer listify-window))))))

(defun listify-completion-in-region (beg end collection predicate)
  "Completion in region replacement with `listify-read'.
BEG, END, COLLECTION, PREDICATE see `completion-in-region-function'."
  (completion-in-region-mode -1)
  (let* ((enable-recursive-minibuffers t)
         (prefix (buffer-substring beg end))
         (boundary (+ beg (car (completion-boundaries
                                prefix collection predicate ""))))
         (choices (nconc (completion-all-completions
                          prefix collection predicate (length prefix))
                         nil))
         (choice (cond ((null choices) nil)
                       ((null (cdr choices)) (car choices))
                       (t (listify-read "complete: " choices)))))
    (when choice
      (delete-region boundary end)
      (insert (substring-no-properties choice)))))

;;;###autoload
(defun listify-tab-completion ()
  "Tab completion with `listify-completion-in-region'."
  (interactive)
  (let* ((completion-in-region-function 'listify-completion-in-region)
         (command (lookup-key `(,(current-local-map) ,(current-global-map)) (kbd "TAB")))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (call-interactively command)))

(defvar recentf-list)

;;;###autoload
(defun listify-switch-to-buffer (arg)
  "Switch to buffer or recent file with `listify-read'.
In other window if ARG not nil."
  (interactive "P")
  (require 'recentf)
  (let* ((buffers (seq-filter
                   (lambda (x)
                     (not (= (aref x 0) ?\s)))
                   (mapcar 'buffer-name (buffer-list))))
         (choice (listify-read "open: " (append buffers recentf-list))))
    (when choice
      (if (member choice buffers)
          (if arg
              (switch-to-buffer-other-window choice)
            (switch-to-buffer choice))
        (if arg
            (find-file-other-window choice)
          (find-file choice))))))

;;;###autoload
(defun listify-project-find-file (arg)
  "Open project file with `listify-read'.
In other window if ARG not nil."
  (interactive "P")
  (let ((choice (listify-read "open: "
                              (split-string (shell-command-to-string "rg --files")))))
    (when choice
      (if arg
          (find-file-other-window choice)
        (find-file choice)))))

(defvar company-prefix)
(defvar company-candidates)
(declare-function company-abort "company")

;;;###autoload
(defun listify-company ()
  "Select company candidate with `listify-read'."
  (interactive)
  (when company-candidates
    (let ((length (length company-prefix))
          (candidates company-candidates))
      (company-abort)
      (let ((candidate (listify-read "complete: " candidates)))
        (when candidate
          (delete-char (- length))
          (insert candidate))))))

(provide 'listify)
;;; listify.el ends here
