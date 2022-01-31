;;; listify.el --- Yet another completion api implementation. -*- lexical-binding: t -*-

;;; Commentary:
;; Add this code to your init file:
;; (global-set-key (kbd "<f2>") 'listify-tab-completion)
;; (global-set-key (kbd "<f5>") 'listify-open)
;; (global-set-key (kbd "C-M-/") 'listify-dabbrev-completion)

;;; Code:
(require 'subr-x)
(require 'hl-line)

(defgroup listify nil
  "Completing read UI."
  :prefix "listify-"
  :group 'listify)

(defcustom listify-idle-delay 0.1
  "The idle delay in seconds to update `listify-window'."
  :type 'number)

(defvar listify-collection nil)
(defvar listify-window nil)
(defvar listify-timer nil)

(defvar listify-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'listify-next-line)
    (define-key map (kbd "C-p") 'listify-previous-line)
    (define-key map (kbd "M-<") 'listify-beginning-of-buffer)
    (define-key map (kbd "M->") 'listify-end-of-buffer)
    (define-key map (kbd "C-o") 'listify-exit-minibuffer)
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
                  (while (and (< count (- (window-height) 2)) current)
                    (insert (car current) "\n")
                    (setq count (1+ count)
                          current (cdr current))))
              (let ((regexp (string-join (split-string query) ".*")))
                (condition-case nil
                    (let ((count 0)
                          (current listify-collection))
                      (while (and (< count (- (window-height) 2)) current)
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

(defun listify-next-line ()
  "Listify next line."
  (interactive)
  (with-selected-window listify-window
    (unless (= (- (window-height) 2) (line-number-at-pos))
      (forward-line))
    (when (= (point) (point-max))
      (forward-line -1))
    (hl-line-highlight)))

(defun listify-previous-line ()
  "Listify previous line."
  (interactive)
  (with-selected-window listify-window
    (forward-line -1)
    (hl-line-highlight)))

(defun listify-beginning-of-buffer ()
  "Listify beginning of buffer."
  (interactive)
  (with-selected-window listify-window
    (goto-char (point-min))
    (hl-line-highlight)))

(defun listify-end-of-buffer ()
  "Listify end of buffer."
  (interactive)
  (with-selected-window listify-window
    (goto-char (point-min))
    (forward-line (- (window-height) 3))
    (when (= (point) (point-max))
      (forward-line -1))
    (hl-line-highlight)))

(defun listify-exit-minibuffer ()
  "Select current candidate."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (insert (with-selected-window listify-window
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))))
  (exit-minibuffer))

(defun listify-read (prompt collection &optional initial-contents)
  "Read from minibuffer and select with listify PROMPT COLLECTION."
  (save-window-excursion
    (let* ((listify-collection collection)
           (listify-window
            (with-selected-window (if (window-minibuffer-p)
                                      (minibuffer-selected-window)
                                    (selected-window))
              (select-window (split-window))
              (switch-to-buffer "*listify*")
              (setq truncate-lines t)
              (hl-line-mode 1)
              (selected-window))))
      (listify-update (or initial-contents ""))
      (setq listify-timer (run-with-idle-timer listify-idle-delay t 'listify-update))
      (unwind-protect
          (read-from-minibuffer prompt initial-contents listify-map)
        (kill-buffer (window-buffer listify-window))))))

(defun listify-completion-in-region (beg end collection &optional predicate)
  "Completion in region replacement with `listify-read'.
BEG, END, COLLECTION, PREDICATE see `completion-in-region-function'."
  (when completion-in-region-mode
    (completion-in-region-mode -1))
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
(defun listify-tab-completion (arg)
  "Tab completion with `listify-completion-in-region'."
  (interactive "P")
  (let* ((completion-in-region-function 'listify-completion-in-region)
         (command (lookup-key `(,(current-local-map) ,(current-global-map))
                              (if arg
                                  (read-key-sequence-vector "command: ")
                                (kbd "TAB"))))
         (command (if (memq command '(indent-for-tab-command c-indent-line-or-region))
                      'completion-at-point
                    command)))
    (call-interactively command)))

(defvar dabbrev-case-replace)

;;;###autoload
(defun listify-dabbrev-completion ()
  "`dabbrev-completion' with `listify-completion-in-region'."
  (interactive)
  (let ((dabbrev-case-replace nil)
        (completion-in-region-function 'listify-completion-in-region))
    (call-interactively 'dabbrev-completion)))

(defvar recentf-list)

;;;###autoload
(defun listify-open (arg)
  "Open buffer or recent file with `listify-read'.
Open file in current project if ARG not nil."
  (interactive "P")
  (if arg
      (let* ((default-directory (if (> (prefix-numeric-value arg) 4)
                                    default-directory
                                  (or (cdr (project-current)) default-directory)))
             (choice (listify-read "open: "
                                   (split-string (shell-command-to-string "rg --files")))))
        (when choice
          (if (eq last-command-event ?\C-m)
              (find-file choice)
            (find-file-other-window choice))))
    (require 'recentf)
    (let* ((buffers (seq-filter
                     (lambda (x)
                       (not (= (aref x 0) ?\s)))
                     (mapcar 'buffer-name (buffer-list))))
           (choice (listify-read "open: " (append buffers recentf-list))))
      (when choice
        (if (member choice buffers)
            (if (eq last-command-event ?\C-m)
                (switch-to-buffer choice)
              (switch-to-buffer-other-window choice))
          (if (eq last-command-event ?\C-m)
              (find-file choice)
            (find-file-other-window choice)))))))

(provide 'listify)
;;; listify.el ends here
