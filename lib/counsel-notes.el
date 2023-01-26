

;;* sluggify
;; copy from denote.el

(defvar counsel-notes-punct-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*")

(defun counsel-notes--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string counsel-notes-punct-regexp "" str))

(defun counsel-notes--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun counsel-notes-sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase (counsel-notes--slug-hyphenate (counsel-notes--slug-no-punct str))))



;;* counsel-notes

(defvar counsel-notes-directory (expand-file-name "notes" user-emacs-directory))

;;;###autoload
(defun counsel-notes ()
  (interactive)
  (if current-prefix-arg
      (let ((time (format-time-string "%y%m%d" (current-time)))
            (title (counsel-notes-sluggify (read-from-minibuffer "title: ")))
            (keyword (counsel-notes-sluggify (read-from-minibuffer "keyword: "))))
        (find-file (expand-file-name (format "%s_%s_%s.md" time keyword title) counsel-notes-directory)))
    (require 'counsel)
    (counsel-find-file (file-name-as-directory counsel-notes-directory))))
