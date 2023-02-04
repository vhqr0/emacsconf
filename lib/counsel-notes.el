

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
      (let ((directory (if (>= (prefix-numeric-value current-prefix-arg) 16)
                           (expand-file-name "posts" counsel-notes-directory)
                         counsel-notes-directory))
            (time (current-time))
            (title (read-from-minibuffer "title: "))
            (keyword (read-from-minibuffer "keyword: ")))
        (find-file (expand-file-name
                    (format "%s_%s_%s.md"
                            (format-time-string "%y%m%d" time)
                            (counsel-notes-sluggify keyword)
                            (counsel-notes-sluggify title))
                    directory))
        (insert (format "---\ntitle: %s\ndate: %s\ntag: %s\n---\n"
                        title (format-time-string "%Y-%m-%d" time) keyword)))
    (require 'counsel-projectile)
    (let ((default-directory counsel-notes-directory))
      (counsel-projectile-find-file))))
