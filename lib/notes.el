

;;* sluggify
;; copy from denote.el

(defvar notes-punct-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*")

(defun notes--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string notes-punct-regexp "" str))

(defun notes--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun notes-sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase (notes--slug-hyphenate (notes--slug-no-punct str))))



;;* notes

(defvar notes-directory (expand-file-name "notes" user-emacs-directory))

;;;###autoload
(defun notes ()
  (interactive)
  (if current-prefix-arg
      (let ((directory (if (>= (prefix-numeric-value current-prefix-arg) 16)
                           (expand-file-name "posts" notes-directory)
                         notes-directory))
            (time (current-time))
            (title (read-from-minibuffer "title: "))
            (keyword (read-from-minibuffer "keyword: ")))
        (find-file (expand-file-name
                    (format "%s_%s_%s.md"
                            (format-time-string "%y%m%d" time)
                            (notes-sluggify keyword)
                            (notes-sluggify title))
                    directory))
        (insert (format "---\ntitle: %s\ndate: %s\ntag: %s\n---\n"
                        title (format-time-string "%Y-%m-%d" time) keyword)))
    (let ((default-directory notes-directory))
      (project-find-file))))

(provide 'notes)
