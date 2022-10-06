(setq org-directory (expand-file-name "~/org")
      org-agenda-files (list org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory))

(with-eval-after-load 'org
  (org-roam-db-autosync-mode 1)
  (define-key org-mode-map [remap eldoc-doc-buffer] 'org-roam-buffer-toggle)
  (define-key org-mode-map [remap evil-ret] 'org-open-at-point)
  (define-key org-mode-map [remap org-set-tags-command] 'counsel-org-tag))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map [remap org-agenda-set-tags] 'counsel-org-tag-agenda))



(defun counsel-org-roam--find(x &optional other-window)
  (let ((node (if (consp x)
                  (cdr x)
                (org-roam-node-create :title x))))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :props '(:finalize find-file)))))

(defun counsel-org-roam--find-other-window(x)
  (counsel-org-roam--find x t))

(defun counsel-org-roam--insert(x &optional append)
  (let ((node (if (consp x)
                  (cdr x)
                (org-roam-node-create :title x))))
    (when (and append (not (eolp)))
      (forward-char))
    (if (org-roam-node-id node)
        (insert (org-link-make-string
                 (concat "id:" (org-roam-node-id node))
                 (org-roam-node-formatted node)))
      (org-roam-capture-
       :node node
       :props '(:finalize insert-link)))))

(defun counsel-org-roam--append(x)
  (counsel-org-roam--insert x t))

(defun counsel-org-roam--capture (x)
  (let ((node (if (consp x)
                  (cdr x)
                (org-roam-node-create :title x))))
    (org-roam-capture-
     :node node
     :props '(:immediate-finish nil))))

(defun counsel-org-roam ()
  (interactive)
  (require 'ivy)
  (require 'org-roam)
  (let ((nodes (org-roam-node-read--completions)))
    (ivy-read "Node: "
	      (org-roam-node-read--completions)
	      :action 'counsel-org-roam--find
	      :history 'org-roam-node-history
	      :caller 'counsel-org-roam)))

(with-eval-after-load 'ivy
  (ivy-set-actions 'counsel-org-roam
                   '(("j" counsel-org-roam--find-other-window "other window")
                     ("i" counsel-org-roam--insert "insert")
                     ("a" counsel-org-roam--append "append")
                     ("c" counsel-org-roam--capture "capture"))))



(defvar org-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'org-store-link)
    (define-key map "i" 'org-insert-link-global)
    (define-key map "o" 'org-open-at-point-global)
    (define-key map "a" 'org-agenda)
    (define-key map "j" 'counsel-org-goto-all)
    (define-key map "J" 'counsel-org-agenda-headlines)
    (define-key map "c" 'counsel-org-capture)
    (define-key map "l" 'counsel-org-roam)
    (define-key map "C" 'org-roam-dailies-capture-today)
    (define-key map "L" 'org-roam-dailies-goto-today)
    (define-key map "u" 'org-roam-ui-mode)
    map))

(global-set-key (kbd "C-c l") org-prefix-map)

(with-eval-after-load 'evil-setup
  (define-key evil-leader-map "l" org-prefix-map))

(provide 'org-setup)
