(require 'ivy)
(require 'org)

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

;;;###autoload
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

(ivy-set-actions 'counsel-org-roam
                 '(("j" counsel-org-roam--find-other-window "other window")
                   ("i" counsel-org-roam--insert "insert")
                   ("a" counsel-org-roam--append "append")
                   ("c" counsel-org-roam--capture "capture")))

(provide 'counsel-org-roam)
