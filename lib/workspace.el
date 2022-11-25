;;; workspace.el --- Yet another workspace manager. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; workspace.el is a simple replacement of persp.el.
;; The differences of them are:
;;
;; 1. Each workspace store the tabs status instead of windows status.
;; 2. Save workspace's files instead of windows and buffers recovery information,
;;    for more security and less bugs.
;;
;; To use `workspace' in your Emacs, just bind `workspace-prefix-map'.
;; And it's recommended to turn on `workspace-mode' to display workspace information.

;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'tab-bar)



;;; Customize

(defgroup workspace nil
  "Customization of the `workspace'."
  :prefix "workspace-"
  :group 'session)

(defcustom workspace-dump-dir (expand-file-name "workspaces/" user-emacs-directory)
  "Default workspaces save file directory."
  :type 'directory
  :group 'workspace)

(defface workspace-face-default
  '((t :inherit italic))
  "Workspace mode lighter default face."
  :group 'workspace)

(defface workspace-face-nil
  '((t :inherit bold-italic))
  "Workspace mode lighter nil face."
  :group 'workspace)

(defface workspace-face-not-in
  '((default . (:background "#f00" :foreground "#00f" :weight bold)))
  "Workspace mode buffer not in workspace face."
  :group 'workspace)



;;; Core struct and global var

(cl-defstruct workspace
  "Store workspace's layout wc&tabs, and related buffers&files."
  (name "")
  (wc nil)
  (tabs nil)
  (buffers nil)
  (files nil))

(defvar workspace-alist nil
  "Alist of workspace (name . workspace).")



;;; Basic util functions

(defun workspace-name-safe (workspace)
  "Safe version of `workspace-name'.
WORKSPACE: see `workspace-name'."
  (when workspace (workspace-name workspace)))

(defun workspace-buffers-safe (workspace)
  "Safe version of `workspace-buffers'.
WORKSPACE: see `workspace-buffers'."
  (when workspace (workspace-buffers workspace)))

(defun workspace-files-safe (workspace)
  "Safe version of `workspace-files'.
WORKSPACE: see `workspace-files'."
  (when workspace (workspace-files workspace)))

(defun workspace-current-workspace (&optional frame)
  "`current-window-configuration'-like api for workspace.
Return the workspace of FRAME.
If FRAME is nil, use selected frame."
  (frame-parameter (or frame (selected-frame)) 'workspace))

(defun workspace-set-workspace (workspace &optional frame)
  "`set-window-configuration'-like api for workspace.
Set FRAME's workspace to WORKSPACE.
If FRAME is nil, use selected frame."
  (let ((frame (or frame (selected-frame))))
    (set-frame-parameter frame 'workspace workspace)
    (when workspace
      (let ((wc (workspace-wc workspace))
            (tabs (workspace-tabs workspace)))
        ;; restore window configuration
        (when wc
          (with-selected-frame frame
            (set-window-configuration wc)))
        ;; restore tab-bar-tabs
        ;; notice that tabs may be nil
        (tab-bar-tabs-set tabs frame))))
  ;; update tab-bar-line for frame
  (tab-bar--update-tab-bar-lines (list frame)))

(defun workspace-alist-add (workspace)
  "Add WORKSPACE to `workspace-alist' if none."
  (when workspace
    (let* ((name (workspace-name workspace))
           (old-cons (assoc name workspace-alist)))
      (unless old-cons
        (setq workspace-alist
              (cons (cons name workspace) workspace-alist))))))

(defun workspace-alist-get (name &optional do-create)
  "Get workspace from `workspace-alist' by NAME.
If DO-CREATE, create workspace if none."
  (when name
    (let ((workspace (cdr-safe (assoc name workspace-alist))))
      (when (and do-create (not workspace))
        (setq workspace (make-workspace :name name))
        (workspace-alist-add workspace))
      workspace)))

(defun workspace-alist-delete (name)
  "Delete workspace from `workspace-alist' by NAME."
  (setq workspace-alist (assoc-delete-all name workspace-alist)))

(defun workspace-alist-clear (&optional do-confirm)
  "Clear all workspaces from `workspace-alist'.
If DO-CONFIRM, confirm before clear."
  (interactive '(t))
  (if (and do-confirm
           (not (y-or-n-p "Clear all workspace?")))
      (user-error "Clear workspaces abort!")
    (setq workspace-alist nil)))

(defalias 'workspace-clear-workspaces 'workspace-alist-clear)



;;; Workspace management functions

(defvar workspace-read-name-history nil)

(defun workspace-read-name (prompt &optional require-match def)
  "Read a workspace name from `workspace-alist'.
PROMPT REQUIRE-MATCH DEF: see `completing-read'."
  (let* ((names (mapcar 'car workspace-alist))
         (name (completing-read
                prompt names nil require-match nil workspace-read-name-history def)))
    ;; should return nil instead of empty string
    (unless (string-empty-p name)
      name)))

(defun workspace-sync-frame (&optional frame)
  "Sync FRAME layout wc&tabs to it's workspace."
  (interactive)
  (let ((frame (or frame (selected-frame)))
        (workspace (workspace-current-workspace frame)))
    (when workspace
      (setf (workspace-wc workspace) (current-window-configuration frame))
      (setf (workspace-tabs workspace) (tab-bar-tabs frame)))))

;; store previous workspace before switch
(defvar workspace-saved-workspace nil)

(defun workspace--switch-to-workspace (workspace frame)
  "Switch FRAME's workspace to WORKSPACE."
  (let* ((frame (or frame (selected-frame))))
    ;; sync layout to old workspace
    (workspace-sync-frame frame)
    ;; store old workspace
    (setq workspace-saved-workspace
          (workspace-current-workspace frame))
    ;; set new Workspace
    (workspace-set-workspace workspace frame)))

;;;###autoload
(defun workspace-switch-to-workspace (name &optional frame)
  "Read a workspace by NAME to switch in FRAME.
If FRAME is nil, use selected frame."
  (interactive `(,(workspace-read-name
                   (format "Switch to workspace (current #%s): "
                           (or (workspace-name-safe (workspace-current-workspace))
                               "nil")))))
  (workspace--switch-to-workspace (workspace-alist-get name t) frame))

;;;###autoload
(defun workspace-switch-to-next-workspace (&optional frame)
  "Switch to next workspace in FRAME.
If FRAME is nil, use selected frame."
  (interactive)
  (let ((name (workspace-name-safe (workspace-current-workspace frame))))
    (if name
        (let ((alist workspace-alist)
              elm stop)
          (while (and alist (not stop))
            (setq elm (car alist)
                  alist (cdr alist))
            (when (string-equal (car elm) name)
              (setq stop t)
              (workspace--switch-to-workspace (cdar (or alist workspace-alist)) frame)))
          (when (and (not stop) workspace-alist)
            (workspace--switch-to-workspace (cdar workspace-alist) frame)))
      (when workspace-alist
        (workspace--switch-to-workspace (cdar workspace-alist) frame)))))

;;;###autoload
(defun workspace-switch-to-workspace-undo (&optional frame)
  "Switch to recent switched workspace in FRAME."
  (interactive)
  (workspace--switch-to-workspace workspace-saved-workspace frame))

;;;###autoload
(defun workspace-remove-workspace (name &optional do-confirm)
  "Read a workspace by NAME to delete.
If DO-CONFIRM, confirm before delete."
  (interactive `(,(let ((name (workspace-name-safe (workspace-current-workspace))))
                    (workspace-read-name
                     (format "Delete workspace (current #%s):" (or name "nil")) t name))
                 t))
  (when name
    (let ((workspace (workspace-alist-get name)))
      (if workspace
          (when (or (not do-confirm)
                    (y-or-n-p (format "Do you sure you want to delete workspace #%s?" name)))
            (workspace-alist-delete name)
            ;; notice may have a frame still hold the deleted workspace
            ;; so we add a flag <d> to it's name to highlight it
            (setf (workspace-name workspace) (format "%s<d>" name)))
        (when do-confirm
          (user-error "Workspace #%s doesn't exists!" name))))))

;;;###autoload
(defun workspace-rename-workspace (workspace name &optional do-duplicate do-confirm)
  "Rename current WORKSPACE to NAME.
If DO-DUPLICATE, duplicate a new workspace to rename.
If DO-CONFIRM, confirm before cover other workspace."
  (interactive `(,(progn
                    ;; sync before rename or duplicate
                    (workspace-sync-frame)
                    (workspace-current-workspace))
                 ,(read-string (format (if current-prefix-arg
                                           "Duplicate workspace #%s: "
                                         "Rename workspace #%s: ")
                                       (or (workspace-name-safe (workspace-current-workspace)) "nil")))
                 ,current-prefix-arg
                 t))
  (let (do-add)
    (cond ((not workspace)
           (when do-confirm
             (user-error "Current workspace is #nil!")))
          ((string-empty-p name)
           (when do-confirm
             (user-error "Workspace name cannot be empty!")))
          ((workspace-alist-get name)
           ;; new name exists, confirm to delete it or not
           (if (or (not do-confirm)
                   (y-or-n-p (format "Workspace #%s is already exists, delete it?" name)))
               (progn
                 (workspace-alist-delete name)
                 (setq do-add t))
             (user-error "Abort rename workspace #%s" name)))
          (t
           (setq do-add t)))
    (when do-add
      (if do-duplicate
          ;; set workspace to a deep copied one
          (setq workspace (make-workspace :name name
                                          :wc (workspace-wc workspace)
                                          :tabs (copy-sequence (workspace-tabs workspace))
                                          :buffers (copy-sequence (workspace-buffers workspace))
                                          :files (copy-sequence (workspace-files workspace))))
        (workspace-alist-delete (workspace-name workspace))
        (setf (workspace-name workspace) name))
      (workspace-alist-add workspace))))



;;; Workspace buffer functions

(defvar workspace-read-buffer-history nil)

(defun workspace-read-buffer (workspace prompt &optional def)
  "Read a buffer from WORKSPACE buffers.
PROMPT DEF: see `completing-read'."
  (let ((buffers (remove nil (mapcar 'buffer-name (workspace-buffers-safe workspace)))))
    (completing-read prompt buffers nil t nil workspace-read-buffer-history def)))

(defun workspace-sync-buffers (workspace)
  "Remove killed buffers from current WORKSPACE buffers."
  (interactive `(,(workspace-current-workspace)))
  (when workspace
    (setf (workspace-buffers workspace)
          (cl-remove-if-not 'buffer-name (workspace-buffers workspace)))))

(defun workspace--buffer-file-name (buffer)
  "Wrap function `buffer-file-name' with `dired-directory' backport.
BUFFER: see function `buffer-file-name'."
  (expand-file-name
   (with-current-buffer buffer
     (if (eq major-mode 'dired-mode)
         dired-directory
       buffer-file-name))))

;;;###autoload
(defun workspace-add-buffer (workspace &optional buffer-or-name do-confirm)
  "Add current BUFFER-OR-NAME to current WORKSPACE.
DO-CONFIRM: compatible to other functions to warn when failed."
  (interactive `(,(workspace-current-workspace) ,(current-buffer) t))
  (cond (workspace
         (let* ((buffers (workspace-buffers workspace))
                (files (workspace-files workspace))
                (buffer (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer)))
                (file (workspace--buffer-file-name buffer)))
           (unless (memq buffer buffers)
             (setf (workspace-buffers workspace) (cons buffer buffers))
             ;; add workspace file if needed
             (when (and file (not (member file files)))
               (setf (workspace-files workspace) (cons file files))))))
        (do-confirm
         (user-error "Current workspace is #nil!"))))

;;;###autoload
(defun workspace-remove-buffer (workspace &optional buffer-or-name do-confirm)
  "Read a BUFFER-OR-NAME to remove from current WORKSPACE buffers.
DO-CONFIRM: compatible to other functions to warn when failed."
  (interactive `(,(workspace-current-workspace)
                 ,(let ((workspace (workspace-current-workspace)))
                    (workspace-sync-buffers workspace)
                    (workspace-read-buffer workspace
                                           (format "Remove buffer from #%s: "
                                                   (or (workspace-name-safe workspace) "nil"))
                                           ;; default to current buffer
                                           (buffer-name (current-buffer))))
                 t))
  (cond (workspace
         (let* ((buffer (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer)))
                (file (workspace--buffer-file-name buffer)))
           (setf (workspace-buffers workspace) (cl-delete buffer (workspace-buffers workspace)))
           ;; remove workspace file if needed
           (when file
             (setf (workspace-files workspace) (cl-delete file (workspace-files workspace)
                                                          :test 'string-equal)))))
        (do-confirm
         (user-error "Current workspace is #nil!"))))

;;;###autoload
(defun workspace-switch-to-buffer (&optional other-window)
  "Read a buffer to switch from current workspace buffers.
If OTHER-WINDOW, switch to buffer other window."
  (interactive)
  (let ((workspace (workspace-current-workspace)))
    (if workspace
        (progn
          (workspace-sync-buffers workspace)
          (let ((buffer (workspace-read-buffer workspace
                                               (format "Switch to buffer from workspace (current is #%s): "
                                                       (workspace-name workspace)))))
            (when buffer
              (if other-window
                  (switch-to-buffer-other-window buffer)
                (switch-to-buffer buffer)))))
      (user-error "Current workspace is #nil!"))))

;;;###autoload
(defun workspace-switch-to-buffer-other-window ()
  "Other window version of `workspace-switch-to-buffer'."
  (interactive)
  (workspace-switch-to-buffer t))



;;; Workspace file functions

(defvar workspace-read-file-history nil)

(defun workspace-read-file (workspace prompt)
  "Read a file from WORKSPACE files.
PROMPT: see `completing-read'."
  (let ((files (workspace-files-safe workspace)))
    (completing-read prompt files nil t nil workspace-read-file-history)))

(defun workspace-sync-files (workspace)
  "Remove not exists files from current WORKSPACE files."
  (interactive `(,(workspace-current-workspace)))
  (when workspace
    (setf (workspace-files workspace)
          (cl-remove-if-not 'file-exists-p (workspace-files workspace)))))

;;;###autoload
(defun workspace-find-file (&optional other-window)
  "Read a file to find from current workspace files.
If OTHER-WINDOW, find file other window."
  (interactive)
  (let ((workspace (workspace-current-workspace)))
    (if workspace
        (let ((file (workspace-read-file workspace
                                         (format "Find file from workspace (current is #%s): "
                                                 (workspace-name workspace)))))
          (when file
            (cond ((not (file-exists-p file))
                   ;; file not exists, remove from workspace files
                   (setf (workspace-files workspace)
                         (cl-delete file (workspace-files workspace) :test 'string-equal))
                   (user-error "File not exists: %s" file))
                  (other-window
                   (find-file-other-window file))
                  (t
                   (find-file file)))))
      (user-error "Current workspace is #nil!"))))

;;;###autoload
(defun workspace-find-file-other-window ()
  "Other window version of `workspace-find-file'."
  (interactive)
  (workspace-find-file t))



;;; Workspace save&load functions

;;;###autoload
(defun workspace-save-file (file &optional do-confirm)
  "Save `workspace-alist' and their files to FILE.
If DO-CONFIRM, confirm before delete exists file."
  (interactive `(,(read-file-name "Workspace dump file: " workspace-dump-dir)
                 t))
  (let ((exists (file-exists-p file)))
    (when (and exists do-confirm)
      (if (y-or-n-p (format "File %s exists, delete it?" file))
          ;; notice that we use trash here
          (delete-file file t)
        (user-error "Save workspace abort!")))
    (unless (and exists (not do-confirm))
      (with-temp-buffer
        (insert (pp-to-string
                 (mapcar (lambda (elm)
                           (let ((workspace (cdr elm)))
                             (cons (workspace-name workspace)
                                   (workspace-files workspace))))
                         workspace-alist)))
        (write-file file)))))

;;;###autoload
(defun workspace-load-file (file &optional do-confirm)
  "Load `workspace-alist' from FILE and recovery workspace files.
If DO-CONFIRM, confirm save current `workspace-alist' before load."
  (interactive `(,(read-file-name "Workspace dump file: " workspace-dump-dir nil t)
                 t))
  (cond ((file-exists-p file)
         (when (and do-confirm workspace-alist
                    (y-or-n-p "Save workspace before load another?"))
           (call-interactively 'workspace-save-file))
         (let ((alist (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (read (current-buffer)))))
           (setq workspace-alist
                 (mapcar (lambda (elm)
                           (let ((name (car elm)) (files (cdr elm)))
                             (cons name (make-workspace :name name :files files))))
                         alist))))
        (do-confirm
         (user-error "File %s doesn't exists!"))))



;;; Workspace mode

(defun workspace-compute-lighter ()
  "Compute `workspace-mode' lighter."
  (let ((workspace (workspace-current-workspace)))
    (if workspace
        (propertize (format " #%s" (workspace-name workspace)) 'face
                    (if (memq (current-buffer) (workspace-buffers workspace))
                        'workspace-face-default
                      'workspace-face-not-in))
      (propertize
       " #nil"
       'face
       'workspace-face-nil))))

;;;###autoload
(define-minor-mode workspace-mode
  "Workspace Mode.
This mode is aim to display workspace information in modeline lighter.
It is not necessary but recommended to turn on this mode when you use workspace."
  :init-value nil
  :global t
  :group 'workspace
  :lighter (:eval (workspace-compute-lighter)))



;;; Workspace keymap

;;;###autoload
(defvar workspace-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m"  'workspace-mode)
    (define-key map "s"  'workspace-switch-to-workspace)
    (define-key map "n"  'workspace-switch-to-next-workspace)
    (define-key map "u"  'workspace-switch-to-workspace-undo)
    (define-key map "K"  'workspace-remove-workspace)
    (define-key map "R"  'workspace-rename-workspace)
    (define-key map "a"  'workspace-add-buffer)
    (define-key map "k"  'workspace-remove-buffer)
    (define-key map "b"  'workspace-switch-to-buffer)
    (define-key map "4b" 'workspace-switch-to-buffer-other-window)
    (define-key map "f"  'workspace-find-file)
    (define-key map "4f" 'workspace-find-file-other-window)
    (define-key map "S"  'workspace-save-file)
    (define-key map "L"  'workspace-load-file)
    map))



(provide 'workspace)
;;; workspace.el ends here
