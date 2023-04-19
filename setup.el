;;; -*- lexical-binding: t -*-

;;* defvars and load custom

(defvar +setup-directory (expand-file-name "setup" +conf-directory))
(defvar +lib-directory   (expand-file-name "lib"   +conf-directory))
(defvar +misc-directory  (expand-file-name "misc"  +conf-directory))

(defvar +package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defvar +package
  '(undo-tree
    evil
    evil-surround
    evil-collection
    avy
    orderless
    vertico
    marginalia
    consult
    embark
    embark-consult
    wgrep
    magit
    projectile
    yasnippet
    company
    hl-indent-scope
    markdown-mode
    edit-indirect
    emmet-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))



;;* setup internal (lib) and external (elpa) packages

(let ((lib-autoload (expand-file-name "lib-autoload.el" +lib-directory)))
  (add-to-list 'load-path +lib-directory)
  (unless (file-exists-p lib-autoload)
    (make-directory-autoloads +lib-directory lib-autoload))
  (load-file lib-autoload))

(setq package-quickstart t
      package-archives +package-archives)

(require 'package)

(let (package-refreshed-p)
  (dolist (pkg +package)
    (unless (package-installed-p pkg)
      (unless package-refreshed-p
        (setq package-refreshed-p t)
        (package-refresh-contents))
      (package-install pkg)))
  (when package-refreshed-p
    (package-quickstart-refresh)))



;;* load *-setup

(dolist (setup
         '("defaults" "leader" "evil" "consult" "prog-tools" "ext-tools" "simple-modes"))
  (load-file (expand-file-name (concat setup "-setup.el") +setup-directory)))
