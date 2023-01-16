;;; -*- lexical-binding: t -*-

;;* defvars and load custom

(defvar +setup-directory (expand-file-name "setup" +conf-directory))
(defvar +lib-directory (expand-file-name "lib" +conf-directory))
(defvar +misc-directory (expand-file-name "misc" +conf-directory))

(defvar +package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                            ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                            ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defvar +package
  '(evil
    evil-surround
    evil-collection
    general
    ivy
    swiper
    counsel
    hydra
    ivy-hydra
    helpful
    amx
    wgrep
    magit
    projectile
    counsel-projectile
    yasnippet
    company
    markdown-mode
    edit-indirect
    htmlize
    org-roam
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

(load-file (expand-file-name "defaults-setup.el" +setup-directory))
(load-file (expand-file-name "maps-setup.el" +setup-directory))
(load-file (expand-file-name "evil-setup.el" +setup-directory))
(load-file (expand-file-name "leader-setup.el" +setup-directory))
(load-file (expand-file-name "ivy-setup.el" +setup-directory))
(load-file (expand-file-name "prog-tools-setup.el" +setup-directory))
(load-file (expand-file-name "ext-tools-setup.el" +setup-directory))
(load-file (expand-file-name "simple-modes-setup.el" +setup-directory))
(load-file (expand-file-name "org-setup.el" +setup-directory))
