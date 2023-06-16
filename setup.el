;;; -*- lexical-binding: t -*-

;;* defvars

(defvar +setup-directory  (expand-file-name "setup"  +conf-directory))
(defvar +lib-directory    (expand-file-name "lib"    +conf-directory))
(defvar +misc-directory   (expand-file-name "misc"   +conf-directory))
(defvar +themes-directory (expand-file-name "themes" +conf-directory))

(defvar +init-setups
  '(defaults
    keybindings
    evil
    consult
    prog-tools
    ext-tools
    simple-modes))
(defvar +misc-setups nil)

;;* load custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;* setup package

(defvar package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                           ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-quickstart t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)

;;* setup internal (lib) and external (elpa) packages

(add-to-list 'custom-theme-load-path +themes-directory)

(let ((lib-autoload (expand-file-name "lib-autoload.el" +lib-directory)))
  (add-to-list 'load-path +lib-directory)
  (unless (file-exists-p lib-autoload)
    (make-directory-autoloads +lib-directory lib-autoload))
  (load-file lib-autoload))



;;* load *-setup

(defun +setup-path (name directory)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (expand-file-name (concat name "-setup.el") directory))

(dolist (setup +init-setups)
  (load-file (+setup-path setup +setup-directory)))

(dolist (setup +misc-setups)
  (load-file (+setup-path setup +misc-directory)))
