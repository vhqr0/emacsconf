(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(when (file-exists-p package-quickstart-file)
  (defvar package-activated-list nil)
  (load package-quickstart-file))
