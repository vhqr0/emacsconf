(prefer-coding-system 'utf-8)

(setq default-process-coding-system '(gbk-dos . gbk-dos)) ; cmd, clip, ...: gbk

(setq process-coding-system-alist
      '(("[rR][gG]" . (utf-8-dos . gbk-dos)))) ; rg: input is gbk and output is utf-8 :-)

;; for git asking password
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")
