(setq org-capture-templates
      '(("i" "inbox" entry (file "inbox.org") "* %?\n  %u\n  %a" :clock-keep t)
        ("t" "todo" entry (file "todo.org") "* TODO %^t %?" :clock-keep t)
        ("p" "payment" table-line (file "payment.org") "|%u|%?||" :clock-keep t)
        ("w" "working" entry (file+headline "sched.org" "sched") "* %u %? :working:" :clock-in t :clock-resume t)
        ("r" "reading" entry (file+headline "sched.org" "sched") "* %u %? :reading:" :clock-in t :clock-resume t)
        ("c" "coding" entry (file+headline "sched.org" "sched") "* %u %? :coding:" :clock-in t :clock-resume t)))
