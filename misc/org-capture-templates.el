(setq org-capture-templates
      '(("i" "inbox" entry (file "inbox.org") "* %u %?\n  %a" :clock-keep t)
        ("t" "todo" entry (file "todo.org") "* TODO %^t %?" :clock-keep t)
        ("p" "pay" table-line (file "pay.org") "|%u|%?||" :clock-keep t)
        ("c" "clock" entry (file+headline "clock.org" "clock") "* %u %? :%^g:" :clock-in t :clock-resume t)))
