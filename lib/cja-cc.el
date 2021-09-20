(require 'cja)
(require 'cc-mode)

(cja-add-abbrevs java-mode-abbrev-table
                 '(("sop" "System.out.println();" . 2)
                   ("str" "String")
                   ("al" "ArrayList")
                   ("ll" "LinkedList")
                   ("ts" "TreeSet")
                   ("hs" "HashSet")
                   ("lhs" "LinkedHashSet")
                   ("tm" "TreeMap")
                   ("hm" "HashMap")
                   ("lhm" "LinkedHashMap")))

(provide 'cja-cc)
