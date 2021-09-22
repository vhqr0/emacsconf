;;; company-keywords.el --- A company backend for programming language keywords

;; Copyright (C) 2009-2011, 2016  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar company-keywords-alist
  '((java-mode
     "abstract" "assert" "boolean" "break" "catch" "class" "continue"
     "default" "double" "extends" "final" "finally" "float"
     "implements" "import" "instanceof" "interface" "native" "package"
     "private" "protected" "public" "return" "short" "static"
     "strictfp" "super" "switch" "synchronized" "throw" "throws"
     "transient" "volatile" "while" "System" "String" "Integer"
     "ArrayList" "LinkedList" "TreeSet" "HashSet" "LinkedHashSet"
     "TreeMap" "HashMap" "LinkedHashMap")
    (c-mode
     "alignof" "assert" "break" "catch" "char8_t" "char16_t"
     "char32_t" "class" "const" "const_cast" "consteval" "constexpr"
     "constinit" "continue" "decltype" "default" "define" "delete"
     "double" "dynamic_cast" "explicit" "export" "extern" "false"
     "final" "float" "friend" "ifdef" "ifndef" "include" "inline"
     "namespace" "noexcept" "nullptr" "operator" "override" "private"
     "protected" "public" "return" "short" "signed" "size_t" "sizeof"
     "static" "static_assert" "static_cast" "struct" "switch"
     "template" "throw" "typedef" "typeid" "typename" "uint8_t"
     "uint16_t" "uint32_t" "uint64_t" "union" "unsigned" "using"
     "virtual" "volatile" "wchar_t" "while" "NULL")
    (c++-mode . c-mode)
    (python-mode
     "assert" "break" "class" "continue" "except" "finally" "global"
     "import" "lambda" "nonlocal" "print" "raise" "return" "while"
     "yield" "None" "True" "False"))
  "Alist mapping major-modes to sorted keywords for `company-keywords'.")

;;;###autoload
(defun company-keywords (command &optional arg &rest ignored)
  "`company-mode' backend for programming language keywords."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-keywords))
    (prefix (and (assq major-mode company-keywords-alist)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates
     (let ((completion-ignore-case t)
           (symbols (cdr (assq major-mode company-keywords-alist))))
       (all-completions arg (if (consp symbols)
                                symbols
                              (cdr (assq symbols company-keywords-alist))))))
    (kind 'keyword)
    (sorted t)
    (ignore-case t)))

(provide 'company-keywords)
;;; company-keywords.el ends here
