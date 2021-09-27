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
  '((c-mode . c++-mode)
    (c++-mode
     "alignof" "assert" "auto" "bool" "break" "case" "catch" "char"
     "char16_t" "char32_t" "char8_t" "class" "const" "const_cast"
     "consteval" "constexpr" "constinit" "continue" "decltype"
     "default" "define" "delete" "double" "dynamic_cast" "else" "enum"
     "explicit" "export" "extern" "false" "final" "float" "for"
     "friend" "goto" "if" "ifdef" "ifndef" "include" "inline" "int"
     "long" "namespace" "new" "noexcept" "nullptr" "operator"
     "override" "private" "protected" "public" "register" "restrict"
     "return" "short" "signed" "size_t" "sizeof" "static"
     "static_assert" "static_cast" "struct" "switch" "template" "this"
     "throw" "true" "try" "typedef" "typeid" "typename" "uint16_t"
     "uint32_t" "uint64_t" "uint8_t" "union" "unsigned" "using"
     "virtual" "void" "volatile" "wchar_t" "while" "do" "NULL")
    (python-mode
     "all" "any" "ascii" "assert" "bin" "bool" "break" "breakpoint"
     "bytearray" "bytes" "callable" "chr" "class" "classmethod"
     "compile" "complex" "continue" "delattr" "dict" "dir" "divmod"
     "elif" "else" "enumerate" "eval" "except" "exec" "filter"
     "finally" "float" "format" "from" "frozenset" "getattr" "global"
     "globals" "hasattr" "hash" "help" "hex" "id" "import" "input"
     "int" "isinstance" "issubclass" "iter" "lambda" "len" "list"
     "locals" "map" "max" "memoryview" "min" "next" "nonlocal"
     "object" "oct" "open" "ord" "pass" "pow" "print" "property"
     "raise" "range" "repr" "return" "reversed" "round" "set"
     "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super"
     "tuple" "type" "vars" "while" "with" "yield" "zip" "abs"
     "Ellipsis" "Exception" "False" "NotImplemented" "True" "None"))
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
