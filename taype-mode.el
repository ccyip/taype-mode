;;; taype-mode.el --- A simple major mode for the taype language -*- lexical-binding: t -*-

;; Copyright (C) 2022 Qianchuan Ye

;; Author: Qianchuan Ye <yeqianchuan@gmail.com>
;; Keywords: languages
;; URL: https://github.com/ccyip/taype-mode
;; License: MIT

;;; Commentary:

;; A major mode for editing taype files in Emacs, which supports simple syntax
;; highlighting (font lock).
;;
;; Partly copied from haskell-mode.

;;; Code:

(defconst taype-obliv-prefix "`")

(defun taype-obliv-name (name)
  (concat taype-obliv-prefix name))

(defconst taype-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}1nb" st)
    (modify-syntax-entry ?\} "){4nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)

    (modify-syntax-entry ?\` "_" st)
    (modify-syntax-entry ?\' "_" st)

    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)

    st))

(defconst taype-top-keywords
  (regexp-opt '("data" "fn" "obliv") 'symbols))
(defconst taype-keywords
  (regexp-opt
   `("let" "in" "if" ,(taype-obliv-name "if") "then" "else" "mux"
     "case" ,(taype-obliv-name "case") "of" "end" "tape")
   'symbols))
(defconst taype-builtin-types
  (regexp-opt `("unit" "bool" ,(taype-obliv-name "bool")
                "int" ,(taype-obliv-name "int")) 'symbols))
(defconst taype-symbols
  (regexp-opt `("\\" "->" ":" "=" "|" ","
                "#[" "]" "<" ">" "(" ,(taype-obliv-name "(") ")"
                "<=" "==" "+" "-" "*" "/" "&&" "||"
                ,(taype-obliv-name "<=") ,(taype-obliv-name "==")
                ,(taype-obliv-name "+") ,(taype-obliv-name "-")
                ,(taype-obliv-name "*") ,(taype-obliv-name "/")
                ,(taype-obliv-name "&&") ,(taype-obliv-name "||"))))
(defconst taype-builtin-funs
  (regexp-opt `("not" ,(taype-obliv-name "not")
                "s_bool" "r_bool" "s_int" "r_int") 'symbols))
(defconst taype-builtin-consts
  (regexp-opt '("True" "False") 'symbols))
(defconst taype-numerals "\\_<[1-9]+\\_>")
(defconst taype-ctors "\\_<[[:upper:]][[:alnum:]_']+\\_>")
(defconst taype-obliv-ctors
  (regexp-opt `(,(taype-obliv-name "inl") ,(taype-obliv-name "inr")) 'symbols))

(defconst taype-font-lock-keywords
  `((,taype-top-keywords . font-lock-keyword-face)
    (,taype-keywords . font-lock-keyword-face)
    (,taype-builtin-types . font-lock-type-face)
    (,taype-symbols . font-lock-builtin-face)
    (,taype-builtin-funs . font-lock-function-name-face)
    (,taype-builtin-consts . font-lock-constant-face)
    (,taype-ctors . font-lock-type-face)
    (,taype-obliv-ctors . font-lock-type-face)
    (,taype-numerals . font-lock-constant-face)))

;;;###autoload
(define-derived-mode taype-mode prog-mode "taype"
  "Major mode for the taype language."
  :syntax-table taype-syntax-table
  (setq font-lock-defaults '(taype-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-padding 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tp\\'" . taype-mode))

(provide 'taype-mode)

;;; taype-mode.el ends here
