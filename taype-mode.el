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
;; Partly copied from haskell-mode and tuareg.

;;; Code:

(defgroup taype nil
  "Support for the Taype language."
  :link '(url-link "https://github.com/ccyip/taype-mode")
  :group 'languages)


(defgroup taype-faces nil
  "Special faces for the Taype mode."
  :group 'taype)

(defface taype-font-lock-governing-face
  '((((class color) (type tty)) (:bold t))
    (((background light)) (:foreground "black" :bold t))
    (t (:foreground "wheat" :bold t)))
  "Face description for governing/leading keywords."
  :group 'taype-facs)
(defvar taype-font-lock-governing-face
  'taype-font-lock-governing-face)

(defface taype-font-lock-obliv-instance-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for oblivious type instances."
  :group 'taype-faces)
(defvar taype-font-lock-obliv-instance-face
  'taype-font-lock-obliv-instance-face)

(defface taype-font-lock-constructor-face
  '((t :inherit font-lock-constant-face))
  "Face description for constructors."
  :group 'taype-faces)
(defvar taype-font-lock-constructor-face
  'taype-font-lock-constructor-face)

(defun taype-obliv-name (name)
  (concat "~" name))

(defconst taype-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)

    (modify-syntax-entry ?~ "_" st)
    (modify-syntax-entry ?\' "_" st)
    (modify-syntax-entry ?# "_" st)
    (modify-syntax-entry ?% "_" st)

    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)

    st))

(defconst taype-font-lock-keywords
  (let* ((keyword-rx (regexp-opt
                      `("let" "in" "if" ,(taype-obliv-name "if")
                        "then" "else" "mux"
                        "match" ,(taype-obliv-name "match")
                        "with" "end")
                      'symbols))
         (var-rx (rx (? ?~) (+ (in alnum "#'_"))))
         (ctor-rx (rx symbol-start
                      upper
                      (* (in alnum "_'"))
                      symbol-end))
         (obliv-ctor-rx (regexp-opt
                         `(,(taype-obliv-name "inl")
                           ,(taype-obliv-name "inr"))
                         'symbols))
         ;; The operator regular expressions are not used at the moment.
         (operator-rx (regexp-opt
                       `("\\" "->" "=>" ":" "=" "|" ","
                         "<=" "==" "+" "-" "*" "/" "&&" "||"
                         ,(taype-obliv-name "<=") ,(taype-obliv-name "==")
                         ,(taype-obliv-name "+") ,(taype-obliv-name "-")
                         ,(taype-obliv-name "*") ,(taype-obliv-name "/")
                         ,(taype-obliv-name "&&") ,(taype-obliv-name "||"))))
         (operator-extra-rx (regexp-opt
                             `("not" ,(taype-obliv-name "not"))
                             'symbols))
         (def-fun-rx (rx symbol-start
                         (group (| "fn" "fn'"))
                         symbol-end
                         (+ space)
                         (group (regexp var-rx))))
         (def-type-rx (rx symbol-start
                          (group (| "data" "obliv"))
                          symbol-end
                          (+ space)
                          (group (regexp var-rx))))
         (lam-rx (rx "\\"
                     (group (*? anything))
                     "=>"))
         (alt-rx (rx "|"
                     (* space)
                     (group (regexp var-rx))
                     (group (*? anything))
                     "=>"))
         (inst-rx (rx symbol-start
                      (? ?~)
                      (+ (in alnum "'_"))
                      "#"
                      (* (in alnum "#'_"))
                      symbol-end))
         (ppx-rx (rx symbol-start
                     ?%
                     (* (in alnum "'_"))
                     symbol-end)))
    `(
      (,keyword-rx . font-lock-keyword-face)
      (,def-fun-rx
        (1 taype-font-lock-governing-face)
        (2 font-lock-function-name-face))
      (,def-type-rx
        (1 taype-font-lock-governing-face)
        (2 font-lock-type-face))
      (,lam-rx (1 font-lock-variable-name-face))
      (,alt-rx
       (1 taype-font-lock-constructor-face)
       (2 font-lock-variable-name-face))
      (,ctor-rx . taype-font-lock-constructor-face)
      (,obliv-ctor-rx . taype-font-lock-constructor-face)
      (,inst-rx (0 taype-font-lock-obliv-instance-face t))
      (,ppx-rx . font-lock-preprocessor-face))))

;;;###autoload
(define-derived-mode taype-mode prog-mode "taype"
  "Major mode for the taype language."
  :syntax-table taype-syntax-table
  (setq font-lock-defaults '(taype-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-padding 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tp\\'" . taype-mode))
(add-to-list 'auto-mode-alist '("\\.tpc\\'" . taype-mode))
(add-to-list 'auto-mode-alist '("\\.oil\\'" . taype-mode))

(provide 'taype-mode)

;;; taype-mode.el ends here
