;;; Unison code syntax highlighter.
;; Homepage: https://github.com/renatoathaydes/unison.el

(defvar unison-highlights nil "Unison highlighter rules.")

(setq unison-highlights
      (let* (
             ;; define several category of keywords
             (x-keywords '("let" "type" "do" "else" "then" "if"
                           "ability" "where" "handle" "with" "cases"))
             (x-types '("Nat" "Text" "Int" "Optional" "Request" "Map" "List"))
             (x-constants '("unique" "structural"))
;;             (x-events '("at_rot_target" "at_target" "attach"))
;;             (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words)))
;;             (x-events-regexp (regexp-opt x-events 'words))
;;             (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          ("{\\|}" . 'font-lock-function-name-face)
          ("{\\([^<]+?\\)}" . (1 'font-lock-constant-face))
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
;;          (,x-events-regexp . 'font-lock-builtin-face)
;;          (,x-functions-regexp . 'font-lock-function-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode my-unison-mode fundamental-mode "Unison"
  "Major mode for editing Unison language code."
  (setq font-lock-defaults '((unison-highlights))))

(add-to-list 'auto-mode-alist '("\\.u\\'" . my-unison-mode))
