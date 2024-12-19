(defun workspace-unison ()
  "Open an Unison workspace."
  (interactive)
  (functions/workspace2
   (lambda ()
     (find-file "~/programming/experiments/unison/scratch.u"))
   (lambda ()
     (shell)
     (insert "ucm")
     (funcall (lookup-key (current-local-map) (kbd "RET"))))))

(defun workspace-lisp ()
  "Open a SLIME workspace."
  (interactive)
  (functions/workspace2
   (lambda ()
     (find-file "~/programming/experiments/lisp/scratch.lisp"))
   (lambda () (slime))))

(defun functions/workspace2 (buffer-left buffer-right)
  "Open the Unison scratch next to a shell running UCM.
`BUFFER-LEFT` should be a function that runs while on the left buffer.
`BUFFER-RIGHT` runs while on the rigth buffer.'"
  (delete-other-windows)
  (funcall buffer-left)
  (split-window-right)
  (other-window 0)
  (funcall buffer-right))

;; Helpful functions (available everywhere, including eshell)
(defun j11 ()
  "Use Java 11."
  (setenv "JAVA_HOME" (expand-file-name "~/.sdkman/candidates/java/11.0.13-zulu")))
(defun j17 ()
  "Use Java 17."
  (setenv "JAVA_HOME" (expand-file-name "/~.sdkman/candidates/java/17.0.5-zulu")))


;; minor for collapse/expand functions
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'rust-mode-hook #'my-rust-hs-setup)
(defun my-rust-hs-setup ()
  "Configure `hs-minor-mode` for Rust."
  (setq-local hs-block-start-regexp
              (rx line-start
                  (0+ space)
                  (or "fn" "struct" "enum" "impl" "mod" "trait")  ;; Keywords
                  (1+ space)
                  (1+ (not (any "{" "\n")))                      ;; Name
                  (0+ space) "{"))                              ;; Ends with {
  (setq-local hs-block-end-regexp "}"))

;; (defun my-hs-setup-overlay (ov)
;;   "Customize the overlay for hidden Rust blocks."
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (overlay-put ov 'display (propertize " ⯈ ... " 'face 'font-lock-comment-face))
;;     (overlay-put ov 'help-echo "Hidden block. Toggle with C-c C-h or C-c C-s.")))

;; (defun my-hs-display-arrows ()
;;   "Add arrows at the beginning of collapsible Rust blocks."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward hs-block-start-regexp nil t)
;;       (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
;;         (overlay-put ov 'before-string
;;                      (propertize " ⯆ " 'face 'font-lock-keyword-face
;;                                  'help-echo "Click to collapse or use C-c C-h"))
;;         (overlay-put ov 'hs-block t)))))

;; (defun my-hs-add-collapse-label ()
;;   "Add a label near the opening `{` to remind you of collapse shortcuts."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward hs-block-start-regexp nil t)
;;       (let ((start (match-end 0))) ;; Position immediately after the `{`
;;         (when (looking-at "{")
;;           (let ((ov (make-overlay start (1+ start))))
;;             (overlay-put ov 'after-string
;;                          (propertize " ⬅ [C-c C-h to collapse]"
;;                                      'face 'font-lock-comment-face
;;                                      'help-echo "Press C-c C-h to collapse this block"))))))))

;; (defun my-hs-mode-line-hint ()
;;   "Add a hint for hs-minor-mode in the mode-line."
;;   (setq mode-name
;;         (concat mode-name
;;                 " [Fold: C-c C-h | Unfold: C-c C-s]")))



;; (add-hook 'hs-minor-mode-hook #'my-hs-display-arrows)
;; (add-hook 'hs-minor-mode-hook #'my-hs-mode-line-hint)

;; (add-hook 'rust-mode-hook
;;           (lambda ()
;;             (add-hook 'after-change-functions #'my-hs-add-collapse-label nil t)))

;; (setq hs-set-up-overlay #'my-hs-setup-overlay)

(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
;; this below conflicts with other things i need (for now)
;; (global-set-key (kbd "C-c C-c") 'hs-hide-all)
;; (global-set-key (kbd "C-c C-a") 'hs-show-all)



(provide 'functions)

;;; functions.el ends here
