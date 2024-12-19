;; When using this directly, you will need to have use-package installed:
;; M-x package-install, select use-package. But if you start via
;; `standalone.el', this is being taken care of automatically.


;; Set default window size and position
(setq default-frame-alist
      '((width . 300)    ;; Frame width in characters
        (height . 70)    ;; Frame height in characters
        ;;(top . 50)       ;; Distance from top of screen (pixels)
        ;;(left . 100)
        ))   ;; Distance from left of screen (pixels)



(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)


;; Work around an Emacs v26 bug.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(when (version< emacs-version "27")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Bind Ctrl+/ to comment/uncomment the current line or region
(global-set-key (kbd "C-/") 'comment-line)

(use-package which-key
  :ensure
  :init
  (which-key-mode))

(use-package selectrum
  :ensure
  :init
  (selectrum-mode)
  :custom
  (completion-styles '(flex substring partial-completion)))

;; Some common sense settings

;;(load-theme 'leuven t)
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))


;; Set theme
;;(load-theme 'leuven-dark)
;; (load-theme 'dracula)
;; (set-face-attribute 'default nil
;;                     :family "Droid Sans Mono"
;;                     :height 140)

(set-face-attribute 'default nil
                    :family "monospace"
                    )

;; vscode theme dark modern baby
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
;; Remove the border around the TODO word on org-mode files
(setq vscode-dark-plus-box-org-todo nil)

;; Do not set different heights for some org faces
(setq vscode-dark-plus-scale-org-faces nil)

;; Avoid inverting hl-todo face
(setq vscode-dark-plus-invert-hl-todo nil)

;; Configure current line highlighting style (works best with Emacs 28 or newer)
(setq vscode-dark-plus-render-line-highlight 'line)

(require 'bind-key)

(put 'erase-buffer 'disabled nil)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(savehist-mode 1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq dired-listing-switches "-alh")
(setq gc-cons-threshold (* 50 1000 1000))
(display-time-mode t)
(show-paren-mode t)
(display-line-numbers-mode t)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default css-indent-offset 2)
(global-hl-line-mode t)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)

;; File backups
;; (setq backup-directory-alist `(("." . "~/.emacs.d/.file-backups"))
;;       backup-by-copying t
;;       delete-old-versions t
;;       kept-new-versions 10
;;       kept-old-versions 5
;;       version-control t)

;; highlight matching parens
(electric-pair-mode 1)
;; electric-newline-add-maybe-indent shortcut is not needed,
;; I want this for inserting snippets as in IntelliJ
(global-unset-key (kbd "C-j"))
(setq inferior-lisp-program (if (eq system-type 'gnu/linux)
                                "/usr/bin/sbcl"
                              "/usr/local/bin/sbcl"))

;; enable recentf to display recently visited files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "s-r") 'revert-buffer-quick)

;; jump back and forth between cursor positions
(global-set-key (kbd "s-[") 'pop-global-mark)
(global-set-key (kbd "s-]") #'(lambda ()
                                (interactive)
                                (set-mark-command t)))

;; comment-uncomment region
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; start marking a region
(global-set-key (kbd "C-,") 'set-mark-command)

;; unindent
(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)

(global-set-key (kbd "M-s-l") 'eglot-format-buffer)

;;; External packages
(setq use-package-always-ensure t)

;; auto-complete inside text/code buffers
(use-package company :init (global-company-mode t))
;; shows which keys can be pressed after an initial keystroke
(use-package which-key :init (which-key-mode))
;; move text up/down easily
(use-package move-text
  :bind (("s-<up>" . 'move-text-up) ("s-<down>" . 'move-text-down)))
;; support for markdown
(use-package markdown-mode)
;; support for git
(use-package magit :defer t)
;; highlight modified/added regions of the code
(use-package diff-hl :init (global-diff-hl-mode))
;; syntax checker on the fly
(use-package flycheck :init (global-flycheck-mode))
;; expands marked region easily
(use-package expand-region
  :bind (("M-<up>" . 'er/expand-region) ("M-<down>" . 'er/contract-region)))
;; better way to switch between open windows
(use-package ace-window :bind ("C-<tab>" . 'ace-window))
(use-package ivy
  :bind (("\C-s" . 'swiper))
  :init (ivy-mode))
(use-package counsel :after (ivy))
;;(use-package multiple-cursors
;;  :bind (("s-;" . 'mc/edit-lines)))

;; God mode (modal editing)
;;(defun my-god-mode-update-cursor-type ()
;;  "Change God mode cursor type."
;;  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
;;(use-package god-mode
;;  :bind (("<escape>" . #'god-mode-all))
;;  :config (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
;;  :init (god-mode))

(use-package gotest)

;; SLIME
(use-package slime
  :config (setq slime-contribs '(slime-fancy slime-cl-indent)))
(use-package slime-company
  :after (slime company)
  :init
  (slime-setup '(slime-fancy slime-company))
  (add-to-list 'slime-contribs 'slime-autodoc)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; D
(use-package d-mode
  :init (with-eval-after-load 'eglot
          (add-to-list 'eglot-server-programs
                       '(d-mode . ("~/programming/apps/serve-d")))))

;; Nim
(use-package nim-mode
  :init (with-eval-after-load 'eglot
          (add-to-list 'eglot-server-programs
                       '(nim-mode . ("nimlsp")))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; Rust
;;(use-package rustic
;;  :defer t
;;  :config
;;  (setq rustic-lsp-client 'eglot)
;;  :custom
;;  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-inline-completion-display t)
  (lsp-inlay-hints-mode)
  (setq lsp-inlay-hint-enable t)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; (add-hook 'dart-mode-hook 'lsp)
;; Dart
(use-package dart-mode)

;; ZIG
(use-package zig-mode
  :bind (("C-x C-t" . zig-test-buffer))
  :custom (zig-format-on-save nil))

;; Lua
(use-package lua-mode)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Python
;; Install and configure LSP mode
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp))
  :commands lsp)

;; Use Pyright (recommended) as the Python Language Server
;; Install pyright with
;; npm install -g pyright
;; then uncomment below
;;
;; (use-package lsp-pyright
;;   :ensure t
;;   :after lsp-mode
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ;; Start LSP automatically
;; until here


;; TODO edit Java formatter in emacs
;; https://gist.github.com/fbricon/30c5971f7e492c8a74ca2b2d7a7bb966

;; Groovy
(use-package groovy-mode)

;; Go
(use-package go-mode
  :hook ((go-mode . (lambda () (setq tab-width 4)))))

;; Inserts code snippets
(use-package yasnippet :init (yas-global-mode 1)
  :bind (("C-j" . yas-insert-snippet)))

;; Crux provides a few helpful basic functions
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("s-<return>" . crux-smart-open-line)
         ("s-S-<return>" . crux-smart-open-line-above)
         ("s-d" . crux-duplicate-current-line-or-region)
         ("C-c k" . crux-kill-other-buffers)))

;; Org-mode
(use-package org
  :init
  ;; Beautify org-mode: https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (setq org-hide-emphasis-markers t))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://renato.athaydes.com/news/index.xml"
          "https://planet.emacslife.com/atom.xml"
          "https://ziglang.org/news/index.xml")))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config filesshell
(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Projectile
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :config
;;   (setq projectile-completion-system 'ivy)
;;   ;; Store the list of projects persistently
;;   (setq projectile-known-projects-file
;;         (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)))

;; (require 'projectile)
;; (projectile-mode +1)
;; optional (integrate with helm)
;;(setq projectile-completion-system 'helm)
;; (setq projectile-completion-system 'ivy)
;;(helm-projectile-on)

;; save and restore projectile projects
;; (add-hook 'kill-emacs-hook #'desktop-save-in-desktop-dir)
;; (add-hook 'emacs-startup-hook #'desktop-read)
;; (desktop-save-mode 1)


;; Recommended keymap prefix on macOS
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; workspace management
;; (use-package persp-projectile
;;   :ensure t
;;   :config
;;   (persp-projectile-mode))  ;; Enable persp-projectile integration

;; (defun my-projectile-persp-setup ()
;;   "Set up persp-projectile to save/load project workspaces."
;;   (add-hook 'projectile-before-switch-project-hook #'persp-save-state-to-file)
;;   (add-hook 'projectile-after-switch-project-hook #'persp-load-state-from-file))

;; (add-hook 'projectile-mode-hook #'my-projectile-persp-setup)
;; (setq persp-save-dir (concat (projectile-project-root) ".emacs.persp/"))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Enable desktop mode
;; (desktop-save-mode 1)

;; ;; Set desktop session file to be stored in the project folder
;; (defun my-desktop-save-in-project-folder ()
;;   "Save the desktop session in the project folder."
;;   (when (and (project-current) (desktop-save-mode 1))
;;     (let ((project-dir (car (project-roots (project-current)))))
;;       (setq desktop-dirname (expand-file-name ".emacs.d/desktop" project-dir))
;;       (desktop-save project-dir))))

;; ;; Automatically save session on exit
;; (add-hook 'kill-emacs-hook 'my-desktop-save-in-project-folder)

;; ;; Restore session when opening a project
;; (defun my-desktop-read-in-project-folder ()
;;   "Restore the desktop session from the project folder."
;;   (when (and (project-current) (desktop-save-mode 1))
;;     (let ((project-dir (car (project-roots (project-current)))))
;;       (setq desktop-dirname (expand-file-name ".emacs.d/desktop" project-dir))
;;       (desktop-read project-dir))))

;; ;; Hook to restore session when opening a project
;; (add-hook 'find-file-hook 'my-desktop-read-in-project-folder)



;; Enable desktop-save-mode for session persistence
(require 'desktop)

;; Automatically save and restore sessions
(setq desktop-path (list ".")) ;; Look for the desktop file in the current directory
(setq desktop-auto-save-timeout 30) ;; Auto-save desktop every 30 seconds
(setq desktop-save 'if-exists) ;; Save session only if a desktop file already exists

;; Save window splits/layouts
(setq desktop-restore-frames t) ;; Restore window configurations (splits)
(add-to-list 'desktop-globals-to-save 'window-configuration) ;; Explicitly save window configs

;; Function to save session in current directory when Emacs exits
(defun my/save-session-on-exit ()
  "Save desktop session in the current directory."
  (let ((desktop-dir (expand-file-name "."))) ;; Current directory
    (setq desktop-dirname desktop-dir) ;; Set the desktop directory
    (desktop-save desktop-dir)))

;; Hook the save function when exiting Emacs
(add-hook 'kill-emacs-hook 'my/save-session-on-exit)

;; Automatically load the session if a desktop file exists in the current directory
(defun my/load-session-on-start ()
  "Load session if a desktop file exists in the current directory."
  (when (file-exists-p (concat (expand-file-name ".") "/.emacs.desktop"))
    (desktop-read ".")
    (desktop-restore-frames))) ;; Explicitly restore window splits after loading the session

(add-hook 'emacs-startup-hook 'my/load-session-on-start)

;; Optional: Prevent confirmation prompts for loading and saving desktops
(setq desktop-load-locked-desktop t) ;; Automatically load the desktop
(setq desktop-save-mode t) ;; Enable desktop-save-mode globally






;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Paste
(setq x-select-enable-clipboard t)
;;(global-set-key (kbd "C-c v") 'clipboard-yank)
(global-set-key (kbd "C-c C-c") 'clipboard-kill-ring-save)  ;; Copy
(global-set-key (kbd "C-c C-x") 'clipboard-kill-region)      ;; Cut
(global-set-key (kbd "C-c C-v") 'clipboard-yank)             ;; Paste

;; loc
(global-display-line-numbers-mode t)
;; text size
(global-set-key (kbd "C-=") 'text-scale-increase) ;; Ctrl + = for bigger text
(global-set-key (kbd "C--") 'text-scale-decrease) ;; Ctrl + - for smaller text



(provide 'init)
(load "~/.emacs.d/functions")
(load "~/.emacs.d/my-unison-mode")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

