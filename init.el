(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t) 
;; basic packages
(use-package which-key
  :init
  (which-key-mode))

(use-package selectrum
  :init
  (selectrum-mode)
  :custom
  (completion-styles '(flex substring partial-completion)))

(use-package gruvbox-theme
  :init)

(use-package evil
  :init
  (evil-mode 1))

(use-package dirvish
  :init
  (dirvish-override-dired-mode))

(use-package hyperbole
  :init
  (hyperbole-mode 1))

(use-package org)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package mixed-pitch
  :config 
  (add-hook 'text-mode-hook #'mixed-pitch-mode)
  (add-hook 'org-mode-hook #'mixed-pitch-mode))

(use-package magit)

(use-package vterm)

(use-package flycheck)

(use-package visual-fill-column)

(use-package writeroom-mode)

(use-package disable-mouse
  :init
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
	(list evil-motion-state-map
	      evil-normal-state-map
	      evil-visual-state-map
	      evil-insert-state-map)))

;; Programming stuff
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
  

(use-package company
  :custom
  (company-idle-delay 0) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last))
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Simple configurations
(add-to-list 'default-frame-alist '(height . 20))
(add-to-list 'default-frame-alist '(width . 80))

(load-theme 'gruvbox-dark-medium t)
(setq inhibit-startup-message t
      org-bullets-bullet-list '("●")
      ring-bell-function 'ignore)
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(set-face-attribute 'default nil :font "Less Perfect DOS VGA-12")
(set-face-attribute 'variable-pitch nil :font "ETBembo")

(set-face-attribute 'org-document-title nil :height 2.0)
(set-face-attribute 'org-level-1 nil :height 1.8)
(set-face-attribute 'org-level-2 nil :height 1.5)
(set-face-attribute 'org-level-3 nil :height 1.25)


(add-hook 'vterm-mode-hook 'turn-off-evil-mode)

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

(require 'ob-python)
(require 'ob-latex)

(font-lock-add-keywords 'org-mode
			'(("^ +\\([-*]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.1
					    header-line-format " "
					    left-margin-width 2
					    right-margin-width 2
					    internal-border-width 2)
				      (hl-line-mode -1)
				      (set-window-buffer nil (current-buffer))
				      (text-scale-set 1)))
(setq org-startup-indented t
      org-directory "~/.emacs.d/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-hide-emphasis-markers t 
      org-ellipsis "..." ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun load-init ()
  (interactive)
  (load "~/.emacs.d/init.el"))


