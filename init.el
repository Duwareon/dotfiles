;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(setq org-roam-v2-ack t)
;; Use straight to grab packages
(straight-use-package 'gruvbox-theme)
(straight-use-package 'undo-fu)
(straight-use-package 'evil)
(straight-use-package 'magit)
(straight-use-package 'org-journal)
(straight-use-package 'elcord)
(straight-use-package 'rust-mode)
(straight-use-package 'org-roam)

;; Load packages
(require 'gruvbox-theme)
(require 'undo-fu)
(require 'evil)
(require 'org)
(require 'magit)
(require 'org-journal)
(require 'epa-file)
(require 'elcord)
(require 'rust-mode)
(require 'org-roam)
(elcord-mode)

(epa-file-enable)
(load-theme 'gruvbox-dark-medium t)
(evil-mode 1)

;; Customizations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 80))
(setq org-startup-truncated nil)

;(set-fringe-mode 0)

;; Keys

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(custom-safe-themes
   '("6f358ac456ee889d132b784afaaa1ce0bd9bdfafc89ef6e3fd5e22179ef59905" default))
 '(inhibit-startup-screen t)
 '(org-agenda-files nil)
 '(org-journal-dir "E:/OneDrive/Documents/Emacs/Journal/")
 '(org-journal-enable-encryption t)
 '(org-journal-encrypt-journal t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
