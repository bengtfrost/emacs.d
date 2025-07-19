;; -*- lexical-binding: t; -*-

;; --- Backup File Configuration ---
;; Stop creating backup~ files in the same directory.
;; Instead, put all backup files into a dedicated directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq make-backup-files t)    ; Ensure backups are on.
(setq backup-by-copying t)    ; Force copying to the backup dir, good for version control.
(setq delete-old-versions t)  ; Don't clutter the backup dir with old versions.
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)      ; Use version numbers for backups.

;; --- Package Management ---
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; --- Load Custom Configuration Modules ---
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (load-file (concat lisp-dir "/ui.el"))
  (load-file (concat lisp-dir "/custom.el"))
  (load-file (concat lisp-dir "/keybinds.el"))
  (load-file (concat lisp-dir "/langs.el")))

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
