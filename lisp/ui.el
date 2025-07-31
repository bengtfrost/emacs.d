;; ~/.emacs.d/lisp/ui.el
;; -*- lexical-binding: t -*-
;; This file is optimized for a terminal-only (emacs-nox) experience.

;; --- Terminal UI Tweaks ---
(menu-bar-mode -1)                   ; Disable top menu bar.
(setq-default cursor-type 'bar)      ; Use a bar-shaped cursor
(setq inhibit-startup-message t)     ; Disable the splash screen
(global-display-line-numbers-mode 1) ; Show line numbers
(column-number-mode 1)               ; Show the current column number

;; --- Indentation Settings ---
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tab characters.
(setq-default tab-width 4)           ; Set the visual width of a tab to 4 spaces.

;; --- Theme Customization ---
;; Set custom theme variables BEFORE loading the theme.
;; (setq doom-one-modeline-bg "darkgrey")

;; --- Theme ---
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

;; --- Minibuffer/Completion Enhancements ---
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Let TAB complete or cycle in the minibuffer
  (define-key vertico-map (kbd "TAB") #'vertico-insert)
  (define-key vertico-map (kbd "<tab>") #'vertico-insert))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(provide 'ui)

