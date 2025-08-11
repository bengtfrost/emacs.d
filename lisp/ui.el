;;; ui.el --- User interface configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains UI configuration for terminal-only Emacs experience.
;; Includes theme setup, completion enhancements, and visual improvements.

;;; Code:

;; --- Basic UI Configuration ---
(menu-bar-mode -1)                      ; Disable top menu bar
(setq-default cursor-type 'bar)         ; Use a bar-shaped cursor
(setq inhibit-startup-message t)        ; Disable the splash screen
(setq inhibit-startup-echo-area-message t) ; Disable startup echo message
(global-display-line-numbers-mode 1)    ; Show line numbers
(column-number-mode 1)                  ; Show the current column number
(show-paren-mode 1)                     ; Highlight matching parentheses
(electric-pair-mode 1)                  ; Auto-insert matching brackets/quotes

;; --- Line number configuration ---
(setq display-line-numbers-type 'relative) ; Relative line numbers
(setq display-line-numbers-width 3)        ; Fixed width for line numbers

;; --- Indentation and Tab Settings ---
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tab characters
(setq-default tab-width 4)              ; Set the visual width of a tab to 4 spaces
(setq standard-indent 4)                ; Standard indentation
(setq tab-always-indent 'complete)      ; Tab can indent or complete

;; --- Scrolling Behavior ---
(setq scroll-margin 3)                  ; Keep 3 lines visible when scrolling
(setq scroll-conservatively 10000)     ; Smooth scrolling
(setq scroll-step 1)                    ; Scroll one line at a time
(setq auto-window-vscroll nil)          ; Disable automatic vertical scrolling

;; --- Better defaults ---
(setq ring-bell-function 'ignore)       ; Disable annoying bell
(setq use-dialog-box nil)               ; Disable dialog boxes
(setq enable-recursive-minibuffers t)   ; Allow recursive minibuffer commands
(setq completion-ignore-case t)         ; Case-insensitive completion
(setq read-file-name-completion-ignore-case t) ; Case-insensitive file completion

;; --- Whitespace visualization ---
(setq whitespace-style '(face tabs tab-mark trailing space-before-tab))
(setq whitespace-display-mappings
      '((tab-mark 9 [?› 9] [?\\ 9])))   ; Show tabs as ›

;; --- Theme Configuration ---
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t        ; Enable bold fonts
        doom-themes-enable-italic t)     ; Enable italic fonts
  
  ;; Load the theme
  (load-theme 'doom-molokai t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; --- Enhanced Minibuffer/Completion ---
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 15)               ; More completion candidates
  (setq vertico-resize t)               ; Resize the minibuffer
  (setq vertico-cycle t))               ; Cycle through candidates

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy
                               marginalia-annotators-light
                               nil)))

(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq savehist-length 25)
  (setq history-length 25)
  (setq savehist-save-minibuffer-history t))

;; --- Orderless completion style ---
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; --- Consult for enhanced commands ---
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; M-# bindings for registers
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :config
  ;; Configure preview
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<"))

;; --- Embark for contextual actions ---
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; --- Mode line enhancement ---
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon nil)          ; Disable icons for terminal
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-workspace-name nil))

;; --- Better help system ---
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; --- Font and display settings ---
;; Adjust these based on your terminal capabilities
(when (display-graphic-p)
  ;; These settings only apply if running in GUI mode
  (set-face-attribute 'default nil :height 110))

;; --- Performance optimizations ---
(setq gc-cons-threshold 100000000)      ; Increase GC threshold for better performance
(setq read-process-output-max (* 1024 1024)) ; Increase read process output max

;; --- Auto-revert files when changed on disk ---
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)          ; Don't announce reverts

;; --- Better undo system ---
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region t))

(provide 'ui)

;;; ui.el ends here
