;;; keybinds.el --- Key binding configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains key binding configuration using general.el and which-key.
;; Provides leader key bindings and enhanced navigation shortcuts.

;;; Code:

;; --- Which-key for command discovery ---
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-max-display-columns 3))

;; --- General.el for easy keybinding configuration ---
(use-package general
  :ensure t
  :config
  ;; Global leader key bindings
  (general-define-key
   :keymaps 'global
   "SPC" '(nil :which-key "Leader")

   "SPC SPC" '(execute-extended-command :which-key "M-x (Command Palette)")

   ;; File operations
   "SPC f"   '(:ignore t :which-key "File")
   "SPC f f" '(find-file :which-key "Find File")
   "SPC f s" '(save-buffer :which-key "Save Buffer")
   "SPC f r" '(recentf-open-files :which-key "Recent Files")
   "SPC f d" '(dired :which-key "Directory")

   ;; Buffer operations
   "SPC b"   '(:ignore t :which-key "Buffer")
   "SPC b b" '(switch-to-buffer :which-key "Switch Buffer")
   "SPC b k" '(kill-current-buffer :which-key "Kill Current Buffer")
   "SPC b r" '(revert-buffer :which-key "Revert Buffer")
   "SPC b l" '(list-buffers :which-key "List Buffers")

   ;; Window operations
   "SPC w"   '(:ignore t :which-key "Window")
   "SPC w /" '(split-window-right :which-key "Split Right")
   "SPC w -" '(split-window-below :which-key "Split Below")
   "SPC w d" '(delete-window :which-key "Delete Window")
   "SPC w o" '(delete-other-windows :which-key "Delete Other Windows")
   "SPC w h" '(windmove-left :which-key "Move Left")
   "SPC w j" '(windmove-down :which-key "Move Down")
   "SPC w k" '(windmove-up :which-key "Move Up")
   "SPC w l" '(windmove-right :which-key "Move Right")

   ;; Code operations
   "SPC c"   '(:ignore t :which-key "Code")
   "SPC c f" '(blfdev/format-buffer :which-key "Format Buffer")
   "SPC c c" '(compile :which-key "Compile")
   "SPC c r" '(recompile :which-key "Recompile")
   "SPC c /" '(blfdev/comment-or-uncomment-line-or-region :which-key "Toggle Comment")

   ;; Search and navigation
   "SPC s"   '(:ignore t :which-key "Search")
   "SPC s s" '(isearch-forward :which-key "Search Forward")
   "SPC s r" '(isearch-backward :which-key "Search Backward")

   ;; Git operations (if magit is available)
   "SPC g"   '(:ignore t :which-key "Git")
   "SPC g s" '(magit-status :which-key "Git Status")
   "SPC g b" '(magit-blame :which-key "Git Blame")
   "SPC g l" '(magit-log :which-key "Git Log")

   ;; Toggle operations
   "SPC t"   '(:ignore t :which-key "Toggle")
   "SPC t l" '(display-line-numbers-mode :which-key "Line Numbers")
   "SPC t w" '(whitespace-mode :which-key "Whitespace")
   "SPC t t" '(toggle-truncate-lines :which-key "Truncate Lines"))

  ;; Project operations (will be enhanced in langs.el)
  (general-define-key
   :keymaps 'global
   "SPC p"   '(:ignore t :which-key "Project")))

;; --- Global Key Bindings (non-leader) ---
(general-define-key
 :keymaps 'global
 
 ;; Enhanced tab behavior
 "<tab>" 'blfdev/smart-tab
 "TAB" 'blfdev/smart-tab
 
 ;; Function keys
 "<f5>" 'compile
 "<f6>" 'recompile
 "<f8>" 'blfdev/format-buffer
 
 ;; Movement and editing
 "C-d" 'blfdev/duplicate-line
 "C-k" 'blfdev/kill-whole-line
 "M-<up>" 'blfdev/move-line-up
 "M-<down>" 'blfdev/move-line-down
 
 ;; Comment toggle
 "C-/" 'blfdev/comment-or-uncomment-line-or-region
 "C-c ;" 'blfdev/comment-or-uncomment-line-or-region
 
 ;; Better window navigation
 "C-x o" 'other-window
 "M-o" 'other-window
 
 ;; Improved search
 "C-s" 'isearch-forward
 "C-r" 'isearch-backward
 
 ;; Buffer switching
 "C-x b" 'switch-to-buffer)

;; --- Mode-specific keybindings ---

;; Company mode bindings
(with-eval-after-load 'company
  (general-define-key
   :keymaps 'company-active-map
   "<tab>" 'company-complete-common-or-cycle
   "TAB" 'company-complete-common-or-cycle
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "C-d" 'company-show-doc-buffer
   "M-." 'company-show-location)
  
  (general-define-key
   :keymaps 'company-search-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

;; LSP mode bindings (will be enhanced in langs.el)
(with-eval-after-load 'lsp-mode
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix "C-c l"
   :prefix-map 'lsp-prefix-map
   :prefix-command 'lsp-prefix-command
   "" '(:ignore t :which-key "LSP")
   "f" 'blfdev/format-buffer
   "r" 'lsp-rename
   "a" 'lsp-execute-code-action
   "d" 'lsp-find-definition
   "i" 'lsp-find-implementation
   "t" 'lsp-find-type-definition
   "x" 'lsp-find-references
   "h" 'lsp-describe-thing-at-point
   "s" 'lsp-workspace-symbol
   "R" 'lsp-workspace-restart
   "D" 'lsp-disconnect))

;; Programming mode bindings
(general-define-key
 :keymaps 'prog-mode-map
 "C-c f" 'blfdev/format-buffer
 "C-c c" 'compile
 "C-c r" 'recompile)

;; Enable recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(provide 'keybinds)

;;; keybinds.el ends here
