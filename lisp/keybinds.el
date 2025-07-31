;; ~/.emacs.d/lisp/keybinds.el
;; -*- lexical-binding: t; -*-

;; --- Which-key for command discovery ---
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; --- General.el for easy keybinding configuration ---
(use-package general
  :ensure t
  :config
  (general-define-key
   :keymaps 'global
   "SPC" '(nil :which-key "Leader")

   "SPC SPC" '(execute-extended-command :which-key "M-x (Command Palette)")

   "SPC f"   '(:ignore t :which-key "File")
   "SPC f f" '(find-file :which-key "Find File")
   "SPC f s" '(save-buffer :which-key "Save Buffer")

   "SPC p"   '(:ignore t :which-key "Project (see langs.el)")

   "SPC b"   '(:ignore t :which-key "Buffer")
   "SPC b b" '(switch-to-buffer :which-key "Switch Buffer")
   "SPC b k" '(kill-current-buffer :which-key "Kill Current Buffer")

   "SPC w"   '(:ignore t :which-key "Window")
   "SPC w /" '(split-window-right :which-key "Split Right")
   "SPC w -" '(split-window-below :which-key "Split Below")
   "SPC w d" '(delete-window :which-key "Delete Window")
   "SPC w o" '(delete-other-windows :which-key "Delete Other Windows")

   ;; Always use our smart-tab on TAB
   "TAB"   '(blfdev/smart-tab :which-key "Smart Tab")
   "<tab>" '(blfdev/smart-tab :which-key "Smart Tab")))

