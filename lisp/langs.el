;; -*- lexical-binding: t; -*-

;; --- LSP (Language Server Protocol) Integration ---
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-format-on-save-mode t
        lsp-format-on-save-allow-no-action t))

;; --- LSP UI for a prettier experience ---
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

;; --- Autocompletion Engine ---
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (define-key company-mode-map (kbd "<tab>") #'blfdev/smart-tab)
  (define-key company-mode-map (kbd "TAB") #'blfdev/smart-tab))

;; --- Treesitter for better syntax highlighting ---
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

;; --- Language-specific setup ---
(use-package rust-mode :ensure t :config (setq lsp-toml-server 'rust-analyzer) (add-hook 'rust-mode-hook #'lsp-deferred))
(use-package python-mode :ensure t :config (add-hook 'python-mode-hook #'lsp-deferred))

;; --- LSP Client Registrations (Executed AFTER lsp-mode is loaded) ---
(eval-after-load 'lsp-mode
  '(progn
     ;; -- General LSP Configuration --
     (add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))

     ;; -- Python / Ruff Client --
     (lsp-register-client
      (make-lsp-client :new-connection (lsp-stdio-connection '("ruff-lsp"))
                       :major-modes '(python-mode)
                       :remote? nil
                       :server-id 'ruff-lsp))

     ;; -- Guile/Scheme Client (Guix) --

     ;; Disable the built-in clients we don't want to be prompted for.
     (add-to-list 'lsp-disabled-clients 'guile-lsp)
     (add-to-list 'lsp-disabled-clients 'semgrep-ls)

     ;; Now, register our own client with a UNIQUE name.
     (lsp-register-client
      (make-lsp-client
       ;; Give our client a unique ID that is NOT on the disabled list.
       :server-id 'guile-lsp-guix
       :major-modes '(scheme-mode)
       :priority 1
       ;; The activation function just needs to find our wrapper script.
       :activation-fn (lambda (filename _server-id) (executable-find "run-guile-lsp.sh"))
       ;; The connection is now extremely simple.
       :new-connection (lsp-stdio-connection '("run-guile-lsp.sh"))))))
;; --- END OF eval-after-load BLOCK ---

;; C/C++ (uses built-in modes)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(use-package typescript-mode :ensure t :mode ("\\.ts\\'" "\\.tsx\\'") :config (add-hook 'typescript-mode-hook #'lsp-deferred))
(use-package web-mode :ensure t :mode ("\\.js\\'" "\\.jsx\\'" "\\.json\\'"))
(use-package markdown-mode :ensure t :mode ("\\.md\\'" . gfm-mode) :config (add-hook 'markdown-mode-hook #'lsp-deferred))
(use-package toml-mode :ensure t :mode ("\\.toml\\'" . toml-mode) :config (add-hook 'toml-mode-hook #'lsp-deferred))
(use-package yaml-mode :ensure t :mode (("\\.ya?ml\\'" . yaml-mode)) :config (add-hook 'yaml-mode-hook #'lsp-deferred))

;; --- Shell Scripts (Bash, Zsh, etc.) ---
(use-package sh-mode
  :ensure nil ; sh-mode is built-in
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode)
         ("\\.profile\\'" . sh-mode))
  :config
  (add-hook 'sh-mode-hook #'lsp-deferred))

;; --- Guix System Integration ---
(use-package guix :ensure t :defer t)

;; --- Guile/Scheme Development (The Final, Robust Configuration) ---
(use-package geiser-guile :ensure t)
(use-package geiser
  :ensure t
  :after geiser-guile
  :config
  (setq geiser-active-implementations '(guile))
  (add-hook 'scheme-mode-hook #'geiser-mode-enable))

;; Add the LSP hook separately.
(add-hook 'scheme-mode-hook #'lsp-deferred)

;; --- Project Management ---
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (general-define-key
   :keymaps 'global
   "SPC p f" '(projectile-find-file :which-key "Find File in Project")
   "SPC p p" '(projectile-switch-project :which-key "Switch Project")
   "SPC p s" '(projectile-save-project-buffers :which-key "Save Project Buffers"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
