;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced Language Configuration for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Helper Functions ---
(defun blfdev/smart-tab ()
  "Try to complete with Company, otherwise indent the current line."
  (interactive)
  (if (and (bound-and-true-p company-mode) 
           (not (company-tooltip-visible-p))
           (company-manual-begin))
      (company-complete-common)
    (indent-for-tab-command)))

(defun blfdev/format-buffer ()
  "Format buffer using LSP or fallback formatters."
  (interactive)
  (cond
   ((and (fboundp 'lsp-mode) lsp-mode)
    (lsp-format-buffer))
   ((derived-mode-p 'python-mode)
    (when (executable-find "ruff")
      (shell-command-on-region (point-min) (point-max) "ruff format --stdin-filename=buffer.py" t t)))
   ((or (derived-mode-p 'typescript-mode) (derived-mode-p 'json-mode) (derived-mode-p 'yaml-mode))
    (when (executable-find "dprint")
      (shell-command-on-region (point-min) (point-max) "dprint fmt --stdin" t t)))
   (t (indent-region (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Core LSP Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Performance optimizations
  (setq lsp-completion-provider :none) ; Use company instead
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-snippet nil)
  
  ;; File watchers
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-enable-file-watchers nil)
  
  ;; Format on save for specific modes
  (setq lsp-before-save-edits nil)
  
  :bind (:map lsp-mode-map
              ("C-c l f" . blfdev/format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-find-definition)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l x" . lsp-find-references)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Completion Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 12)
  (setq company-show-numbers t)
  (setq company-backends '(company-capf company-files company-keywords company-dabbrev))
  
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("TAB" . company-complete-common-or-cycle)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Syntax Highlighting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treesitter configuration with error handling
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  ;; Only enable for languages where grammar is working
  (setq treesit-auto-langs '(python typescript javascript json yaml toml bash zig))
  ;; Disable problematic rust grammar
  (add-to-list 'treesit-auto-fallback-alist '(rust-mode . rust-mode))
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Language-Specific Configurations ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Rust ---
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . (lambda () (setq tab-width 4))))
  :config
  (setq rust-format-on-save t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

;; --- Zig ---
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . (lambda () (setq tab-width 4))))
  :config
  (setq zig-format-on-save t))

;; --- Python ---
(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :hook ((python-mode . lsp-deferred)
         (python-mode . (lambda () (setq tab-width 4))))
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Prefer ruff-lsp if available, fallback to pylsp
  (when (executable-find "ruff-lsp")
    (setq lsp-pyright-disable-language-services t)
    (add-to-list 'lsp-disabled-clients 'pyright)))

;; --- TypeScript/JavaScript ---
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook ((typescript-mode . lsp-deferred)
         (typescript-mode . (lambda () (setq tab-width 2))))
  :config
  (setq typescript-indent-level 2))

;; JavaScript mode configuration
(add-hook 'js-mode-hook #'lsp-deferred)
(setq js-indent-level 2)

;; --- JSON ---
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :hook (json-mode . lsp-deferred)
  :config
  (setq json-reformat:indent-width 2))

;; --- YAML ---
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . lsp-deferred)
  :config
  (setq yaml-indent-offset 2))

;; --- TOML ---
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'"
  :hook (toml-mode . (lambda () 
                      (setq-local lsp-toml-server 'taplo)
                      (lsp-deferred))))

;; --- Markdown ---
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                          (when (executable-find "marksman")
                            (lsp-deferred))))
  :config
  (setq markdown-command "markdown")
  (setq markdown-fontify-code-blocks-natively t))

;; --- Shell Scripts ---
(setq sh-basic-offset 2)

;; Only enable LSP for bash, not zsh (bash-language-server doesn't handle zsh well)
(add-hook 'sh-mode-hook 
          (lambda ()
            (when (and (eq sh-shell 'bash)
                       (executable-find "bash-language-server"))
              (lsp-deferred))))

;; Alternative: Enable LSP for specific shell types
;; (add-hook 'sh-mode-hook 
;;           (lambda ()
;;             (when (member sh-shell '(bash sh))
;;               (lsp-deferred))))

;; Disable treesitter for shell scripts if causing issues
(with-eval-after-load 'treesit-auto
  (setq treesit-auto-langs (remove 'bash treesit-auto-langs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Lisp/Scheme Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Geiser for Guile/Scheme ---
(use-package geiser-guile
  :ensure t
  :config
  (setq geiser-guile-binary "guile"))

(use-package geiser
  :ensure t
  :after geiser-guile
  :config
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-query-on-kill-p nil)
  :hook (scheme-mode . geiser-mode))

;; --- Zig LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  ;; Only register if zls is available
  (when (executable-find "zls")
    (lsp-register-client
     (make-lsp-client
      :server-id 'zls
      :major-modes '(zig-mode)
      :priority 1
      :activation-fn (lambda (filename _server-id)
                      (and (derived-mode-p 'zig-mode)
                           (executable-find "zls")))
      :new-connection (lsp-stdio-connection '("zls"))
      :initialization-options (lambda () '())))))

;; --- Guile LSP Server Configuration ---
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))
  
  ;; Only register if guile-lsp-server is available
  (when (executable-find "guile-lsp-server")
    (lsp-register-client
     (make-lsp-client
      :server-id 'guile-lsp
      :major-modes '(scheme-mode)
      :priority 1
      :activation-fn (lambda (filename _server-id) 
                      (and (derived-mode-p 'scheme-mode)
                           (executable-find "guile-lsp-server")))
      :new-connection (lsp-stdio-connection '("guile-lsp-server"))
      :initialization-options (lambda () '())))))

(add-hook 'scheme-mode-hook 
          (lambda ()
            (when (executable-find "guile-lsp-server")
              (lsp-deferred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- System Integration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Guix System Integration ---
(use-package guix
  :ensure t
  :defer t
  :config
  (setq guix-repl-use-server t)
  (setq guix-repl-use-latest t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Additional Enhancements ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Flycheck for additional linting ---
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.3))

;; --- Project management ---
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf))

;; --- Which-key for discoverability ---
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; --- Global keybindings ---
(global-set-key (kbd "C-c f") #'blfdev/format-buffer)
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f6>") #'recompile)

;; --- Mode-specific format on save ---
(add-hook 'rust-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
(add-hook 'zig-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook #'blfdev/format-buffer nil t)))
(add-hook 'typescript-mode-hook
          (lambda () (add-hook 'before-save-hook #'blfdev/format-buffer nil t)))

(provide 'langs)
