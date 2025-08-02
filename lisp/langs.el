;;; langs.el --- Language-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains language-specific configuration including LSP setup,
;; completion, syntax highlighting, and development tools.

;;; Code:

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
  (setq lsp-completion-provider :none)   ; Use company instead
  (setq lsp-idle-delay 0.3)              ; Reduce delay for responsiveness
  (setq lsp-log-io nil)                  ; Disable logging for performance
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-snippet t)

  ;; File watchers
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-enable-file-watchers t)

  ;; Diagnostics
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-eldoc-enable-hover t)

  ;; Better completion
  (setq lsp-completion-enable t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)

  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-c l f" . blfdev/format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-find-definition)
              ("C-c l i" . lsp-find-implementation)
              ("C-c l t" . lsp-find-type-definition)
              ("C-c l x" . lsp-find-references)
              ("C-c l h" . lsp-describe-thing-at-point)
              ("C-c l s" . lsp-workspace-symbol)
              ("C-c l R" . lsp-workspace-restart)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.3)

  ;; Disable some features for terminal use
  (setq lsp-ui-doc-use-childframe nil)
  (setq lsp-ui-doc-use-webkit nil)

  :bind (:map lsp-ui-mode-map
              ("C-c l u" . lsp-ui-imenu)
              ("C-c l p" . lsp-ui-peek-find-definitions)
              ("C-c l P" . lsp-ui-peek-find-references)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Completion Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1)          ; Faster completion
  (setq company-minimum-prefix-length 1) ; Complete after 1 character
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 15)        ; More candidates
  (setq company-show-numbers t)          ; Show numbers for quick selection
  (setq company-selection-wrap-around t) ; Wrap around when navigating
  (setq company-require-match nil)       ; Allow non-matching input
  (setq company-dabbrev-downcase nil)    ; Don't downcase completions
  (setq company-backends '(company-capf
                          company-files
                          company-keywords
                          company-dabbrev-code
                          company-dabbrev))

  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("TAB" . company-complete-common-or-cycle)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)
              ("<escape>" . company-abort))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package company-box
  :ensure t
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Syntax Highlighting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-langs '(python typescript tsx javascript json yaml toml 
                            zig c cpp rust java kotlin swift))
  ;; Fallback configurations for problematic grammars
  (setq treesit-auto-fallback-alist '((rust-mode . rust-mode)
                                     (c-mode . c-mode)
                                     (cpp-mode . c++-mode)))
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Language-Specific Configurations ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Rust ---
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . (lambda ()
                       (setq-local tab-width 4)
                       (setq-local indent-tabs-mode nil))))
  :config
  (setq rust-format-on-save nil)  ; We'll handle formatting manually
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

;; --- Zig ---
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . (lambda ()
                      (setq-local tab-width 4)
                      (setq-local indent-tabs-mode nil))))
  :config
  (setq zig-format-on-save nil))

;; --- Python ---
(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4)
  ;; Ensure lsp-mode uses ruff-lsp for python
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("ruff-lsp"))
                      :major-modes '(python-mode python-ts-mode)
                      :server-id 'ruff-lsp))))

;; **THE DEFINITIVE FIX for the python 'file-missing' error**
;; This hook runs whenever flycheck-mode is enabled in a buffer.
;; It checks if the buffer is a python-mode buffer and, if so,
;; immediately restricts flycheck to *only* use the 'lsp' checker.
;; This is more reliable than using the python-mode-hook.
(defun blfdev/configure-python-flycheck ()
  "Configure Flycheck for Python mode to only use the LSP."
  (when (derived-mode-p 'python-mode)
    (setq-local flycheck-checkers '(lsp))))

(add-hook 'flycheck-mode-hook #'blfdev/configure-python-flycheck)

;; --- TypeScript/JavaScript ---
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook ((typescript-mode . lsp-deferred)
         (typescript-mode . (lambda ()
                             (setq-local tab-width 2)
                             (setq-local indent-tabs-mode nil))))
  :config
  (setq typescript-indent-level 2))

(add-hook 'js-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)))
(setq js-indent-level 2)

;; --- JSON ---
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :hook ((json-mode . lsp-deferred)
         (json-mode . (lambda ()
                       (setq-local tab-width 2)
                       (setq-local indent-tabs-mode nil))))
  :config
  (setq json-reformat:indent-width 2))

;; --- YAML ---
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . lsp-deferred)
         (yaml-mode . (lambda ()
                       (setq-local tab-width 2)
                       (setq-local indent-tabs-mode nil))))
  :config
  (setq yaml-indent-offset 2))

;; --- TOML ---
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'"
  :hook (toml-mode . (lambda ()
                      (when (executable-find "taplo")
                        (lsp-deferred)))))

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
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

;; --- C/C++ ---
(add-hook 'c-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)
            (setq c-basic-offset 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)
            (setq c-basic-offset 4)))

;; --- Shell Scripts ---
(setq sh-basic-offset 2)
;; For executing scripts, default to the stable system bash.
(setq sh-shell-file "/run/current-system/profile/bin/bash")

(add-hook 'sh-mode-hook
          (lambda ()
            ;; This hook runs when you open any .sh file.
            ;; `sh-shell` is a buffer-local variable that `sh-mode` smartly sets
            ;; based on the file's shebang (e.g., #!/bin/bash).
            ;;
            ;; This check correctly starts the bash LSP *only* when you are
            ;; editing a file that is specifically a bash script.
            (when (and (eq sh-shell 'bash)
                       (executable-find "bash-language-server"))
              (lsp-deferred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Lisp/Scheme Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq geiser-mode-start-repl-p t)
  :hook (scheme-mode . geiser-mode))

;; Enhanced Lisp editing
(use-package smartparens
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . smartparens-strict-mode)
  :config
  ;; **FIX**: The (require 'smartparens-config) line was removed to fix
  ;; the "cl is deprecated" warning.
  (sp-use-paredit-bindings))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- LSP Server Configurations ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'lsp-mode
  ;; Zig LSP Server
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
      :initialization-options (lambda () '()))))

  ;; Guile LSP Server
  (add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))
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

(use-package guix
  :ensure t
  :defer t
  :config
  (setq guix-repl-use-server t)
  (setq guix-repl-use-latest t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Additional Development Tools ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled new-line))
  (setq flycheck-display-errors-delay 0.3)
  (setq flycheck-indication-mode 'right-fringe))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-project-search-path '("~/projects/" "~/code/"))
  (defun blfdev/projectile-switch-project-action ()
    "Custom action when switching projects."
    (if (projectile-project-p)
        (projectile-find-file)
      (find-file ".")))
  (setq projectile-switch-project-action #'blfdev/projectile-switch-project-action))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind (("C-c p f" . consult-projectile-find-file)
         ("C-c p b" . consult-projectile-switch-to-buffer)
         ("C-c p p" . consult-projectile-switch-project)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Format on Save Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar blfdev/format-on-save-modes
  '(rust-mode zig-mode python-mode typescript-mode js-mode json-mode)
  "List of modes where format-on-save should be enabled.")

(defun blfdev/maybe-format-buffer ()
  "Format buffer if current mode is in format-on-save list."
  (when (member major-mode blfdev/format-on-save-modes)
    (blfdev/format-buffer)))

(dolist (mode blfdev/format-on-save-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (add-hook 'before-save-hook #'blfdev/maybe-format-buffer nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Enhanced Project Keybindings ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'projectile
  (with-eval-after-load 'general
    (general-define-key
     :keymaps 'global
     "SPC p f" '(consult-projectile-find-file :which-key "Find File in Project")
     "SPC p p" '(consult-projectile-switch-project :which-key "Switch Project")
     "SPC p b" '(consult-projectile-switch-to-buffer :which-key "Project Buffer")
     "SPC p g" '(projectile-grep :which-key "Grep in Project")
     "SPC p r" '(projectile-replace :which-key "Replace in Project")
     "SPC p c" '(projectile-compile-project :which-key "Compile Project")
     "SPC p t" '(projectile-test-project :which-key "Test Project")
     "SPC p d" '(projectile-dired :which-key "Project Root")
     "SPC p k" '(projectile-kill-buffers :which-key "Kill Project Buffers"))))

(provide 'langs)

;;; langs.el ends here
