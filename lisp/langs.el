;; ~/.emacs.d/lisp/langs.el
;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced Language Configuration for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq lsp-completion-provider :none)
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
              ("TAB"   . company-complete-common-or-cycle)
              ("C-n"   . company-select-next)
              ("C-p"   . company-select-previous))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; ... the rest of your language configs unchanged ...

(provide 'langs)

