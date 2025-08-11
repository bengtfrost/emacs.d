;; -*- lexical-binding: t; -*-

;; Enhanced smart tab command that handles multiple scenarios
(defun blfdev/smart-tab ()
  "A smart tab command that completes, indents, or inserts spaces.
Priority order:
1. If company-mode completion is active, complete
2. If LSP mode is active and can complete, try LSP completion
3. If at beginning of line (only whitespace), indent the line
4. If in a programming mode, try to indent intelligently
5. Otherwise, insert tab/spaces according to mode settings"
  (interactive)
  (cond
   ;; Condition 1: Company completion is active
   ((and (bound-and-true-p company-mode) 
         (company--active-p))
    (company-complete-common-or-cycle))

   ;; Condition 2: LSP completion available but company not active
   ((and (bound-and-true-p lsp-mode)
         (not (company--active-p))
         (company-manual-begin))
    (company-complete-common))

   ;; Condition 3: At beginning of line (only whitespace before point)
   ((looking-back "^[ \t]*" (line-beginning-position))
    (indent-for-tab-command))

   ;; Condition 4: In programming modes, try smart indentation
   ((derived-mode-p 'prog-mode)
    (if (and (not (eolp)) ; not at end of line
             (or (looking-at "\\s-") ; looking at whitespace
                 (looking-back "\\s-" (1- (point))))) ; or after whitespace
        (indent-for-tab-command)
      ;; Try completion first, fallback to tab
      (if (and (bound-and-true-p company-mode)
               (not (company--active-p)))
          (progn
            (company-manual-begin)
            (if (company--active-p)
                (company-complete-common)
              (insert-tab)))
        (insert-tab))))

   ;; Condition 5: Default case - insert tab/spaces
   (t
    (insert-tab))))

;; Helper function for better tab behavior in specific contexts
(defun blfdev/insert-tab-or-spaces ()
  "Insert appropriate whitespace based on current mode and settings."
  (if indent-tabs-mode
      (insert "\t")
    (insert (make-string tab-width ?\s))))

;; Override insert-tab to use our helper
(defun insert-tab ()
  "Insert a tab character or equivalent spaces."
  (blfdev/insert-tab-or-spaces))

;; Enhanced buffer formatting function
(defun blfdev/format-buffer ()
  "Format buffer using the best available formatter.
Priority: LSP formatter > language-specific formatter > basic indentation."
  (interactive)
  (save-excursion
    (cond
     ;; LSP formatting
     ((and (bound-and-true-p lsp-mode) (lsp-feature? "textDocument/formatting"))
      (call-interactively #'lsp-format-buffer))
     
     ;; Language-specific formatters
     ((derived-mode-p 'python-mode)
      (blfdev/format-python-buffer))
     
     ((or (derived-mode-p 'typescript-mode) 
          (derived-mode-p 'javascript-mode)
          (derived-mode-p 'json-mode) 
          (derived-mode-p 'yaml-mode))
      (blfdev/format-web-buffer))
     
     ((derived-mode-p 'rust-mode)
      (if (executable-find "rustfmt")
          (shell-command-on-region (point-min) (point-max) "rustfmt" t t)
        (indent-region (point-min) (point-max))))
     
     ((derived-mode-p 'zig-mode)
      (if (executable-find "zig")
          (shell-command-on-region (point-min) (point-max) "zig fmt --stdin" t t)
        (indent-region (point-min) (point-max))))
     
     ;; Default: basic indentation
     (t 
      (indent-region (point-min) (point-max)))))
  (message "Buffer formatted"))

(defun blfdev/format-python-buffer ()
  "Format Python buffer with available tools."
  (cond
   ((executable-find "ruff")
    (shell-command-on-region (point-min) (point-max) 
                            "ruff format --stdin-filename=buffer.py" t t))
   ((executable-find "black")
    (shell-command-on-region (point-min) (point-max) 
                            "black --quiet -" t t))
   (t (indent-region (point-min) (point-max)))))

(defun blfdev/format-web-buffer ()
  "Format web-related buffers (JS, TS, JSON, YAML)."
  (cond
   ((executable-find "prettier")
    (let ((file-ext (cond
                     ((derived-mode-p 'typescript-mode) "ts")
                     ((derived-mode-p 'javascript-mode) "js")
                     ((derived-mode-p 'json-mode) "json")
                     ((derived-mode-p 'yaml-mode) "yaml")
                     (t "js"))))
      (shell-command-on-region (point-min) (point-max) 
                              (format "prettier --parser %s" 
                                      (if (string= file-ext "ts") "typescript" file-ext))
                              t t)))
   ((executable-find "dprint")
    (shell-command-on-region (point-min) (point-max) "dprint fmt --stdin" t t))
   (t (indent-region (point-min) (point-max)))))

;; Utility functions for development
(defun blfdev/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (end-of-line)
    (newline)
    (insert line)
    (previous-line)))

(defun blfdev/kill-whole-line ()
  "Kill the entire current line including newline."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun blfdev/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun blfdev/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun blfdev/comment-or-uncomment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(provide 'functions)
