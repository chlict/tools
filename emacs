;; .emacs

;; package-list-packages
;; package-refresh-contents
; package-install
(when (>= emacs-major-version 24)
  (progn
    ;; load emacs 24's package system.
    (require 'package)
    ;; Add MELPA repository.
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (when (< emacs-major-version 27) (package-initialize)))

;; Emacs Load Path
(add-to-list 'load-path "~/.emacs-lisp/")

;; lazy loading support
(eval-when-compile
  (add-to-list 'load-path "~/.emacs-lisp/use-package-2.4.1")
  (require 'use-package))

(add-to-list 'load-path "~/.emacs-lisp/auto-complete-1.5.1")
(add-to-list 'load-path "~/.emacs-lisp/popup-el-0.5.8")
(use-package auto-complete-config
  :config
  (ac-config-default))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp performance tuning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package ido
  :config(ido-mode t)
  :bind("M-0" . ido-switch-buffer))

;; Customized key bindings
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)     ;; "M-k" originally binds to 'kill-sentence
(global-set-key (kbd "M-h") 'backward-char)     ;; "M-h" originally binds to 'mark-paragraph
(global-set-key (kbd "M-l") 'forward-char)      ;; "M-l" originally binds to 'downcase-word
(global-set-key (kbd "M-n") 'fast-forward-line)              ;; "M-n" originally binds to nothing
(global-set-key (kbd "M-F") 'find-file)         ;; "M-F" originally binds to 'select forward'
(global-set-key (kbd "M-S") 'save-buffer)       ;; "M-S" originally binds to nothing
(global-set-key (kbd "M-RET") 'my-insert-line-above)         ;; "M-RET" originally binds to nothing
(global-set-key (kbd "M-u") 'kill-region)       ;; "M-u" originally binds to 'upcase-word
(global-set-key (kbd "M-i") 'select-line)       ;; "M-i" originally binds to 'tab-to-tab-stop
(global-set-key (kbd "M-o") 'other-window)      ;; "M-o" origninally binds to Set Face
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-9") 'eshell)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-s") 'avy-goto-char-2)   ;; "M-s" originally binds to "Prefix Command"
(define-key isearch-mode-map "\M-s" 'isearch-repeat-forward) ; remaps the binding for isearch-repeat-forward

(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'imenu-list-smart-toggle)
(global-set-key [f7] 'cscope-find-this-symbol)
(global-set-key [f8] 'cscope-find-functions-calling-this-function)
(global-set-key [f9] 'cscope-find-global-definition-no-prompting)
(global-set-key [f11] 'list-and-other-window)
(global-set-key [f12] 'grep)

;; Display line numbers if emacs version > 26.0.5
;; To toggle line numbers on and off for a specific buffer: M-x display-line-numbers-mode
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Disable line-numbers minor mode for eshell
(add-hook 'eshell-mode-hook (lambda (&rest _) (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook (lambda (&rest _) (display-line-numbers-mode -1)))

;; Highlight symbols
(use-package highlight-symbol
  :bind (([f3] . highlight-symbol)
         ("C-<f3>" . highlight-symbol-next)
         ("C-<f3>" . highlight-symbol-prev)))

;; ibuffer
(use-package ibuffer
  :bind("C-x C-b" . ibuffer)
)

;; rect-mark.el
(use-package rect-mark
  :bind("%" . match-paren)
)
(defun match-paren (arg)
     "Go to the matching paren if on a paren; otherwise insert %."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
           ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
           (t (self-insert-command (or arg 1)))))

;; use y/n to represent yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; show column number and line number at status bar
(setq column-number-mode t)

;; auto complement function and variable names in minibuffer
(icomplete-mode 1)

;; do not generate backup files
(setq-default make-backup-files nil)

;; check parentheses matching
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(defun select-line ()
    "select whole line"
    (interactive)
    (progn (move-beginning-of-line nil)
           (set-mark-command nil)
           (forward-line 1)
))

(defun fast-forward-line ()
  "forward 5 lines"
  (interactive)
  (progn (forward-line 5))
)

(defun list-and-other-window ()
    (interactive)
    (progn (list-buffers)
           (other-window 1)
))

;; insert a line above the current line
(defun my-insert-line-above()
    (interactive)
    (progn (beginning-of-line)
           (indent-according-to-mode)
           (newline-and-indent)
           (forward-line -1)
           (indent-according-to-mode)
))

(setq sentence-end "[^.].[.?!]+\\([]\"')}]*\\|<[^>]+>\\)\\($\\| $\\|\t\\| \\)[ \t\n]*")

(use-package clang-format
  :bind ("M-'" . clang-format-region)
)

;; indentation configuration for C/C++
(setq-default c-indent-tabs-mode t     ; Pressing TAB should cause indentation
              c-basic-offset 2         ; A basic indent level
              ;; c-indent-level 2      ; A TAB is equivilent to # spaces ;; same effect with above
              c-argdecl-indent 0       ; Do not indent argument decl's extra
              c-tab-always-indent t
              backward-delete-function nil) ; DO NOT expand tabs when deleting
(c-set-offset 'inline-open '+)
(c-set-offset 'block-open '+)
(c-set-offset 'brace-list-open '+)     ; all "opens" should be indented by the c-indent-level
(c-set-offset 'case-label '+)          ; indent case labels by c-indent-level, too
(c-set-offset 'substatement-open '0)   ; brackets should be at same indentation level as the statements they open
                                       ; if (cond)
                                       ; { <-
                                       ; }

(setq-default indent-tabs-mode nil)

(defvar my-syntax-table
    (let ((table (make-syntax-table)))
          (modify-syntax-entry ?_ "w")
     table))

(defun my-c-mode-hook ()
  (which-function-mode)
  (local-set-key (kbd "M-e") 'move-end-of-line)
  (lambda () (set-syntax-table my-syntax-table))
)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(defun my-eshell-mode-set ()
  (local-set-key (kbd "M-m") 'eshell-bol)
  (local-set-key (kbd "M-u") 'eshell-kill-input)
 )
(add-hook 'eshell-mode-hook 'my-eshell-mode-set)

;; Timing utilities
;; report init time
(add-to-list 'after-init-hook
  (lambda ()
    (message (concat "emacs (" (number-to-string (emacs-pid)) ") started in " (emacs-init-time)))))

;; usage: (with-timer "command name" (command))
(defmacro with-timer (name &rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f" ,name (float-time (time-since time)))))

;; etags `find include/ lib/`

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key yasnippet lsp-mode company-irony irony imenu-list flycheck neotree vterm company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
