;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; Emacs Load Path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "/home/chenlong/.emacs-lisp/")

;;Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/chenlong/.emacs-lisp/ac-dict")
(ac-config-default)

(require 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; don't show menu bar
(menu-bar-mode nil)

;; show column number and line number at status bar
(setq column-number-mode t)
(setq line-number-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; use y/n to represent yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; check parentheses matching
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; auto newline at column 80
(setq default-fill-column 80)

;; recent file list
(recentf-mode 1)

;; auto complement function and variable names in minibuffer
(icomplete-mode 1)

;; do not generate backup files
(setq-default make-backup-files nil)

;; use ansi color in shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

(defvar my-syntax-table
    (let ((table (make-syntax-table)))
          (modify-syntax-entry ?_ "w")
     table))

(setq-default c-indent-tabs-mode t     ; Pressing TAB should cause indentation
              c-indent-level 4         ; A TAB is equivilent to four spaces
              c-argdecl-indent 0       ; Do not indent argument decl's extra
              c-tab-always-indent t
              backward-delete-function nil) ; DO NOT expand tabs when deleting
(c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4

(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (linum-mode 1)
  (which-function-mode)
  (local-set-key (kbd "M-e") 'move-end-of-line)
  (lambda () (set-syntax-table my-syntax-table))
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(defun my-eshell-mode-set ()
  (local-set-key (kbd "M-m") 'eshell-bol)
  (local-set-key (kbd "M-u") 'eshell-kill-input)
 )

(defun my-text-mode-set ()
  (linum-mode 1)
  (lambda () (set-syntax-table my-syntax-table))
)

(defun my-java-mode-set ()
  (linum-mode 1)
)

(add-hook 'text-mode-hook 'my-text-mode-set)
(add-hook 'eshell-mode-hook 'my-eshell-mode-set)
(add-hook 'java-mode-hook 'my-java-mode-set)

(setq linum-format "%d ") 

(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)     ;; "M-k" originally binds to 'kill-sentence
(global-set-key (kbd "M-h") 'backward-char)     ;; "M-h" originally binds to 'mark-paragraph 
(global-set-key (kbd "M-l") 'forward-char)      ;; "M-l" originally binds to 'downcase-word
(global-set-key (kbd "M-n") 'fast-forward-line)              ;; "M-n" originally binds to nothing

(global-set-key (kbd "M-u") 'kill-region)       ;; "M-u" originally binds to 'upcase-word 
(global-set-key (kbd "M-i") 'select-line)       ;; "M-i" originally binds to 'tab-to-tab-stop
(global-set-key (kbd "M-o") 'other-window)      ;; "M-o" origninally binds to Set Face

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-9") 'eshell)
(global-set-key (kbd "M-0") 'ido-switch-buffer)
(global-set-key (kbd "M--") 'undo)

(global-set-key [f5] 'revert-buffer)
(global-set-key [f7] 'cscope-find-this-symbol) 
(global-set-key [f8] 'cscope-find-functions-calling-this-function)
(global-set-key [f9] 'cscope-find-global-definition-no-prompting) 
(global-set-key [f11] 'list-and-other-window)
(global-set-key [f12] 'grep)

;;(setq sentence-end "[^.].[.?!]+\\([]\"')}]*\\|<[^>]+>\\)\\($\\| $\\|\t\\| \\)[ \t\n]*")

;;Cscope
;; C-c s s         Find symbol.
;; C-c s d         Find global definition.
;; C-c s g         Find global definition (alternate binding).
;; C-c s G         Find global definition without prompting.
;; C-c s c         Find functions calling a function.
;; C-c s C         Find called functions (list functions called
;; 					                    from a function).
;; C-c s t         Find text string.
;; C-c s e         Find egrep pattern.
;; C-c s f         Find a file.
;; C-c s i         Find files #including a file.

;; C-c s b         Display *cscope* buffer.
;; C-c s B         Auto display *cscope* buffer toggle.
;; C-c s n         Next symbol.
;; C-c s N         Next file.
;; C-c s p         Previous symbol.
;; C-c s P         Previous file.
;; C-c s u         Pop mark.

;; find ./project-path -name "*.[chs]" -print > cscope.files
;; cscope -b -i cscope.files -f cscope.out
(require 'xcscope)
;(setq cscope-do-not-update-database-t)

(setq-default indent-tabs-mode nil)
;;    (setq-default tab-width 4)
;;    (setq indent-line-function 'insert-tab)

;; newline and indent when hit <return>
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "C-<return>") 'newline)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; switch buffer
(require 'ido)
(ido-mode t)

;; switch between recent buffers
 ;; (require 'swbuff)
 ;; (global-set-key (kbd "M--") 'swbuff-switch-to-previous-buffer)
;;;; (setq swbuff-exclude-buffer-regexps
;;;;             '("^ " "\*.*\*"))
 ;; (setq swbuff-status-window-layout 'scroll)
 ;; (setq swbuff-clear-delay 2)
 ;; (setq swbuff-separator "|")
 ;; (setq swbuff-window-min-text-height 1)

;;rect-mark.el
(require 'rect-mark)
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
     "Go to the matching paren if on a paren; otherwise insert %."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	   (t (self-insert-command (or arg 1)))))

;;etags * / etags -a subdir/*
;;`M-.’ (‘find-tag’) – find a tag, that is, use the Tags file to look up a definition. 
;;`C-u M-.’ to go to the next match.
;;`M-*’ (‘pop-tag-mark’) – jump back
;;‘M-x tags-search’ – regexp-search through the source files indexed by a tags file (a bit like ‘grep’)
;;‘M-x tags-query-replace’ – query-replace through the source files indexed by a tags file
;;`M-,’ (‘tags-loop-continue’) – resume ‘tags-search’ or ‘tags-query-replace’ starting at point in a source file
;;‘M-x tags-apropos’ – list all tags in a tags file that match a regexp
;;‘M-x list-tags’ – list all tags defined in a source file
(require 'etags-select)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)

;; Mouse support
;; mouse-1 left-button
;; mouse-3 right-button

(xterm-mouse-mode 1)

;; dragging the mouse over a stretch of text also adds the text to the kill ring
(setq mouse-drag-copy-region t)

;; C-down-mouse-1 & C-mouse-1 must both be rebind

;; Ctrl + left-button
(global-set-key (kbd "<C-down-mouse-1>") 'mouse-set-point)
(global-set-key (kbd "<C-mouse-1>") 'etags-select-find-tag-at-point)

;; Ctrl + right-button
(global-set-key (kbd "<C-down-mouse-3>") nil)
(global-set-key (kbd "<C-mouse-3>") 'pop-tag-mark)

;; Shift + left-button
(global-set-key (kbd "<S-down-mouse-1>") 'mouse-set-point)
(global-set-key (kbd "<S-mouse-1>") 'highlight-symbol)