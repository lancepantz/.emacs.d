;;uniquify buffer names
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; for ^u ^<space>
(setq set-mark-command-repeat-pop t)

;; fancy file opening
(ido-mode 1)

;; turn off scrollbars
(scroll-bar-mode 0)

;; turn off emacs startup message
(setq inhibit-startup-message t)
;; do not wrap lines
;; (setq-default truncate-lines t)

;; tab width as two, using spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; add all subdirs of ~/.emcas.d to your load-path
(dolist (f (file-expand-wildcards "~/.emacs.d/*"))
  (add-to-list 'load-path f))

;; load color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;; use wombat
(load-file "~/.emacs.d/color-theme/themes/wombat.el")
(color-theme-wombat)

;; default frame size
(add-to-list 'default-frame-alist (cons 'height 24))
(add-to-list 'default-frame-alist (cons 'width 80))
(add-to-list 'default-frame-alist '(alpha 85 75))

;; f5
(global-set-key [f5] 'revert-buffer)

;; load clojure mode
(require 'clojure-mode)

;; load slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
          (setq slime-protocol-version 'ignore)))

(require 'slime)
(require 'slime-repl)

;; load clojure test mode
(autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;; load paredit
(require 'paredit)
(dolist (mode '(clojure emacs-lisp lisp scheme lisp-interaction))
  (add-hook (first (read-from-string (concat (symbol-name mode) "-mode-hook")))
            (lambda ()
            (paredit-mode 1)
            (local-set-key (kbd "<M-left>") 'paredit-convolute-sexp)
;;            (auto-complete-mode 1)
)))

(global-set-key (kbd "M-7")           'paredit-convolute-sexp)


;; correctly tab defprotocols, etc


;; rainbow parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
       "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; magic, haven't broken this down yet
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

; Dim parens - http://briancarper.net/blog/emacs-clojure-colors
(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

;;macros
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "<C-return>") 'apply-macro-to-region-lines)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq frame-title-format '("%f"))

(eval-after-load 'slime-repl-mode
  '(progn (define-key slime-repl-mode-map (kbd "<C-return>") nil)))

;;processing
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(defun smart-line-beginning ()
  "Move point to the beginning of text
on the current line; if that is already
the current position of point, then move
it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key "\C-a" 'smart-line-beginning)

;; auto-complete-mode
(require 'auto-complete-config)
(ac-config-default)

;; slime auto complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fix indenting in repl
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "<C-return>") nil)
            (setq lisp-indent-function 'clojure-indent-function)
            (set-syntax-table clojure-mode-syntax-table)))

;;use § as interpunct
(global-set-key (kbd "§") (lambda ()
                            (interactive)
                            (insert "·")))
;;other bindings
(defmacro bind-character (key char)
  `(global-set-key (kbd ,key) (lambda ()
                                (interactive)
                                (insert ,char))))
(bind-character "s-s" "·")
(bind-character "s-x" "λ")
(bind-character "s-z" "Φ")

;;load erc znc
(require 'znc)

;;load gist.el
(require 'gist)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; word count
(defun word-count (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))


(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

(setq js-indent-level 2)

(setq font-lock-verbose nil)
(setq slime-net-coding-system 'utf-8-unix)
