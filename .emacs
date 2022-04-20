;;; package -- Sumary
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight / Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-enable-at-startup nil)

(defvar straight-use-package-by-default t)
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package emacs
  :config
  (setq ring-bell-function 'ignore
        frame-resize-pixelwise t
        default-directory "~/")
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq
   ;; gc-cons-threshold (* 1000 1024 1024)
   read-process-output-max (* 1024 1024)
   redisplay-dont-pause t
   maximum-scroll-margin 0.5
   scroll-margin 99999
   scroll-step 1
   scroll-preserve-screen-position t
   ;; fast-but-imprecise-scrolling nil
   ;; jit-lock-defer-time 0
   auto-window-vscroll nil
   visible-bell       nil
   ring-bell-function #'ignore
   enable-recursive-minibuffers t
   inhibit-startup-screen t
   confirm-kill-processes t
   create-lockfiles nil
   make-backup-files nil)
  (setq-default indent-tabs-mode nil
                bidi-paragraph-direction nil
                require-final-newline t)

  (global-display-fill-column-indicator-mode +1)
  (global-visual-line-mode +1))

;; (use-package scroll-margs)

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;
(use-package restart-emacs)

(use-package diminish)

(use-package ws-butler
  :init
  (ws-butler-global-mode +1))

;; (use-package files
;;   :config
;;   (setq confirm-kill-processes t
;;         create-lockfiles nil
;;         make-backup-files nil))

(use-package autorevert
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  :diminish editorconfig-mode)

(use-package use-package-chords
  :config (key-chord-mode +1))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package yasnippet ;; not yasnippet-bundle
  :config
  (yas-global-mode 1))

(use-package corfu
  :straight (:files ("corfu.el"
                     "extensions/corfu-history.el"))
  :hook ((prog-mode . corfu-mode))
  :bind ((:map corfu-map
               ("C-s" . corfu-next)
               ("C-r" . corfu-previous)
               ("M-SPC" . corfu-insert-separator)))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-on-exact-match nil
        corfu-preview-current t
        corfu-max-width 30
        corfu-count 4
        corfu-auto-delay 1.0)

  ;; Recommended: Enable Corfu globally.
  :init
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin -1.1 :radius 0 :height 0.5 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package flycheck
  :init
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages)
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck)

(use-package expand-region
  :bind (("C-j" . er/expand-region)
         ("C-S-j" . er/contract-region)))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package vertico
  :straight (:files ("vertico.el"
                     "extensions/vertico-directory.el"))
  :init
  (vertico-mode +1)
  :bind (("C-s" . vertico-next)
         ("C-r" . vertico-previous)
         (:map vertico-map
               ("RET" . vertico-directory-enter)
               ("DEL" . vertico-directory-delete-char)
               ("M-DEL" . vertico-directory-delete-word))
         ))

;;(use-package vertico-directory
;;  :after vertico
;;  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after evil
  :bind (("C-x C-b" . consult-buffer)
         (:map evil-normal-state-map
               ("/" . consult-line))))

(use-package tree-sitter
  :hook ((python-mode typescript-mode go-mode sh-mode clojure-mode) . tree-sitter-mode)
  :config
  (add-to-list
   'tree-sitter-major-mode-language-alist
   '(clojure-mode . clojure))
  (require 'tree-sitter-langs)
  (push '(typescript-tsx-mode . typescript) tree-sitter-major-mode-language-alist)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package dockerfile-mode)

;;;;;;;;;;;
;; MaGit ;;
;;;;;;;;;;;

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package forge
  :init
  ;; https://magnus.therning.org/2021-12-08-magit_forge-and-self-hosted-gitlab.html
  (setq
   forge-alist '(("gitlab.deondigital.com" "gitlab.deondigital.com/api/v4"
                  "gitlab.deondigital.com" forge-gitlab-repository)
                 ("github.com" "api.github.com"
                  "github.com" forge-github-repository)
                 ("gitlab.com" "gitlab.com/api/v4"
                  "gitlab.com" forge-gitlab-repository)))
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Window management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Window focus
(define-prefix-command 'window-map)
(global-set-key (kbd "C-q") 'window-map)
(define-key window-map (kbd "C-b") 'windmove-left)
(define-key window-map (kbd "C-n") 'windmove-down)
(define-key window-map (kbd "C-p") 'windmove-up)
(define-key window-map (kbd "C-f") 'windmove-right)

;; Window split
(define-key window-map (kbd "C-a") 'split-window-below)
(define-key window-map (kbd "C-s") 'split-window-right)
(define-key window-map (kbd "C-d") 'delete-window)
(define-key window-map (kbd "C-k") 'kill-buffer)
(define-key window-map (kbd "C-q") 'delete-other-windows)

;; Buffer cycling
(define-key window-map (kbd "C-e") 'next-buffer)
(define-key window-map (kbd "C-w") 'previous-buffer)

;; Window resize
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;;;;;;;;;;
;; Evil ;;
;;;;;;;;;;
(use-package undo-fu)

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode +1))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-respect-visual-line-mode t)

  :bind ((:map evil-motion-state-map
               ("C-i" . nil))
         (:map evil-visual-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line))
         (:map evil-normal-state-map
               ("M-." . nil))
		 (:map evil-insert-state-map
               ("C-r" . nil)))
  :chords ("jk" . evil-normal-state)
  :hook (after-init . evil-mode))

(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :after evil
  :bind (("C->" . evil-mc-make-and-goto-next-match)
         ("C-<" . evil-mc-make-and-goto-prev-match))
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-undo-cursors-on-keyboard-quit 1))

(use-package evil-collection
  :after evil
  :init
  (setq forge-add-default-bindings nil)
  :config
  (evil-collection-init))

;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;
(use-package smartparens
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

(use-package evil-cleverparens
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode))

(use-package avy)

(use-package vilpy
  :straight (vilpy :type git
                   :host github
                   :repo "Andre0991/vilpy")
  :after (avy smartparens)
  :init
  ;; Autoload / :hook does apparently not work
  (add-hook 'emacs-lisp-mode-hook #'vilpy-mode)
  (add-hook 'clojure-mode-hook #'vilpy-mode)
  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at-p "[[:blank:]]*$")))
  (defun vilpy-special-ins ()
    (interactive)
    (call-interactively 'vilpy-special)
    (call-interactively 'evil-insert))
  (defun vilpy-delete-special ()
    (interactive)
    (call-interactively 'vilpy-delete)
    (when (current-line-empty-p)
      (call-interactively 'evil-delete-whole-line))
    (vilpy-special))
  (defun wrap-parens ()
    (interactive)
    (call-interactively 'sp-wrap-round)
    (call-interactively 'vilpy-backward)
    (call-interactively 'evil-insert))
  (defun wrap-brackets ()
    (interactive)
    (call-interactively 'sp-wrap-square)
    (call-interactively 'vilpy-backward)
    (call-interactively 'evil-insert))
  (defun wrap-braces ()
    (interactive)
    (call-interactively 'sp-wrap-curly)
    (call-interactively 'evil-insert))
  (defun comment ()
    (interactive)
    (if (memq major-mode vilpy-clojure-modes)
        (call-interactively 'vilpy-underscore)
      (call-interactively 'vilpy-comment)))
  :bind (("C-f" . vilpy-special-ins)
         ("s-l" . vilpy-slurp)
         ("s-h" . vilpy-barf)
         (:map evil-normal-state-map
               ("C-f" . vilpy-special-ins))
         (:map vilpy-mode-map
               ("C-M-." . wrap-parens)
               ("C-M->" . wrap-brackets)
               ("C-M-," . wrap-braces))
         (:map vilpy-mode-map-vilpy
               ("C-j" . nil)))
  :config
  (vilpy-define-key vilpy-mode-map "i" 'vilpy-mark-list)
  (vilpy-define-key vilpy-mode-map "d" 'vilpy-delete-special)
  (vilpy-define-key vilpy-mode-map "D" 'vilpy-delete)
  (vilpy-define-key vilpy-mode-map "." 'wrap-parens)
  (vilpy-define-key vilpy-mode-map ">" 'wrap-brackets)
  (vilpy-define-key vilpy-mode-map "," 'wrap-braces)
  (vilpy-define-key vilpy-mode-map "c" 'comment)
  (vilpy-define-key vilpy-mode-map "y" 'vilpy-copy)

  (vilpy-mode +1))

(use-package aggressive-indent
  :init
  (setq aggressive-indent-protected-commands
        '(undo undo-tree-undo undo-tree-redo whitespace-cleanup recenter))
  ;; Autoload / :hook does apparently not work
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :config
  (add-hook 'before-save-hook 'clojure-sort-ns t t))

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clj-kondo)

(use-package cider
  :defer t
  :commands (cider cider-connect cider-jack-in)
  :init
  (setq cider-auto-select-error-buffer nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        cider-repl-history-file nil
        ;; Stop error buffer from popping up while
        ;; working in buffers other than the REPL:
        nrepl-popup-stacktraces nil
        cider-repl-require-ns-on-set t)
  :bind (:map cider-mode-map
         ("C-c C-p" . cider-eval-print-last-sexp))
  :config
  (flycheck-clojure-setup)
  (setq-default flycheck-disabled-checkers
                '(clojure-cider-eastwood clojure-cider-typed))
  :after clojure-mode)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

;; Dependencies
;; pip3 install jupyterlab pyright pylint mypy flake8

(use-package eval-sexp-fu)

(use-package elpy
  :init
  (eval-sexp-fu-flash-mode)
  (elpy-enable)
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --existing --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        elpy-get-info-from-shell t)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  :bind (:map elpy-mode-map
              ("C-c C-v" . nil)
              ("C-c C-v C-v" . elpy-shell-send-statement)
              ("C-c C-v C-r" . elpy-shell-send-region-or-buffer)
              ("C-c C-c" . elpy-shell-send-group)
              ("C-c C-k" . elpy-shell-send-buffer)))

;;;;;;;;;;;;;;;;
;; Typescript ;;
;;;;;;;;;;;;;;;;

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (js-mode . tide-setup)
         (web-mode . tide-setup))
  :init
  (defun my-tide-mode-hook ()
    (tide-hl-identifier-mode 1))
  :config
  (setq flycheck-javascript-eslint-executable "eslint")
  (setq typescript-indent-level 4)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))

;;;;;;;;
;; UI ;;
;;;;;;;;

(setq show-paren-delay 0.1)
(show-paren-mode +1)

(use-package fira-code-mode
  :custom
  ;; List of ligatures to turn off
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode
  :config
  (set-face-attribute 'default nil :family "Fira Code" :height 170))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode)

(setq display-line-numbers-width-start 3)
(global-display-line-numbers-mode)

(use-package powerline
  :config
  (powerline-vim-theme))

(require 'uniquify)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Centered point mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-change ()
  (when (eq (get-buffer-window)
            (selected-window))
    (recenter)))

(define-minor-mode centered-point-mode
  "Alaways center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	    (t (remove-hook 'post-command-hook 'line-change))))

;; (centered-point-mode)

;;;;;;;;;
;; LSP ;;
;;;;;;;;;

(setq lsp-use-plists t)
(use-package lsp-mode
  :init
  (setq lsp-completion-provider :none
        lsp-keymap-prefix "C-l"
        lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.500
        lsp-auto-configure t
        lsp-log-io nil
        ;; to avoid conflicting with CIDER eldoc
        lsp-eldoc-enable-hover nil
        lsp-lens-enable t
        ;; clojure-lsp runs cljfmt on indent which is too aggresive
        lsp-enable-indentation nil
        treemacs-space-between-root-nodes nil
        lsp-signature-auto-activate nil)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 1.0
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-max-width 30
        lsp-ui-doc-max-height 6
        lsp-ui-doc-header t
        lsp-ui-doc-text-scale-level -4
        lsp-ui-doc-enhanced-markdown t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((cider-default-cljs-repl . shadow)))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
