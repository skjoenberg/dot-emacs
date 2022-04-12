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
  (setq scroll-margin 0
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        visible-bell       nil
        ring-bell-function #'ignore
        enable-recursive-minibuffers t)
  (setq-default indent-tabs-mode nil)
  (setq-default require-final-newline t)
  (setq inhibit-startup-screen t)
  (setq confirm-kill-processes t
        create-lockfiles nil
        make-backup-files nil))

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

(use-package company
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-dabbrev-ignore-case t
        company-dabbrev-downcase t
        company-tooltip-limit 5
        company-tooltip-minimum 4
        company-tooltip-flip-when-above t
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend) )
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-s") 'company-select-next)
  (define-key company-active-map (kbd "C-r") 'company-select-previous))

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

(use-package embark)

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

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :bind ((:map evil-motion-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("C-i" . nil))
         (:map evil-visual-state-map
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line))
         (:map evil-normal-state-map
               ("M-." . nil)))
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
  :config
  (evil-collection-init))

;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;
;; (defvar lisp-modes '(clojure-mode emacs-lisp-mode))

(use-package avy)

(use-package vilpy
  :straight (vilpy :type git
                   :host github
                   :repo "Andre0991/vilpy")
  :after avy
  :bind (("C-f" . vilpy-special)
         (:map evil-normal-state-map
               ("C-f" . vilpy-special))
         (:map vilpy-mode-map
               ("C-M-." . vilpy-parens)
               ("C-M->" . vilpy-brackets)
               ("C-M-," . vilpy-braces))
         (:map vilpy-mode-map-special
               ("C-f" . vilpy-other))
         (:map vilpy-mode-map-vilpy
               ("C-j" . nil)))
  :init
  ;; Autoload / :hook does apparently not work
  (add-hook 'emacs-lisp-mode-hook #'vilpy-mode)
  (add-hook 'clojure-mode-hook #'vilpy-mode)
  :config
  (vilpy-define-key vilpy-mode-map "TAB" 'vilpy-other)
  (vilpy-define-key vilpy-mode-map "i" 'vilpy-mark-list)
  (vilpy-mode +1))

(use-package aggressive-indent
  :init
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

(setq show-paren-delay 0)
(show-paren-mode +1)

(use-package fira-code-mode
  :custom
  ;; List of ligatures to turn off
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode
  :config
  (set-face-attribute 'default nil :family "Fira Code" :height 180))

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

(centered-point-mode)

;;;;;;;;;
;; LSP ;;
;;;;;;;;;

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l"
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        lsp-headerline-breadcrumb-enable nil
        lsp-auto-configure t
        ;; to avoid conflicting with CIDER eldoc
        lsp-eldoc-enable-hover nil
        lsp-lens-enable t
        ;; clojure-lsp runs cljfmt on indent which is too aggresive
        lsp-enable-indentation nil
        treemacs-space-between-root-nodes nil
        company-minimum-prefix-length 2
        lsp-signature-auto-activate nil)

  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0
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
 '(safe-local-variable-values '((cider-default-cljs-repl . shadow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
