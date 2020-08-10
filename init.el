;; -*- lexical-binding: t; -*-

;; Disable garbage collection to improve startup time
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
 
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; Install straight.el
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
(setq straight-use-package-by-default t)

;; Aesthetic changes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package display-line-numbers
  :straight (:type built-in)
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package dashboard
  :config
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Disable unuseful UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1) 
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "<SPC>" 'projectile-find-file
   "ff"    'find-file
   "fr"    'counsel-recentf)
  (global-evil-leader-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package ivy 
  :config
  (ivy-mode 1)
  (setq ivy-height 14))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy))
  
;; Set up indentation
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(setq backward-delete-char-untabify-method 'hungry)
