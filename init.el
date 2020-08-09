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

(setq menu-bar-mode -1)

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "<SPC>" 'find-file
   "f"     'find-file)
  (global-evil-leader-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package selectrum
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

;; Set up indentation
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
