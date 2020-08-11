;;; init.el --- My configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;
;;; Code:
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
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mood-line
  :config
  (mood-line-mode))
  
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package dashboard
  :custom
  (dashboard-show-shortcuts nil)
  :config
  (dashboard-setup-startup-hook))

; I like the scientifica font
(set-frame-font
 "-HBnP-scientifica-normal-normal-normal-*-11-*-*-*-*-0-iso10646-1")

;; Disable unuseful UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-height 20))

(use-package counsel
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package projectile
  :commands project-find-file
  :custom
  (projectile-completion-system 'ivy))

(electric-pair-mode 1)

;; Set up indentation
(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c))

(setq-default indent-tabs-mode t
              tab-width 8
              electric-indent-inhibit t)

(defvaralias 'c-basic-offset 'tab-width)
(setq backward-delete-char-untabify-method 'hungry)

;; Display a Pipe in tabs
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9])))

(add-hook 'c-mode-hook 'whitespace-mode)

;; Customize faces for whitespace mode
(custom-set-faces
 '(whitespace-indentation ((t (:background "#"))))
 '(whitespace-space-after-tab ((t nil))))

(custom-set-variables
 '(whitespace-line-column 100))

;; Set up code completion and checking, for C
(use-package irony
  :hook
  (c-mode     . irony-mode)
  (irony-mode . irony-cdb-autosetup-compile-options))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-irony
  :after flycheck
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package company
  :hook (prog-mode . company-mode)
  :bind
  ("M-j" . 'company-select-next)
  ("M-k" . 'company-select-previous)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  :config
  (add-to-list 'company-backends '(merlin-company-backend
                                  company-irony-c-headers
                                  company-irony)))

(use-package company-irony
  :after irony company)

(use-package company-irony-c-headers
  :after irony company)

;; Set up code completion and checking, for Ocaml
(use-package caml-mode
  :hook
  (caml-mode . merlin-mode))

(use-package tuareg
  :hook
  (tuareg-mode . merlin-mode)
  :custom
  (tuareg-match-patterns-aligned t))

;; Set up Merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)))

;; Email, you have to set up the email address yourself
(use-package mu4e
  :commands mu4e
  :bind (:map mu4e-headers-mode-map
              ("j" . mu4e-headers-next)
              ("k" . mu4e-headers-prev))
  :custom
  (mu4e-maildir           "~/.mail")
  (mu4e-sent-folder       "/INBOX.OUTBOX")
  (mu4e-drafts-folder     "/INBOX.DRAFT")
  (mu4e-trash-folder      "/INBOX.TRASH")
  (mu4e-refile-folder     "/INBOX")
  (mu4e-html2text-command "html2text"))

;; git
(use-package magit)

(use-package git-gutter-fringe)

;; Taken from Doom emacs
(setq-default fringes-outside-margins t)
;; thin fringe bitmaps
(define-fringe-bitmap 'git-gutter-fr:added [224]
  nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224]
  nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
  nil nil 'bottom)

(add-hook 'prog-mode-hook 'git-gutter-mode)

;; Key bindings
(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "<SPC>" 'projectile-find-file
   "sb"    'swiper
   "ff"    'find-file
   "fr"    'counsel-recentf
   "ec"    'counsel-flycheck
   "cr"    'comment-region
   "cc"    'comment-line
   "gc"    'magit-commit)
  (global-evil-leader-mode))

(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode))

(use-package evil
  :config
  (evil-mode 1))

;; El-patch
(use-package el-patch)

(el-patch-feature mood-line)
(with-eval-after-load 'mood-line
  (el-patch-defun mood-line--update-flycheck-segment (&optional status)
    "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
    (setq mood-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat
                                          (el-patch-swap "⚑ Issues: " "Issues: ") ;; The '⚑' character doesn't work well with scientifica.
                                          (number-to-string sum)
                                          "  ")
                                         'face (if .error
                                                   'mood-line-status-error
                                                 'mood-line-status-warning))))
                       (propertize "✔ Good  " 'face 'mood-line-status-success)))
          ('running (propertize "Δ Checking  " 'face 'mood-line-status-info))
          ('errored (propertize "✖ Error  " 'face 'mood-line-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'mood-line-status-neutral))
          ('no-checker "")))))

;;; init.el ends here
