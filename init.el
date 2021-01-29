;;; init.el --- My configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;
;;; Code:
(org-babel-load-file "~/.emacs.d/config.org")
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" default)))
 '(package-selected-packages
   (quote
    (circe plan9-theme go-mode eglot use-package-ensure-system-package undo-tree tuareg toc-org smart-tabs-mode rainbow-delimiters projectile notmuch mood-line merlin-eldoc ivy-prescient irony-eldoc git-gutter-fringe general flycheck-ocaml flycheck-irony evil-magit evil-collection elfeed el-patch doom-themes dashboard counsel company-prescient company-irony-c-headers company-irony cmake-font-lock avy auto-package-update anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "#EBCB8B"))))
 '(eww-form-submit ((t (:inherit custom-button))))
 '(eww-form-text ((t (:box (:line-width (1 . 1) :color nil :style none) :foreground "#ECEFF4" :background "#242832"))))
 '(eww-valid-certificate ((t (:weight bold :foreground "#A3BE8C"))))
 '(fixed-pitch ((t nil)))
 '(flycheck-error ((t (:underline "#BF616A"))))
 '(flycheck-info ((t (:underline "#A3BE8C"))))
 '(flycheck-warning ((t (:underline "#EBCB8B"))))
 '(variable-pitch ((t nil))))
