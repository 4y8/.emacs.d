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
   '("356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" default))
 '(package-selected-packages
   '(general cmake-font-lock irony-eldoc use-package-ensure-system-package tuareg toc-org smart-tabs-mode sly rainbow-delimiters projectile mood-line merlin-eldoc ivy-prescient git-gutter-fringe flycheck-ocaml flycheck-irony evil-magit evil-collection elfeed el-patch doom-themes dashboard counsel company-prescient company-irony-c-headers company-irony avy auto-package-update anzu)))
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
 '(variable-pitch ((t nil)))
 '(widget-field ((t (:box (:line-width (1 . 1) :color nil :style none) :foreground "#ECEFF4" :background "#242832")))))
