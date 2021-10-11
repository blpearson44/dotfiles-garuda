;; Emacs init.el
;; Written by Ben Pearson
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(set-face-attribute 'default nil :font "Fira Code" :height 150)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Automatic -- Do not touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(vterm which-key all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich counsel dashboard dashboard-hackernews dashboard-ls dashboard-project-status emojify eshell-syntax-highlighting evil-tutor general ivy-posframe ivy-rich magit markdown-mode neotree org org-ac org-agenda-property org-appear org-autolist org-beautify-theme org-bullets org-evil perspective projectile pylint python python-mode smex toc-org evil-smartparens evil-tabs evil evil-better-visual-line evil-collection evil-commentary evil-leader evil-surround use-package ivy doom-modeline command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Evil mode
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

;; General.el for keybindings
