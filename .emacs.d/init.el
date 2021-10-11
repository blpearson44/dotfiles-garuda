;; Emacs init.el
;; Written by Ben Pearson

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
 '(org-agenda-files nil)
 '(package-selected-packages
   '(page-break-lines dired-open peep-dired gcmh vterm which-key all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich counsel dashboard dashboard-hackernews dashboard-ls dashboard-project-status emojify eshell-syntax-highlighting evil-tutor general ivy-posframe ivy-rich magit markdown-mode neotree org org-ac org-agenda-property org-appear org-autolist org-beautify-theme org-bullets org-evil perspective projectile pylint python python-mode smex toc-org evil-smartparens evil-tabs evil evil-better-visual-line evil-collection evil-commentary evil-leader evil-surround use-package ivy doom-modeline command-log-mode)))
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
(use-package general
  :config
  (general-evil-setup t))

;; Garbage collection magic trick to make faster
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentag4e 0.6)
;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Silence compiler warnings
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))
(setq load-prefer-newer noninteractive)

;; Enable all-the-icons
(use-package all-the-icons)

;; Buffer keybindings
(nvmap :prefix "SPC"
  "b b" '(ibuffer :which-key "Ibuffer")
  "b c" '(kill-current-buffer :which-key "Close current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer")
  "b B" '(ibuffer-list-buffers :which-key "Ibuffer list buffers"))
  
;; Dashboard
(dashboard-setup-startup-hook)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


;; Set initial buffer to dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


;; emoji
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Dired
(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)

(nvmap :states'(normal visual) :keymaps 'override :prefix "SPC"
  "d d" '(dired :which-key "Open dired")
  "f d" '(dired-jump :which-key "Open dired in cwd")
  "d p" '(peep-dired :which-key "Peep-dired"))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

;; Files

;; mappings
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "f f" '(find-file :which-key "Find file")
  "f r" '(counsel-recentf :which-key "Recent files")
  "f y" '(dt/show-and-copy-buffer-path :which-key "Yank file path")
  "f C" '(copy-file :which-key "Copy file")
  "f D" '(delete-file :which-key "Delete file")
  "f R" '(rename-file :which-key "Rename file"))

;; Copy path function courtesy of distrotube
(defun dt/show-and-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))


;; Fonts

(set-face-attribute 'default nil
		    :font "Fira Code"
		    :height 120
		    :weight 'medium)
(set-face-attribute 'variable-pitch nil
		    :font "Source Code Pro"
		    :height 120
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :font "Fira Code"
		    :height 120
		    :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)
(setq global-prettify-symbols-mode t)

;; zoom
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Visual changes
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 15)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

(use-package doom-modeline)
(doom-modeline-mode 1)

;; icy and counsel, completion mechanisms
(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
			       'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))
(use-package swiper
     :after ivy
     :bind (("C-s" . swiper)
	    ("C-r" . swiper)))

;; M-x changes
(setq ivy-initial-inputs-alist nil)
(use-package smex)
(smex-initialize)

;; ivy-posframe
(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
    '((swiper                     . ivy-posframe-display-at-point)
      (complete-symbol            . ivy-posframe-display-at-point)
      (counsel-M-x                . ivy-display-function-fallback)
      (counsel-esh-history        . ivy-posframe-display-at-window-center)
      (counsel-describe-function  . ivy-display-function-fallback)
      (counsel-describe-variable  . ivy-display-function-fallback)
      (counsel-find-file          . ivy-display-function-fallback)
      (counsel-recentf            . ivy-display-function-fallback)
      (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
      (dmenu                      . ivy-posframe-display-at-frame-top-center)
      (nil                        . ivy-posframe-display))
    ivy-posframe-height-alist
    '((swiper . 20)
      (dmenu . 20)
      (t . 10)))
  :config
  (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

;; Modes
(use-package markdown-mode)
;; Magit
(use-package magit)
;; Neotree
(use-package neotree
  :config
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        ;;neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))
;; Show hidden files
(setq-default neo-show-hidden-files t)

(nvmap
  "C-n" '(neotree-toggle :which-key "Toggle neotree file viewer"))


;; Org mode
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/Org/"
      org-agenda-files '("~/Org/agenda.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ▼ "
      org-log-done 'time
      org-journal-dir "~/Org/journal/"
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org"
      org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

;; Beautify the bullets
(use-package org-bullets)
(add-hook 'org-mode-hook (org-bullets-mode 1))

;; Emmet style org completion
(use-package org-tempo
  :ensure nil)

;; Org syntax highlighting
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))


;; Projectile project management
(use-package projectile
  :config
  (projectile-global-mode 1))


;; Scrolling
(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse


;; Shell
(use-package vterm)
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)


;; Window controls
(nvmap :prefix "SPC"
  ;;Splits
  "w c" '(evil-window-delete :which-key "Close window")
  "w n" '(evil-window-new :which-key "New window")
  "w s" '(evil-window-split :which-key "Horizontal split")
  "w v" '(evil-window-vsplit :which-key "Vertical split")
  ;;Motions
  "w h" '(evil-window-left :which-key "Window left")
  "w j" '(evil-window-down :which-key "Window down")
  "w k" '(evil-window-up :which-key "Window up")
  "w l" '(evil-window-right :which-key "Window right")
  "w w" '(evil-window-next :which-key "Goto next window")
  "SPC" '(evil-window-prev :which-key "Previous window"))



;; Which-key
(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
