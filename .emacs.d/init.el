;; Emacs init.el
;; Written by Ben Pearson
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar


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
   '(dired-open peep-dired gcmh vterm which-key all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich counsel dashboard dashboard-hackernews dashboard-ls dashboard-project-status emojify eshell-syntax-highlighting evil-tutor general ivy-posframe ivy-rich magit markdown-mode neotree org org-ac org-agenda-property org-appear org-autolist org-beautify-theme org-bullets org-evil perspective projectile pylint python python-mode smex toc-org evil-smartparens evil-tabs evil evil-better-visual-line evil-collection evil-commentary evil-leader evil-surround use-package ivy doom-modeline command-log-mode)))
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
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (agenda . 5)
			  (bookmarks . 3)
			  (projects . 3)
			  (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book"))))
;; Set initial buffer to dashboard
(setq initial-buffer-choice (get-buffer "*dashboard*"))

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

