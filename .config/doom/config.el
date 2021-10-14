;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Ben Pearson")
(setq user-mail-address "blpearson44@icloud.com")
(setq doom-font (font-spec :family "Fira Code" :size 18))
(setq doom-theme 'doom-one)
(setq org-directory "~/Org/")
(setq display-line-numbers-type t)

;; Projectile
(setq projectile-project-search-path '("~/Projects/"))
(setq projectile-auto-discover t)

;; Treemacs
(setq treemacs-set-width 20)

;; Org
;; Change heading fonts
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Org-bullets
(use-package! org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; LaTeX
(use-package! org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        "~/node_modules/mathjax-node-cli/bin/tex2svg"))
(setq org-highlight-latex-and-related '(latex script entities))

;; Custom mappings
(general-evil-setup)

;; Treemacs
(add-hook 'treemacs-mode-hook
          (lambda ()
            (define-key treemacs-mode-map "cw" 'treemacs-rename)
            (define-key treemacs-mode-map "o" 'treemacs-create-file)))
