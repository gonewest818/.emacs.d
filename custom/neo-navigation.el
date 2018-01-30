;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION, SEARCH AND COMPLETION

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish "Co"
  :hook ((cider-mode      . company-mode)
         (cider-repl-mode . company-mode))
  ;; try without (global-company-mode) to see what's lost
  :bind (:map company-active-map
         ("<tab>"   . company-complete-common-or-cycle)
         ("S-<tab>" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 2))

(use-package ivy
  :ensure t
  :pin melpa-stable
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :pin melpa-stable
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)))

(use-package swiper
  :ensure t
  :pin melpa-stable
  :bind ("C-s" . swiper))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :diminish "prj"
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode 1))
