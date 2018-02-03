;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION, SEARCH AND COMPLETION

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish "co"
  :hook ((cider-mode      . company-mode)
         (cider-repl-mode . company-mode))
  ;; try without (global-company-mode) to see what's lost
  :config
  (setq company-minimum-prefix-length 2)
  (bind-keys :map company-active-map
             ("<tab>"   . company-complete-common-or-cycle)
             ("S-<tab>" . company-select-previous)))

(use-package ivy
  :ensure t
  :pin melpa-stable
  :commands (ivy-mode
             ivy-completing-read
             ivy-completion-in-region
             ivy-read)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b"   . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  ;;(ivy-mode 1)
  )

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
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  ;;(projectile-global-mode 1)
  )

(use-package ibuffer-projectile
  :ensure t
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Works similarly to the ibuffer-projectile default, but we have a
  ;; few more custom filter groups we want to concatenate to the list.
  (defun neo-set-filter-groups ()
    (interactive)
    (setq ibuffer-filter-groups
          (nconc (ibuffer-projectile-generate-filter-groups)
                 '(("irc" (mode . erc-mode)))))
    (message "ibuffer: filter groups set")
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))
  (add-hook 'ibuffer-hook #'neo-set-filter-groups))
