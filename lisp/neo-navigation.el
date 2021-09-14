;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION, SEARCH AND COMPLETION

(use-package company
  :ensure t
  :diminish "co"
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (bind-keys :map company-active-map
             ("<tab>"   . company-complete-common-or-cycle)
             ("S-<tab>" . company-select-previous)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (ivy-mode t))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         :map swiper-map
         ("C-c C-t" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("C-c C-t" . swiper-isearch-toggle)))

(use-package counsel
  :ensure t
  :diminish "cn"
  :config
  (counsel-mode t))

(use-package projectile
  :ensure t
  :diminish "prj"
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-project-search-path
        (append '("~" "~/work")
                (file-expand-wildcards "~/work/[a-z0-9]*")))
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-top-down-recurring
          projectile-root-bottom-up))
  (projectile-global-mode 1))

(use-package projectile-ripgrep
  :ensure t
  :commands (projectile-ripgrep))

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
                 '(("irc"    (mode . erc-mode))
                   ("ement"  (name . "^\\*Ement Room"))
                   ("browse" (or (mode . elfeed-search-mode)
                                 (name . "^\\*elfeed-.*\\*$")
                                 (mode . eww-mode)))
                   ("slack"  (mode . slack-message-buffer-mode))
                   ("sf511"  (name . "^\\*sf511-.*\\*$")))))
    (message "ibuffer: filter groups set")
    (let ((ibuf (get-buffer "*Ibuffer*")))
      (when ibuf
        (with-current-buffer ibuf
          (pop-to-buffer ibuf)
          (ibuffer-update nil t)))))
  (add-hook 'ibuffer-hook #'neo-set-filter-groups))

(use-package avy
  :ensure t
  :bind (("M-g g" . avy-goto-line)
         ("C-:"   . avy-goto-char)
         ("C-\""  . avy-goto-char-2)))

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

;; when visiting emacs source buffers, make them read-only
(dir-locals-set-class-variables
 'emacs-sources
 '((nil . ((buffer-read-only . t)))))
(mapc (lambda (d)
        (dir-locals-set-directory-class d 'emacs-sources))
      (list "/usr/local/share/emacs"
            "/Applications/Emacs.app/Contents/Resources"))
