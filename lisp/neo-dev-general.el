;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(use-package docker
  :ensure t
  :pin melpa
  :diminish "d*"
  :bind ("C-c d" . docker))

(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-repository-directories
        `((,(concat (getenv "HOME") "/Documents/code") . 2)
          (,user-emacs-directory . 1)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :ensure t
  :pin melpa
  :after magit)

(use-package magit-circleci
  :ensure t
  :after magit
  :hook (magit-mode . magit-circleci-mode)
  :config
  (setq magit-circleci-token (auth-source-pick-first-password
                              :host "circleci.com")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package restclient
  :ensure t
  :commands (restclient-mode))

(use-package company-restclient
  :ensure t
  :after company
  :commands (company-restclient)
  :init
  (add-to-list 'company-backends 'company-restclient))

(use-package hcl-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :ensure t
  :after company
  :config (company-terraform-init))

(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'"  . gfm-mode)
         ("CHANGELOG\\.md'" . gfm-mode)
         ("\\.md\\'"        . markdown-mode)
         ("\\.markdown\\'"  . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)                     ; visualize matching parens

(setq-default comment-column 60)
