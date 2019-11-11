;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(if (setq nokamoto-docker-use-dev nil)
    (use-package docker
      :load-path "~/.emacs.d/dev/docker.el/"
      :diminish "D*"
      :config
      (docker-global-mode)
      (use-package request
        :ensure t
        :defer t
        :commands (request)))
  (use-package docker
    :ensure t
    :pin melpa-stable
    :diminish ""
    :config (docker-global-mode)))

(use-package magit
  :ensure t
  :pin melpa-stable
  :after (ivy)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-repository-directories
        `((,(concat (getenv "HOME") "/Documents/code") . 1)
          (,user-emacs-directory . 2)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :init (global-flycheck-mode))

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
