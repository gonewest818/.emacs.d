;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(use-package docker
  :ensure t
  :pin melpa
  :diminish "d*"
  :bind ("C-c d" . docker))

(use-package magit
  :ensure t
  :pin melpa-stable
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
