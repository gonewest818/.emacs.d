;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(use-package gptel
  :ensure t
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq-default
   gptel-model 'openai/gpt-oss-20b:free
   gptel-backend (gptel-make-openai "OpenRouter"
                   :host "openrouter.ai"
                   :endpoint "/api/v1/chat/completions"
                   :stream t
                   :key (lambda () (auth-source-pick-first-password
                                    :host "openrouter.ai"
                                    :user "neil.okamoto@gmail.com"))
                   :models '(openai/gpt-oss-20b:free
                             anthropic/claude-sonnet-4.5
                             anthropic/claude-3.7-sonnet))))

(use-package aidermacs
  ;; also need aider installed:
  ;;   pip install aider-install
  ;;   aider-install
  :ensure t
  :pin melpa-stable
  :bind (("C-c v" . aidermacs-transient-menu))
  :config
  (defun neo-set-openrouter-key ()
   "Set OPENROUTER_API_KEY environment variable from auth-source."
   (unless (getenv "OPENROUTER_API_KEY")
      (setenv "OPENROUTER_API_KEY"
               (auth-source-pick-first-password
                :host "openrouter.ai"
                :user "neil.okamoto@gmail.com"))))
   (advice-add 'aidermacs-transient-menu :before #'neo-set-openrouter-key)
  :custom
  (aidermacs-program "aider")
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4"))

(use-package docker
  :ensure t
  :pin melpa
  :diminish "d*"
  :bind ("C-c d" . docker)
  :config (setq docker-command "podman"
                docker-compose-command "podman-compose"))

(use-package dockerfile-mode
  :ensure t
  :config (setq dockerfile-mode-command "podman"))

(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-repository-directories
        `((,(concat (getenv "HOME") "/work") . 2)
          (,user-emacs-directory . 1)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package magit-lfs
  :ensure t
  :pin melpa
  :after magit)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :after cmake-mode
  :hook ((c++-mode . lsp)
         (cmake-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-prefer-flymake nil
        lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"
        lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.75
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package jq-mode
  :ensure t
  :mode (("\\.jq$" . jq-mode)))

(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto$" . protobuf-mode)))

(use-package hcl-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'"     . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :hook (terraform-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-)))

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

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)                     ; visualize matching parens
(electric-pair-mode 1)                  ; insert matching brackets

;; open .env files in sh-mode
(setq auto-mode-alist
      (append '(("\\.env\\'" . sh-mode)
                ("\\.env\\.tmpl\\'" . sh-mode))
              auto-mode-alist))

(setq-default comment-column 60)
(setq enable-local-eval t)              ; necessary evil, using eval forms in .dir-locals.el
