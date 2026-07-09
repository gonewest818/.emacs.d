;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(defun neo/get-openrouter-api-key ()
  "Retrieve API key for openrouter.ai from auth-source."
  (auth-source-pick-first-password :host "openrouter.ai"
                                   :user "neil.okamoto@gmail.com"))

(use-package gptel
  :ensure t
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (gptel-make-openai-oauth "ChatGPT")
  (setq-default
   gptel-model 'gpt-5.4-mini
   gptel-backend (gptel-make-openai-oauth "ChatGPT"))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (lambda () (neo/get-openrouter-api-key))
    :models '(deepseek/deepseek-v4-flash
              deepseek/deepseek-v4-pro
              anthropic/claude-sonnet-5
              anthropic/claude-fable-5
              google/gemini-3.5-flash)))

(use-package agent-shell
    :ensure t
    :pin melpa
    :init
    (setq shell-maker-root-path
          (no-littering-expand-var-file-name "shell-maker/"))
    :bind (("C-c b" . agent-shell))
    ;; :ensure-system-package
    ;; ((opencode . "brew install anomalyco/tap/opencode")
    ;;  (tokenscope . "npm install -g @ramtinj95/opencode-tokenscope"))
    :config
    (setq agent-shell-preferred-agent-config 'opencode
          ;; select models after connecting with c-c c-v
          ;; ollama models configured in ~/.config/opencode/opencode.json
          ;; ... or pick one explicitly
          ;; agent-shell-opencode-default-model-id "openrouter/anthropic/claude-sonnet-4.6"
          agent-shell-opencode-default-session-mode-id "plan"))

(use-package agent-shell-tramp
  :ensure t
  :after (quelpa-use-package tramp)
  :quelpa (agent-shell-tramp :fetcher github :repo "junyi-hou/agent-shell-tramp")
  :commands  (agent-shell-tramp-mode))

(with-eval-after-load 'tramp
  ;; Else tramp uses a stripped-down path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (defun neo/disable-company-on-tramp ()
    "Disable Company mode if buffer or shell is hosted over TRAMP."
    (when (and (fboundp 'tramp-tramp-file-p)
               (or (tramp-tramp-file-p (or buffer-file-name ""))
                   (tramp-tramp-file-p default-directory)))
      (company-mode -1)))

  ;; Disables Company in remote shells/comint buffers
  (add-hook 'comint-mode-hook #'neo/disable-company-on-tramp)
  (add-hook 'shell-mode-hook #'neo/disable-company-on-tramp))

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
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-repository-directories
        `((,(concat (getenv "HOME") "/work") . 2)
          (,user-emacs-directory . 1)))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :ensure t
  :after magit)

(use-package magit-lfs
  :ensure t
  :after magit)

(use-package magit-delta
  ;; magit-delta requires "delta" from homebrew
  ;; and the following in your `.gitconfig':
  ;; [core]
  ;; 	pager = delta
  ;; [delta]
  ;; 	syntax-theme = zenburn
  ;; 	dark = true
  ;; 	plus-style = syntax "#5a6f5a"
  ;; 	minus-style = syntax "#6f5a5a"
  ;; 	plus-emph-style = syntax "#296529"
  ;; 	minus-emph-style = syntax "#652929"
  :ensure t
  :after magit
  :hook ((magit-mode . (lambda () (magit-delta-mode +1))))
  :config
  (setq magit-delta-default-dark-theme "zenburn"
        magit-delta-hide-plus-minus-markers nil))

(use-package envrc
  :ensure t
  :hook ((after-init . envrc-global-mode))
  :config (setq envrc-remote 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)      ; ty
         (cmake-mode . eglot-ensure)       ; cmake-language-server
         (markdown-mode . eglot-ensure)    ; marksman
         (terraform-mode . eglot-ensure)   ; terraform-ls
         (sh-mode . eglot-ensure)          ; bash-language-server
         (bash-ts-mode . eglot-ensure)))   ; bash-language-server

;; Language server registrations are centralized here so eglot setup stays auditable.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ty" "server")))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("marksman" "server")))
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-show-with-cursor t
        eldoc-box-show-with-mouse t))

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
         ("\\.tfvars\\'" . terraform-mode)))

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
