;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL DEVELOPMENT

(defun neo/get-openrouter-api-key ()
  "Retrieve API key for openrouter.ai from auth-source."
  (auth-source-pick-first-password :host "openrouter.ai"
                                   :user "neil.okamoto@gmail.com"))

(defvar neo/ollama-api-base
  (let ((api-base (getenv "OLLAMA_API_BASE")))
    (or api-base "http://localhost:11434"))
  "Base URL for our ollama server, i.e. http://localhost:11434")

(defvar neo/ollama-host
  (replace-regexp-in-string "^https?://" "" neo/ollama-api-base)
  "Host and port for the ollama server, i.e. localhost:11434")

(use-package gptel
  :ensure t
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (gptel-make-ollama "Ollama"
    :host neo/ollama-host
    :stream t
    :models '("qwen2.5-coder:7b" "qwen3-coder-30b-128kctx"))
  (setq-default
   gptel-model 'anthropic/claude-sonnet-4.6
   gptel-backend (gptel-make-openai "OpenRouter"
                   :host "openrouter.ai"
                   :endpoint "/api/v1/chat/completions"
                   :stream t
                   :key (lambda () (neo/get-openrouter-api-key))
                   :models '(anthropic/claude-sonnet-4.6
                             google/gemini-3-flash-preview))))

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
          agent-shell-opencode-default-session-mode-id "plan")
    (defun neo/agent-shell--set-opencode-env (&rest _)
      "Set OpenCode environment with OPENROUTER_API_KEY lazily."
      (setq agent-shell-opencode-environment
            (agent-shell-make-environment-variables
             "OPENROUTER_API_KEY" (neo/get-openrouter-api-key))))
    (advice-add 'agent-shell :before #'neo/agent-shell--set-opencode-env))

(use-package aidermacs
  ;; need aider installed separately:
  ;;    pip install aider-install
  ;;    aider-install
  ;; or
  ;;    uv tool install aider-chat --python 3.12
  ;;    uv tool install aider-skills --python 3.12
  ;;
  ;; export OLLAMA_API_BASE=http://host:11434
  ;;
  ;; need a `~/.aider.conf.yml`:
  ;;    model: openrouter/anthropic/claude-sonnet-4
  ;;    editor-model: ollama/qwen2.5-coder:7b
  ;;    weak-model: ollama/qwen2.5-coder:7b
  ;;    edit-format: diff
  ;;    auto-commits: false
  :ensure t
  :pin melpa-stable
  :bind (("C-c v" . aidermacs-transient-menu))
  :config
  (defun neo/aidermacs--set-openrouter-key ()
    "Set OPENROUTER_API_KEY environment variable from auth-source."
    (let ((api-key (neo/get-openrouter-api-key)))
      (when api-key
        (setenv "OPENROUTER_API_KEY" api-key))))
  (advice-add 'aidermacs-transient-menu :before #'neo/aidermacs--set-openrouter-key)
  (setq aidermacs-show-diff-after-change nil)
  :custom
  (aidermacs-program "aider"))

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

(use-package envrc
  :ensure t
  :hook ((after-init . envrc-global-mode)))

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
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
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
