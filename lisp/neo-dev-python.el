;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON DEVELOPMENT

(use-package dap-mode
  ;; For now use dap-mode only for python
  ;; Must also install debugpy into the venv for your project
  ;; to configure and launch
  ;;   M-x dap-debug-edit-template
  ;;   M-x dap-debug
  :ensure t
  :after lsp-mode
  :hook ((python-mode . (lambda ()
                          (require 'dap-python)
                          (dap-mode t)
                          ;(dap-ui-mode t)
                          )))
  :custom
  (dap-python-debugger 'debugpy))


;; ty language server
(use-package emacs
  :after lsp-mode
  :init
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ty" "server"))
    :major-modes '(python-mode)
    :activation-fn (lsp-activate-on "python")
    :server-id 'ty-lsp-server
    :multi-root nil)))

;; (use-package lsp-pyright
;;   ;; For more information see
;;   ;;   https://github.com/microsoft/pyright
;;   ;;   https://emacs-lsp.github.io/lsp-pyright
;;   ;; Install as follows:
;;   ;;   $ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
;;   ;;                             # (restart shell)
;;   ;;   $ nvm install node        # (install the default node)
;;   ;;   $ nvm current             # (check which node version is current)
;;   ;;   $ npm install -g pyright
;;   :ensure t
;;   :if (executable-find "pyright")
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))

;; (use-package pyvenv
;;   ;; To create a venv environment
;;   ;;   $ python3 -m venv --system-site-packages myvenv
;;   ;; To use
;;   ;;   M-x pyvenv-activate
;;   :ensure t
;;   :after python
;;   :hook (python-mode . pyvenv-mode)
;;   :custom
;;   (pyvenv-default-virtual-env-name "venv")
;;   (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:"
;;                                                          pyvenv-virtual-env-name "]"))))

(use-package lark-mode
  :after quelpa-use-package
  :quelpa (lark-mode :fetcher url :url "https://hg.sr.ht/~zondo/lark-mode/raw/lark-mode.el")
  :mode (("\\.lark$" . lark-mode)))
