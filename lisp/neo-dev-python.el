;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON DEVELOPMENT

(use-package dap-mode
  ;; For now use dap-mode only for python
  ;; Must also install debugpy into the venv for your project
  ;; to configure and launch
  ;;   M-x dap-debug-edit-template
  ;;   M-x dap-debug
  :ensure t
  :hook ((python-mode . (lambda ()
                          (require 'dap-python)
                          (dap-mode t)
                          ;(dap-ui-mode t)
                          )))
  :custom
  (dap-python-debugger 'debugpy))

(use-package lark-mode
  :after quelpa-use-package
  :quelpa (lark-mode :fetcher url :url "https://hg.sr.ht/~zondo/lark-mode/raw/lark-mode.el")
  :mode (("\\.lark$" . lark-mode)))
