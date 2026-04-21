;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON DEVELOPMENT

(use-package dape
  ;; `debugpy` must be installed in the Python environment that Dape will use.
  ;; Dape's built-in Python config runs `python -m debugpy.adapter`, so the
  ;; selected interpreter is whatever `python` resolves to in Emacs's current
  ;; environment. With direnv + envrc, that is typically the project's venv.
  ;; Projects that need a different interpreter can override Dape via
  ;; `.dir-locals.el`.
  :ensure t
  :bind (("C-c D" . dape))
  :config
  (setq dape-buffer-window-arrangement 'gud)
  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t))))

(use-package lark-mode
  :after quelpa-use-package
  :quelpa (lark-mode :fetcher url :url "https://hg.sr.ht/~zondo/lark-mode/raw/lark-mode.el")
  :mode (("\\.lark$" . lark-mode)))
