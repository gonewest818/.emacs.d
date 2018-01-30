;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERACTIVE SHELL

(use-package virtualenvwrapper
  :ensure t
  :pin melpa-stable
  :config
  ;; where virtualenv dirs are expected to be
  (setq venv-location "~/python-venv/")

  ;; make eshell aware of virtualenv
  (venv-initialize-eshell)
  (setq eshell-prompt-function
        (lambda nil
          (concat (if venv-current-name
                      (concat "(" venv-current-name ") "))
                  (eshell/pwd) " $ "))))

(use-package eshell
  :config
  ;; tell eshell about commands that use pager
  (setq eshell-visual-options '(("aws" "help")))

  ;; always have an eshell running
  (add-hook 'emacs-startup-hook
            (lambda ()
              (cd default-directory)
              (eshell))))
