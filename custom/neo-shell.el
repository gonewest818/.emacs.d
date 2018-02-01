;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERACTIVE SHELL

(use-package eshell
  :bind (("C-c M-e" . eshell))
  :config
  ;; tell eshell about commands that use pager
  (setq eshell-visual-options '(("aws" "help")
                                ("git" "--help" "--paginate")))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

  ;; python venv needed only in eshell
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
                    (eshell/pwd) " $ ")))))
