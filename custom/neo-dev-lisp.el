;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE DEVELOPMENT

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :config
  (setq clojure-align-forms-automatically t)
  (define-clojure-indent
    ;; compojure
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package cider
  :ensure t
  :pin melpa-stable
  :commands (cider cider-connect cider-jack-in)
  :hook (clojure-mode . cider-mode)
  :bind (("<f9>"       . cider-jack-in)
         :map cider-mode-map
         ("<return>"   . newline-and-indent)
         :map cider-repl-mode-map
         ("<return>"   . cider-repl-return)
         ("C-<return>" . cider-repl-newline-and-indent))
  :config
  ;; use adafruit for connection messages
  (setq cider-connection-message-fn #'adafruit-wisdom-select)

  ;; pprint setup
  (setq cider-repl-use-pretty-printing 't)
  (setq cider-pprint-fn 'fipp))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish "par"
  :hook ((ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP DEVELOPMENT

(use-package header2
  :ensure t
  :commands (make-header))

(use-package package-lint
  :ensure t
  :pin melpa-stable
  :commands (package-lint-buffer))

