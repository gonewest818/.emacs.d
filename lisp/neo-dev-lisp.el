;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE DEVELOPMENT

(use-package flycheck-clj-kondo
  :ensure t
  :pin melpa-stable)

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :after (flycheck flycheck-clj-kondo)
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
  ;; Uncomment this section for packaged version
  :ensure t
  :pin melpa-stable
  ;; Uncomment for local version
  ;:load-path "~/Documents/code/cider"
  ;; Common configuration below
  :commands (cider)
  :hook (clojure-mode . cider-mode)
  :bind (("C-c M-j"    . cider-jack-in)  ; unnecessary if cider-mode hook handles?
         ("C-c M-c"    . cider-connect)  ; unnecessary if cider-mode hook handles?
         ("C-c M-s"    . cider-selector) ; unnecessary if cider-mode hook handles?
         :map cider-mode-map
         ("<return>"   . newline-and-indent)
         :map cider-repl-mode-map
         ("<return>"   . cider-repl-return)
         ("C-<return>" . cider-repl-newline-and-indent))
  :config
  (setq cider-connection-message-fn #'adafruit-wisdom-select)
  (setq cider-default-repl-command "lein")
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
;; FENNEL-LANG DEVELOPMENT

(use-package fennel-mode
  :load-path "~/.emacs.d/dev/fennel-mode"
  :mode "\\.fnl\\'"
  :config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP DEVELOPMENT

(use-package package-lint
  :ensure t
  :pin melpa-stable
  :commands (package-lint-buffer))

