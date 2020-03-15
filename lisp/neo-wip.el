;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

(use-package quelpa
  :ensure t
  :init (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

(use-package quelpa-use-package
  :ensure t
  :after quelpa)

(use-package bufler
  :ensure t
  :demand t
  :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el")
  ;;:quelpa (bufler :fetcher github :repo "gonewest818/bufler.el" :branch "projectile")
  :init
  (bind-key "C-x C-b" #'bufler)
  (bind-key "C-x b" #'bufler-workspace-switch-buffer)
  ;; (bufler-mode t)
  )

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

(defun tablist--patch-wisent-total-conflicts (fn &rest args)
  "Monkey patch `wisent-total-conflicts' to work around errors in 27.0.90.
Supply a fictitious `load-file-name' to satisfy the code, and
then call FN with ARGS.  This patch can be removed when the bug
is resolved: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39911"
  (if (wisent-source)
      (apply fn args)
    (let ((load-file-name "tablist-fictitious-file.el"))
      (apply fn args))))

(unless (version< "27" emacs-version)
  (advice-add 'wisent-total-conflicts
              :around #'tablist--patch-wisent-total-conflicts))

(use-package sf511
  :load-path "~/.emacs.d/dev/sf511.el"
  :bind (("C-c qs" . sf511-operators)))

(use-package mincal
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))

