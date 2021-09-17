;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS PACKAGES IN DEVELOPMENT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs orphanage packages

(use-package anzu
  :quelpa (anzu
           :fetcher github
           :repo "emacsorphanage/anzu"
           :branch "master")
  :ensure t)

(use-package dired-k
  :quelpa (dired-k
           :fetcher github
           :repo "emacsorphanage/dired-k"
           :branch "master")
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ((kbd "K") . dired-k)
             ((kbd "g") . dired-k))
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(use-package git-gutter
  :quelpa (git-gutter
           :fetcher github
           :repo "emacsorphanage/git-gutter"
           :branch "master")
  :ensure t
  :bind (("C-x v g"   . git-gutter)
         ("C-x v ="   . git-gutter:popup-hunk)
         ("C-x v s"   . git-gutter:stage-hunk)
         ("C-x v r"   . git-gutter:revert-hunk)
         ("C-x v SPC" . git-gutter:mark-hunk)
         ("C-x p"     . git-gutter:previous-hunk)
         ("C-x n"     . git-gutter:next-hunk))
  :diminish "gg"
  :init
  (setq git-gutter:unchanged-sign " ")
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
  :disabled t
  :quelpa (git-gutter-fringe
           :fetcher github
           :repo "emacsorphanage/git-gutter-fringe"
           :branch "master")
  :ensure t
  :after git-gutter)

(use-package git-messenger
  :quelpa (git-messenger
           :fetcher github
           :repo "emacsorphanage/git-messenger"
           :branch "master")
  :ensure t
  :bind (("C-x vp" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :config
  (setq git-messenger:use-magit-popup t))

(use-package smeargle
  :quelpa (smeargle
           :fetcher github
           :repo "emacsorphanage/smeargle"
           :branch "master")
  :ensure t
  :bind (("C-x vS" . smeargle)
         ("C-x vC" . smeargle-commits)
         ("C-x vZ" . smeargle-clear)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package deferred
  :ensure t)

(use-package request-deferred
  :ensure t)

;; monkey patch not needed in emacs-27 builds *newer* than 27.0.90

;; (defun tablist--patch-wisent-total-conflicts (fn &rest args)
;;   "Monkey patch `wisent-total-conflicts' to work around errors in 27.0.90.
;; Supply a fictitious `load-file-name' to satisfy the code, and
;; then call FN with ARGS.  This patch can be removed when the bug
;; is resolved: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39911"
;;   (if (wisent-source)
;;       (apply fn args)
;;     (let ((load-file-name "tablist-fictitious-file.el"))
;;       (apply fn args))))

;; (unless (version< "27" emacs-version)
;;   (advice-add 'wisent-total-conflicts
;;               :around #'tablist--patch-wisent-total-conflicts))

(use-package sf511
  :if (file-exists-p "~/.emacs.d/dev/sf511.el")
  :load-path "~/.emacs.d/dev/sf511.el"
  :bind (("C-c qs" . sf511-operators)))

(use-package mincal
  :if (file-exists-p "~/.emacs.d/dev/mincal.el")
  :load-path "~/.emacs.d/dev/mincal.el"
  :commands (mincal-display mincal-retrieve)
  :bind ("C-c qm" . mincal-display))
