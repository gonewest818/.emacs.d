;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EARLY INITIALIZATION

;; Package system setup (before GUI loads)
(setq package-enable-at-startup nil)

;; Performance optimizations
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; UI optimizations (prevent flash)
(setq frame-inhibit-implied-resize t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Configure native compilation cache directory
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (load (expand-file-name "etc/native-comp-exclusions.el" user-emacs-directory) nil t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation-deny-list
        (append (if (boundp 'neo-native-comp-exclusion-regexps)
                    neo-native-comp-exclusion-regexps
                  nil)
                (if (boundp 'native-comp-jit-compilation-deny-list)
                    native-comp-jit-compilation-deny-list
                  nil)))
  (startup-redirect-eln-cache
   (convert-standard-filename
     (expand-file-name "var/eln-cache/" user-emacs-directory))))
