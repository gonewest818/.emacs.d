;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS

(use-package better-defaults
  :ensure t)

(use-package quelpa
  :ensure t
  :init (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

(use-package quelpa-use-package
  :ensure t
  :after quelpa)

(set-language-environment "UTF-8")

;; allow erase-buffer without warning
(put 'erase-buffer 'disabled nil)

;; Print settings
;; When changing the font size you also need to adjust the pagination
;; as follows:
;;   if "lpr-headers-switches" is non-nil, the flags are passed directly to lpr
;;   if "lpr-headers-switches" is nil, then pagination is done by pr
;;   you can pass flags to pr by setting "lpr-page-header-switches"
(setq lpr-switches '("-o" "cpi=14"          ; characters per inch
                     "-o" "lpi=8"           ; lines per inch
                     "-o" "page-top=32"     ; margins in points (1/72 inch)
                     "-o" "page-bottom=32"
                     "-o" "page-left=32"
                     "-o" "page-right=32"))
(setq lpr-page-header-switches '("-l" "80")) ; lines per page

;; Dired configuration
;; See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)
