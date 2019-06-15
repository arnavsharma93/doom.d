;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! tldr)
(package! alert)
(package! flycheck-golangci-lint)
(package! go-gen-test)
(package! go-tag)
(package! godoctor)
(package! org-journal)
(package! org-super-agenda)
(package! aweshell
  :recipe (:fetcher github
  :repo "manateelazycat/aweshell"))
(package! mu4e-alert)
(package! link-hint)
(package! org-cliplink)
(package! pandoc-mode)
(package! org-autolist)
(package! org-protocol-capture-html
  :recipe (:fetcher github
                    :repo "alphapapa/org-protocol-capture-html"))
(package! deadgrep)
(package! flycheck-posframe)
;; dired
(package! ivy-dired-history)
(package! dired-filter)
(package! dired-subtree)
(package! dired-narrow)
(package! dired-sidebar)
(package! diredfl)
(package! company-box)
