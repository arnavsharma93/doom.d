;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(disable-packages! evil-snipe)
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
(package! pandoc-mode)
(package! org-autolist)
(package! org-protocol-capture-html
  :recipe (:fetcher github
  :repo "alphapapa/org-protocol-capture-html"))
