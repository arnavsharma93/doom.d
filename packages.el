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
