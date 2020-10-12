;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Arnav Sharma"
      user-mail-address "arnavsharma93@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; s for save
(after! evil-snipe
  (evil-snipe-mode -1))

(map! :n "s" #'save-buffer)
(setq doom-localleader-key ",")


(defhydra hydra-yank-pop ()
  "yank"
  ("C-p" evil-paste-pop "prev")
  ("C-n" evil-paste-pop-next "next")
  ("p" evil-paste-after nil)
  ("P" evil-paste-before nil)
  ("C-L" counsel-yank-pop "list" :color blue))

(map!
 :n "p" #'hydra-yank-pop/evil-paste-after
 :n "P" #'hydra-yank-pop/evil-paste-before)

;; magit keymap changes
(map! :map with-editor-mode-map
      :after magit
      :localleader
      "," #'with-editor-finish
      "c" #'with-editor-finish
      "k" #'with-editor-cancel)

;; moving here and there quickly
(map! "C-'" 'evil-avy-goto-line)
(map! :leader
      "SPC" #'evil-avy-goto-char-timer)
(setq avy-all-windows t)


;; window movement here and there

(map! :leader
      "ww" #'ace-window
      "wd" #'+workspace/close-window-or-workspace
      "wD" #'ace-delete-window)

(after! ace-window
:pre-config
 (set-face-attribute
  'aw-leading-char-face nil
  :foreground "deep sky blue"
  :weight 'bold
  :height 3.0)
 (set-face-attribute
  'aw-mode-line-face nil
  :inherit 'mode-line-buffer-id
  :foreground "lawn green")
 (ace-window-display-mode t))


;; link copy and stuff
(map! :leader
      "x" nil)
(use-package! link-hint
  :defer t
  :init
  (map! :leader
        :prefix ("x")
        "o" 'link-hint-open-link
        "O" 'link-hint-open-multiple-links
        "y" 'link-hint-copy-link))

;; workspace key bindings
(map! :leader
      :after ivy
      (:when (featurep! :ui workspaces)
        (:prefix-map ("l" . "workspace")
          :desc "Display tab bar"           "d" #'+workspace/display
          :desc "Switch workspace"          "l"   #'+workspace/switch-to
          :desc "New workspace"             "c"   #'+workspace/new
          :desc "Load workspace from file"  "L"   #'+workspace/load
          :desc "Save workspace to file"    "s"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "n"   #'+workspace/switch-right
          :desc "Previous workspace"        "p"   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"   "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"   "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"   "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"   "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"   "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"   "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"   "8"   (λ! (+workspace/switch-to 7))
          (:prefix-map ("v" . "views")
           :desc "switch view" "v" #'ivy-switch-view
           :desc "push view" "a" #'ivy-push-view
           :desc "pop view" "x" #'ivy-pop-view)
          :desc "Switch to 9th workspace"   "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace"  "0"   #'+workspace/switch-to-last)))

;; project keybindings
(map! :leader
      :desc "Search project"                "/" #'+default/search-project
      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Browse project"               "p" #'+default/browse-project
        :desc "Find file in project"  "f"  #'projectile-find-file
        :desc "Switch project"               "l" #'projectile-switch-project
        :desc "Pop up scratch buffer"        "S" #'doom/open-project-scratch-buffer))

;; buffer keybindings
(map! :leader
      (:prefix-map ("b" . "buffer")
        :desc "Pop up scratch buffer"       "d"   #'kill-current-buffer
        :desc "Pop up scratch buffer"       "s"   #'doom/open-scratch-buffer)
      :desc "Toggle other buffer" "TAB" #'mode-line-other-buffer)

;; search keybindings
(map! :leader
      ;;; <leader> / --- search
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                 "s" #'swiper
       :desc "Search current directory"      "d" #'+default/search-cwd
       :desc "Search other directory"      "D" #'+default/search-other-cwd
       :desc "Jump to symbol"                "i" #'imenu
       :desc "Jump to link"                  "l" #'ace-link
       :desc "Look up online"                "o" #'+lookup/online-select
       :desc "Search project"                "p" #'+default/search-project
       :desc "Search other project" "P" #'+default/search-other-project)

      ;;; <leader> s --- snippets
      (:prefix-map ("S" . "snippets")
       :desc "New snippet"                "n" #'yas-new-snippet
       :desc "Insert snippet"             "i" #'yas-insert-snippet
       :desc "Jump to mode snippet"       "/" #'yas-visit-snippet-file
       :desc "Jump to snippet"            "s" #'+snippets/find-file
       :desc "Browse snippets"            "S" #'+snippets/browse
       :desc "Reload snippets"            "r" #'yas-reload-all
       :desc "Create temporary snippet"   "c" #'aya-create
       :desc "Use temporary snippet"      "e" #'aya-expand)

      (:prefix-map ("c" . "code")
       :desc "go to implementation" "i" #'lsp-goto-implementation))

;; git kyebindings
(map! :leader
      (:prefix-map ("g" . "git")
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "m"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit status"              "s"   #'magit-status
          :desc "Magit blame"               "b"   #'magit-blame-addition)))

;; vterm toggle and ivy resume
(map! :leader
      "'" #'+vterm/toggle
      "\"" #'+vterm/here
      "+" #'vterm)

(setq aya-persist-snippets-dir +snippets-dir)


;; python stuff
(after! python
  (pyenv-mode-set "venv")
  (pyvenv-activate "~/.pyenv/versions/venv")
  (setq python-indent-offset 4
        python-shell-interpreter "python3"))

(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'python-mypy))

(map!
 (:after python
  :localleader
  :map python-mode-map
 (:prefix ("v" . "ENV")
  "p" #'pyenv-mode-set
  "P" #'pyenv-mode-unset
  "v" #'pyvenv-activate
  "V" #'pyvenv-deactivate))
 (:after pyenv-mode
  (:map pyenv-mode-map
   "C-c C-s" nil
   "C-c C-u" nil)))

(after! company
  (setq company-idle-delay 0.05
        company-tooltip-idle-delay 0.05))

;; enable word wrapping globally
;; (+global-word-wrap-mode 1)

;; modeline things
(after! doom-modeline
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-buffer-encoding nil))

;; lsp help popup
(set-popup-rule!
  "^\\*lsp-help" :size 0.5 :vslot -4 :select nil :quit t :side 'right :autosave 'ignore)
(set-popup-rule!
  "^\\*tide-" :size 0.5 :vslot -4 :select nil :quit t :side 'right :autosave 'ignore :modeline t)
(set-popup-rule!
  "^\\*format-all" :size 0.5 :vslot -1 :select nil :quit t :side 'right :autosave 'ignore :modeline nil)
