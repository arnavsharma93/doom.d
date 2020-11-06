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
(setq doom-theme 'doom-moonlight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/roam/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


(setq arnav/org-inbox-file "~/roam/20201105194221-inbox.org")
(setq arnav/org-gtd-file "~/roam/20201106172455-gtd.org")
(setq arnav/org-someday-file "~/roam/20201106173207-someday.org")
(setq arnav/org-tickler-file  "~/roam/20201106172535-tickler.org")
(setq org-agenda-files (list arnav/org-inbox-file arnav/org-gtd-file arnav/org-tickler-file))
(setq +org-capture-todo-file arnav/org-inbox-file)

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
  ("l" counsel-yank-pop "list" :color blue))

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
  :height 1.5)
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

(map! :map org-mode-map
      :localleader
      "i" nil
      (:prefix ("i" . "insert")
       "s" #'org-insert-subheading
       "a"      #'+org/insert-item-above
       "b"    #'+org/insert-item-below
       "i" #'org-toggle-item))

(map! :map proced-mode-map
      :localleader
      "k" #'proced-send-signal)

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
(add-hook! 'python-mode-local-vars-hook :local
  (pyenv-mode-set "venv")
  (pyvenv-activate "~/.pyenv/versions/venv")
  (setq python-indent-offset 4
        python-shell-interpreter "python3"))

(add-hook! 'python-mode-hook
  (push 'python-mypy flycheck-disabled-checkers))

(map! :leader
      (:prefix ("a" . "apps")
       "t" #'ivy-taskrunner))

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



(defun ivy-connect-bluetooth ()
  "Connect/Disconnect to paired bluetooth device"
  (interactive)
  (ivy-read "(Dis)connect"
            (seq-map (lambda ( item )
                       (let* ((device (split-string item " - "))
                              (mac (nth 0 device))
                              (name (nth 1 device)))
                         (propertize name 'mac mac)))
                     (seq-filter (lambda (line)
                                   (string-match-p "^[0-9a-f]\\{2\\}" line))
                                 (with-current-buffer (get-buffer-create "*BluetoothConnector*")
                                   (erase-buffer)
                                   (unless (eq 64 (call-process "BluetoothConnector" nil (current-buffer)))
                                     (error (buffer-string)))
                                   (split-string (buffer-string) "\n"))))
            :require-match t
            :preselect (when (boundp 'misc-bluetooth-connect--history)
                         (nth 0 misc-bluetooth-connect--history))
            :history 'misc-bluetooth-connect--history
            :caller 'toggle-bluetooth-connection
            :action (lambda (device)
                      (start-process "BluetoothConnector" (get-buffer-create "*BluetoothConnector*") "BluetoothConnector" (get-text-property 0 'mac device) "--notify"))))

(map! :leader
      "ab" #'ivy-connect-bluetooth)

(setq org-roam-directory "~/roam")
(setq deft-directory "~/roam")


(map! :localleader
      :map org-capture-mode-map
      :n "c" #'org-capture-finalize
      :n "k" #'org-capture-kill
      :n "w" #'org-capture-refile)


(after! org
  (setq org-capture-templates
        `(("t" "TODO" entry (file arnav/org-inbox-file)
           ,(concat "* TODO %?\n"
                    "/Entered on/ %u"))
          ("p" "PROJ" entry (file arnav/org-inbox-file)
           ,(concat "* PROJ %?\n"
                    "/Entered on/ %u"))
          ("f" "Future" entry
           (file+headline arnav/org-tickler-file "Tickler")
           "* %i%? \n %U")

          ("c" "org-protocol-capture" entry (file arnav/org-inbox-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)))
  (setq org-refile-targets '((arnav/org-gtd-file :maxlevel . 3)
                             (arnav/org-someday-file :level . 1)
                             (arnav/org-tickler-file :maxlevel . 2)))
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-report-include-clocking-task t)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "ACTIVE(a)"  ; A task that is in progress
           "WAIT(w@/!)"  ; Something external is holding up this task
           "HOLD(h!)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k@/!)")) ; Task was cancelled, aborted or is no longer applicable
        org-todo-keyword-faces
        '(("ACTIVE" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))

  (setq org-clock-in-switch-to-state "ACTIVE"))


(defun arnav/visit-inbox-file ()
  (interactive)
  (find-file arnav/org-inbox-file))

(defun arnav/visit-gtd-file ()
  (interactive)
  (find-file arnav/org-gtd-file))

(use-package! dired-subtree)

(use-package! dired-narrow
  :config
  (map! :map dired-mode-map
        :n "/" #'dired-narrow))

(map! :leader
      (:prefix "o"
      "A" #'org-agenda-list
       (:prefix ("g" . "goto")
        "i" #'arnav/visit-inbox-file
        "g" #'arnav/visit-gtd-file)))


(use-package! atomic-chrome
  :defer t
  :config
  (atomic-chrome-start-server))

