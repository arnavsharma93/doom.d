  (add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.lock\\'" . toml-mode))
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(setq doom-theme 'doom-one-light)

  (define-key (current-global-map)
    [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
    [remap shell-command] 'with-editor-shell-command)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)

 (setq magit-log-show-refname-after-summary t)

(map! :map with-editor-mode-map
      :after magit
      :localleader
      "," #'with-editor-finish
      "c" #'with-editor-finish
      "k" #'with-editor-cancel)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
(map! :map ediff-mode-map
      :n "B" 'ediff-copy-both-to-C)

(after! evil-snipe (evil-snipe-mode -1))
(map! :n "s" #'save-buffer)
(setq doom-localleader-key ",")

(map!
 :v "s" #'evil-surround-region

 (:when (featurep! :editor fold)
   :nv "zz" #'+fold/toggle))

(map! :leader
      (:prefix-map ("f" . "file")
        :desc "Delete this file"            "D"   #'doom/delete-this-file)

      (:prefix-map ("g" . "git")
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "m"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit status"              "s"   #'magit-status
          :desc "Magit blame"               "b"   #'magit-blame-addition))
      )

 (map! :leader
      "r" #'ivy-resume
      )

(map! "C-s" 'evil-avy-goto-char-timer
      "C-l" 'evil-avy-goto-line)
(after! avy
  (setq avy-all-windows t))

(map! :leader "ss" #'swiper)

(map! :leader
      "ww" #'ace-window
      "wd" #'evil-quit
      "wD" #'ace-delete-window
      )

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
 (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
       aw-dispatch-always t
       aw-dispatch-alist
       '((?d aw-delete-window "Ace - Delete Window")
         (?c aw-swap-window "Ace - Swap Window")
         (?n aw-flip-window)
         (?v aw-split-window-vert "Ace - Split Vert Window")
         (?h aw-split-window-horz "Ace - Split Horz Window")
         (?m delete-other-windows "Ace - Maximize Window")
         (?g delete-other-windows)
         (?b balance-windows)
         (?u (lambda ()
               (progn
                 (winner-undo)
                 (setq this-command 'winner-undo))))
         (?r winner-redo)))
 (ace-window-display-mode t))

(after! projectile
  :post-config
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".gen")
  (add-to-list 'projectile-globally-ignored-directories "go-build")
  (setq projectile-sort-order 'recentf)
  )

(map! :leader
      (:when (featurep! :ui workspaces)
        (:prefix-map ("l" . "workspace")
          :desc "Display tab bar"           "d" #'+workspace/display
          :desc "Switch workspace"          "l"   #'+workspace/switch-to
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load workspace from file"  "L"   #'+workspace/load
          :desc "Save workspace to file"    "s"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "]"   #'+workspace/switch-right
          :desc "Previous workspace"        "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"   "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"   "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"   "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"   "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"   "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"   "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"   "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"   "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace"  "0"   #'+workspace/switch-to-last)))

(map! :leader
      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Browse project"               "p" #'+default/browse-project
        :desc "Find file in project"  "f"  #'projectile-find-file
        :desc "Switch project"               "l" #'projectile-switch-project
        :desc "Pop up scratch buffer"        "S" #'doom/open-project-scratch-buffer
        :desc "Search project"                "/" #'+default/search-project)
      )

(map! :leader
      (:prefix-map ("b" . "buffer")
        :desc "Pop up scratch buffer"       "d"   #'kill-current-buffer
        :desc "Pop up scratch buffer"       "s"   #'doom/open-scratch-buffer)
      "TAB" #'previous-buffer
      )

(map! :leader
      ;;; <leader> / --- search
      (:prefix-map ("s" . "search")
        :desc "Search buffer"                 "s" #'swiper
        :desc "Search current directory"      "d" #'+default/search-from-cwd
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Look up in local docsets"      "k" #'+lookup/in-docsets
        :desc "Look up in all docsets"        "K" #'+lookup/in-all-docsets
        :desc "Search project"                "p" #'+default/search-project)

      ;;; <leader> s --- snippets
      (:when (featurep! :editor snippets)
        (:prefix-map ("/" . "snippets")
          :desc "New snippet"                "n" #'yas-new-snippet
          :desc "Insert snippet"             "i" #'yas-insert-snippet
          :desc "Jump to mode snippet"       "/" #'yas-visit-snippet-file
          :desc "Jump to snippet"            "s" #'+snippets/find-file
          :desc "Browse snippets"            "S" #'+snippets/browse
          :desc "Reload snippets"            "r" #'yas-reload-all
          :desc "Create temporary snippet"   "c" #'aya-create
          :desc "Use temporary snippet"      "e" #'aya-expand)))

(set-docsets! 'go-mode "go")

(def-package! tldr
:commands tldr
:defer t)

 (defun arnav/maybe-set-quit-key ()
   (when (string= (buffer-name) "*Async Shell Command*")
     (local-set-key (kbd "q") #'quit-window)))

 (add-hook 'shell-mode-hook #'arnav/maybe-set-quit-key)

(def-package! alert
:defer t
:config
  (setq alert-default-style 'osx-notifier)
)

(map!
 :map ivy-minibuffer-map "C-c o" #'ivy-occur)

(setq +latex-viewers '(pdf-tools))

(after! ivy-bibtex
  :pre-config
  (setq bibtex-completion-bibliography "~/Papers/references.bib"
        bibtex-completion-library-path '("~/Papers/pdfs/")
        bibtex-completion-notes-path "~/Papers/notes/")
  )

  (setq eshell-aliases-file "/Users/arnav/dotfiles/eshell-aliases")

(def-package! aweshell
  :commands aweshell-new
  :defer
  :after eshell
  :config
  (map! :leader
        (:prefix ("a" . "awesomesll")
          "c" #'aweshell-new
          "a" #'aweshell-switch-buffer
          "n" #'aweshell-next
          "p" #'aweshell-prev
          "S" #'aweshell-sudo-toggle
          )
        )

  )


(after! lsp
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-delay 15)
  (setq company-lsp-cache-candidates 'auto))

(map! :leader
      "X" nil)
(map! :leader
      :desc "Open mu4e" "X" #'mu4e)

(setq +mu4e-backend 'offlineimap)
(setq mu4e-maildir (expand-file-name "~/.Mail/arnav@uber.com"))
(setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")
  (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-maildir-shortcuts
      '( ("/[Gmail].Important" . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

  (setq mu4e-get-mail-command "offlineimap")

  (setq
     user-mail-address "arnav@uber.com"
     user-full-name  "Arnav Sharma"
     mu4e-compose-signature "Arnav Sharma\n")

(after! smtpmail
(setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
          '(("smtp.gmail.com" 587 "arnav@uber.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
     (setq message-kill-buffer-on-exit t))

(map! :map mu4e-headers-mode-map
      "{"  #'mu4e-headers-query-prev
      "}"  #'mu4e-headers-query-next
      ;; "o"  #'my/org-capture-mu4e

      "A"  #'mu4e-headers-mark-for-action

      "`"  #'mu4e-update-mail-and-index
      "|"  #'mu4e-view-pipe)

(def-package! flycheck-golangci-lint
  :init
  (setq flycheck-golangci-lint-config "~/.golangci.yml")
  :hook (go-mode . flycheck-golangci-lint-setup))

(def-package! go-gen-test
  :defer t
  :init
  (map! :mode go-mode
        :localleader
        (:prefix "t"
          (:prefix-map ("g" . "tests generate")
            :desc "Generate missing tests" "g" #'go-gen-test-dwim
            :desc "Generate exported tests" "e" #'go-gen-test-exported
            :desc "Generate missing tests" "a" #'go-gen-test-all))
        )

  )

(def-package! go-tag :defer t)
(def-package! godoctor :defer t)
(map! :mode go-mode
      :localleader
      (:prefix-map ("r" . "refactor")
        :desc "add tag" "t"  #'go-tag-add
        :desc "remove tag" "T" #'go-tag-remove
        :desc "add godoc" "d" #'godoctor-godoc
        :desc "extract godoc"  "e" #'godoctor-extract
        :desc "rename"  "r" #'godoctor-rename
        :desc "toggle" "t" #'godoctor-toggle))

(map! :mode go-mode
      :localleader
      (:prefix ("g" . "go to")
      "ga" #'ff-find-other-file))

(map! :leader
 :mode go-mode
 :after lsp
 :prefix "c"
   :desc "page menu" "M" #'lsp-ui-imenu
   :desc "show doc" "c" #'lsp-describe-thing-at-point
   :desc "restart lsp" "R" #'lsp-restart-workspace
   :desc "lsp rename" "r" #'lsp-rename
   :desc "find implementations" "i" #'lsp-find-implementation
   :desc "peek definition" "d" #'lsp-ui-peek-find-definitions
   :desc "peek references" "D" #'lsp-ui-peek-find-references)

(map! :localleader
      :after org
      :map org-mode-map
      "RET" #'org-ctrl-c-ret
      "," #'org-ctrl-c-ctrl-c)
(map! :localleader
      :after org
      :map org-src-mode-map
      :n "," #'org-edit-src-exit
      :n "k" #'org-edit-src-abort
      :n "s" #'org-edit-src-save)

(def-package! org-journal
:commands (org-journal-new-entry org-journal-search-forever)
;; TODO buffer not opening in journal mode
  :config
  (setq
        org-journal-dir "~/Dropbox/org/journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A, %B %d %Y"
        org-journal-time-prefix "* "
        org-journal-time-format "")
  (map! :leader
        (:prefix ("o" . "Orggg")
          (:prefix ("j". "Journal")
            :desc "new journal entry" "j" #'org-journal-new-entry
            :desc "new journal entry" "s" #'org-journal-search-forever
            )))
  (map! :map org-journal-mode-map
        :localleader
        "j" #'org-journal-new-entry
        "n" #'org-journal-next-entry
        "p" #'org-journal-previous-entry))

(def-package! org-secretary
  :config

  (defun my/org-sec-with-view (par &optional who)
    "Select tasks marked as dowith=who, where who
     defaults to the value of org-sec-with."
    (org-tags-view '(4) (join (split-string (if who
                                                who
                                              (org-sec-get-with)))
                              "|" "with=\"" "\"")))
  (defun my/org-sec-who-view (par)
    "Builds agenda for a given user.  Queried. "
    ;; (let ((who (read-string "Build todo for user/tag: "
    ;;                         "" "" "")))
    (let ((who "arnav"))
      (ivy-read "Folks:" org-sec-with-history
                :action (lambda (candidate) (setq who candidate)))
      (my/org-sec-with-view "TODO with" who)
      (org-sec-assigned-with-view "TASK with" who)
      (org-sec-stuck-with-view "STUCK with" who)))

  (defun my/wrapper-get-with (par &optional who)
    "Select tasks marked as dowith=who, where who
     defaults to the value of org-sec-with."
    (org-sec-get-with))
  )

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(p/!)" "WAIT(w@/!)" "|" "DONE(d/!)" "CANCELLED(c@/!)")
          (sequence "TASK(f)" "|" "CANCELLED(c)" "DONE(D)")))
  (setq org-log-states-order-reversed t)

  
)

(setq arnav/inbox-file "~/Dropbox/org/gtd/inbox.org")
(setq arnav/gtd-file "~/Dropbox/org/gtd/gtd.org")
(setq arnav/notes-file "~/Dropbox/org/notes.org")
(setq arnav/someday-file "~/Dropbox/org/gtd/someday.org")
(setq arnav/tickler-file "~/Dropbox/org/gtd/tickler.org")
(setq arnav/uber-calendar-file "~/Dropbox/org/calendar/arnav@uber.org")
(map! :leader
      "x" nil
      (:prefix "x"
        (:prefix ("g". "goto")
          :desc "open inbox file" "i" (λ! (find-file arnav/inbox-file) )
          :desc "open gtd file" "g" (λ! (find-file arnav/gtd-file) )
          :desc "open notes file" "n" (λ! (find-file arnav/notes-file) )
          :desc "open someday file" "s" (λ! (find-file arnav/someday-file) )
          :desc "open tickler file" "T" (λ! (find-file arnav/tickler-file) )
          )
        "x" #'org-agenda-list
        "c" #'org-capture
        "a" #'org-agenda))

(setq org-refile-targets '((arnav/gtd-file :maxlevel . 1)
                           (arnav/someday-file :level . 1)
                           (arnav/tickler-file :maxlevel . 2)))
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-files (list arnav/inbox-file
                         arnav/gtd-file
                         arnav/uber-calendar-file
                         arnav/tickler-file))

(after! org
        (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                       (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
                                       (file "~/Dropbox/org/templates/todo.orgcaptmpl")
                                       :empty-lines 1)
                                      ("f" "Task [inbox]" entry
                                       (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
                                       (file "~/Dropbox/org/templates/task.orgcaptmpl")
                                       :empty-lines 1)
                                      ("a" "Appointment" entry
                                       (file  "~/Dropbox/org/calendar/arnav@uber.org" )
                                       "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
                                      ("T" "Tickler" entry
                                       (file+headline "~/Dropbox/org/gtd/tickler.org" "Tickler")
                                       "* %i%? \n %U" :empty-lines 1)
                                      ("i" "Interview"
                                       entry
                                       (file "~/Dropbox/org/interviews.org")
                                       (file "~/Dropbox/org/templates/interview.orgcaptmpl"))
                                      ("n" "Note" entry
                                       (file+headline "~/Dropbox/org/notes.org" "Notes")
                                       "* %i%? %^g\nLogged on %U" :empty-lines 1)
                                      ))
        (map! :map org-capture-mode-map
              :localleader
              "," #'org-capture-finalize
              "k" #'org-capture-kill))

(after! org
  ;; Hydra for org agenda (graciously taken from Spacemacs)
  (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil)
                                   :hint none)
    "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   link-hint-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo))

  (map! :map org-agenda-mode-map
        :localleader
        "." #'hydra-org-agenda/body))

(def-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)

  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Important"
                 :priority>= "B"
                 ;; Show this section after "Today" and "Important", because
                 ;; their order is unspecified, defaulting to 0. Sections
                 ;; are displayed lowest-number-first.
                 :order 0)
          (:name "Today"  ; Optionally specify section name
                 :todo ("INPROGRESS" "TODO")
                 :order 1)  ; Items that have this TODO keyword
          ;; Set order of multiple groups at once
          (:order-multi (2 (:name "Waiting"
                                  :todo "WAIT"
                                  )
                           (:name "Done"
                                  :todo ("DONE"))))
          (:auto-property "with")
          (:name "Lead tasks"
                 ;; Single arguments given alone
                 :todo "TASK"
                 )
          (:discard
           (:regexp ("gym" "deployment" "yoga" "office hours")))
          (:name "Calendar"
                 :time-grid t
                 :order 5
                 )
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          ))
  :config
  (org-super-agenda-mode)
  )

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("h" "Work todos" tags-todo
           "-personal-doat={.+}-dowith={.+}/!-TASK"
           ((org-agenda-todo-ignore-scheduled t)))
          ("u" "Unscheduled TODO"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))) nil)
          ("H" "All work todos" tags-todo "-personal/!-TASK"
           ((org-agenda-todo-ignore-scheduled nil)))
          ("A" "Work todos with doat or dowith" tags-todo
           "-personal+doat={.+}|dowith={.+}/!-TASK"
           ((org-agenda-todo-ignore-scheduled nil)))
          ("j" "Interactive TODO dowith and TASK with"
           ((my/org-sec-who-view "TODO dowith")))
          )))

(after! org-gcal

  (setq org-gcal-client-id "609584643994-unjps7piimpal1v8fq14n61ru410vc7f.apps.googleusercontent.com"
        org-gcal-client-secret "djhZ6XBKwe67H8syu9Q24gEU"
        org-gcal-file-alist '(("arnav@uber.com" .  "/Users/arnav/Dropbox/org/calendar/arnav@uber.org")))
  (setq org-gcal-auto-archive t)
  (setq org-gcal-down-days 30)
  (setq org-gcal-up-days 30)
  (run-with-timer 0 (* 10 60) 'org-gcal-sync)
  )

(after! org
   ;; Resume clocking task when emacs is restarted
   (org-clock-persistence-insinuate)
   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
   (setq org-clock-persist t)
   ;; Resume clocking task on clock-in if the clock is open
   (setq org-clock-in-resume t)
   ;; Do not prompt to resume an active clock, just resume it
   (setq org-clock-persist-query-resume nil)

   ;; Change tasks to whatever when clocking out
   (setq org-clock-out-switch-to-state "DONE")
   ;; Change tasks to whatever when clocking in
   (setq org-clock-in-switch-to-state "INPROGRESS")
   ;; Save clock data and state changes and notes in the LOGBOOK drawer
   (setq org-clock-into-drawer t)
   ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
   ;; with 0:00 duration
   (setq org-clock-out-remove-zero-time-clocks t)
   ;; Clock out when moving task to a done state
   (setq org-clock-out-when-done t)
   ;; Enable auto clock resolution for finding open clocks
   ;; commenting out as I don't know what this does
   ;; (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
   ;; Include current clocking task in clock reports
   (setq org-clock-report-include-clocking-task t)
   ;; use pretty things for the clocktable
   (setq org-pretty-entities t)
  )

(after! org
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)
     (go . t)
     (plantuml . t)
     (latex . t)))
  (setq org-plantuml-jar-path
        (expand-file-name "/usr/local/Cellar/plantuml/1.2019.5/libexec/plantuml.jar"))


  )

  (alert "Successfully loaded: booyeah" :title "Doomed")
