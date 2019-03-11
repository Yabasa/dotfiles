;;-----------------------------------------------------------
;; org-mode configuration mostly taken from Bernt Hansen
;; NOTE: Keeping it simple for now until outgrow it.
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; General Org mode setup
;;-----------------------------------------------------------
(require 'org)
(require 'org-habit)

(setq org-directory (substitute-in-file-name "$org_home/"))
(setq org-default-notes-file (substitute-in-file-name "$org_home/refile.org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Standard key bindings
(global-set-key (kbd "\C-cl") 'org-store-link)
(global-set-key (kbd "\C-cc") 'org-capture)
(global-set-key (kbd "\C-ca") 'org-agenda)
(global-set-key (kbd "\C-cb") 'org-iswitchb)

;; Custom key bindings
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f12>") 'save-some-buffers)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

;; I need to log interruptions very regularly so having a single keystroke is useful
(global-set-key (kbd "<f7>") (lambda () (interactive) (org-capture nil "i")))

;; I use my custom agenda view very often so having a single keystroke is useful
(fset 'agenda-macro "\C-x1\C-ca ")
(global-set-key (kbd "<f8>") 'agenda-macro)

;; Tasks that i regularly have to clock into during the day. It's too difficult to go find them to clock into each time so creating a key bind.
(global-set-key (kbd "<f6>") (lambda () (interactive) (bh/clock-in-task-by-id "my-3ma1l-ta5k")))
(global-set-key (kbd "<f5>") (lambda () (interactive) (bh/clock-in-task-by-id "3mac5-g3n3ral-c0nf1gurat10n")))

;; Use clean indentation for lists
(setq org-startup-indented t)

;; Use org-bullets to prettify headings
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Define the drawers needed for org mode
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK")))

;; Put notes into the LOGBOOK drawer.
(setq org-log-into-drawer t)

;; Hide blank line headings.
(setq org-cycle-separator-lines 0)

;; Don't allow editing of text that is not visible inside a fold.
(setq org-catch-invisible-edits 'error)


;;-----------------------------------------------------------
;; TODO setup
;;-----------------------------------------------------------
;; Define org todo keywords and their behaviour during state transitions
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "NEXT(n!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)"))))

;; TODO font settings
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red")
              ("NEXT" :foreground "#2d7eff")
              ("DONE" :foreground "forest green")
              ("WAITING" :foreground "orange")
              ("HOLD" :foreground "magenta")
              ("CANCELLED" :foreground "green"))))

;; Define the priority settings. Using Steven Coveys 4 quadrant approach.
(setq org-highest-priority ?1)
(setq org-lowest-priority ?4)
(setq org-default-priority ?2)

;; Priority font settings
(setq org-priority-faces
      (quote ((?1 :foreground "red")
              (?2 :foreground "orange")
              (?3 :foreground "green")
              (?4 :foreground "blue"))))

;; Define global tags for fast selection and also group them so only one can be picked at a time. Using the 4 types of work as described in The Phoenix Project.
(setq org-tag-persistent-alist '((:startgroup . nil)
				 ("BPROJ" . ?b)
				 ("IPROJ" . ?i)
				 ("PCHNG" . ?p)
				 ("UPWRK" . ?u)
				 ("ADMIN" . ?a)
				 ("TRN" . ?t)
				 (:endgroup . nil)
				 ("INTRPT" . ?r)
				 ("HABIT" . ?h)
				 ("NOTE" . ?n)
				 ("PRSNL" . ?s)
				 ("FAB1" . ?f)
				 ("MEETING" . ?m)
				 ("EOB" . ?e)))

;; Adds triggers to TODO state changes. Right now it is just adding tags.
;; Not using this right now. Need to think about how i want to use tags.
;;(setq org-todo-state-tags-triggers
;;      (quote (("CANCELLED" ("CANCELLED" . t))
;;              ("WAITING" ("WAITING" . t))
;;              ("HOLD" ("WAITING" . t) ("HOLD" . t))
;;              (done ("WAITING") ("HOLD"))
;;              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture templates for: TODO tasks, notes, meetings, issues etc...
(setq org-capture-templates
      (quote (("t" "TODO" entry (file "refile.org")
	 "* TODO [#2] %? %^G\n:PROPERTIES:\n:REQUESTER: %^{prompt|Me}\n:OWNER: %^{prompt|Me}\n:Effort: %^{prompt}\n:CREATED: %U\n:END:\n:CLOCK:\n:END:" :clock-in t :clock-resume t)
	("m" "Meeting" entry (file "refile.org")
	 "* TODO [#2] Meeting with %^{prompt} about %^{prompt} :MEETING:%^G\nSCHEDULED: %^T\n:PROPERTIES:\n:ORGANISER: %^{prompt|Me}\n:Effort: %^{prompt|1:00}\n:WEB_CONF_LINK: \n:CREATED: %U\n:END:\n:CLOCK:\n:END:\n** Attendees\n- Me\n- \n** Agenda\n- \n** Minutes\n- " :clock-in t :clock-resume t)
	("l" "Training" entry (file "refile.org")
	 "* TODO [#2] Training with %^{prompt} on %^{prompt} :MEETING:TRN:%^G\nSCHEDULED: %^T\n:PROPERTIES:\n:ORGANISER: %^{prompt|Me}\n:Effort: %^{prompt|1:00}\n:WEB_CONF_LINK: \n:CREATED: %U\n:END:\n:CLOCK:\n:END:\n** Attendees\n- Me\n- \n** Training plan\n- " :clock-in t :clock-resume t)
	("i" "Interruption" entry (file "interruptions.org")
	 "* Talked with %? :INTRUPT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n:CLOCK:\n:END:" :clock-in t :clock-resume t)
	("n" "Note" entry (file ,(substitute-in-file-name "$org_home/refile.org"))
	 "* %^{prompt} :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n:CLOCK:\n:END:" :clock-in t :clock-resume t :immediate-finish t)
	("h" "Habit" entry (file "refile.org")
	 "* TODO [#2] %? :HABIT: \nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:Effort: %^{prompt}\n:END:\n:CLOCK:\n:END:" :clock-in t :clock-resume t))))
      

;; Mark parent tasks as DONE if all children tasks are DONE
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))     
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Add a done timestamp below the heading when item is marked done
(setq org-log-done 'time)

;; Allows changing TODO states using S-left and S-right without recording timestamp or requiring note.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Whenever the ORDERED property is used for an ordered list of tasks ensure that the order of completion is followed
(setq org-enforce-todo-dependencies t)


;;-----------------------------------------------------------
;; Task properties
;;-----------------------------------------------------------
;; Standardise task properties
(setq org-global-properties '(("SEVERITY_ALL". "High Low")))

;; Add default task properites to a task without going through capture.
(defun rm/add-default-properties (pom)
  (org-entry-put pom "REQUESTER" "Me")
  (org-entry-put pom "OWNER" "Me")
  (org-entry-put pom "Effort" "")
  (org-entry-put pom "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]" (org-current-time org-clock-rounding-minutes t)))
  (org-id-get-create t))

(global-set-key (kbd "\C-cp") (lambda () (interactive) (rm/add-default-properties (point))))

;; Convert heading to a TODO using these defaults
(defun rm/convert-to-default-todo (pom)
  (org-todo "TODO")
  (org-priority-up)
  (rm/add-default-properties (point)))

(global-set-key (kbd "\C-c\S-p") (lambda () (interactive) (rm/convert-to-default-todo (point))))


;;-----------------------------------------------------------
;; Task ID's
;; Will be useful later if extracting all data into a DB.
;;-----------------------------------------------------------
;; Add a unique ID to each new task in capture mode.
(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

;; This function extracts the todo keywords from org-todo-keywords into a simple list.
(defun rm/get-org-todo-keywords ()
  (remove nil
          (mapcar (lambda (x) (when (stringp x)
                                (let ((case-fold-search nil))
                                  (when (string-match "[A-Z]+" x)
                                    (match-string 0 x)))))
                  (apply 'append org-todo-keywords))))

;; Add ID properties to all headlines that have a todo keyword in the current file which do not already have one.
(defun rm/org-add-ids-to-headlines-in-file ()
  (interactive)
  (org-map-entries 'org-id-get-create (concat "/+" (mapconcat 'identity (rm/get-org-todo-keywords) "|"))))

;; Add hook to assign task ID's to all tasks within the file before saving. This will cover cases where a heading was not added by capture.
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'rm/org-add-ids-to-headlines-in-file nil 'local)))


;;-----------------------------------------------------------
;; Org Agenda setup
;;-----------------------------------------------------------
;; Define the folders which contain Organiser org files
(setq org-agenda-files `(,org-directory))

;; Define custom Agenda views
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" ((org-agenda-span 'day)))
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags "GENERAL+LEVEL=2"
                      ((org-agenda-overriding-header "Tasks requiring final refiling")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
;                (tags-todo "-REFILE/!-WAITING-HOLD"
;                           ((org-agenda-overriding-header (concat "Project Subtasks"
;                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
;                                                                      ""
;                                                                    " (including WAITING and SCHEDULED tasks)")))
;                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
;                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;                            (org-agenda-sorting-strategy
;                             '(priority-down category-keep))))
                (tags-todo "-REFILE-MEETING/!-WAITING-HOLD"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-MEETING/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-MEETING/!-HOLD"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-MEETING/!+WAITING|+HOLD"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
               nil)))))


;; Turn on agenda time grid
(setq org-agenda-use-time-grid t)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   "--------------------"
                                   (0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800))))

;; Remove the agenda block separator lines (cygwin doesn't render correctly)
(setq org-agenda-block-separator "")

;; Format the agenda
(setq org-agenda-prefix-format "  %-22c%?-12t")
(setq org-agenda-todo-keyword-format "%-9s")

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Display tags farther right
(setq org-agenda-tags-column -110)

;; Display agenda as full window
(setq org-agenda-window-setup 'only-window)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:maxlevel 3 :scope agenda :narrow 100 :link t :indent t :level t :emphasize nil :block thisweek :fileskip0 t)))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Move the habits graph farther to the right
(setq org-habit-graph-column 150)

;; Bernt Hansens custom code for defining projects
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))


;;-----------------------------------------------------------
;; Columns
;;-----------------------------------------------------------
(setq org-columns-default-format "%TODO %40ITEM %15REQUESTER(Requester) %10OWNER(Owner) %10Effort(Time Estimate){:} %10CLOCKSUM(Time Spent) %22SCHEDULED(Scheduled) %22DEADLINE(Deadline)")


;;-----------------------------------------------------------
;; Refiling tasks
;;-----------------------------------------------------------
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Allow creating of parent nodes
(setq org-refile-allow-creating-parent-nodes (quote confirm))


;;-----------------------------------------------------------
;; Clocking setup (mostly copied from Bernt Hansen and Sacha Chua)
;;-----------------------------------------------------------
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Save clock data into the CLOCK drawer
(setq org-clock-into-drawer "CLOCK")

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Do not prompt to resume an active clock
;; not using for now... (setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
;; not using for now... (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Use discrete minute intervals instead of rounded increments (5 mins interval is default).
(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "my-d3fau1t-ta5k")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

;; This function was originally built by Bernt Hansen but it's altered so that when clocking out of a task it always clocks into the default task rather than the parent task.
(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
        (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))


;;(defun rm/clock-in-email-task ()
;;  (interactive)
;;  (org-with-point-at (org-id-find rm/email-task-id 'marker)
;;    (org-clock-in '(4))))

;; All of this is not working right now. Trying to implement Sachas code to auto clock out when state changes to WAITING.
;;(defun sacha/org-clock-out-if-waiting ()
;;  "Clock out if the task is marked WAITING."
;;  (when (and (string= state "WAITING")
;;             (not (string= last-state state)))
;;    (org-clock-out)))
;;
;;(add-hook 'org-after-todo-state-change-hook
;;          'sacha/org-clock-out-if-waiting)

;;(eval-after-load 'org
;;  '(progn
;;     (defadvice org-clock-in (after wicked activate)
;;      "Set this task's status to 'STARTED'."
;;      (org-todo "STARTED"))
;;    (defun wicked/org-clock-out-if-waiting ()
;;      "Clock out when the task is marked WAITING."
;;      (when (and (string= state "WAITING")
;;                 (equal (marker-buffer org-clock-marker) (current-buffer))
;;                 (< (point) org-clock-marker)
;;          (> (save-excursion (outline-next-heading) (point))
;;       org-clock-marker)
;;       (not (string= last-state state)))
;;(org-clock-out)))
;;    (add-hook 'org-after-todo-state-change-hook
;;      'wicked/org-clock-out-if-waiting)))


;; Update Effort estimate to be the sum of the children
;;(defun rm/sum_child_efforts
;;    
;;   )

;; Will need to add hook to update parent if child is updated and apply the trigger recursively until the root task is updated

;;-----------------------------------------------------------
;; Export options
;;-----------------------------------------------------------
;;(setq org-export-html-style-include-scripts nil
;;      org-export-html-style-include-default nil)

;;(setq org-export-html-style
;;      "<link rel=\"stylesheet\" type=\"text/css\" href=\"/html_export/default_style.css\" />")
