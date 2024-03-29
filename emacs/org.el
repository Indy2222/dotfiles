(use-package org
  :ensure t
  :demand t)

(defun indy/org-clock-out (&optional switch-to-state)
  "Call org-clock-out and save."
  (interactive "P")
  (let ((buf (org-clocking-buffer)))
    (org-clock-out switch-to-state)
    (org-clock-save)
    (with-current-buffer buf (save-buffer))))

(defun indy/org-clock-in-last (&optional arg)
  "Call org-clock-in-last and save."
  (interactive "P")
  (org-clock-in-last arg)
  (org-clock-save)
  (with-current-buffer (org-clocking-buffer) (save-buffer)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-x C-o") 'indy/org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'indy/org-clock-in-last)

(setq
 org-catch-invisible-edits 'error
 org-highlight-latex-and-related '(latex)
 org-clock-mode-line-total 'current
 org-clock-persist t
 org-todo-keywords '((sequence "TODO" "|" "WONT-DO" "DONE"))

 org-directory "~/Documents/Notes/"

 indy--dtml-notes-directory (concat (file-name-as-directory org-directory) "Datamole")
 indy--dtml-clock-file (concat (file-name-as-directory indy--dtml-notes-directory) "Clock.org")
 indy--dtml-notes-file (concat (file-name-as-directory indy--dtml-notes-directory) "Notes.org")
 indy--dtml-report-file (concat (file-name-as-directory indy--dtml-notes-directory) "MonthlyReport.org")
 indy--personal-notes-directory (concat (file-name-as-directory org-directory) "Personal")
 indy--personal-notes-file (concat (file-name-as-directory indy--personal-notes-directory) "Notes.org")
 indy--de-notes-directory (concat (file-name-as-directory org-directory) "DE")
 indy--de-notes-file (concat (file-name-as-directory indy--de-notes-directory) "Notes.org")
 indy--de-clock-file (concat (file-name-as-directory indy--de-notes-directory) "Clock.org")
 indy--ddm-notes-directory (concat (file-name-as-directory org-directory) "DDM")
 indy--ddm-notes-file (concat (file-name-as-directory indy--ddm-notes-directory) "Notes.org")
 indy--ddm-clock-file (concat (file-name-as-directory indy--ddm-notes-directory) "Clock.org")

 org-capture-templates
 '(
   ("p" "Personal" entry
    (file+olp+datetree indy--personal-notes-file)
    "* TODO %^{Title}\n\n%i%?"
    :empty-lines 1
    :kill-buffer t)

   ("e" "Digital Extinction")

   ("en" "Notes" entry
    (file+olp+datetree indy--de-notes-file)
    "* TODO %^{Title}\n\n%i%?"
    :empty-lines 1
    :kill-buffer t)

   ("ec" "Clock" entry
    (file indy--de-clock-file)
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("t" "DDM")

   ("tn" "Notes" entry
    (file+olp+datetree indy--ddm-notes-file)
    "* TODO %?\n\n%i"
    :empty-lines 1
    :kill-buffer t)

   ("tc" "Clock" entry
    (file indy--ddm-clock-file)
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("d" "Datamole")

   ("dn" "Notes" entry
    (file+olp+datetree indy--dtml-notes-file)
    "* TODO %? %^g\n\n%i"
    :empty-lines 1
    :kill-buffer t)

   ("dr" "Monthly Report" item
    (file+olp+datetree indy--dtml-report-file)
    nil
    :time-prompt t
    :tree-type 'month
    :kill-buffer t)

   ("dc" "Clocking")

   ("dcd" "Datamole" entry
    (file+olp indy--dtml-clock-file "Datamole")
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("dcm" "ML Platform" entry
    (file+olp indy--dtml-clock-file "Datamole" "ML Platform")
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("dcr" "Possibler" entry
    (file+olp indy--dtml-clock-file "Datamole" "Possibler")
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("dcp" "PTO" entry
    (file+olp indy--dtml-clock-file "Lely" "Milking Setting Optimization")
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ("dcf" "MFPC" entry
    (file+olp indy--dtml-clock-file "Lely" "MilkFlowProfile")
    "* %? %^g\n"
    :clock-in t
    :clock-keep t)

   ))

(load-library "find-lisp")
(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))))


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(use-package org-clock-csv
  :ensure t)
