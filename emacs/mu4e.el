(require 'mu4e)
(require 'auth-source-pass)

(setq
 ;; GMail takes care of this
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-fields '((:human-date . 12)
                       (:flags . 6)
                       (:mailing-list . 10)
                       (:from-or-to . 22)
                       (:thread-subject))
 mu4e-view-show-addresses t
 mu4e-view-scroll-to-next nil
 mu4e-attachment-dir "~/Downloads"
 shr-color-visible-luminance-min 80
 mu4e-decryption-policy 'ask
 user-full-name "Martin Indra"
 mu4e-context-policy 'pick-first
 mu4e-compose-context-policy 'ask-if-none
 mu4e-context-changed-hook (lambda ()
                             (when (string= " *mu4e-main*" (buffer-name (current-buffer)))
                                 (revert-buffer)))
 message-kill-buffer-on-exit t
 mu4e-use-fancy-chars nil
 auth-sources '(password-store)
 mu4e-get-mail-command "true"
 mu4e-update-interval 1200
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program  "firefox"
 browse-url-generic-args '("-P" "Files")
 mu4e-decryption-policy 'ask)

(add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser))

(defun indy/mu4e-compose-prep ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1)
  (visual-line-mode 1))

(add-hook 'mu4e-compose-mode-hook #'indy/mu4e-compose-prep)

(defun indy/offlineimap-args (repository host username)
  (concat
   "-k Repository_"
   repository
   "-remote:remotepass="
   (funcall
    (plist-get
     (nth 0 (auth-source-search :host host :user username))
     :secret))))

(defun indy/offlineimap-cmd ()
  (concat
   "offlineimap -o "
   (indy/offlineimap-args "mgn" "gmail.com"  "martin.indra@mgn.cz")
   " "
   (indy/offlineimap-args "datamole" "gmail.com" "martin.indra@datamole.cz")
   " "
   (indy/offlineimap-args "fjfi" "fjfi.cvut.cz" "indrama1")))

(setq
 mu4e-main-mode-hook
 (lambda ()
   (when (string= mu4e-get-mail-command "true")
     (setq mu4e-get-mail-command  (indy/offlineimap-cmd)))))

(defun enter-mu4e-context-mgn ()
  (setq
   mu4e-sent-folder       "/mgn/Sent"
   mu4e-drafts-folder     "/mgn/Drafts"
   mu4e-trash-folder      "/mgn/Trash"
   mu4e-refile-folder     "/mgn/Archive"
   user-mail-address      "martin.indra@mgn.cz"
   mu4e-compose-signature "Martin Indra\n"
   mu4e-bookmarks
   `((:name "Unread INBOX" :query "maildir:/mgn/INBOX AND flag:unread AND NOT flag:trashed" :key ?i)
     (:name "Unread other" :query "maildir:/mgn* AND NOT maildir:/mgn/INBOX AND flag:unread AND NOT flag:trashed" :key ?o)
     (:name "Last 7 days" :query "maildir:/mgn* AND date:7d..now" :hide-unread t :key ?w)))

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-smtp-user "martin.indra@mgn.cz"))

(defun enter-mu4e-context-datamole ()
  (setq
   mu4e-sent-folder       "/datamole/Sent"
   mu4e-drafts-folder     "/datamole/Drafts"
   mu4e-trash-folder      "/datamole/Trash"
   mu4e-refile-folder     "/datamole/Archive"
   user-mail-address      "martin.indra@datamole.ai"
   mu4e-compose-signature "Martin Indra\nLead Engineer\n"
   mu4e-bookmarks
   `((:name "Unread INBOX" :query "maildir:/datamole/INBOX AND flag:unread AND NOT flag:trashed" :key ?i)
     (:name "Unread other" :query "maildir:/datamole* AND NOT maildir:/datamole/INBOX AND flag:unread AND NOT flag:trashed" :key ?o)
     (:name "Last 7 days" :query "maildir:/datamole* AND date:7d..now" :hide-unread t :key ?w)))

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-smtp-user "martin.indra@datamole.cz"))

(defun enter-mu4e-context-fjfi ()
  (setq
   mu4e-sent-folder       "/fjfi/Sent"
   mu4e-drafts-folder     "/fjfi/Drafts"
   mu4e-trash-folder      "/fjfi/Trash"
   mu4e-refile-folder     "/fjfi/Archive"
   user-mail-address      "indrama1@fjfi.cvut.cz"
   mu4e-compose-signature (concat
                           "Ing. Martin Indra\n"
                           "BS Student in Mathematical Modelling, P_MIB\n"
                           "Faculty of Nuclear Sciences and Physical Engineering,\n"
                           "Czech Technical University in Prague\n"
                           "(e): indrama1@fjfi.cvut.cz\n"
                           "(m): +420 603 331 063\n"
                           "(w): https://mgn.cz/\n")
   mu4e-bookmarks
   `((:name "Unread INBOX" :query "maildir:/fjfi/INBOX AND flag:unread AND NOT flag:trashed" :key ?i)
     (:name "All INBOX" :query "maildir:/fjfi/INBOX AND NOT flag:trashed" :key ?a)
     (:name "Drafts" :query "maildir:/fjfi/Drafts" :key ?d)
     (:name "Sent" :query "maildir:/fjfi/Sent" :key ?s)))

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-smtp-server "smtp.fjfi.cvut.cz"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user "indrama1"))

(setq mu4e-contexts
      `(
        ,(make-mu4e-context
	   :name "mgn"
	   :enter-func (lambda ()
                         (progn
                           (mu4e-message "Entering mgn context")
                           (enter-mu4e-context-mgn)))
	   :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/mgn" (mu4e-message-field msg :maildir)))))
       ,(make-mu4e-context
	  :name "datamole"
	  :enter-func (lambda ()
                        (progn
                          (mu4e-message "Entering datamole context")
                          (enter-mu4e-context-datamole)))
	  :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/datamole" (mu4e-message-field msg :maildir)))))
       ,(make-mu4e-context
	  :name "FJFI"
	  :enter-func (lambda ()
                        (progn
                          (mu4e-message "Entering FJFI context")
                          (enter-mu4e-context-fjfi)))
	  :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/fjfi" (mu4e-message-field msg :maildir)))))
       ))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))
