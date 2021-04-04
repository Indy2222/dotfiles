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
 ;; TODO get a proper font which can render these
 mu4e-use-fancy-chars nil
 auth-sources '(password-store)
 mu4e-get-mail-command "true")

(defun indy/offlineimap-args (repository username)
  (concat
   "-k Repository_"
   repository
   "-remote:remotepass="
   (funcall
    (plist-get
     (nth 0 (auth-source-search :host "gmail.com" :user username))
     :secret))))

(defun indy/offlineimap-cmd ()
  (concat
   "offlineimap -o "
   (indy/offlineimap-args "mgn" "martin.indra@mgn.cz")
   " "
   (indy/offlineimap-args "datamole" "martin.indra@datamole.cz")))

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
   `((:name "Unread" :query "maildir:/mgn* AND flag:unread AND NOT flag:trashed" :key ?i)
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
       ))
