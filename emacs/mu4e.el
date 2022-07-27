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
 mu4e-attachment-dir "~/"
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
 mu4e-get-mail-command "mbsync --all --quiet"
 mu4e-update-interval 300
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program  "firefox"
 mu4e-decryption-policy 'ask
 mu4e-headers-sort-direction 'ascending)

(add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser))

(defun indy/mu4e-compose-prep ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1)
  (visual-line-mode 1))

(add-hook 'mu4e-compose-mode-hook #'indy/mu4e-compose-prep)

(defun enter-mu4e-context-mgn ()
  (setq
   mu4e-sent-folder       "/mgn/Sent"
   mu4e-drafts-folder     "/mgn/Drafts"
   mu4e-trash-folder      "/mgn/Trash"
   mu4e-refile-folder     "/mgn/Archive"
   user-mail-address      "martin.indra@mgn.cz"
   mu4e-compose-signature "Martin Indra\n"
   mu4e-bookmarks
   `((:name "Unread Inbox" :query "maildir:/mgn/INBOX AND flag:unread AND NOT flag:trashed" :key ?i)
     (:name "Last 7 days" :query "maildir:/mgn* AND date:7d..now" :hide-unread t :key ?w)))

  (require 'smtpmail)
  (setq send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user "martin.indra@mgn.cz"))

(defun enter-mu4e-context-datamole ()
  (setq
   mu4e-sent-folder       "/dtml/Sent"
   mu4e-drafts-folder     "/dtml/Drafts"
   mu4e-trash-folder      "/dtml/Trash"
   mu4e-refile-folder     "/dtml/Archive"
   user-mail-address      "martin.indra@datamole.ai"
   mu4e-compose-signature "Martin Indra\nLead Engineer\n"
   mu4e-bookmarks
   `((:name "Unread INBOX" :query "maildir:/dtml/INBOX AND flag:unread AND NOT flag:trashed" :key ?i)
     (:name "Unread other" :query "maildir:/dtml* AND NOT maildir:/dtml/INBOX AND flag:unread AND NOT flag:trashed" :hide-unread t :key ?o)
     (:name "Last 7 days" :query "maildir:/dtml* AND date:7d..now" :hide-unread t :key ?w)))

  (require 'smtpmail)
  (setq send-mail-function 'smtpmail-send-it
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
			  (string-match-p "^/dtml" (mu4e-message-field msg :maildir)))))))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))
