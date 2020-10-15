;;; ~/.config/doom/lisp/mu4e.el -*- lexical-binding: t; -*-

;; NOTE See https://github.com/djcb/mu/issues/1692 for some
;; customizations.

(add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")

(after! mu4e
  (require 'org-mu4e)
  (setq mu4e-headers-show-threads nil
        mu4e-headers-include-related nil
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-display-update-status-in-modeline nil
        mu4e-index-lazy-check t
        mu4e-use-fancy-chars nil
        mail-host-address "aliquote.org"
        mu4e-compose-signature "Christophe Lalanne\n@even4void"
        message-citation-line-format "On %a, %b %d %Y (%H:%M), %N (%n) wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        mu4e-compose-signature-auto-include nil
        mu4e-compose-dont-reply-to-self t
        smtpmail-queue-dir "~/.mail/queue/cur"
        mu4e-attachment-dir "~/Downloads")

  (setq mu4e-headers-fields
        '((:date          .  18)
          (:flags         .   6)
          (:from          .  22)
          (:subject)))

  (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "i icloud"
             :enter-func (lambda () (mu4e-message "Enter mac.com context"))
             :leave-func (lambda () (mu4e-message "Leave mac.com context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "ch.lalanne@mac.com")))
             :vars '((user-mail-address      . "ch.lalanne@mac.com")
                     (user-full-name         . "Christophe Lalanne")
                     (mu4e-sent-folder       . "/icloud/Sent Messages")
                     (mu4e-drafts-folder     . "/icloud/Drafts")
                     (mu4e-trash-folder      . "/icloud/Deleted Messages")
                     (smtpmail-smtp-server   . "smtp.mail.me.com")
                     (smtpmail-stream-type   . starttls)
                     (smtpmail-smtp-service  . 587)))

           ,(make-mu4e-context
             :name "a aliquote"
             :enter-func (lambda () (mu4e-message "Enter aliquote.org context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "chl@aliquote.org")))
             :vars '((user-mail-address       . "chl@aliquote.org")
                     (user-full-name          . "Christophe Lalanne")
                     (mu4e-sent-folder        . "/aliquote/Sent")
                     (mu4e-drafts-folder      . "/aliquote/Drafts")
                     (mu4e-trash-folder       . "/aliquote/Trash")
                     (smtpmail-smtp-server    . "ssl0.ovh.net")
                     (smtpmail-smtp-service   . 587)))))
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil))

(setq mu4e-maildir-shortcuts
      '((:maildir "/aliquote/Archive" :key ?a)
        (:maildir "/icloud/Archive" :key ?i)
        (:maildir "/jussieu/Archive" :key ?j)
        (:maildir "/archives" :key ?z)
        (:maildir "/queue" :key ?q)))

(setq mu4e-bookmarks
      '((:name  "Main inboxes"
         :query "maildir:/aliquote/INBOX OR maildir:/icloud/INBOX"
         :key ?i)
        (:name  "Unread messages"
         :query "flag:unread AND NOT flag:trashed AND NOT maildir:/icloud/Deleted*"
         :key ?u)
        (:name "Today's messages"
         :query "date:today..now"
         :key ?t)
        (:name "Last 7 days"
         :query "date:7d..now"
         :key ?w)
        (:name "All drafts"
         :query "maildir:/aliquote/Drafts OR maildir:/icloud/Drafts"
         :key ?d)
        (:name  "Messages ≥ 1 Mo"
         :query "size:1m..100m"
         :hide-unread t
         :key ?l)
        (:name  "All attachments"
         :query "flag:attach"
         :hide-unread t
         :key ?a)))
