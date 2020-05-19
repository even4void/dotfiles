;;; ~/.config/doom/lisp/mu4e.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")

(after! mu4e
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-change-filenames-when-moving t
        mu4e-compose-format-flowed t
        mu4e-view-use-gnus nil    ;; not that tasty
        mu4e-headers-show-threads nil
        mu4e-headers-include-related nil
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-confirm-quit nil
        mu4e-use-fancy-chars nil  ;; too bad actually
        mu4e-compose-signature "-- chl"
        smtpmail-queue-dir "~/.mail/queue/cur"
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-fields
        '( (:date          .  25)
           (:flags         .   6)
           (:mailing-list  .  10)
           (:from          .  22)
           (:subject)))
  (remove-hook 'mu4e-compose-mode-hook #'flyspell-mode)
  (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "i icloud"
             :enter-func (lambda () (mu4e-message "Enter ch.lalanne@mac.com context"))
             :leave-func (lambda () (mu4e-message "Leave ch.lalanne@mac.com context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "ch.lalanne@mac.com")))
             :vars '((user-mail-address      . "ch.lalanne@mac.com")
                     (user-full-name         . "Christophe Lalanne")
                     (mu4e-sent-folder       . "/icloud/Sent Messages")
                     (mu4e-drafts-folder     . "/icloud/Drafts")
                     ;; ( mu4e-trash-folder      . "/icloud/Trash" )
                     (smtpmail-smtp-server   . "smtp.mail.me.com")
                     (smtpmail-stream-type   . starttls)
                     (smtpmail-smtp-service  . 587)))

           ,(make-mu4e-context
             :name "a aliquote"
             :enter-func (lambda () (mu4e-message "Enter chl@aliquote.org context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "chl@aliquote.org")))
             :vars '((user-mail-address       . "ch.lalanne@aliquote.org")
                     (user-full-name          . "Christophe Lalanne")
                     (mu4e-sent-folder        . "/aliquote/Sent")
                     (mu4e-drafts-folder      . "/aliquote/Drafts")
                     (mu4e-trash-folder       . "/aliquote/Trash")
                     (smtpmail-smtp-server    . "ssl0.ovh.net")
                     (smtpmail-smtp-service   . 587)))))
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil)

  (add-to-list 'mu4e-bookmarks
               '("size:1m..100m" "Large files (> 1Mo)" ?l))
  (add-to-list 'mu4e-bookmarks
               '("flag:attach" "Messages with attachment" ?a))
  (add-to-list 'mu4e-bookmarks
               '("maildir:/archives" "Archives" ?z))
  (add-to-list 'mu4e-bookmarks
               '("maildir:/aliquote/INBOX OR maildir:/icloud/INBOX" "All inboxes" ?i)))
