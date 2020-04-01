;;; ~/.config/doom/lisp/mu4e.el -*- lexical-binding: t; -*-

;; NOTE Gmail is in read-only mode (2019-05); deactivated again (2019-08)
(after! mu4e
  (setq mu4e-get-mail-command "mbsync -a"
        ; smtpmail-stream-type 'starttls
        mu4e-change-filenames-when-moving t
        ; Looks like maildirs-extension is no longer available
        ; https://github.com/hlissner/doom-emacs/commit/5b094b622030b21b43b57d7cd00b69ddd395a83b
        ; mu4e-maildirs-extension-default-collapse-level 0
        ; mu4e-maildirs-extension-maildir-expanded-prefix "»"
        ; mu4e-maildirs-extension-maildir-default-prefix "◉"
        ; mu4e-maildirs-extension-toggle-maildir-key "+"
        mu4e-compose-format-flowed t
        mu4e-headers-show-threads t
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-confirm-quit nil
        ; mu4e-completing-read-function 'completing-read
        smtpmail-queue-dir "~/.mail/queue/cur"
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-fields
        '( (:date          .  25)
           (:flags         .   6)
           (:mailing-list  .  10)
           (:from          .  22)
           (:subject)))
  (remove-hook 'mu4e-compose-mode-hook #'flyspell-mode)
  (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "C-c C-w") #'mu4e-choose-signature)))

  ;; one-shot email address

  ;; (setq user-mail-address "ch.lalanne@aliquote.org"
  ;;       user-full-name "Christophe Lalanne"
  ;;       mu4e-sent-folder "/aliquote/Sent"
  ;;       mu4e-drafts-folder "/aliquote/Drafts"
  ;;       mu4e-trash-folder "/aliquote/Trash"
  ;;       smtpmail-smtp-server "ssl0.ovh.net"
  ;;       smtpmail-smtp-service 587
  ;;       mu4e-compose-signature "chl")

  (setq mu4e-user-mail-address-list '("ch.lalanne@aliquote.org" "ch.lalanne@mac.com"))
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
                     (smtpmail-smtp-service  . 587)
                     (mu4e-compose-signature . (concat "chl\n"))))

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
                     (smtpmail-smtp-service   . 587)
                     (mu4e-compose-signature  . (concat "chl\n"))))))
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil)

  (add-to-list 'mu4e-bookmarks
               '("maildir:/aliquote/INBOX OR maildir:/icloud/INBOX" "All Inboxes" ?i))
  )
