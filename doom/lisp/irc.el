;;; ~/.config/doom/lisp/irc.el -*- lexical-binding: t; -*-

(load! "private.el")

(setq circe-default-nick "even4void")

;; I don't really use this and just launch Circe, but in case I would need it
;; NOTE that I should update the way I retrieve my password, e.g.
;; https://github.com/jorgenschaefer/circe/wiki/Configuration
;;
;; To connect to discord via bitlbee:
;; First time:
;; > account add discord <email> <password>
;; > account discord on
;; > account discord off
;; > chat list discord
;; > chat add discord !4 #borabora
;; > /join #borabora
;;
;; See help for more information on available commands
;;
;; Also, to start a private conversation, just use
;; > /msg <nick> message
;;
;; More help:
;; https://help.ubuntu.com/community/Bitlbee

(after! circe
  (set-irc-server! "chat.freenode.net" `(:tls t
                                         :port 6697
                                         :nick "even4void"
                                         :sasl-username "chl"
                                         :sasl-password "YEhmw?hxV6JD"
                                         :nickserv-password ,freenode-password
                                         :channels ("#lisp" "#scheme" :after-auth "#racket" "##borabora"))))
