;; IMAP, gmail:
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "peter.brovchenko@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "peter.brovchenko"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com"
      wl-message-id-domain "smtp.gmail.com")

(setq wl-from "Петр Бровченко <peter.brovchenko@gmail.com>"
      wl-default-folder "%inbox"
      wl-draft-folder   "%[Gmail]/Черновики"
      wl-trash-folder   "%[Gmail]/Корзина"
	  wl-fcc            "%[Gmail]/Отправленные"
      wl-fcc-force-as-read    t
      wl-default-spec "%")
