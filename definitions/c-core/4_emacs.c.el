;; -*- lexical-binding: t; -*-

(et-declare
 (@def invocation-name () String)
 (@def invocation-directory () String?)
 (@def kill-emacs (&optional arg: Any restart: Bool) Never)
 (@def daemonp () String|Boolean)
 (@def daemon-initialized () True))
