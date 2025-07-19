;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "guix" "20250525.1711"
  "Interface for GNU Guix."
  '((emacs         "24.3")
    (dash          "2.11.0")
    (geiser        "0.8")
    (bui           "1.2.0")
    (transient     "0.3.0")
    (edit-indirect "0.1.4"))
  :url "https://emacs-guix.gitlab.io/website/"
  :commit "66b935020d93cdbbff0b0ed3b1d2195724a46821"
  :revdesc "66b935020d93"
  :keywords '("tools")
  :authors '(("Alex Kost" . "alezost@gmail.com"))
  :maintainers '(("Alex Kost" . "alezost@gmail.com")))
