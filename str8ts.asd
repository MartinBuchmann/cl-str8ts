;;;; str8ts.asd

(asdf:defsystem #:str8ts
  :description "My str8ts puzzle solver"
  :author "Martin Buchmann <Martin.Buchmann@googlemail.com"
  :license "WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria #:prove #:iterate #:log4cl #:cl-annot)
  :components ((:file "package")
               (:file "str8ts")))
