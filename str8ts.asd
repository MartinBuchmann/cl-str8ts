;;;; str8ts.asd

(asdf:defsystem #:str8ts
  :description "My str8ts puzzle solver"
  :author "Martin Buchmann <Martin.Buchmann@googlemail.com"
  :license  "Public domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:prove #:iterate)
  :components ((:file "package")
               (:file "str8ts")))
