;;;; str8ts.asd

(asdf:defsystem #:str8ts
  :description "My str8ts puzzle solver"
  :author "Martin Buchmann <Martin.Buchmann@googlemail.com"
  :license "WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/"
  :version "1.0"
  :serial t
  :depends-on (#:alexandria #:iterate #:log4cl #:cl-annot)
  :components ((:file "package")
               (:file "str8ts"))
  :in-order-to ((test-op (test-op str8ts/test))))

;; Testing ASDF system
(asdf:defsystem #:str8ts/test
  :depends-on (:str8ts
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
