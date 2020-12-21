(defsystem :et
  :depends-on (:tooter :yason :websocket-driver-client
               :bordeaux-threads)
  :components ((:module "ui"
                :components ((:file "main")
                             (:file "wizard")
                             (:file "status")))
               (:module "lisp"
                :serial t
                :depends-on ("ui")
                :components ((:file "package")
                             (:file "vars")
                             (:file "util")
                             (:file "ui-helpers")
                             (:file "ui-factory")
                             (:file "main")))))
