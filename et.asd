(defsystem :et
  :serial t
  :depends-on (:tooter :yason :websocket-driver-client :with-user-abort)
  :components ((:module "ui" :serial t
                :components ((:file "main")
                             (:file "wizard")
                             (:file "status")))
               (:file "package")
               (:module "lisp" :serial t
                :components ((:file "vars")
                             (:file "util")
                             (:file "ui-helpers")
                             (:file "ui-factory")
                             (:file "main")))))
