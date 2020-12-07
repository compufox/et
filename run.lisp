(require :cmp)
(load "dependencies")

;; compiles UI files into lisp files
(dolist (f (uiop:directory-files "ui/" "*.ui"))
  (let ((name (pathname-name f)))
    (ext:system (x:cc "uic -o ui/" name ".h " (namestring f)))
    (sleep 1)
    (eql:quic (x:cc "ui/" name ".h")
              (x:cc "ui/" name ".lisp")
              (intern (string-upcase (x:cc "ui-" name)) :keyword))))

(mapcar #'delete-file (uiop:directory-files "ui/" "*.h"))

(push "./" asdf:*central-registry*)

(asdf:load-system "et")

