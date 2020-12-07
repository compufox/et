#-eql5
(error "Please use the EQL5 executable (see README)")

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

(load "tr")

(push "./" asdf:*central-registry*)

(asdf:make-build "et"
                 :monolithic t
                 :type :static-library
                 :move-here "./"
                 :init-name "init_lib_ET__ALL_SYSTEMS")

(let ((lib-name #+msvc "et_lib.lib"
                #-msvc "libet_lib.a"))
  (when (probe-file lib-name)
    (delete-file lib-name))
  (rename-file (x:cc "et--all-systems"
                      #+msvc ".lib"
                      #-msvc ".a")
               lib-name))

(eql:qq)
