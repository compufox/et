(in-package :et)

(defun start ()
  ;; set some QSettings defaults
  (|setOrganizationName.QCoreApplication| "GeckerInc")
  (|setOrganizationDomain.QCoreApplication| "computerfox.xyz")
  (|setApplicationName.QCoreApplication| "ET")

  (let ((account (or (qsetting-value "default_account")
                     (add-new-account :quit t :set-default-account t))))
    (setf *tooter-client* (make-instance 'tooter:client
                                         :name "extratootrestrial"
                                         :base (qsetting-value (x:cc "acct_" account "/base-url"))
                                         :website "https://github.com/compufox/et"
                                         :access-token (qsetting-value (x:cc "acct_" account "/token"))))
    (start-websocket account "user"
                     #'(lambda (m)
                         (dispatch m "home" t)))
    (start-websocket account "public:local"
                     #'(lambda (m)
                         (dispatch m "local")))
    (start-websocket account "public"
                     #'(lambda (m)
                         (dispatch m "fedi"))))
  
  ;; initialize main window widgets
  (initialize-ui)
    
  ;; show our main application widget
  (show))

(defun dispatch (message timeline &optional handle-notifications)
  (let* ((parsed (yason:parse message))
         (payload (yason:parse (gethash "payload" parsed))))

    (cond
      ((string= (gethash "event" parsed) "update")
       (update-handler (tooter:find-status *tooter-client* (gethash "id" payload)) timeline))

      ((string= (gethash "event" parsed) "delete")
       (delete-handler payload timeline))

      ((and (string= (gethash "event" parsed) "notification")
            handle-notifications)
       (notification-handler (tooter:find-notification *tooter-client* (gethash "id" payload))))

      (t nil))))

(handler-case 
    (with-user-abort 
        (start))
  (user-abort ()))
