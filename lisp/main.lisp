(in-package :et)

(defun start ()
  ;; set some QSettings defaults
  (|setOrganizationName.QCoreApplication| "GeckerInc")
  (|setOrganizationDomain.QCoreApplication| "computerfox.xyz")
  (|setApplicationName.QCoreApplication| "ET")

  (let ((account (or (qsetting-value "default_account")
                     (add-new-account :should-quit t :set-default-account t))))
    (setf *tooter-client* (make-instance 'tooter:client
                                         :name "extratootrestrial"
                                         :base (qsetting-value (x:cc "acct_" account "/base-url"))
                                         :website "https://github.com/compufox/et"
                                         :access-token (qsetting-value (x:cc "acct_" account "/token"))))
    
    ;; initialize main window widgets
    (initialize-ui)

    ;; start our websockets for each standard timeline
    (start-websocket account "user"
                     #'(lambda (m)
                         (qrun*
                           (dispatch m "home" t))))
    (start-websocket account "public:local"
                     #'(lambda (m)
                         (qrun*
                           (dispatch m "local"))))
    (start-websocket account "public"
                     #'(lambda (m)
                         (qrun* 
                           (dispatch m "fedi")))))
    
  ;; show our main application widget
  (show))

(defun app-close (_)
  (declare (ignore _))
  (save-application-state)
  (mapcar #'wsd:close-connection *websockets*))

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
