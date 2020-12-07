(in-package :et)

(defun start ()
  ;; set some QSettings defaults
  (|setOrganizationName.QCoreApplication| "GeckerInc")
  (|setOrganizationDomain.QCoreApplication| "computerfox.xyz")
  (|setApplicationName.QCoreApplication| "ET")

  (let ((account (or (qsetting-value "default_account")
                     (add-new-account :should-quit t :set-default-account t))))
    
    ;; initialize backend
    (initialize-client account)
    
    ;; initialize main window widgets
    (initialize-ui))
    
  ;; show our main application widget
  (show))

(defun app-close (_)
  "handle the close event"
  (declare (ignore _))
  (save-application-state)
  (qdel *account-action-group*)
  (close-sockets))

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

(start)
