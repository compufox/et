(in-package :et)

(declaim (inline set-char-count initialize-ui))

(defun initialize-ui ()
  "initializes main window before being shown"
  (ini)
  (connect-signals)

  (|setVisible| *lyt-reply* nil)
  (|setVisible| *edt-compose-cw* nil)

  ;; placeholders set here because of bug in eql5 -quic
  (qset *edt-compose-cw* "placeholderText" "Content Warning...")
  (qset *txt-compose-content* "placeholderText" "Whats up?")
  
  (unless (restore-application-state)
    (|tabifyDockWidget| *main-window* *dock-notifs* *dock-local*)
    (|tabifyDockWidget| *main-window* *dock-notifs* *dock-fedi*))
  
  (|raise| *dock-notifs*)

  (setf *max-post-char* (instance-max-chars (tooter:base *tooter-client*)))
  (set-char-count *max-post-char*))

(defun connect-signals ()
  "connect signals and install overrides"
  (qconnect *actionquit* "triggered()" *main-window* "close()")
  (qconnect *txt-compose-content* "textChanged()" 'update-char-count)
  (qconnect *edt-compose-cw* "textChanged(QString)" 'update-char-count)
  (qconnect *chk-compose-cw* "toggled(bool)" *edt-compose-cw* "setVisible(bool)")
  (qconnect *chk-compose-cw* "toggled(bool)" 'update-char-count)
  (qconnect *btn-compose-post* "clicked()" 'send-post)
  (qconnect *btn-reply-clear* "clicked()" 'clear-reply)
  (qoverride *main-window* "closeEvent(QCloseEvent*)" 'app-close))

(defun save-application-state ()
  "saves the current application dock state and geometry"
  (let ((state (qvariant-from-value (|saveState| *main-window*) "QByteArray"))
        (geom (qvariant-from-value (|saveGeometry| *main-window*) "QByteArray")))
    (setf (qsetting-value "window/state") state
          (qsetting-value "window/geometry") geom)))

(defun restore-application-state ()
  "restores the application window dock state and geometry"
  (let ((state (qsetting-value "window/state"))
        (geom (qsetting-value "window/geometry")))
    (when state (|restoreState| *main-window* state))
    (when geom (|restoreGeometry| *main-window* geom))))

(defun update-char-count (&optional _)
  "handler to update the character count"
  (declare (ignore _))
  (let* ((status-text (qget *txt-compose-content* "plainText"))
         (cw-text (qget *edt-compose-cw* "text"))
         (count (- *max-post-char* (+ (length status-text)
                                      (if (qget *chk-compose-cw* "checked")
                                          (length cw-text)
                                          0)))))
    
    (set-char-count count)

    ;; enable/disable post button
    (if (< count 0)
        (qset *btn-compose-post* "enabled" nil)
        (when (and (>= count 0)
                 (not (qget *btn-compose-post* "enabled")))
            (qset *btn-compose-post* "enabled" t)))))

(defun set-char-count (text)
  "set the character count to TEXT"
  (qset *lbl-compose-count* "text" (princ-to-string text)))

(defun prepare-reply (to)
  "displays reply layout and sets values for it"
  (let ((acct (tooter:account to)))
    (setf *reply-id* (tooter:id to))
    (qset *lbl-reply-acct* "text" (tooter:username acct))
    (qlet ((p "QPixmap(QString)" (account-avatar-path acct)))
      (qfun p "scaled" 64 64)
      (qset *lbl-reply-avatar* "pixmap" p))
    (qset *lbl-reply-content* "text" (tooter:content to))

    ;; maybe set this under a preference option?
    (qset *edt-compose-cw* "text" (tooter:spoiler-text to))
    
    (|show| *lyt-reply*)))

(defun clear-reply ()
  "clears the reply ID and hides the reply layout"
  (setf *reply-id* nil)
  (|setVisible| *lyt-reply* nil))

(defun send-post ()
  ;; disable our post button until we send the status
  (qset *btn-compose-post* "enabled" nil)
  (let ((content (qget *txt-compose-content* "plainText"))
        (visibility (to-keyword (qget *cmb-compose-privacy* "currentText")))
        (hide-media (qget *chk-compose-hide* "checked"))
        (cw (qget *edt-compose-cw* "text")))

    ;; need to catch for errors here
    (tooter:make-status *tooter-client*
                        content
                        :visibility visibility
                        :sensitive hide-media
                        :spoiler-text (when (qget *chk-compose-cw* "checked") cw)
                        :in-reply-to *reply-id*)

    ;; set stuff back to defaults
    (qset *txt-compose-content* "plainText" "")
    (qset *edt-compose-cw* "text" "")
    (qset *chk-compose-cw* "checked" nil)
    (qset *chk-compose-hide* "checked" nil)
    (qset *btn-compose-post* "enabled" t)

    ;; TODO
    ;; this should take into account current account
    ;; default
    (qset *cmb-compose-privacy* "currentIndex" 0)
    
    (clear-reply)))

(defun add-new-account (&key should-quit set-default-account)
  (qrequire :webengine)
  (ui-wizard:ini)

  (qoverride ui-wizard:*page-2* "validatePage()" 'validate-instance-url)
  (qoverride ui-wizard:*page-3* "validatePage()" 'validate-access-token)
  
  (when (and (zerop (|exec| ui-wizard:*wiz-new-account*))
             should-quit)
    (eql:qq))

  (let ((id (tooter:id (tooter:account *tooter-client*))))
    (qlet ((token "QVariant(QString)" (tooter:access-token *tooter-client*))
           (streaming-url "QVariant(QString)" (streaming-url))
           (vid "QVariant(QString)" id)
           (base-url "QVariant(QString)" (tooter:base *tooter-client*)))
      (setf (qsetting-value (x:cc "acct_" id "/token")) token
            (qsetting-value (x:cc "acct_" id "/streaming-url")) streaming-url
            (qsetting-value (x:cc "acct_" id "/base-url")) base-url)

      (when set-default-account
        (setf (qsetting-value "default_account") vid)))
    
    id))

(defun update-handler (post timeline)
    (let ((status (generate-status-widget post))
          (item (qnew "QListWidgetItem"))
          (tl (cond
                ((string= timeline "home") *tl-home*)
                ((string= timeline "local") *tl-local*)
                ((string= timeline "fedi") *tl-fedi*))))
      (qfun status "show")
      (qfun item "setSizeHint" (qfun status "sizeHint"))
      (qfun tl "insertItem" 0 item)
      (qfun tl "setItemWidget" item status)))

(defun delete-handler (id timeline)
  (let* ((tl (cond
              ((string= timeline "home") *tl-home*)
              ((string= timeline "local") *tl-local*)
              ((string= timeline "fedi") *tl-fedi*)))
         (count (qget tl "count")))
    
    ;; iterate over model to find widget with objectName matching ID
    ;; then remove it
    (loop for i upto count
          for item = (qfun tl "item" i)

          when (qfind-child item id) do
            (qfun tl "removeItemWidget" item))))

(defun notification-handler (post)
  #|
    (let ((status (generate-status-widget post))
          (item (qnew "QListWidgetItem")))
      ;(qset item "sizeHint" (qget status "sizeHint"))
      (qfun *tl-notif* "addItem" item)
      (qfun *tl-notif* "setItemWidget" item status)
      (qfun item "show")))
  |#
  )
