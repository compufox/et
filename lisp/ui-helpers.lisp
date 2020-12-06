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
    (qlet ((p "QPixmap(QString)" (namestring (account-avatar-path acct))))
      (qset *lbl-reply-avatar* "pixmap" (qfun p "scaled" 30 30 1)))
    (qset *lbl-reply-content* "text" (tooter:content to))
    (qset *txt-compose-content* "plainText" (x:cc "@" (tooter:account-name acct)))

    ;; maybe set this under a preference option?
    (when (tooter:spoiler-text to)
      (qset *chk-compose-cw* "enabled" t)
      (qset *edt-compose-cw* "visible" t)
      (qset *edt-compose-cw* "text" (tooter:spoiler-text to)))
    
    (qfun *lyt-reply* "show")))

(defun clear-reply ()
  "clears the reply ID and hides the reply layout"
  (setf *reply-id* nil)
  (qset *lyt-reply* "visible" nil))

(defun send-post ()
  ;; disable our post button until we send the status
  (qset *btn-compose-post* "enabled" nil)
  (let ((content (qget *txt-compose-content* "plainText"))
        (visibility (to-keyword (qget *cmb-compose-privacy* "currentText")))
        (hide-media (qget *chk-compose-hide* "checked"))
        (cw (qget *edt-compose-cw* "text")))
    (async 
      ;; need to catch for errors here
      (tooter:make-status *tooter-client*
                          content
                          :visibility visibility
                          :sensitive hide-media
                          :spoiler-text (when (qget *chk-compose-cw* "checked") cw)
                          :in-reply-to *reply-id*)
      
      ;; set stuff back to defaults
      (qrun*
        (qset *txt-compose-content* "plainText" "")
        (qset *edt-compose-cw* "text" "")
        (qset *chk-compose-cw* "checked" nil)
        (qset *chk-compose-hide* "checked" nil)
        (qset *btn-compose-post* "enabled" t)
        
        ;; TODO
        ;; this should take into account current account
        ;; defautl
        (qset *cmb-compose-privacy* "currentIndex" 0)
        (clear-reply)))))

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
              ((string= timeline "fedi") *tl-fedi*)))
        (status-name (x:cc "status_"
                           (princ-to-string (tooter:id post)))))
    (flet ((set-size ()
             (list (qfun tl "frameWidth")
                   (cadr (qfun status "sizeHint")))))
      (qfun status "show")
      (qfun item "setSizeHint" (set-size))
      ;(qoverride item "data(int)"
      ;           #'(lambda (_)
      ;               (declare (ignore _))
      ;               (qvariant-from-value status-name "QString")))
      (qconnect (qfind-child status "btn_cw") "clicked()" #'set-size)
      (qfun tl "insertItem" 0 item)
      (qfun tl "setItemWidget" item status))))

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

          when (equal (qvariant-value (qfun item "data" 0))
                      (x:cc "status_" (princ-to-string id)))
            do (qfun tl "removeItemWidget" item))))

(defun notification-handler (post)
  (declare (ignore post))
  #|
    (let ((status (generate-status-widget post))
          (item (qnew "QListWidgetItem")))
      ;(qset item "sizeHint" (qget status "sizeHint"))
      (qfun *tl-notif* "addItem" item)
      (qfun *tl-notif* "setItemWidget" item status)
      (qfun item "show")))
  |#
  )

(defun favourite-status (status widget)
  (let* ((faved (tooter:favourited status))
         (icon-path (if faved
                        "resources/icons/star.png"
                        "resources/icons/star-filled.png"))
         (btn (qfind-child widget "btn_fave")))

    (async 
     ;; toggle favourite property of status
     (if faved
         (tooter:unfavourite *tooter-client* status)
         (tooter:favourite *tooter-client* status))
     
     ;; update the button icon
     (qrun* 
       (qlet ((p "QPixmap(QString)" icon-path)
              (i "QIcon(QPixmap)" (qfun p "scaled" 16 16 1)))
         (qset btn "icon" i))))))

(defun reblog-status (status widget)
  (let* ((boosted (tooter:reblogged status))
         (icon-path (if boosted
                        "resources/icons/refresh.png"
                        "resources/icons/refresh-blue.png"))
         (btn (qfind-child widget "btn_boost")))

    (async
     ;; toggle favourite property of status
     (if boosted
         (tooter:unreblog *tooter-client* status)
         (tooter:reblog *tooter-client* status))
     
     ;; update the button icon
     (qrun* 
       (qlet ((p "QPixmap(QString)" icon-path)
              (i "QIcon(QPixmap)" (qfun p "scaled" 16 16 1)))
         (qset btn "icon" i))))))
