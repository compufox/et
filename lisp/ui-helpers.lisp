(in-package :et)

(declaim (inline set-char-count))

(defun initialize-ui ()
  "initializes main window before being shown"
  (ini)
  (connect-signals)

  ;; generate accounts dropdown menu and action group
  (setf *account-action-group*
        (generate-action-group *main-window*
                               :exclusive t
                               :visible t))
  (loop :with default := (qsetting-value "default_account")
        :with accounts := (qsetting-value "available-accounts")
        
        :for account :in (typecase accounts
                           (list accounts)
                           (string (list accounts)))
        :do (add-account-option account :set-checked (equal account default)))
  
  ;; by default hide reply layout and CW composer
  ;; TODO: preferences to always show CW composer
  (qset *lyt-reply* "visible" nil)
  (qset *edt-compose-cw* "visible" nil)
  
  (unless (restore-application-state)
    (qfun *main-window* "tabifyDockWidget" *dock-notifs* *dock-local*)
    (qfun *main-window* "tabifyDockWidget" *dock-notifs* *dock-fedi*)
    (qfun *dock-notifs* "raise"))

  (qlet ((post "QPixmap(QString)" "resources/icons/paper-airplane.png")
         (icon "QIcon(QPixmap)" post))
    (qfun *btn-compose-post* "setIcon" icon))
  
  (qset *cmb-compose-privacy* "currentIndex" *visibility-default*)
  (setf *max-post-char* (instance-max-chars (tooter:base *tooter-client*)))
  (set-char-count *max-post-char*)
  (preload-timelines))

(defun add-account-option (account &key set-checked)
  (generate-menu-action (qsetting-value (x:cc "acct_" account "/name"))
                                  *mnu-account*
                                  :checkable t
                                  :checked set-checked
                                  :visible t
                                  :tooltip "change to account"
                                  :action-group *account-action-group*
                                  :callback
                                  (l (change-account-to account))))

(defun connect-signals ()
  "connect signals and install overrides"
  (qconnect *act-quit* "triggered()" *main-window* "close()")
  (qconnect *act-new-account* "triggered()"
            (l (add-account-option (add-new-account :reinitialize-client t))))
  (qconnect *txt-compose-content* "textChanged()" 'update-char-count)
  (qconnect *edt-compose-cw* "textChanged(QString)" 'update-char-count)
  (qconnect *chk-compose-cw* "toggled(bool)" *edt-compose-cw* "setVisible(bool)")
  (qconnect *chk-compose-cw* "toggled(bool)" 'update-char-count)
  (qconnect *btn-compose-post* "clicked()" 'send-post)
  (qconnect *btn-reply-clear* "clicked()" 'clear-reply)
  (qconnect *chk-compose-schedule* "clicked(bool)"
            (l (qset *dte-compose-time* "enabled" _)))
  (qoverride *main-window* "closeEvent(QCloseEvent*)" 'app-close))

(defun save-application-state ()
  "saves the current application dock state and geometry"
  (let ((state (qvariant-from-value (qfun *main-window* "saveState") "QByteArray"))
        (geom (qvariant-from-value (qfun *main-window* "saveGeometry") "QByteArray")))
    (setf (qsetting-value "window/state") state
          (qsetting-value "window/geometry") geom)))

(defun restore-application-state ()
  "restores the application window dock state and geometry"
  (let ((state (qsetting-value "window/state"))
        (geom (qsetting-value "window/geometry")))
    (when state (qfun *main-window* "restoreState" state))
    (when geom (qfun *main-window* "restoreGeometry" geom))))

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
    (if (and (> count 0) (not (zerop (length status-text))))
        (qset *btn-compose-post* "enabled" t)
        (qset *btn-compose-post* "enabled" nil))))

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
    (qset *txt-compose-content* "plainText" (x:cc "@" (tooter:account-name acct)
                                                  " "))

    (qset *cmb-compose-privacy* "currentIndex"
          (visibility-to-int (tooter:visibility to)))
    
    ;; maybe set this under a preference option?
    (unless (equal (tooter:spoiler-text to) "")
      (qset *chk-compose-cw* "checked" t)
      (qset *edt-compose-cw* "visible" t)
      (qset *edt-compose-cw* "text" (tooter:spoiler-text to)))
    
    (qfun *lyt-reply* "show")))

(defun clear-reply ()
  "clears the reply ID and hides the reply layout"
  (setf *reply-id* nil)
  (qset *lyt-reply* "visible" nil)
  (qset *txt-compose-content* "plainText" ""))

(defun send-post ()
  ;; disable our post button until we send the status
  (unless (zerop (length (qget *txt-compose-content* "plainText")))
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
         (qset *cmb-compose-privacy* "currentIndex" *visibility-default*)
         (clear-reply))))))
  
(defun add-new-account (&key should-quit set-default-account reinitialize-client)
  (ui-wizard:ini)

  (qoverride ui-wizard:*page-2* "validatePage()" 'validate-instance-url)
  (qoverride ui-wizard:*page-3* "validatePage()" 'validate-access-token)
  
  (if (zerop (qfun ui-wizard:*wiz-new-account* "exec"))
      (when should-quit (eql:qq))
      
      (let ((id (tooter:id (tooter:account *tooter-client*)))
            (accounts (listify (qsetting-value "available-accounts"))))
        (qlet ((token "QVariant(QString)" (tooter:access-token *tooter-client*))
               (streaming-url "QVariant(QString)" (streaming-url))
               (vid "QVariant(QString)" id)
               (base-url "QVariant(QString)" (tooter:base *tooter-client*))
               (acct-name "QVariant(QString)" (x:cc "@" (tooter:account-name (tooter:account *tooter-client*))))
               (default-visibility "QVariant(int)" (visibility-to-int
                                                    (account-preference :posting-visibility)))
               (acct-list "QVariant(QStringList)" (append accounts (list id))))
          (setf (qsetting-value (x:cc "acct_" id "/token")) token
                (qsetting-value (x:cc "acct_" id "/streaming-url")) streaming-url
                (qsetting-value (x:cc "acct_" id "/name")) acct-name
                (qsetting-value (x:cc "acct_" id "/base-url")) base-url
                (qsetting-value (x:cc "acct_" id "/visibility-default")) default-visibility
                (qsetting-value "available-accounts") acct-list)
          
          (when set-default-account
            (setf (qsetting-value "default_account") vid)))
        
        (when reinitialize-client
          (clear-timeline *tl-home* *tl-notif* *tl-fedi* *tl-local*)
          (initialize-client id)
          (qset *cmb-compose-privacy* "currentIndex" *visibility-default*))
        
        id)))

(defun change-account-to (id)
  ;; clears the timelines
  (close-sockets)
  (clear-timeline *tl-home* *tl-notif* *tl-fedi* *tl-local*)
  (initialize-client id)
  (qset *cmb-compose-privacy* "currentIndex" *visibility-default*))

(defun update-handler (post timeline)
  (let ((status (generate-status-widget post))
        (item (qnew "QListWidgetItem"))
        (tl (case timeline
              (:home *tl-home*)
              (:local *tl-local*)
              (:fedi *tl-fedi*)))
        (status-name (x:cc "status_"
                           (princ-to-string (tooter:id post)))))
    (flet ((set-size ()
             (list (qfun tl "frameWidth")
                   (cadr (qfun status "sizeHint")))))
      (qfun status "show")
      (qfun item "setSizeHint" (set-size))
      ;(qoverride item "data(int)"
      ;           (l (qvariant-from-value status-name "QString")))
      (qconnect (qfind-child status "btn_cw") "clicked()" #'set-size)
      (qfun tl "insertItem" 0 item)
      (qfun tl "setItemWidget" item status))))

(defun delete-handler (id timeline)
  (let* ((tl (case timeline
              (:home *tl-home*)
              (:local *tl-local*)
              (:fedi *tl-fedi*)))
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

(defun clear-all-timelines ()
  (clear-timeline *tl-home* *tl-local* *tl-fedi* *tl-notif*))

(defun clear-timeline (&rest timelines)
  (dolist (tl timelines)
    (loop until (zerop (qfun tl "count"))
          do (qdel (qfun tl "takeItem" 0)))))

(defun preload-timelines ()
  (async*
   (loop :for status :in (reverse (tooter:timeline *tooter-client* :home))
         :do (qrun* (update-handler status :home)))
   (loop :for status :in (reverse (tooter:timeline *tooter-client* :public :local t))
         :do (qrun* (update-handler status :local)))
   (loop :for status :in (reverse (tooter:timeline *tooter-client* :public))
         :do (qrun* (update-handler status :fedi)))))
