(in-package :et)

;;(defun generate-dock-timeline (title)
;;  (let ((main (qnew "QDockWidget(QWidget*)" *main-window*)))))


;; TODO
;;  - needs to properly handle boosts
;;     (currently just acts like the person boosting
;;     is the one who posted it)
;;  - properly setup status button signals
;;  - try and implement cloning of sorts.
;;     currently experiences errors re: cws and
;;     i think thats tied to not cloning the status
;;  - handle media uploads
(defun generate-status-widget (from)
  (ui-status:ini)
  (qset ui-status:*status* "objectName" (tooter:id from))
  (qset ui-status:*lbl-content* "text"
        (from-utf8 (tooter:content from)))

  (qlet ((boost "QPixmap(QString)" "resources/icons/refresh.png")
         (star "QPixmap(QString)" "resources/icons/star.png")
         (reply "QPixmap(QString)" "resources/icons/reply.png")
         (icon-boost "QIcon")
         (icon-star "QIcon")
         (icon-reply "QIcon"))
    (qfun icon-boost "addPixmap"
          (qfun boost "scaled" 16 16 1))
    (qfun icon-reply "addPixmap"
          (qfun reply "scaled" 16 16 1))
    (qfun icon-star "addPixmap"
          (qfun star "scaled" 16 16 1))

    (qset ui-status:*lbl-boost-icon* "pixmap" (qfun boost "scaled" 24 24 1))

    (qfun ui-status:*btn-reply* "setIcon" icon-reply)
    (qfun ui-status:*btn-boost* "setIcon" icon-boost)
    (qfun ui-status:*btn-fave* "setIcon" icon-star))
  
  (let ((account (tooter:account from)))
    (qset ui-status:*lbl-account* "text" (format nil "<p><strong>~A</strong> @~A"
                                                 (from-utf8 (tooter:display-name account))
                                                 (from-utf8 (tooter:account-name account))))
    
    ;; test for reblog and set accordingly
    (qset ui-status:*lbl-boosted-by* "text" (from-utf8 (x:cc "@" (tooter:account-name account)))))

  ;; if there IS a cw then we need to make sure we set
  ;;  up the proper handlers and show the correct widgets
  (if (string> (tooter:spoiler-text from) "")
      (progn
        (qset ui-status:*lbl-cw* "text" (from-utf8 (tooter:spoiler-text from)))
        (qfun ui-status:*lbl-content* "setVisible" nil)
        (qconnect ui-status:*btn-cw* "clicked()" #'(lambda ()
                                                     (qset ui-status:*lbl-content* "visible"
                                                           (not (qget ui-status:*lbl-content* "visible"))))))
      (progn
        (qfun ui-status:*lbl-cw* "setVisible" nil)
        (qfun ui-status:*btn-cw* "setVisible" nil)))
    
  
  ;; need to check for cache'd copy of avatar and load it
  ;;  or fetch it if it doesn't exist
  (let* ((account (tooter:account from))
         (filepath (account-avatar-path account)))
    (unless (probe-file filepath)
      (download-avatar (tooter:avatar-static account) filepath))

    (qlet ((p "QPixmap(QString)" (namestring filepath)))
      (qset ui-status:*lbl-avatar* "pixmap" (qfun p "scaled" 60 60 1)))) ;; 1 = |Qt.AspectRatioMode.KeepAspectRatio|

  (let ((vis (tooter:visibility from)))
    (qlet ((p "QPixmap(QString)" (x:cc "resources/icons/" (case vis
                                                            (:public "globe.png")
                                                            (:unlisted "lock-open.png")
                                                            (:private "lock-closed.png")
                                                            (:direct "mail.png")))))
      (qset ui-status:*lbl-privacy* "toolTip" (string-downcase (string vis)))
      (qset ui-status:*lbl-privacy* "pixmap" (qfun p "scaled" 24 24 1)))

    (when (or (eql vis :private) (eql vis :direct))
      (qset ui-status:*btn-boost* "enabled" nil)))

  (qconnect ui-status:*btn-reply* "clicked()" #'(lambda () (prepare-reply from)))
  (qconnect ui-status:*btn-fave* "clicked()" #'(lambda () )) ;;favorite toot, show UI thing to show user
  (qconnect ui-status:*btn-boost* "clicked()" #'(lambda () )) ;;boost toot, show UI thing to show user

  ;; connect btn-more actions here

  ;; return the status widget
  ui-status:*status*)


