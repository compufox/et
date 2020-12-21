(in-package :et)

;;(defun generate-dock-timeline (title)
;;  (let ((main (qnew "QDockWidget(QWidget*)" *main-window*)))))


;; TODO
;;  - needs to properly handle boosts
;;     (currently just acts like the person boosting
;;     is the one who posted it)
;;  - handle media uploads
(defun generate-status-widget (from)
  (ui-status:ini)
  (let* ((status ui-status:*status*)
         (lbl-content (qfind-child status "lbl_content"))
         (btn-fave (qfind-child status "btn_fave"))
         (btn-boost (qfind-child status "btn_boost"))
         (btn-reply (qfind-child status "btn_reply"))
         (lbl-boost-icon (qfind-child status "lbl_boost_icon"))
         (lbl-boosted-by (qfind-child status "lbl_boosted_by"))
         (btn-cw (qfind-child status "btn_cw"))
         (lbl-cw (qfind-child status "lbl_cw"))
         (lyt-cw (qfind-child status "lyt_cw"))
         (lbl-avatar (qfind-child status "lbl_avatar"))
         (lbl-account (qfind-child status "lbl_account"))
         (lbl-privacy (qfind-child status "lbl_privacy")))
    (qset status "objectName" (x:cc "status_" (tooter:id from)))
    (qset lbl-content "text"
          (from-utf8 (tooter:content from)))

    (qlet ((boost "QPixmap(QString)" (if (tooter:reblogged from)
                                         "resources/icons/refresh-blue.png"
                                         "resources/icons/refresh.png"))
           (star "QPixmap(QString)" (if (tooter:favourited from)
                                        "resources/icons/star-filled.png"
                                        "resources/icons/star.png"))
           (reply "QPixmap(QString)" "resources/icons/reply.png")
           (icon-boost "QIcon(QPixmap)" (qfun boost "scaled" 16 16 1))
           (icon-star "QIcon(QPixmap)" (qfun star "scaled" 16 16 1))
           (icon-reply "QIcon(QPixmap)" (qfun reply "scaled" 16 16 1)))
      
      (qfun btn-reply "setIcon" icon-reply)
      (qfun btn-boost "setIcon" icon-boost)
      (qfun btn-fave "setIcon" icon-star))
  
    (let* ((parent (tooter:parent from))
           (account (if parent
                        (tooter:account (tooter:parent from))
                        (tooter:account from))))
      (qset lbl-account "text"
            (format nil "<p><strong>~A</strong> @~A"
                    (from-utf8 (tooter:display-name account))
                    (from-utf8 (tooter:account-name account))))
      
      ;; test for reblog and set accordingly
      (if parent
          (qlet ((p "QPixmap(QString)" "resources/icons/refresh.png"))
            (qset lbl-boost-icon "pixmap" (qfun p "scaled" 24 24 1))
            (qset lbl-boosted-by "text"
                  (from-utf8 (x:cc "boosted by @"
                                   (tooter:account-name
                                    (tooter:account from))))))
          (progn
            (qset lbl-boost-icon "visible" nil)
            (qset lbl-boosted-by "visible" nil)))
      
      
      ;; if there IS a cw then we need to make sure we set
      ;;  up the proper handlers and show the correct widgets
      (let ((spoiler-text (if parent
                              (tooter:spoiler-text parent)
                              (tooter:spoiler-text from))))
        (if (string> spoiler-text "")
            (progn
              (qset lbl-cw "text" (from-utf8 spoiler-text))
              (qfun lbl-content "setVisible" nil)
              (qconnect btn-cw "clicked()"
                        (l (qset lbl-content "visible"
                                 (not (qget lbl-content "visible"))))))
            (qset lyt-cw "visible" nil)))
      
      
      ;; need to check for cache'd copy of avatar and load it
      ;;  or fetch it if it doesn't exist
      ;; TODO: check create-time for FILEPATH and if its older
      ;;       than like a week re-download it
      (async 
       (let ((filepath (account-avatar-path account)))
         (unless (probe-file filepath)
           (download (tooter:avatar-static account) filepath))

         (qrun*
           (qlet ((p "QPixmap(QString)" (namestring filepath)))
             ;; 1 = |Qt.AspectRatioMode.KeepAspectRatio|
             (qset lbl-avatar "pixmap" (qfun p "scaled" 60 60 1))))))
      
      ;; set the privacy icon
      (let ((vis (if parent
                     (tooter:visibility parent)
                     (tooter:visibility from))))
        (qlet ((p "QPixmap(QString)" (privacy-icon vis)))
          (qset lbl-privacy "toolTip" (string-downcase (string vis)))
          (qset lbl-privacy "pixmap" (qfun p "scaled" 16 16 1)))
        
        ;; ensure we cant boost statuses that cannot be boosted
        (unless (boostablep from)
          (qset btn-boost "enabled" nil))))
    
    ;; set button signals here
    (qconnect ui-status:*btn-reply* "clicked()"
              (l (prepare-reply from)))
    (qconnect ui-status:*btn-fave* "clicked()"
              (l (favourite-status from status)))
    (qconnect ui-status:*btn-boost* "clicked()"
              (l (reblog-status from status)))
    
    ;; connect btn-more actions here
    
    ;; return the status widget
    status))

(defun generate-action-group (parent &key exclusive visible)
  (let ((group (qnew "QActionGroup(QObject*)" parent)))
    (when exclusive (qset group "exclusionPolicy" 1)) ;; QActionGroup.ExclusionPolicy.Exclusive
    (qset group "visible" visible)
    group))

(defun generate-menu-action (text parent &key checkable checked action-group tooltip icon icon-size shortcut callback visible)
  (let ((action (qfun parent "addAction" text)))
    (qset action "checkable" checkable)
    (qset action "checked" checked)
    (qset action "visible" visible)
    (qset action "toolTip" (or tooltip ""))
    (when icon
      (qlet ((p "QPixmap(QString)" icon)
             (i "QIcon(QPixmap)" (qfun p "scaled" icon-size icon-size 1)))
        (qset action "icon" i)))
    (when shortcut
      (qlet ((ks "QKeySequence(QString)" shortcut)
             (s "QShortcut(QKeySequence)" ks))
        (qset action "shortcut" s)))
    (when callback
      (qconnect action "triggered()" callback))
    (when action-group
      (qfun action-group "addAction" action))))
