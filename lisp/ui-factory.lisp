(in-package :et)

(defun generate-dock-timeline (title)
  (let ((main (qnew "QDockWidget(QWidget*)" *main-window*)))))

(defun generate-status-widget (from)
  (ui-status:ini)
  (qset ui-status:*status* "objectName" (tooter:id from))
  (qset ui-status:*lbl-content* "text"
        (tooter:content from))
  
  ;; need to check for cache'd copy of avatar and load it
  ;;  or fetch it if it doesn't exist
  (let* ((account (tooter:account from))
         (filepath (account-avatar-path account)))
    (unless (probe-file filepath)
      (download-avatar (tooter:avatar-static account) filepath))

    (qlet ((p "QPixmap(QString)" filepath))
      (qfun p "scaled" 128 128)
      (qset ui-status:*lbl-avatar* "pixmap" p)))

  (qconnect ui-status:*btn-reply* "clicked()" '(lambda () (prepare-reply from)))
  (qconnect ui-status:*btn-fave* "clicked()" '(lambda () )) ;;favorite toot, show UI thing to show user
  (qconnect ui-status:*btn-boost* "clicked()" '(lambda () )) ;;boost toot, show UI thing to show user

  ;; connect btn-more actions here

  ;; return the status widget
  ui-status:*status*)


