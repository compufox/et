(in-package :et)

(declaim (inline to-keyword streaming-url ensure-https))

(defun qsetting-value (key &optional default)
  "retrieve a value from qsettings"
  (qlet ((settings "QSettings"))
    (or (qvariant-value (|value| settings key))
        default)))

(defun (setf qsetting-value) (value key)
  "defines a setf handler for setting-value"
  (qlet ((settings "QSettings"))
    (|setValue| settings key value)))

(defun to-keyword (val)
  "converts VAL to a keyword"
  (intern (string-upcase val) :keyword))

(defun standard-paths (type)
  "returns Qt standard path given keyword TYPE"
  (qfun "QStandardPaths" "standardLocations"
        (case type
          (:desktop-location |QStandardPaths.DesktopLocation|)
          (:documents-location |QStandardPaths.DocumentsLocation|)
          (:fonts-location |QStandardPaths.FontsLocation|)
          (:applications-location |QStandardPaths.ApplicationsLocation|)
          (:music-location |QStandardPaths.MusicLocation|)
          (:movies-location |QStandardPaths.MoviesLocation|)
          (:pictures-location |QStandardPaths.PicturesLocation|)
          (:temp-location |QStandardPaths.TempLocation|)
          (:home-location |QStandardPaths.HomeLocation|)
          (:data-location |QStandardPaths.DataLocation|)
          (:cache-location |QStandardPaths.CacheLocation|)
          (:generic-cache-location |QStandardPaths.GenericCacheLocation|)
          (:generic-data-location |QStandardPaths.GenericDataLocation|)
          (:runtime-location |QStandardPaths.RuntimeLocation|)
          (:config-location |QStandardPaths.ConfigLocation|)
          (:download-location |QStandardPaths.DownloadLocation|)
          (:generic-config-location |QStandardPaths.GenericConfigLocation|)
          (:app-data-location |QStandardPaths.AppDataLocation|)
          (:app-local-data-location |QStandardPaths.AppLocalDataLocation|)
          (:app-config-location |QStandardPaths.AppConfigLocation|))))

(defun download-avatar (url path)
  (with-open-file (out path :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite
                            :element-type '(unsigned-byte 8))
    (loop with in = (drakma:http-request url :want-stream t)
          for byte = (read-byte in nil nil)
          until byte do (write-byte byte out))))

(defun instance-max-chars (url)
  "gets the max toot character count"
  (gethash "max_toot_chars"
           (yason:parse (map 'string #'code-char (drakma:http-request (x:cc url "/api/v1/instance"))))
           500))

;; TODO
;; store under
;; cache/URL/accounts/ID.png
;; ensure that URL/accounts/ is created
(defun account-avatar-path (account)
  (merge-pathnames (x:cc "accounts/" (tooter:id account) ".png")
                   (ensure-directories-exist (car (standard-paths :cache-location)))))

(defun streaming-url ()
  (gethash "streaming_api" (tooter:urls (tooter:instance *tooter-client*))))

(defun ensure-https (url)
  (if (search "https://" url :test #'string=)
      url
      (x:cc "https://" url)))

(defun start-websocket (for timeline callback &key restart-on-error timeline-arg)
  "start websocket FOR account TIMELINE with CALLBACK.

FOR is the account ID we're loading configs for
TIMELINE is one of user, public, public:local, hashtag, hashtag:local, list, direct
CALLBACK is a function to recieve new websocket updates

if RESTART-ON-CLOSE is non-nil then the websocket will attempt to be restarted if it closes in error
TIMELINE-ARG is used when TIMELINE is a hashtag or list timeline. it should contain an appropriate identifier"
  (let ((socket (wsd:make-client (format nil "~A/api/v1/streaming?access_token=~a&stream=~a~A"
                                         (qsetting-value (x:cc "acct_" for "/streaming-url"))
                                         (qsetting-value (x:cc "acct_" for "/token"))
                                         timeline
                                         (if timeline-arg
                                             (if (string= timeline "list")
                                                 (x:cc "&list=" timeline-arg)
                                                 (x:cc "&tag=" timeline-arg))
                                             "")))))
    (wsd:on :message socket callback)
    (wsd:on :close socket #'print-close)

    (wsd:start-connection socket)
    (push socket *websockets*)))

(defun print-close (&key code reason)
  "prints a message when the websocket is closed"
  (when (and code reason)
    (format t "disconnected because ~A (code=~A)~%" reason code)))

(defun validate-instance-url ()
  (let ((url (ensure-https (qget ui-wizard:*wiz-serv-entry* "text")))
        (qurl (qnew "QUrl")))
    
    ;; ensure we dont get a blank url
    (when (string= "" (qget ui-wizard:*wiz-serv-entry* "text"))
      (return-from validate-instance-url nil))
    
    (handler-case 
        (progn
          (setf *tooter-client* (make-instance 'tooter:client
                                               :base url
                                               :name "extratootrestrial"
                                               :website "https://github.com/compufox/et"))
          (multiple-value-bind (authed auth-url) (tooter:authorize *tooter-client*)
            (declare (ignore authed))
            (qfun qurl "setUrl" auth-url)
            (qfun ui-wizard:*wiz-auth-view* "load" qurl)))
      (error (e)
        (format t "~A~%" e)))
    t))

(defun validate-access-token ()
  (handler-case
      (progn
        (tooter:authorize *tooter-client*
                          (qget ui-wizard:*wiz-auth-entry* "text"))
        (tooter:account *tooter-client*))
    (error (e)
      nil))
  t)
