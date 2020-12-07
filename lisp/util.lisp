(in-package :et)

(declaim (inline streaming-url ensure-https parse-response
                 determine-extension from-utf8 privacy-icon
                 visibility-to-int))

(defun qsetting-value (key &optional default)
  "retrieve a value from qsettings"
  (qlet ((settings "QSettings"))
    (or (qvariant-value (|value| settings key))
        default)))

(defun (setf qsetting-value) (value key)
  "defines a setf handler for setting-value"
  (qlet ((settings "QSettings"))
    (|setValue| settings key value)))

(defmacro async (&body body)
  "runs BODY in a different thread"
  `(bt:make-thread
    (lambda ()
      ,@body)))

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

(defun download (url path)
  "downloads file at URL and saves to PATH "
  (with-open-file (out path :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite
                            :element-type '(unsigned-byte 8))
    (loop with in = (drakma:http-request url :want-stream t)
          for byte = (read-byte in nil nil)
          while byte do (write-byte byte out))))

(defun instance-max-chars (url)
  "gets the max toot character count"
  (gethash "max_toot_chars"
           (yason:parse (map 'string #'code-char (drakma:http-request (x:cc url "/api/v1/instance"))))
           500))

(defun determine-extension (path)
  "gets the file extension for PATH"
  (let ((num (search "." path :test #'string= :from-end t)))
    (subseq path num)))

(defun account-avatar-path (account)
  "returns the pathstring to ACCOUNT's cached avatar"
  (let ((filename (x:cc (tooter:id account) (determine-extension (tooter:avatar-static account))))
        (path (x:cc (subseq (tooter:base *tooter-client*) 8) "/accounts/"))
        (cache (car (standard-paths :cache-location))))
    (merge-pathnames filename
                     (ensure-directories-exist
                      (merge-pathnames path
                                       (ensure-directories-exist cache))))))

(defun streaming-url ()
  "return the streaming url for the current instance"
  (gethash "streaming_api" (tooter:urls (tooter:instance *tooter-client*))))

(defun ensure-https (url)
  "ensures that URL starts with https://"
  (if (search "https://" url :test #'string=)
      url
      (x:cc "https://" url)))

(defun start-websocket (for timeline callback &key restart-on-error timeline-arg)
  "start websocket FOR account TIMELINE with CALLBACK.

FOR is the account ID we're loading configs for
TIMELINE is one of user, public, public:local, hashtag, hashtag:local, list, direct
CALLBACK is a function to recieve new websocket updates

if RESTART-ON-ERROR is non-nil then the websocket will attempt to be restarted if it closes in error
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

    (when restart-on-error
      ;; create some kind of handler to restart the socket if it's closed
      )

    (wsd:start-connection socket)
    (push socket *websockets*)))

(defun print-close (&key code reason)
  "prints a message when the websocket is closed"
  (when (and code reason)
    (format t "disconnected because ~A (code=~A)~%" reason code)))

(defun validate-instance-url ()
  "validates the instance url entered into the new account wizard"
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
            (qfun ui-wizard:*wiz-auth-view* "load" qurl)
            (qdel qurl)))
      (error (e)
        (format t "~A~%" e)))
    t))

(defun validate-access-token ()
  "validates the access token entered in the new account wizard"
  (handler-case
      (progn
        (tooter:authorize *tooter-client*
                          (qget ui-wizard:*wiz-auth-entry* "text"))
        (tooter:account *tooter-client*))
    (error (e)
      nil))
  t)

(defun from-utf8 (string)
  "run STIRNG through QString.fromUTF8"
  (qfrom-utf8 (map 'vector #'char-code string)))

(defun privacy-icon (visibility)
  ""
  (x:cc "resources/icons/"
        (case visibility
          (:public "globe.png")
          (:unlisted "lock-open.png")
          (:private "lock-closed.png")
          (:direct "mail.png"))))

(defun boostablep (status)
  "checks to see if STATUS is boostable by the current account"
  (let ((vis (tooter:visibility status))
        (boostable t))
    (unless (self-post-p status)
      (when (or (eql vis :private) (eql vis :direct))
        (setf boostable nil)))
    boostable))

(defun self-post-p (status)
  "checks to see if STATUS was made by the current account"
  (let ((user-account (tooter:id (tooter:account *tooter-client*)))
        (status-account (tooter:id (or (tooter:parent status)
                                       (tooter:account status)))))
    (equal user-account status-account)))

(defun parse-response (resp)
  (yason:parse (map 'string #'code-char resp)))

(defun account-preference (pref)
  (let* ((key (case pref
                (:posting-visibility "posting:default:visibility")
                (:posting-sensitive "posting:default:sensitive")
                (:posting-language "posting:default:language")
                (:expand-media "reading:expand:media")
                (:expand-spoilers "reading:expand:spoilers")))
         (headers (list (cons "Authorization"
                              (x:cc "Bearer " (tooter:access-token *tooter-client*)))))
         (response (drakma:http-request (format nil "~a/api/v1/preferences"
                                                (tooter:base *tooter-client*))
                                        :additional-headers headers)))
    (to-keyword (gethash key (parse-response response)))))

(defun visibility-to-int (visibility)
  (search (list visibility) '(:public :unlisted :private :direct)))
