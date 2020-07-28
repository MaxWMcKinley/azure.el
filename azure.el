(require 'json)
(require 'aio)
(require 'log4e)

(setq lexical-binding t)

(log4e:deflogger "azure" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
						      (error . "error")
						      (warn  . "warn")
						      (info  . "info")
						      (debug . "debug")
						      (trace . "trace")))
(setq log4e--log-buffer-leetcode "*azure-log*")

(defun json-parse (str)
  "parse json from str as hashtable"
    (let ((json-object-type 'hash-table)
	(json-array-type 'list)
	(json-key-type 'string))
    (json-read-from-string str)))

(aio-defun request-with-header (token url)
  "Make a request to `url` with the given `token` as the bearer token"
  (let ((url-request-method "GET")
	(url-request-extra-headers
	  `(("Content-Type" . "application/json")
	    ("Authorization" . ,(concat "Bearer " token)))))
    (aio-await (aio-url-retrieve url))))

;; curl -X POST -d 'grant_type=client_credentials&client_id=[APP_ID]&client_secret=[PASSWORD]&resource=https%3A%2F%2Fmanagement.azure.com%2F' https://login.microsoftonline.com/[TENANT_ID]/oauth2/token
(aio-defun get-oauth (app-id tenant password)
  "Return the bearer token"
  (let* ((url-request-method "POST")
	 (url-request-data (format "grant_type=client_credentials&client_id=%s&client_secret=%s&resource=https://management.azure.com/" app-id password))
	(resp (aio-await (aio-url-retrieve (format "https://login.microsoftonline.com/%s/oauth2/token" tenant))))
	(buf (cdr resp)))
    (with-current-buffer buf
      (buffer-string))))

(aio-defun create-principal ()
  "Create service principal"
  (let*
    ((json (json-parse (shell-command-to-string "az ad sp create-for-rbac --name azure.el 2>/dev/null")))
     (password (gethash "password" json))
     (tenant (gethash "tenant" json))
     (app-id (gethash "appId" json)))
   `((app-id . ,app-id) (tenant . ,tenant) (password . ,password))))

(aio-defun azure--login ()
  "Populate the global bearer-token for use with future requests"
  (let*
    ((alist (aio-await (create-principal)))
    (app-id (alist-get 'app-id alist))
    (tenant (alist-get 'tenant alist))
    (password (alist-get 'password alist))
    (token (aio-await (get-oauth app-id tenant password))))
   (setq bearer-token token)))

(aio-wait-for (azure--login))
