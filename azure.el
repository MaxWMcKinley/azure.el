(require 'json)
(require 'log4e)
(require 'url)

(setq lexical-binding t)

(log4e:deflogger "azure" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
						      (error . "error")
						      (warn  . "warn")
						      (info  . "info")
						      (debug . "debug")
						      (trace . "trace")))
(setq log4e--log-buffer-leetcode "*azure-log*")

(setq endpoints '())

(defun json-parse (str)
  "parse json from str as hashtable"
    (let ((json-object-type 'hash-table)
	(json-array-type 'list)
	(json-key-type 'string))
    (json-read-from-string str)))

(defun get-request (url)
  (let* ((url-request-method "GET")
         (resp (url-retrieve-synchronously url))
	 (buf resp))
    (with-current-buffer buf
      (buffer-string))))

(defun request-with-header (token url)
  "Make a request to `url` with the given `token` as the bearer token"
  (let ((url-request-method "GET")
	(url-request-extra-headers
	  `(("Content-Type" . "application/json")
	    ("Authorization" . ,(concat "Bearer " token)))))
    (url-retrieve-synchronously url)))

;; curl -X POST -d 'grant_type=client_credentials&client_id=[APP_ID]&client_secret=[PASSWORD]&resource=https%3A%2F%2Fmanagement.azure.com%2F' https://login.microsoftonline.com/[TENANT_ID]/oauth2/token
(defun get-oauth (app-id tenant password)
  "Return the bearer token"
  (let* ((url-request-method "POST")
	 (url-request-data (format "grant_type=client_credentials&client_id=%s&client_secret=%s&resource=https://management.azure.com/" app-id password))
	(resp (url-retrieve-synchronously (format "https://login.microsoftonline.com/%s/oauth2/token" tenant)))
	(buf (cdr resp)))
    (with-current-buffer buf
      (buffer-string))))

(defun create-principal ()
  "Create service principal"
  (let*
    ((json (json-parse (shell-command-to-string "az ad sp create-for-rbac --name azure.el 2>/dev/null")))
     (password (gethash "password" json))
     (tenant (gethash "tenant" json))
     (app-id (gethash "appId" json)))
   `((app-id . ,app-id) (tenant . ,tenant) (password . ,password))))

(defun azure-login ()
  "Populate the global bearer-token for use with future requests"
  (interactive)
  (let*
    ((alist (create-principal))
    (app-id (alist-get 'app-id alist))
    (tenant (alist-get 'tenant alist))
    (password (alist-get 'password alist))
    (token (get-oauth app-id tenant password)))
   (setq bearer-token token)))

(defun azure-func-start ()
  "Start Functions server in the background"
  (interactive)
  (with-current-buffer (generate-new-buffer "*Azure Functions*")
    (when (boundp 'functions-server)
      (message "Closing existing server")
      (azure-func-stop))
    ;; NOTE: c# and ts use a different command
    (setq functions-server (start-process "Azure Functions Server" (current-buffer) "func" "start")))
  (set-process-sentinel functions-server 'func-cleanup)
  (set-process-filter functions-server 'insertion-filter))

(defun func-cleanup (proc str)
  "Kill func buffer when process completed"
  (when (or (null (process-status (process-name proc)))
	(= (process-status (process-name proc)) 'exit))
    (kill-buffer (process-buffer proc))))

(defun azure-func-query ()
  (interactive)
  (cond
    ((not (boundp 'functions-server))
     (message "Start functions server first with M-x azure-func-start RET"))
    ((null endpoints)
     (message "No endpoints logged yet; wait a few more seconds"))
    (t
     (query-and-display (concat (car endpoints) "/" (read-string (concat "Query: " (car endpoints) "/")))))))

(defun query-and-display (e)
  (message (get-request e)))

(defun insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
	  (set-endpoint-if-present string)
          (set-marker (process-mark proc) (point)))
        (when moving (goto-char (process-mark proc)))))))

(defun set-endpoint-if-present (string)
 (string-match "^Http Functions:\n\n[ \t\n]*HttpTrigger: \\[.*\\] *\\(http.*\\)" string)
 (let ((match (match-string 1 string)))
   (when (and
	   (not (null match))
	   ;; TODO: figure out why regex gives false positives like "["
	   (not (= 1 (length match))))
     (push match endpoints))))

(defun azure-func-stop ()
  "Stop the Functions server"
  (interactive)
  (when (boundp 'functions-server)
    (kill-buffer (process-buffer functions-server))
    (delete-process functions-server)
    (makunbound 'functions-server))
  't)

;; (setq s "Http Functions:

;; 	HttpTrigger: [GET,POST] http://localhost:7071/api/HttpTrigger")
;; (string-match "^Http Functions:\n\n[ \t\n]*HttpTrigger: \\[.*\\] *\\(.*\\)" s)
;; (match-string 1 s)
