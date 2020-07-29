(require 'json)
(require 'log4e)
(require 'url)
(require 'transient)
(require 'ivy)

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

(defun get-request (url)
  (let* ((url-request-method "GET")
         (resp (url-retrieve-synchronously url)))
    (with-current-buffer resp
      (buffer-string))))

(defun azure-func-start ()
  "Start Functions server in the background"
  (interactive)
  (when (boundp 'functions-server)
    (message "Closing existing server")
    (azure-func-stop))
  (message "Starting Functions server...")
  (setq endpoints nil)
  (with-current-buffer (generate-new-buffer "*Azure Functions*")
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
     (query-and-display endpoints))))

(defun query-and-display (endpoints)
  (let* ((endpoint (ivy-read "Endpoint to query: " endpoints))
	 (params (read-string (concat "Query: " endpoint "/"))))
  (message (get-request (concat endpoint "/" params)))))

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
 (let ((match (ignore-errors (match-string 1 string))))
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

;; Remove any resources of "null" kind
(defun filter-resources (resources)
  (seq-filter (lambda (x) (gethash "kind" x)) resources))

;; Get detailed information for any resource given its id
(defun get-resource-details (id)
  (json-parse (shell-command-to-string (format "az resource show --ids %s" id))))

;; The status field is called different things depending on the type of resource
;; This function could be optimized by eagerly exiting when finding a valid status field
(defun get-status-from-details (details)
  (let* ((properties (gethash "properties" details))
         (status (gethash "status" properties))
         (state (gethash "state" properties))
         (primary (gethash "statusOfPrimary" properties)))
    (cond (status status)
          (state state)
          (primary primary)
          (t "N/A"))))

;; Add status of a resource to its hash table
(defun enrich-resource-status (resource-hash-table)
  (let* ((id (gethash "id" resource-hash-table))
         (details (get-resource-details id))
         (status (get-status-from-details details)))
    (puthash "status" status resource-hash-table)
    resource-hash-table))

;; Enrich a list of resources with data only visible in their detailed views
(defun enrich-resources (resources)
  (mapcar (lambda (x) (enrich-resource-status x)) resources))

;; TODO: This fails if the az command returns an empty array
(defun run-resource-list (&optional args)
  (json-parse (shell-command-to-string (concat "az resource list " (mapconcat 'identity args " ")))))

;; Fetch resource list with az, then format result as a vector in the format tabulated-list-mode can read
(defun get-resources (&optional args)
  (let ((res (enrich-resources (filter-resources (run-resource-list args)))))
    (mapcar (lambda (x) (vector (gethash "name" x)
                                (gethash "status" x)
                                (gethash "kind" x)
                                (gethash "location" x)
                                (gethash "resourceGroup" x))
              ) res)))

(defun update-resources (&optional args)
  (interactive
   (list (transient-args 'azure-resource-transient)))
  (let ((rows (mapcar (lambda (x) `(nil ,x)) (get-resources args))))
    (update-table-entries rows)))

;; Create azure major made based on tabulated list
(define-derived-mode azure-mode tabulated-list-mode "Azure"
  "Azure Mode"
  (use-local-map azure-map)
  (let ((columns [("Resource" 20) ("Status" 20) ("Kind" 20) ("Location" 20) ("Resource Group" 20)])
        (rows (mapcar (lambda (x) `(nil ,x)) (get-resources))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun update-table ()
  (interactive)
  (update-table-entries (list `(nil ["a" "b" "c" "d" "e"]))))

(defun update-table-entries (entries)
  (setq tabulated-list-entries entries)
  (tabulated-list-print))

;; Interactive function to launch azel
(defun azure ()
  (interactive)
  (switch-to-buffer "*azure*")
  (azure-mode))

;; Just a temporary test function to test transient functionality
(defun test-popup (&optional args)
  (interactive
   (list (transient-args 'azure-transient)))
  (message "Args: %s" args))

(defun query-function-main (&optional args)
  "Start functions server if not started, then prompt to query the endpoint."
  (interactive)
  (when (not (boundp 'functions-server))
    (azure-func-start)
    ;; wait for endpoint to populate
    (sleep-for 2 0))
  (azure-func-query))

(define-transient-command azure-transient ()
  "Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-l" "Location" "--location=")
   ("-g" "Resource Group" "--group=")]
  ["Actions"
   ("f" "Query Function" query-function-main)
   ("d" "Deploy code" test-popup)
   ("r" "Resources" set-resources)
   ("u" "Update" update-table)
   ("m" "View Metrics" test-popup)
   ("s" "Download App Settings" test-popup)])

;; TODO: Some combinations aren't allowed e.g. tag and anything else
(define-transient-command azure-resource-transient ()
  "Azure Resource Commands"
  ["Arguments"
   ("-s" "Subscription" "--subscription=")
   ("-g" "Resource Group" "--resource-group=")
   ("-l" "Location" "--location=")
   ("-t" "Tag" "--tag=")
   ("-r" "Resource Type" "--resource-type=")
   ("-n" "Name" "--name=")
   ("-p" "Namespace" "--namespace=")]
  ["Actions"
   ("r" "Display Resources" update-resources)
   ("u" "Update with example data" update-table)])

;; Attempting to add transient keybinding
;; Seems to be shadowed by evil mode map
(defvar azure-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'azure-transient)
    map))

