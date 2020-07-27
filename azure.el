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

(aio-defun request-with-header (token url)
  "docstring"
  (let* ((url-request-method "GET")
	 (url-request-extra-headers
	  `(("Content-Type" . "application/json")
	    ("Authorization" . ,(concat "Bearer " token)))))
    (aio-await (aio-url-retrieve url))))

;; set token and subscription-id with M-; (setq token "{token}") RET M-; (setq subscription-id "{id}") RET
(aio-wait-for (request-with-header token
  (format "https://management.azure.com/subscriptions/%s/resources?api-version=2019-10-01" subscription-id)))
