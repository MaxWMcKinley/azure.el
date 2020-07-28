(defun test-popup (&optional args)
  (interactive
   (list (transient-args 'azure-transient)))
  (message "Args: %s" args))

(define-transient-command azure-transient ()
  "Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-g" "Resource Group" "--group=")]
  ["Actions"
   ("f" "Filter" test-popup)
   ("r" "Run Function" test-popup)
   ("d" "Deploy code" test-popup)
   ("r" "Restart" test-popup)
   ("l" "View Logs" test-popup)
   ("m" "View Metrics" test-popup)
   ("s" "Download App Settings" test-popup)])

(azure-transient)

(defvar azure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'azure-transient)
    map))
