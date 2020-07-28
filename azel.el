(defun get-resources ()
  (list '("azel-test-func" "Running" "azel-test" "test-free")
        '("azel-test-ase" "Down" "azel-test" "test-free")
        '("azel-test-db" "Running" "azel-test" "test-free")))

(define-derived-mode azure-mode tabulated-list-mode "Azure"
  "Azure Mode"
  (let ((columns [("Resource" 25) ("Status" 25) ("Group" 25) ("Subscription" 25)])
        (rows (mapcar (lambda (x) `(nil ,(vconcat x))) (get-resources))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun azure ()
  (interactive)
  (switch-to-buffer "*azure*")
  (azure-mode))
