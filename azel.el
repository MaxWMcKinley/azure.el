;; Parse json as a hashtable, basically a repeat of what is in azure.el
(defun json-parse (str)
  "Parse json from str as hashtable"
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (json-read-from-string str)))

;; Remove any resources of "null" kind
(defun filter-resources (res)
  (seq-filter (lambda (x) (gethash "kind" x)) res))

;; Fetch resource list with az, then format result as a vector in the format tabulated-list-mode can read
(defun get-resources ()
  (let ((res (filter-resources (json-parse (shell-command-to-string "az resource list")))))
    (mapcar (lambda (x) (vector (gethash "name" x)
                                (gethash "kind" x)
                                (gethash "location" x)
                                (gethash "resourceGroup" x))
              ) res)))

;; Create azure major made based on tabulated list
(define-derived-mode azure-mode tabulated-list-mode "Azure"
  "Azure Mode"
  (let ((columns [("Resource" 20) ("Kind" 20) ("Location" 20) ("Resource Group" 20)]); ("Tags" 20)])
        (rows (mapcar (lambda (x) `(nil ,x)) (get-resources))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

;; Interactive function to launch azel
(defun azure ()
  (interactive)
  (switch-to-buffer "*azure*")
  (azure-mode))
