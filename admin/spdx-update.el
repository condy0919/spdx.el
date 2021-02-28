(require 'cl-lib)

(defun spdx-update--get-license-ids ()
  (with-temp-buffer
    (url-insert-file-contents
     (concat "https://raw.githubusercontent.com/"
             "spdx/license-list-data"
             "/master/"
             "json/licenses.json"))
    (let* ((json (json-read))
           (release-date (cdr (assoc 'releaseDate json)))
           (licenses (cdr (assoc 'licenses json)))
           (license-ids (mapcar (lambda (license)
                                  (cdr (assoc 'licenseId license)))
                                licenses)))
      (list release-date (sort license-ids (lambda (a b)
                                             (string< (downcase a)
                                                      (downcase b))))))))

(defun spdx-update ()
  (cl-destructuring-bind (release-date license-ids)
      (spdx-update--get-license-ids)
    (with-temp-buffer
      (insert
       ";;; spdx-data.el --- SPDX license database"
       " -*- lexical-binding: t -*-" "\n"
       ";;" "\n"
       ";; Copyright 2020 SPDX" "\n"
       ";; SPDX-License-Identifier: MIT" "\n"
       ";;" "\n"
       ";;; Commentary:" "\n"
       ";;" "\n"
       ";; Automatically converted from licenses.json." "\n"
       ";;" "\n"
       ";;; Code:" "\n"
       "\n"
       (format "(defconst spdx-data-release-date %S)\n" release-date)
       "\n"
       "(defconst spdx-data-license-identifiers" "\n"
       "  '(\n")
      (dolist (license-id (butlast license-ids))
        (insert (format "    %S\n" license-id)))
      (let ((license-id (car (last license-ids))))
        (insert (format "    %S))\n" license-id)))
      (insert "\n"
              "(provide 'spdx-data)" "\n"
              "\n"
              ";;; spdx-data.el ends here" "\n")
      (write-region (point-min) (point-max) "spdx-data.el.new"))))
