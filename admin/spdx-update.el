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
           (license-ids '())
           (deprecated-ids '()))
      (mapc (lambda (license)
              (let ((license-id (cdr (assoc 'licenseId license)))
                    (deprecated-p
                     (eql t (cdr (assoc 'isDeprecatedLicenseId license)))))
                (if deprecated-p
                    (push license-id deprecated-ids)
                  (push license-id license-ids))))
            licenses)
      (cl-flet ((string-ci< (a b) (string< (downcase a) (downcase b))))
        (list release-date
              (sort license-ids #'string-ci<)
              (sort deprecated-ids #'string-ci<))))))

(defun spdx-update--insert-list (name xs)
  (insert (format "(defconst %S\n" name)
          "  '(")
  (dolist (x xs) (insert (format "\n    %S" x)))
  (insert "))\n"))

(defun spdx-update ()
  (cl-destructuring-bind (release-date license-ids deprecated-ids)
      (spdx-update--get-license-ids)
    (with-temp-buffer
      (insert
       ";;; spdx-data.el --- SPDX license database"
       " -*- lexical-binding: t -*-" "\n"
       "\n"
       ";; Copyright 2020 SPDX" "\n"
       ";; SPDX-License-Identifier: MIT" "\n"
       "\n"
       ";;; Commentary:" "\n"
       "\n"
       ";; Automatically converted from licenses.json." "\n"
       "\n"
       ";;; Code:" "\n"
       "\n"
       (format "(defconst spdx-data-release-date %S)\n" release-date)
       "\n")
      (spdx-update--insert-list 'spdx-data-license-identifiers
                                license-ids)
      (insert "\n")
      (spdx-update--insert-list 'spdx-data-deprecated-license-identifiers
                                deprecated-ids)
      (insert "\n"
              "(provide 'spdx-data)" "\n"
              "\n"
              ";;; spdx-data.el ends here" "\n")
      (write-region (point-min) (point-max) "spdx-data.el.new"))))
