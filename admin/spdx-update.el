;;; spdx-update.el --- Update the SPDX license database -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-handlers)

(defun spdx-update--valid-release-date-p (date)
  "Return non-nil if DATE is an ISO date or a UTC timestamp."
  (and (stringp date)
       (string-match-p
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\)?\\'"
        date)
       (condition-case nil
           (let ((timestamp (> (length date) 10)))
             (equal date
                    (format-time-string
                     (if timestamp "%Y-%m-%dT%H:%M:%SZ" "%Y-%m-%d")
                     (encode-time
                      (if timestamp (string-to-number (substring date 17 19)) 0)
                      (if timestamp (string-to-number (substring date 14 16)) 0)
                      (if timestamp (string-to-number (substring date 11 13)) 0)
                      (string-to-number (substring date 8 10))
                      (string-to-number (substring date 5 7))
                      (string-to-number (substring date 0 4)) t)
                     t)))
         (error nil))))

(defun spdx-update--read-license-ids ()
  "Read and validate license data from JSON in the current buffer.
Return the release date, active identifiers, and deprecated identifiers."
  (let* ((json (json-parse-buffer :object-type 'alist :array-type 'array
                                  :null-object :null :false-object :false))
         (release-date (and (listp json) (alist-get 'releaseDate json)))
         (licenses (and (listp json) (alist-get 'licenses json)))
         (seen (make-hash-table :test #'equal))
         license-ids deprecated-ids)
    (skip-chars-forward " \t\r\n")
    (unless (eobp)
      (error "Unexpected content after SPDX JSON data"))
    (unless (spdx-update--valid-release-date-p release-date)
      (error "Invalid or missing SPDX releaseDate: %S" release-date))
    (unless (and (vectorp licenses) (> (length licenses) 0))
      (error "SPDX licenses must be a non-empty JSON array"))
    (mapc (lambda (license)
            (let ((id (and (listp license) (alist-get 'licenseId license)))
                  (deprecated (and (listp license)
                                   (alist-get 'isDeprecatedLicenseId license))))
              (unless (and (stringp id) (not (string-blank-p id)))
                (error "Invalid or missing SPDX licenseId: %S" id))
              (unless (memq deprecated '(t :false))
                (error "Invalid or missing isDeprecatedLicenseId for %S" id))
              (when (gethash id seen)
                (error "Duplicate SPDX licenseId: %S" id))
              (puthash id t seen)
              (if (eq deprecated t)
                  (push id deprecated-ids)
                (push id license-ids))))
          licenses)
    (cl-flet ((string-ci< (a b) (string< (downcase a) (downcase b))))
      (list release-date
            (sort license-ids #'string-ci<)
            (sort deprecated-ids #'string-ci<)))))

(defun spdx-update--get-license-ids ()
  "Download the SPDX license list once and return its validated contents."
  (with-temp-buffer
    (url-insert-file-contents
     (concat "https://raw.githubusercontent.com/"
             "spdx/license-list-data"
             "/master/"
             "json/licenses.json"))
    (goto-char (point-min))
    (spdx-update--read-license-ids)))

(defun spdx-update--insert-list (name xs)
  (insert (format "(defconst %S\n" name)
          "  '(")
  (dolist (x xs) (insert (format "\n    %S" x)))
  (insert "))\n"))

(defun spdx-update (&optional output-file)
  "Download and validate SPDX data, then write it to OUTPUT-FILE.
OUTPUT-FILE defaults to spdx-data.el.new.  Invalid data signals an error
before the output file is written."
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
      (write-region (point-min) (point-max)
                    (or output-file "spdx-data.el.new")))))

(provide 'spdx-update)
;;; spdx-update.el ends here
