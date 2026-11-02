;;; spdx-update-test.el --- Tests for SPDX data updates -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'spdx-update)

(defconst spdx-update-test--source-directory
  (file-name-directory (directory-file-name
                        (file-name-directory load-file-name))))

(defconst spdx-update-test--valid-json
  "{\"releaseDate\":\"2026-10-28T00:00:00Z\",\"licenses\":[{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":false}]}")

(defun spdx-update-test--parse (json)
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (spdx-update--read-license-ids)))

(ert-deftest spdx-update-test-valid-data-is-grouped-and-sorted ()
  (should
   (equal
    (spdx-update-test--parse
     "{\"releaseDate\":\"2026-10-28T00:00:00Z\",\"licenses\":[{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":false},{\"licenseId\":\"GPL-2.0\",\"isDeprecatedLicenseId\":true},{\"licenseId\":\"Apache-2.0\",\"isDeprecatedLicenseId\":false}]}")
    '("2026-10-28T00:00:00Z" ("Apache-2.0" "MIT") ("GPL-2.0")))))

(ert-deftest spdx-update-test-rejects-invalid-document ()
  (dolist (json '("{}" "[]" "null" "{"
                  "{\"releaseDate\":null,\"licenses\":[]}"
                  "{\"releaseDate\":\"\",\"licenses\":[]}"
                  "{\"releaseDate\":\"2026-02-30T00:00:00Z\",\"licenses\":[]}"
                  "{\"releaseDate\":\"2026-10-28\"}"
                  "{\"releaseDate\":\"2026-10-28\",\"licenses\":null}"
                  "{\"releaseDate\":\"2026-10-28\",\"licenses\":{}}"
                  "{\"releaseDate\":\"2026-10-28\",\"licenses\":[]}"))
    (should-error (spdx-update-test--parse json)))
  (should-error (spdx-update-test--parse
                 (concat spdx-update-test--valid-json " {}"))))

(ert-deftest spdx-update-test-validates-release-date ()
  (dolist (date '("2024-02-29" "2026-10-28T00:00:00Z"))
    (should (spdx-update--valid-release-date-p date)))
  (dolist (date '(nil "" "2026-02-29" "2026-13-01" "2026-10-28T24:00:00Z"
                  "2026-10-28T00:60:00Z" "2026-10-28 garbage"))
    (should-not (spdx-update--valid-release-date-p date))))

(ert-deftest spdx-update-test-rejects-invalid-license-records ()
  (dolist (records '("null" "false" "42" "{}"
                     "{\"licenseId\":null,\"isDeprecatedLicenseId\":false}"
                     "{\"licenseId\":42,\"isDeprecatedLicenseId\":false}"
                     "{\"licenseId\":\"\",\"isDeprecatedLicenseId\":false}"
                     "{\"licenseId\":\"  \",\"isDeprecatedLicenseId\":false}"
                     "{\"licenseId\":\"MIT\"}"
                     "{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":null}"
                     "{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":0}"
                     "{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":\"false\"}"
                     "{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":false},{\"licenseId\":\"MIT\",\"isDeprecatedLicenseId\":true}"))
    (should-error
     (spdx-update-test--parse
      (format "{\"releaseDate\":\"2026-10-28\",\"licenses\":[%s]}" records)))))

(ert-deftest spdx-update-test-invalid-data-preserves-output ()
  (let ((output (make-temp-file "spdx-update-output-")))
    (unwind-protect
        (progn
          (write-region "Existing data" nil output nil 'silent)
          (cl-letf (((symbol-function 'url-insert-file-contents)
                     (lambda (&rest _) (insert "{}"))))
            (should-error (spdx-update output)))
          (with-temp-buffer
            (insert-file-contents output)
            (should (equal (buffer-string) "Existing data"))))
      (delete-file output))))

(defun spdx-update-test--command (&rest args)
  "Run ARGS in the temporary repository and return their standard output."
  (with-temp-buffer
    (let ((status (apply #'call-process (car args) nil t nil (cdr args))))
      (ert-info ((buffer-string))
        (should (eql status 0)))
      (string-trim (buffer-string)))))

(ert-deftest spdx-update-test-script-updates-on-content-change ()
  "Check real shell and Git behavior with local JSON instead of a network."
  (let* ((directory (make-temp-file "spdx-update-repo-" t))
         (default-directory (file-name-as-directory directory))
         (process-environment (copy-sequence process-environment))
         (json-file (expand-file-name "licenses.json"))
         (loader (expand-file-name "fixture-loader.el"))
         (wrapper (expand-file-name "fixture-emacs"))
         (output (expand-file-name "spdx-data.el")))
    (unwind-protect
        (progn
          (make-directory "admin")
          (dolist (name '("spdx-update.el" "spdx-update.sh"))
            (copy-file (expand-file-name (concat "admin/" name)
                                        spdx-update-test--source-directory)
                       (concat "admin/" name)))
          (write-region spdx-update-test--valid-json nil json-file nil 'silent)
          (cl-letf (((symbol-function 'url-insert-file-contents)
                     (lambda (&rest _) (insert-file-contents json-file))))
            (spdx-update output))
          (with-temp-file loader
            (insert "(require 'url-handlers)\n"
                    "(defun url-insert-file-contents (&rest _)\n"
                    "  (write-region \"fetch\\n\" nil \"../fetches\" t 'silent)\n"
                    "  (insert-file-contents (getenv \"SPDX_TEST_JSON\")))\n"))
          (with-temp-file wrapper
            (insert "#!/bin/sh\n"
                    "exec \"$SPDX_TEST_EMACS\" -l \"$SPDX_TEST_LOADER\" \"$@\"\n"))
          (set-file-modes wrapper #o755)
          (setenv "EMACS" wrapper)
          (setenv "SPDX_TEST_EMACS"
                  (expand-file-name invocation-name invocation-directory))
          (setenv "SPDX_TEST_LOADER" loader)
          (setenv "SPDX_TEST_JSON" json-file)
          (spdx-update-test--command "git" "init" "-q")
          (spdx-update-test--command "git" "config" "user.name" "SPDX Test")
          (spdx-update-test--command "git" "config" "user.email" "spdx@example.invalid")
          (spdx-update-test--command "git" "config" "commit.gpgsign" "false")
          (spdx-update-test--command "git" "config" "core.hooksPath" "/dev/null")
          (spdx-update-test--command "git" "add" "spdx-data.el")
          (spdx-update-test--command "git" "commit" "-qm" "Initial data")
          (let ((initial-head (spdx-update-test--command "git" "rev-parse" "HEAD")))
            (spdx-update-test--command "sh" "admin/spdx-update.sh")
            (should (equal initial-head
                           (spdx-update-test--command "git" "rev-parse" "HEAD")))
            ;; A date-only update must not create a commit or dirty the file.
            (let ((initial-data (with-temp-buffer
                                  (insert-file-contents output)
                                  (buffer-string))))
              (write-region (string-replace "2026-10-28" "2026-10-29"
                                            spdx-update-test--valid-json)
                            nil json-file nil 'silent)
              (spdx-update-test--command "sh" "admin/spdx-update.sh")
              (should (equal initial-head
                             (spdx-update-test--command "git" "rev-parse" "HEAD")))
              (with-temp-buffer
                (insert-file-contents output)
                (should (equal (buffer-string) initial-data))))
            ;; The release date is unchanged; only license content changes.
            (write-region (string-replace "MIT" "Apache-2.0"
                                          spdx-update-test--valid-json)
                          nil json-file nil 'silent)
            (spdx-update-test--command "sh" "admin/spdx-update.sh")
            (let ((updated-head (spdx-update-test--command "git" "rev-parse" "HEAD"))
                  (updated-data (with-temp-buffer
                                  (insert-file-contents output)
                                  (buffer-string))))
              (should-not (equal initial-head updated-head))
              (should (string-match-p "Apache-2.0" updated-data))
              (should (= (file-modes output) #o644))
              (write-region "{}" nil json-file nil 'silent)
              (should-not (eql (call-process "sh" nil nil nil "admin/spdx-update.sh") 0))
              (should (equal updated-head
                             (spdx-update-test--command "git" "rev-parse" "HEAD")))
              (with-temp-buffer
                (insert-file-contents output)
                (should (equal (buffer-string) updated-data)))))
          (with-temp-buffer
            (insert-file-contents "fetches")
            (should (equal (buffer-string) "fetch\nfetch\nfetch\nfetch\n")))
          (should-not (directory-files directory nil "\\`\\.spdx-data\\.")))
      (delete-directory directory t))))

(provide 'spdx-update-test)
;;; spdx-update-test.el ends here
