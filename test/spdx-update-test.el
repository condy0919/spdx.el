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

(defun spdx-update-test--with-repository (function)
  "Call FUNCTION with JSON and output paths in a temporary Git repository."
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
          (write-region ";;; Package fixture\n" nil "spdx.el" nil 'silent)
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
          (spdx-update-test--command "git" "add" "spdx-data.el" "spdx.el")
          (spdx-update-test--command "git" "commit" "-qm" "Initial data")
          (funcall function json-file output)
          (should-not (directory-files directory nil "\\`\\.spdx-data\\.")))
      (delete-directory directory t))))

(defun spdx-update-test--file-contents (file)
  "Return the literal contents of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun spdx-update-test--repository-state ()
  "Return HEAD, index bytes, and the package files' contents and timestamps."
  (list (spdx-update-test--command "git" "rev-parse" "HEAD")
        (spdx-update-test--file-contents ".git/index")
        (mapcar (lambda (file)
                  (list (spdx-update-test--file-contents file)
                        (file-attribute-modification-time
                         (file-attributes file))))
                '("spdx-data.el" "spdx.el"))))

(ert-deftest spdx-update-test-script-updates-on-content-change ()
  "Check real shell and Git behavior with local JSON instead of a network."
  (spdx-update-test--with-repository
   (lambda (json-file output)
     ;; Repeated date-only updates must preserve file bytes, timestamps,
     ;; HEAD and index; they must never update the package.
     (let ((initial-state (spdx-update-test--repository-state)))
       (dolist (date '("2026-10-28" "2026-10-29" "2026-10-30"))
         (write-region (string-replace "2026-10-28" date
                                       spdx-update-test--valid-json)
                       nil json-file nil 'silent)
         (spdx-update-test--command "sh" "admin/spdx-update.sh")
         (should (equal initial-state (spdx-update-test--repository-state)))))
     ;; An unchanged release date must not hide license content changes.
     (let ((initial-head (spdx-update-test--command "git" "rev-parse" "HEAD"))
           (package (spdx-update-test--file-contents "spdx.el")))
       (write-region (string-replace "MIT" "Apache-2.0"
                                     spdx-update-test--valid-json)
                     nil json-file nil 'silent)
       (spdx-update-test--command "sh" "admin/spdx-update.sh")
       (should-not (equal initial-head
                          (spdx-update-test--command "git" "rev-parse" "HEAD")))
       (should (string-match-p "Apache-2.0"
                               (spdx-update-test--file-contents output)))
       (should (= (file-modes output) #o644))
       (should (equal package (spdx-update-test--file-contents "spdx.el"))))
     ;; Moving an ID to the deprecated list is also a real data change.
     (let ((previous-head (spdx-update-test--command "git" "rev-parse" "HEAD")))
       (write-region (string-replace "false" "true"
                                     (spdx-update-test--file-contents json-file))
                     nil json-file nil 'silent)
       (spdx-update-test--command "sh" "admin/spdx-update.sh")
       (should-not (equal previous-head
                          (spdx-update-test--command "git" "rev-parse" "HEAD")))
       (with-temp-buffer
         (insert-file-contents output)
         (goto-char (point-min))
         (search-forward "(defconst spdx-data-deprecated-license-identifiers")
         (should (search-forward "Apache-2.0" nil t))))
     (let ((updated-state (spdx-update-test--repository-state)))
       (write-region "{}" nil json-file nil 'silent)
       (should-not (eql (call-process "sh" nil nil nil "admin/spdx-update.sh") 0))
       (should (equal updated-state (spdx-update-test--repository-state))))
     (should (equal (spdx-update-test--file-contents "fetches")
                    (apply #'concat (make-list 6 "fetch\n")))))))

(ert-deftest spdx-update-test-script-preserves-data-on-comparison-failure ()
  "Comparison errors must not overwrite files or the real Git index."
  (spdx-update-test--with-repository
   (lambda (json-file _output)
     (write-region (string-replace "MIT" "Apache-2.0"
                                   spdx-update-test--valid-json)
                   nil json-file nil 'silent)
     ;; Include a staged user change to catch any accidental index writes.
     (write-region ";;; Staged package change\n" nil "spdx.el" nil 'silent)
     (spdx-update-test--command "git" "add" "spdx.el")
     (let ((initial-state (spdx-update-test--repository-state))
           (git (executable-find "git")))
       (dolist (failure '(("diff" "" 2) ("git" "diff" 128) ("git" "show" 128)))
         (pcase-let* ((`(,command ,subcommand ,status) failure)
                      (bin (make-temp-file
                            (expand-file-name "failing-tools-") t))
                      (wrapper (expand-file-name command bin))
                      (process-environment (copy-sequence process-environment)))
           (with-temp-file wrapper
             (insert "#!/bin/sh\n")
             (if (equal command "git")
                 (insert (format "if [ \"$1\" = %s ]; then exit %d; fi\n"
                                 subcommand status)
                         "exec \"$SPDX_TEST_GIT\" \"$@\"\n")
               (insert (format "exit %d\n" status))))
           (set-file-modes wrapper #o755)
           (setenv "SPDX_TEST_GIT" git)
           (setenv "PATH" (concat bin path-separator (getenv "PATH")))
           (should (eql (call-process "sh" nil nil nil "admin/spdx-update.sh")
                        status))
           (should (equal initial-state (spdx-update-test--repository-state)))
           (should-not (directory-files default-directory nil
                                        "\\`\\.spdx-data\\."))))))))

(ert-deftest spdx-update-test-script-keeps-local-data-when-candidate-matches-head ()
  "An already committed candidate should leave local edits and staging alone."
  (spdx-update-test--with-repository
   (lambda (_json-file output)
     (write-region (string-replace "MIT" "Apache-2.0"
                                   (spdx-update-test--file-contents output))
                   nil output nil 'silent)
     (spdx-update-test--command "git" "add" "spdx-data.el")
     (let ((initial-state (spdx-update-test--repository-state)))
       (spdx-update-test--command "sh" "admin/spdx-update.sh")
       (should (equal initial-state (spdx-update-test--repository-state)))))))

(provide 'spdx-update-test)
;;; spdx-update-test.el ends here
