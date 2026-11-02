;;; spdx-test.el --- Tests for SPDX insertion -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'spdx)

(defconst spdx-test--source-directory
  (file-name-directory (directory-file-name
                        (file-name-directory load-file-name))))

(ert-deftest spdx-test-project-autoload-in-fresh-emacs ()
  "Project detection must work before project.el has been loaded."
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "-Q" "--batch" "-L" spdx-test--source-directory
            "-l" "spdx" "--eval"
            (prin1-to-string
             '(progn
                (unless (autoloadp (symbol-function 'project-current))
                  (error "Expected project-current to be autoloaded"))
                (setq spdx-project-detection 'project
                      project-find-functions
                      (list (lambda (dir) (cons 'transient dir))))
                (unless (equal (spdx--guess-project-name)
                               (file-name-nondirectory
                                (directory-file-name default-directory)))
                  (error "Incorrect project name")))))))
      (ert-info ((buffer-string))
        (should (eql status 0))))))

(ert-deftest spdx-test-cancel-license-inserts-nothing ()
  (dolist (command '(spdx-insert-spdx tempo-template-spdx
                    spdx-insert-spdx-only spdx-insert-spdx-identifier-only
                    spdx-insert-spdx-copyright tempo-template-spdx-copyright))
    (with-temp-buffer
      (c-mode)
      (insert "int value;\n")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'spdx-copyright-format)
                 (lambda () "Copyright (C) 2026 Alice")))
        (should (eq (condition-case nil (funcall command)
                      (quit 'cancelled))
                    'cancelled))
        (should (equal (buffer-string) "int value;\n"))))))

(ert-deftest spdx-test-cancel-copyright-inserts-nothing ()
  (dolist (command '(spdx-insert-copyright tempo-template-copyright
                    spdx-insert-spdx-copyright tempo-template-spdx-copyright))
    (with-temp-buffer
      (c-mode)
      (insert "int value;\n")
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'spdx-make-default-copyright)
                 (lambda () "(C) 2026 Alice")))
        (should (eq (condition-case nil (funcall command)
                      (quit 'cancelled))
                    'cancelled))
        (should (equal (buffer-string) "int value;\n"))))))

(ert-deftest spdx-test-cancel-tempo-expansion-inserts-no-header ()
  (dolist (tag '("spdx" "cpy" "spdxcpy"))
    (with-temp-buffer
      (c-mode)
      (spdx-tempo-setup)
      (insert "int value;\n" tag)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'spdx-copyright-format)
                 (lambda ()
                   (if (equal tag "cpy")
                       (signal 'quit nil)
                     "Copyright (C) 2026 Alice"))))
        (should (eq (condition-case nil (tempo-expand-if-complete)
                      (quit 'cancelled))
                    'cancelled))
        ;; Tempo removes its tag before evaluating our template.
        (should (equal (buffer-string) "int value;\n"))))))

(ert-deftest spdx-test-successful-commented-insertion ()
  (dolist (mode '(emacs-lisp-mode c-mode))
    (dolist (entry '((spdx-insert-spdx . ("SPDX-License-Identifier: MIT"))
                     (spdx-insert-copyright . ("Copyright (C) 2026 Alice"))
                     (spdx-insert-spdx-copyright
                      . ("Copyright (C) 2026 Alice"
                         "SPDX-License-Identifier: MIT"))))
      (with-temp-buffer
        (funcall mode)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "MIT"))
                  ((symbol-function 'spdx-copyright-format)
                   (lambda () "Copyright (C) 2026 Alice")))
          (funcall (car entry))
          (should (equal (buffer-string)
                         (mapconcat
                          (lambda (line)
                            (if (eq mode 'c-mode)
                                (concat "/* " line " */\n")
                              (concat ";; " line "\n")))
                          (cdr entry) ""))))))))

(ert-deftest spdx-test-text-mode-comment-initialization ()
  (dolist (entry '(("spdx" spdx-insert-spdx
                    "# SPDX-License-Identifier: MIT\n")
                   ("cpy" spdx-insert-copyright
                    "# Copyright (C) 2026 Alice\n")
                   ("spdxcpy" spdx-insert-spdx-copyright
                    "# Copyright (C) 2026 Alice\n# SPDX-License-Identifier: MIT\n")))
    (dolist (expand '(nil t))
      (with-temp-buffer
        (text-mode)
        (should-not comment-start)
        (let ((prompts 0))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) (cl-incf prompts) "# "))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "MIT"))
                    ((symbol-function 'spdx-copyright-format)
                     (lambda () "Copyright (C) 2026 Alice")))
            (if expand
                (progn
                  (spdx-tempo-setup)
                  (insert (nth 0 entry))
                  (should (tempo-expand-if-complete)))
              (funcall (nth 1 entry)))
            (should (= prompts 1))
            (should (equal (buffer-string) (nth 2 entry)))))))))

(ert-deftest spdx-test-uncommented-insertion-needs-no-comment-syntax ()
  (dolist (entry '((spdx-insert-spdx-only . "SPDX-License-Identifier: MIT")
                   (spdx-insert-spdx-identifier-only . "MIT")))
    (with-temp-buffer
      (text-mode)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (ert-fail "Unexpected comment prompt")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "MIT")))
        (funcall (car entry))
        (should (equal (buffer-string) (cdr entry)))
        (should-not comment-start)))))

(ert-deftest spdx-test-empty-license-inserts-nothing ()
  (dolist (command '(spdx-insert-spdx spdx-insert-spdx-only
                    spdx-insert-spdx-identifier-only spdx-insert-spdx-copyright))
    (with-temp-buffer
      (c-mode)
      (insert "int value;\n")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) ""))
                ((symbol-function 'spdx-copyright-format)
                 (lambda () "Copyright (C) 2026 Alice")))
        (should-error (funcall command) :type 'user-error)
        (should (equal (buffer-string) "int value;\n"))))))

(ert-deftest spdx-test-real-completion-rejects-empty-input ()
  "Exercise completing-read itself with an empty line on standard input."
  (with-temp-buffer
    (insert "\n")
    (let ((status
           (call-process-region
            (point-min) (point-max)
            (expand-file-name invocation-name invocation-directory)
            t t nil "-Q" "--batch" "-L" spdx-test--source-directory
            "-l" "spdx" "--eval"
            (prin1-to-string
             '(condition-case nil
                  (progn (spdx-identifier-only) (kill-emacs 1))
                (user-error (kill-emacs 0)))))))
      (ert-info ((buffer-string))
        (should (eql status 0))))))

(provide 'spdx-test)
;;; spdx-test.el ends here
