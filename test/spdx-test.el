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
  (dolist (command '(spdx-insert-copyright tempo-template-spdx-copyright-only
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

(defconst spdx-test--license-commands
  '((spdx-insert-spdx . ";; SPDX-License-Identifier: %s\n")
    (spdx-insert-spdx-only . "SPDX-License-Identifier: %s")
    (spdx-insert-spdx-identifier-only . "%s")
    (spdx-insert-spdx-copyright
     . ";; Copyright (C) 2026 Alice\n;; SPDX-License-Identifier: %s\n")))

(ert-deftest spdx-test-explicit-license-skips-completion ()
  (dolist (entry spdx-test--license-commands)
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((copyright-prompts 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Unexpected license prompt")))
                  ((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _)
                     (cl-incf copyright-prompts)
                     "(C) 2026 Alice"))
                  ((symbol-function 'spdx-make-default-copyright)
                   (lambda () "(C) 2026 Alice")))
          (funcall (car entry) "MIT")
          (should (equal (buffer-string) (format (cdr entry) "MIT")))
          (should (= copyright-prompts
                     (if (eq (car entry) 'spdx-insert-spdx-copyright)
                         1
                       0))))))))

(ert-deftest spdx-test-invalid-explicit-license-fails-before-prompts ()
  (dolist (entry spdx-test--license-commands)
    (dolist (license '("" "Not-A-License" MIT 42 ("MIT")))
      (with-temp-buffer
        (text-mode)
        (insert "Existing text\n")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Unexpected license prompt")))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (ert-fail "Unexpected comment prompt")))
                  ((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) (ert-fail "Unexpected copyright prompt"))))
          (should-error (funcall (car entry) license) :type 'user-error)
          (should (equal (buffer-string) "Existing text\n")))))))

(ert-deftest spdx-test-explicit-license-does-not-affect-later-prompts ()
  (dolist (entry spdx-test--license-commands)
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((prompts 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (cl-incf prompts) "Apache-2.0"))
                  ((symbol-function 'spdx-copyright-format)
                   (lambda () "Copyright (C) 2026 Alice")))
          (funcall (car entry) "MIT")
          (should (= prompts 0))
          ;; Expand through Tempo directly: a public command would rebind
          ;; the license and could hide a value leaked by the previous call.
          (erase-buffer)
          (spdx-tempo-setup)
          (insert "spdxi")
          (should (tempo-expand-if-complete))
          (should (equal (buffer-string) "Apache-2.0"))
          (should (= prompts 1))
          (dolist (args '(() (nil)))
            (erase-buffer)
            (apply (car entry) args)
            (should (equal (buffer-string)
                           (format (cdr entry) "Apache-2.0"))))
          (should (= prompts 3)))))))

(ert-deftest spdx-test-explicit-license-obeys-deprecated-filter ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected license prompt"))))
      (let ((spdx-ignore-deprecated nil))
        (spdx-insert-spdx-identifier-only "GPL-2.0")
        (should (equal (buffer-string) "GPL-2.0")))
      (let ((spdx-ignore-deprecated t))
        (should-error (spdx-insert-spdx-identifier-only "GPL-2.0")
                      :type 'user-error)
        (should (equal (buffer-string) "GPL-2.0"))))))

(ert-deftest spdx-test-cancel-copyright-with-explicit-license ()
  (with-temp-buffer
    (c-mode)
    (insert "int value;\n")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected license prompt")))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'spdx-make-default-copyright)
               (lambda () "(C) 2026 Alice")))
      (should (eq (condition-case nil
                      (spdx-insert-spdx-copyright "MIT")
                    (quit 'cancelled))
                  'cancelled))
      (should (equal (buffer-string) "int value;\n")))
    ;; A cancelled call must also restore the dynamically bound license.
    (let ((prompts 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (cl-incf prompts) "Apache-2.0")))
        (spdx-tempo-setup)
        (insert "spdxi")
        (should (tempo-expand-if-complete))
        (should (= prompts 1))
        (should (equal (buffer-string) "int value;\nApache-2.0"))))))

(ert-deftest spdx-test-format-explicit-license ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (ert-fail "Unexpected license prompt"))))
    (should (equal (spdx-identifier-only "MIT") "MIT"))
    (should (equal (spdx-license-format "MIT") "SPDX-License-Identifier: MIT"))
    (should-error (spdx-identifier-only "") :type 'user-error)
    (should-error (spdx-license-format "Not-A-License") :type 'user-error)))

(ert-deftest spdx-test-user-copyright-does-not-detect-project ()
  (let ((spdx-copyright-holder 'user))
    (cl-letf (((symbol-function 'spdx--guess-project-name)
               (lambda () (ert-fail "Unexpected project detection")))
              ((symbol-function 'spdx--guess-user-name)
               (lambda () "Alice")))
      (should (equal (spdx-get-default-copyright-holder) "Alice")))))

(ert-deftest spdx-test-project-copyright-does-not-look-up-user ()
  (let ((spdx-copyright-holder 'project))
    (dolist (project '("Example" nil))
      (cl-letf (((symbol-function 'spdx--guess-project-name)
                 (lambda () project))
                ((symbol-function 'spdx--guess-user-name)
                 (lambda () (ert-fail "Unexpected user lookup"))))
        (should (equal (spdx-get-default-copyright-holder) project))))))

(ert-deftest spdx-test-auto-copyright-prefers-project-without-user-lookup ()
  (let ((spdx-copyright-holder 'auto))
    (cl-letf (((symbol-function 'spdx--guess-project-name)
               (lambda () "Example"))
              ((symbol-function 'spdx--guess-user-name)
               (lambda () (ert-fail "Unexpected user lookup"))))
      (should (equal (spdx-get-default-copyright-holder) "Example Authors")))))

(ert-deftest spdx-test-auto-copyright-falls-back-to-user ()
  (let ((spdx-copyright-holder 'auto))
    (cl-letf (((symbol-function 'spdx--guess-project-name) (lambda () nil))
              ((symbol-function 'spdx--guess-user-name) (lambda () "Alice")))
      (should (equal (spdx-get-default-copyright-holder) "Alice")))))

(ert-deftest spdx-test-ffip-project-name-is-directory-basename ()
  (let ((spdx-project-detection 'ffip))
    (dolist (root '("/tmp/example/" "/tmp/example" nil))
      (cl-letf (((symbol-function 'ffip-get-project-root-directory)
                 (lambda () root)))
        (should (equal (spdx--guess-project-name) (and root "example")))))))

(ert-deftest spdx-test-copyright-template-does-not-collide ()
  "Other copyright templates can be defined before or after loading SPDX."
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "-Q" "--batch" "-L" spdx-test--source-directory
            "--eval"
            (prin1-to-string
             '(progn
                (require 'tempo)
                (tempo-define-template "copyright" '("Before SPDX"))
                (require 'spdx)
                (unless (equal (symbol-value 'tempo-template-copyright)
                               '("Before SPDX"))
                  (error "SPDX replaced another template"))
                (tempo-define-template "copyright" '("After SPDX"))
                (fset 'spdx-copyright-format (lambda () "Copyright Alice"))
                (with-temp-buffer
                  (emacs-lisp-mode)
                  (spdx-insert-copyright)
                  (unless (equal (buffer-string) ";; Copyright Alice\n")
                    (error "Copyright command used the wrong template"))
                  (erase-buffer)
                  (spdx-tempo-setup)
                  (insert "cpy")
                  (tempo-expand-if-complete)
                  (unless (equal (buffer-string) ";; Copyright Alice\n")
                    (error "Copyright tag used the wrong template"))
                  (erase-buffer)
                  (tempo-template-copyright)
                  (unless (equal (buffer-string) "After SPDX")
                    (error "Other template was modified"))))))))
      (ert-info ((buffer-string))
        (should (eql status 0))))))

(ert-deftest spdx-test-auto-project-tries-backends-in-order ()
  (let ((spdx-project-detection 'auto))
    (dolist (empty '(nil "" " \t"))
      (let (calls)
        (cl-letf (((symbol-function 'ffip-get-project-root-directory)
                   (lambda () (push 'ffip calls) nil))
                  ((symbol-function 'projectile-project-name)
                   (lambda () (push 'projectile calls) empty))
                  ((symbol-function 'project-current)
                   (lambda () (push 'project calls) '(transient . "/tmp/project/")))
                  ((symbol-function 'project-name) (lambda (_) "project")))
          (should (equal (spdx--guess-project-name) "project"))
          (should (equal (reverse calls) '(ffip projectile project))))))))

(ert-deftest spdx-test-auto-project-stops-after-success ()
  (let ((spdx-project-detection 'auto))
    (cl-letf (((symbol-function 'ffip-get-project-root-directory)
               (lambda () "/tmp/first/"))
              ((symbol-function 'projectile-project-name)
               (lambda () (ert-fail "Unexpected second backend")))
              ((symbol-function 'project-current)
               (lambda () (ert-fail "Unexpected third backend"))))
      (should (equal (spdx--guess-project-name) "first")))))

(ert-deftest spdx-test-auto-project-skips-unavailable-backends ()
  (let ((spdx-project-detection 'auto))
    (cl-letf (((symbol-function 'ffip-get-project-root-directory) nil)
              ((symbol-function 'projectile-project-name) nil)
              ((symbol-function 'project-current) (lambda () nil)))
      (should (equal (spdx--project-backends) '(project)))
      (should-not (spdx--guess-project-name)))))

(ert-deftest spdx-test-auto-project-all-backends-empty ()
  (let ((spdx-project-detection 'auto))
    (cl-letf (((symbol-function 'ffip-get-project-root-directory) (lambda () nil))
              ((symbol-function 'projectile-project-name) (lambda () ""))
              ((symbol-function 'project-current) (lambda () nil)))
      (should-not (spdx--guess-project-name)))))

(ert-deftest spdx-test-explicit-and-disabled-project-detection ()
  (cl-letf (((symbol-function 'ffip-get-project-root-directory) (lambda () nil))
            ((symbol-function 'projectile-project-name)
             (lambda () (ert-fail "Unexpected backend fallback"))))
    (let ((spdx-project-detection 'ffip))
      (should-not (spdx--guess-project-name)))
    (let ((spdx-project-detection nil))
      (cl-letf (((symbol-function 'ffip-get-project-root-directory)
                 (lambda () (ert-fail "Project detection is disabled"))))
        (should-not (spdx--guess-project-name))))))

(ert-deftest spdx-test-auto-project-propagates-errors-and-quits ()
  (let ((spdx-project-detection 'auto))
    (dolist (condition '(error quit))
      (cl-letf (((symbol-function 'ffip-get-project-root-directory)
                 (lambda () (signal condition '("Backend failed"))))
                ((symbol-function 'projectile-project-name)
                 (lambda () (ert-fail "Unexpected fallback after failure"))))
        (should (eq (condition-case err (spdx--guess-project-name)
                      ((error quit) (car err)))
                    condition))))))

(provide 'spdx-test)
;;; spdx-test.el ends here
