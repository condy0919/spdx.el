;;; spdx.el --- Insert SPDX license and copyright headers -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021, 2022 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: license, tools
;; URL: https://github.com/condy0919/spdx.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; # spdx.el

;; `spdx.el` provides SPDX license header and copyright insertion.

;; ## Installation

;; Put `spdx.el` in your Emacs system. Add the following to your `.emacs`:

;; ```elisp
;; (require 'spdx)
;; (define-key prog-mode-map (kbd "C-c i l") #'spdx-insert-spdx)
;; ```

;; Or Use [use-package](https://github.com/jwiegley/use-package) with
;; [straight.el](https://github.com/raxod502/straight.el)

;; ``` emacs-lisp
;; (use-package spdx
;;   :ensure t
;;   :straight (:host github :repo "condy0919/spdx.el")
;;   :bind (:map prog-mode-map
;;          ("C-c i l" . spdx-insert-spdx))
;;   :custom
;;   (spdx-copyright-holder 'auto)
;;   (spdx-project-detection 'auto))
;; ```

;; Then you can press `C-c i l` to trigger `spdx-insert-spdx`

;; Or manually run:

;;     M-x spdx-insert-spdx

;; Then, `spdx.el` will ask you to select a license. It's done by
;; `completing-read'.

;; After that, the license header will be written. An example follows.

;; `;; SPDX-License-Identifier: AGPL-1.0-only`

;; ## Customization

;; - `spdx-copyright-holder'
;; - `spdx-copyright-sign'
;; - `spdx-copyright-prefix'
;; - `spdx-project-detection'
;; - `spdx-ignore-deprecated'

;;; Code:

(require 'newcomment)
(require 'subr-x)
(require 'tempo)

(require 'spdx-data)

(defgroup spdx nil
  "SPDX license header inserter."
  :prefix "spdx-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/spdx.el"))

(defcustom spdx-copyright-holder 'auto
  "The copyright holder.

The priority of auto is `project' > `user'.
When explicitly set to project, signal an error if no project is found."
  :type '(choice (const auto)
                 (const user)
                 (const project))
  :group 'spdx)

(defcustom spdx-copyright-sign 'ascii
  "The type of copyright sign."
  :type '(choice (const ascii)
                 (const unicode)
                 (const none))
  :group 'spdx)

(defcustom spdx-copyright-prefix "Copyright "
  "The prefix of copyright.

Another reasonable prefix could be \"SPDX-FileCopyrightText: \"."
  :type 'string
  :group 'spdx)

;; Stole from `doom-modeline`
(defcustom spdx-project-detection 'auto
  "How to detect the project root.

The default priority is \='ffip' > \='projectile' > \='project'.
In auto mode, try each available backend until it returns a non-empty
project name.  Backend errors are not suppressed.
nil means not to use project information."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Find File in Project" ffip)
                 (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 (const :tag "Disable" nil))
  :group 'spdx)

(defcustom spdx-ignore-deprecated nil
  "Whether to ignore the deprecated licenses."
  :type 'boolean
  :group 'spdx)

(defun spdx--command-stdout-or-nil (command &rest args)
  "Internal helper to run an external program.

If COMMAND exists, run it with ARGS. If the program is
successful, return everything it wrote to standard output as a
string (trimming whitespace). Otherwise return nil."
  (let ((command (executable-find command)))
    (and command
         (with-temp-buffer
           (let ((status (apply #'call-process command
                                nil (current-buffer) nil args)))
             (and (equal 0 status)
                  (let ((output (string-trim (buffer-string))))
                    (and (not (string-blank-p output))
                         output))))))))

(defun spdx--guess-user-name-from-git ()
  "Get local or global user name from Git configuration."
  (spdx--command-stdout-or-nil "git" "config" "user.name"))

(defun spdx--guess-user-name ()
  "Guess a user name for the current buffer."
  (or (spdx--guess-user-name-from-git)
      user-full-name))

(defun spdx--project-backends ()
  "Return the configured available project backends in priority order."
  (when spdx-project-detection
    (let ((loaded
           (append (when (fboundp 'ffip-get-project-root-directory) '(ffip))
                   (when (fboundp 'projectile-project-name) '(projectile))
                   (when (fboundp 'project-current) '(project)))))
      (cond ((equal 'auto spdx-project-detection)
             loaded)
            ((member spdx-project-detection loaded)
             (list spdx-project-detection))
            (t
             (error "Unknown method: spdx-project-detection method %S not loaded"
                    spdx-project-detection))))))

(declare-function ffip-get-project-root-directory "ext:find-file-in-project" ())
(declare-function projectile-project-name "ext:projectile" ())
(declare-function project-name "project" (project))

(defun spdx--project-name (backend)
  "Get the current project name from BACKEND."
  (pcase backend
    ('ffip
     (let ((root (ffip-get-project-root-directory)))
       (when root
         (file-name-nondirectory (directory-file-name root)))))
    ('projectile
     (projectile-project-name))
    ('project
     (let ((proj (project-current)))
       (and proj (project-name proj))))
    (_ nil)))

(defun spdx--guess-project-name ()
  "Return the first non-blank project name from the configured backends.
Return nil if none finds a project.  Errors and quits propagate to the caller."
  (catch 'project-name
    (dolist (backend (spdx--project-backends))
      (let ((name (spdx--project-name backend)))
        (when (and name (not (string-blank-p name)))
          (throw 'project-name name))))))

(defun spdx-get-default-copyright-sign ()
  "Get the copyright sign to use for the current buffer.

ASCII, Unicode, or none."
  (pcase spdx-copyright-sign
    ('ascii "(C) ")
    ('unicode (concat (string #x00A9) " "))
    (_ "")))

(defun spdx-get-default-copyright-years ()
  "Get the copyright year(s) for the current buffer."
  (format-time-string "%Y"))

(defun spdx-get-default-copyright-holder ()
  "Get the copyright holder for the current buffer.
Only consult the sources needed by `spdx-copyright-holder'."
  (pcase spdx-copyright-holder
    ('auto
     (let ((proj (spdx--guess-project-name)))
       (if proj
           (concat proj " Authors")
         (spdx--guess-user-name))))
    ('user (spdx--guess-user-name))
    ('project (or (spdx--guess-project-name)
                  (user-error "No project found for the copyright holder")))
    (_ (error "Unknown spdx-copyright-holder: %S"
              spdx-copyright-holder))))

(defun spdx-make-default-copyright ()
  "Build a default SPDX Copyright line that can be edited by the user."
  (concat (spdx-get-default-copyright-sign)
          (spdx-get-default-copyright-years) " "
          (spdx-get-default-copyright-holder)))

(defun spdx-get-existing-copyright ()
  "Get existing SPDX Copyright line in current buffer as string.

Returns nil if no existing Copyright line is found."
  nil)

(defun spdx--copyright-argument (copyright no-prompt)
  "Validate COPYRIGHT without prompting, requiring it when NO-PROMPT is non-nil."
  (cond
   (copyright
    (unless (and (stringp copyright)
                 (not (string-blank-p copyright))
                 (not (string-match-p "[\r\n]" copyright)))
      (user-error "Copyright must be a non-empty single-line string")))
   (no-prompt
    (user-error "An explicit copyright is required when prompts are disabled")))
  copyright)

(defun spdx-copyright-format (&optional copyright no-prompt)
  "Format COPYRIGHT with `spdx-copyright-prefix', or prompt if it is nil.
COPYRIGHT is a non-blank single-line body without the prefix, such as
\"(C) 2026 Alice\".  A non-nil value bypasses copyright inference.
When NO-PROMPT is non-nil, require an explicit COPYRIGHT instead of prompting."
  (let ((copyright (spdx--copyright-argument copyright no-prompt))
        (prefix spdx-copyright-prefix))
    (concat prefix
            (or copyright
                (read-from-minibuffer
                 prefix
                 (or (spdx-get-existing-copyright)
                     (spdx-make-default-copyright)))))))

(defun spdx--validate-license-identifier (license)
  "Return LICENSE if it is an allowed SPDX identifier, otherwise signal an error.
Deprecated identifiers are allowed unless `spdx-ignore-deprecated' is non-nil."
  (unless (stringp license)
    (user-error "License identifier must be a string"))
  (when (string-empty-p license)
    (user-error "License identifier must not be empty"))
  (unless (or (member license spdx-data-license-identifiers)
              (and (not spdx-ignore-deprecated)
                   (member license spdx-data-deprecated-license-identifiers)))
    (user-error "Unknown or excluded SPDX license identifier: %S" license))
  license)

(defun spdx--license-argument (license no-prompt)
  "Validate LICENSE without prompting, requiring it when NO-PROMPT is non-nil."
  (if license
      (spdx--validate-license-identifier license)
    (when no-prompt
      (user-error "An explicit license is required when prompts are disabled"))))

(defun spdx-identifier-only (&optional license no-prompt)
  "Return LICENSE, or read a non-empty license identifier if it is nil.
A non-nil LICENSE must be an SPDX identifier allowed by
`spdx-ignore-deprecated' and is used without prompting.
When NO-PROMPT is non-nil, require an explicit LICENSE instead of prompting."
  (let ((identifier
         (or (spdx--license-argument license no-prompt)
             (completing-read
              "License: "
              (append spdx-data-license-identifiers
                      (unless spdx-ignore-deprecated
                        spdx-data-deprecated-license-identifiers))
              nil t))))
    (when (string-empty-p identifier)
      (user-error "License identifier must not be empty"))
    identifier))

(defun spdx-license-format (&optional license no-prompt)
  "Format a license header using LICENSE, or prompt if it is nil.
When NO-PROMPT is non-nil, require an explicit LICENSE instead of prompting.
See `spdx-identifier-only' for the allowed values of LICENSE."
  (concat "SPDX-License-Identifier: " (spdx-identifier-only license no-prompt)))

(defvar spdx--no-prompt nil
  "Non-nil while an insertion command must not prompt for missing input.")

(defun spdx-comment-start ()
  "Construct a comment start with padding."
  (comment-normalize-vars spdx--no-prompt)
  (when (and spdx--no-prompt
             (or (not comment-start) (string-blank-p comment-start)))
    (user-error "No comment syntax defined; configure it before inserting"))
  (let ((add (comment-add nil)))
    (comment-padright comment-start add)))

(defun spdx-comment-end ()
  "Construct a comment end with padding."
  (let ((add (comment-add nil)))
    (unless (string= "" comment-end)
      (comment-padleft comment-end add))))

(defvar spdx-tempo-tags nil
  "Tempo tags for SPDX license.")

(defvar spdx--license-identifier nil
  "License identifier dynamically bound while an insertion command runs.
Nil means that the Tempo template should prompt for a license.")

(defvar spdx--copyright-text nil
  "Copyright body dynamically bound while an insertion command runs.")

;; Evaluate all prompts in one expression before Tempo inserts any text.
(tempo-define-template "spdx"
  '((list 'l (spdx-comment-start)
          (spdx-license-format spdx--license-identifier spdx--no-prompt)
          (spdx-comment-end) '> 'n>))
  "spdx"
  "Insert a SPDX license header."
  'spdx-tempo-tags)

(tempo-define-template "spdx-only"
  '((spdx-license-format spdx--license-identifier spdx--no-prompt) >)
  "spdxo"
  "Insert a SPDX license header without comments."
  'spdx-tempo-tags)

(tempo-define-template "spdx-identifier-only"
  '((spdx-identifier-only spdx--license-identifier spdx--no-prompt) >)
  "spdxi"
  "Insert a SPDX identifier only."
  'spdx-tempo-tags)

(tempo-define-template "spdx-copyright-only"
  '((list 'l (spdx-comment-start)
          (spdx-copyright-format spdx--copyright-text spdx--no-prompt)
          (spdx-comment-end) '> 'n>))
  "cpy"
  "Insert a copyright header."
  'spdx-tempo-tags)

(tempo-define-template "spdx-copyright"
  '((list 'l (spdx-comment-start)
          (spdx-copyright-format spdx--copyright-text spdx--no-prompt)
          (spdx-comment-end) '> 'n>
          (spdx-comment-start)
          (spdx-license-format spdx--license-identifier spdx--no-prompt)
          (spdx-comment-end) '> 'n>))
  "spdxcpy"
  "Insert a SPDX license and copyright header."
  'spdx-tempo-tags)

;; Silence undefined warning
(declare-function tempo-template-spdx "spdx" (&optional arg))
(declare-function tempo-template-spdx-only "spdx" (&optional arg))
(declare-function tempo-template-spdx-identifier-only "spdx" (&optional arg))
(declare-function tempo-template-spdx-copyright-only "spdx" (&optional arg))
(declare-function tempo-template-spdx-copyright "spdx" (&optional arg))

;;;###autoload
(defun spdx-insert-spdx (&optional license no-prompt)
  "Insert a SPDX license header.
If LICENSE is nil, prompt for a license.  Otherwise, use LICENSE,
which must be an SPDX identifier allowed by `spdx-ignore-deprecated'.
When NO-PROMPT is non-nil, require LICENSE and configured comment syntax;
signal an error instead of prompting if either is missing."
  (interactive)
  (let ((spdx--license-identifier
         (spdx--license-argument license no-prompt))
        (spdx--no-prompt no-prompt))
    (tempo-template-spdx)))

;;;###autoload
(defun spdx-insert-spdx-only (&optional license no-prompt)
  "Insert a SPDX license without comments.
If LICENSE is nil, prompt for a license.  Otherwise, use LICENSE,
which must be an SPDX identifier allowed by `spdx-ignore-deprecated'.
When NO-PROMPT is non-nil, require an explicit LICENSE instead of prompting."
  (interactive)
  (let ((spdx--license-identifier
         (spdx--license-argument license no-prompt))
        (spdx--no-prompt no-prompt))
    (tempo-template-spdx-only)))

;;;###autoload
(defun spdx-insert-spdx-identifier-only (&optional license no-prompt)
  "Insert a SPDX license identifier only.
If LICENSE is nil, prompt for a license.  Otherwise, use LICENSE,
which must be an SPDX identifier allowed by `spdx-ignore-deprecated'.
When NO-PROMPT is non-nil, require an explicit LICENSE instead of prompting."
  (interactive)
  (let ((spdx--license-identifier
         (spdx--license-argument license no-prompt))
        (spdx--no-prompt no-prompt))
    (tempo-template-spdx-identifier-only)))

;;;###autoload
(defun spdx-insert-copyright (&optional copyright no-prompt)
  "Insert a copyright header using COPYRIGHT, or prompt if it is nil.
COPYRIGHT is a non-blank single-line body without `spdx-copyright-prefix'.
When NO-PROMPT is non-nil, require COPYRIGHT and configured comment syntax;
signal an error instead of prompting if either is missing."
  (interactive)
  (let ((spdx--copyright-text (spdx--copyright-argument copyright no-prompt))
        (spdx--no-prompt no-prompt))
    (tempo-template-spdx-copyright-only)))

;;;###autoload
(defun spdx-insert-spdx-copyright (&optional license copyright no-prompt)
  "Insert a SPDX license and copyright header.
If LICENSE is nil, prompt for a license.  Otherwise, use LICENSE,
which must be an SPDX identifier allowed by `spdx-ignore-deprecated'.
COPYRIGHT is a non-blank single-line body without `spdx-copyright-prefix'.
If it is nil, prompt for copyright information as usual.
When NO-PROMPT is non-nil, require both arguments and configured comment
syntax; signal an error instead of prompting if any is missing."
  (interactive)
  (let ((spdx--license-identifier
         (spdx--license-argument license no-prompt))
        (spdx--copyright-text (spdx--copyright-argument copyright no-prompt))
        (spdx--no-prompt no-prompt))
    (tempo-template-spdx-copyright)))

;;;###autoload
(defun spdx-tempo-setup ()
  "Setup tempo template for SPDX license header."
  (tempo-use-tag-list 'spdx-tempo-tags))

(provide 'spdx)

;;; spdx.el ends here
