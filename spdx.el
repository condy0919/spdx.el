;;; spdx.el --- Insert SPDX license and copyright headers -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: license, tools
;; URL: https://github.com/condy0919/spdx.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))

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

The priority of auto is `project' > `user'."
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

;; Stole from `doom-modeline`
(defcustom spdx-project-detection 'auto
  "How to detect the project root.

The default priority is `ffip' > `projectile' > 'project'.
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

(defun spdx--detect-project-type ()
  "Detect project type for current buffer.

Returns one of the symbols 'ffip, 'projectile, 'project, or nil
if we can't detect any of those."
  (when spdx-project-detection
    (let ((loaded
           (append (when (fboundp 'ffip-get-project-root-directory) '(ffip))
                   (when (fboundp 'projectile-project-root) '(projectile))
                   (when (fboundp 'project-current) '(project)))))
      (cond ((equal 'auto spdx-project-detection)
             (car loaded))
            ((member spdx-project-detection loaded)
             spdx-project-detection)
            (t
             (error "Unknown method: spdx-project-detection method %S not loaded"
                    spdx-project-detection))))))

(defun spdx--guess-project-name ()
  "Guess a project name for the current buffer.

Returns a string, or nil if we can't make a good guess."
  (pcase (spdx--detect-project-type)
    ('ffip
     (directory-file-name (funcall (symbol-function 'ffip-project-root))))
    ('projectile
     (funcall (symbol-function 'projectile-project-name)))
    ('project
     (let ((proj (funcall (symbol-function 'project-current))))
       (and proj (directory-file-name
                  (if (stringp proj) proj (cdr proj))))))
    (_ nil)))

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
  "Get the copyright holder for the current buffer."
  (let* ((user (spdx--guess-user-name))
         (proj (spdx--guess-project-name))
         (proj-authors (when proj (concat proj " Authors"))))
    (pcase spdx-copyright-holder
      ('auto (or proj-authors user))
      ('user user)
      ('project proj)
      (_ (error "Unknown spdx-copyright-holder: %S"
                spdx-copyright-holder)))))

(defun spdx-make-default-copyright ()
  "Build a default SPDX Copyright line that can be edited by the user."
  (concat (spdx-get-default-copyright-sign)
          (spdx-get-default-copyright-years) " "
          (spdx-get-default-copyright-holder)))

(defun spdx-get-existing-copyright ()
  "Get existing SPDX Copyright line in current buffer as string.

Returns nil if no existing Copyright line is found."
  nil)

(defun spdx-copyright-format ()
  "Prompt for SPDX Copyright line, with a guess for the default line."
  (let ((prefix "Copyright "))
    (concat prefix
            (read-from-minibuffer
             prefix
             (or (spdx-get-existing-copyright)
                 (spdx-make-default-copyright))))))

(defun spdx-identifier-only ()
  "License identifier."
  (let ((identifiers (append spdx-data-license-identifiers
                             (when (not spdx-ignore-deprecated)
                               spdx-data-deprecated-license-identifiers))))
    (completing-read "License: " identifiers nil t)))

(defun spdx-license-format ()
  "License format."
  (concat "SPDX-License-Identifier: " (spdx-identifier-only)))

(defun spdx-comment-start ()
  "Construct a comment start with padding."
  (let ((add (comment-add nil)))
    (comment-padright comment-start add)))

(defun spdx-comment-end ()
  "Construct a comment end with padding."
  (let ((add (comment-add nil)))
    (unless (string= "" comment-end)
      (comment-padleft comment-end add))))

(defvar spdx-tempo-tags nil
  "Tempo tags for SPDX license.")

(tempo-define-template "spdx"
  '((spdx-comment-start)
    (spdx-license-format)
    (spdx-comment-end) > n>)
  "spdx"
  "Insert a SPDX license header."
  'spdx-tempo-tags)

(tempo-define-template "spdx-only"
  '((spdx-license-format) >)
  "spdxo"
  "Insert a SPDX license header with comments."
  'spdx-tempo-tags)

(tempo-define-template "spdx-identifier-only"
  '((spdx-identifier-only) >)
  "spdxi"
  "Insert a SPDX identifier only."
  'spdx-tempo-tags)

(tempo-define-template "copyright"
  '((spdx-comment-start)
    (spdx-copyright-format)
    (spdx-comment-end) > n>)
  "cpy"
  "Insert a copyright header."
  'spdx-tempo-tags)

(tempo-define-template "spdx-copyright"
  '((spdx-comment-start)
    (spdx-copyright-format)
    (spdx-comment-end) > n>
    (spdx-comment-start)
    (spdx-license-format)
    (spdx-comment-end) > n>)
  "spdxcpy"
  "Insert a SPDX license and copyright header."
  'spdx-tempo-tags)

;; Silence undefined warning
(declare-function tempo-template-spdx "spdx" (&optional arg))
(declare-function tempo-template-spdx-only "spdx" (&optional arg))
(declare-function tempo-template-spdx-identifier-only "spdx" (&optional arg))
(declare-function tempo-template-copyright "spdx" (&optional arg))
(declare-function tempo-template-spdx-copyright "spdx" (&optional arg))

;;;###autoload
(defun spdx-insert-spdx ()
  "Insert a SPDX license header."
  (interactive)
  (comment-normalize-vars)
  (tempo-template-spdx))

;;;###autoload
(defun spdx-insert-spdx-only ()
  "Insert a SPDX license without comments."
  (interactive)
  (tempo-template-spdx-only))

;;;###autoload
(defun spdx-insert-spdx-identifier-only ()
  "Insert a SPDX license identifier only."
  (interactive)
  (tempo-template-spdx-identifier-only))

;;;###autoload
(defun spdx-insert-copyright ()
  "Insert a copyright header."
  (interactive)
  (comment-normalize-vars)
  (tempo-template-copyright))

;;;###autoload
(defun spdx-insert-spdx-copyright ()
  "Insert a SPDX license and copyright header."
  (interactive)
  (comment-normalize-vars)
  (tempo-template-spdx-copyright))

;;;###autoload
(defun spdx-tempo-setup ()
  "Setup tempo template for SPDX license header."
  (tempo-use-tag-list 'spdx-tempo-tags))

(provide 'spdx)

;;; spdx.el ends here
