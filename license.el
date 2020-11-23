;;; license.el --- Insert SPDX license header -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: license, tools
;; URL: https://github.com/condy0919/license.el

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

;; # license.el

;; `license.el` provides SPDX license header insertion.

;; ## Installation

;; Put `license.el` in your Emacs system. Add the following to your `.emacs`:

;; ```elisp
;; (require 'license)
;; (define-key prog-mode-map (kbd "C-c i l") #'license-insert)
;; ```

;; Or using [straight.el](https://github.com/raxod502/straight.el) with
;; [use-package](https://github.com/jwiegley/use-package):

;; ``` emacs-lisp
;; (use-package license
;;   :ensure t
;;   :straight (:host github :repo "condy0919/license.el")
;;   :bind (:map prog-mode-map
;;          ("C-c i l" . license-insert))
;;   :custom
;;   (license-copyright-holder 'auto)
;;   (license-project-detection 'projectile))
;; ```

;; Then you can press `C-c i l` to trigger `license-insert`

;; Or manual run:

;;     M-x license-insert

;; Then, `license.el` will ask you to select a license. It's done by
;; `completing-read'.

;; After that, the copyright and license header will be written. An example
;; follows.

;; `;Copyright (C) 2020  license.el Authors`
;; `;SPDX-License-Identifier: AGPL-1.0-only`

;; ## Customization

;; - `license-copyright-holder'
;; - `license-project-detection'

;;; Code:

(require 'tempo)
(require 'newcomment)

(defgroup license nil
  "SPDX license header inserter."
  :prefix "license-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/license.el"))

(defcustom license-copyright-holder 'auto
  "The copyright holder.

The priority of auto is `project' > `user'."
  :type '(choice (const auto)
                 (const user)
                 (const project))
  :group 'license)

;; Stole from `doom-modeline`
(defcustom license-project-detection 'auto
  "How to detect the project root.

The default priority is `ffip' > `projectile' > 'project'.
nil means not to use project information."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Find File in Project" ffip)
                 (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 (const :tag "Disable" nil))
  :group 'license)

(defconst license-spdx-identifiers
  ;; from https://spdx.org/licenses/
  '(0BSD
    AAL
    Abstyles
    Adobe-2006
    Adobe-Glyph
    ADSL
    AFL-1.1
    AFL-1.2
    AFL-2.0
    AFL-2.1
    AFL-3.0
    Afmparse
    AGPL-1.0-only
    AGPL-1.0-or-later
    AGPL-3.0-only
    AGPL-3.0-or-later
    Aladdin
    AMDPLPA
    AML
    AMPAS
    ANTLR-PD
    Apache-1.0
    Apache-1.1
    Apache-2.0
    APAFML
    APL-1.0
    APSL-1.0
    APSL-1.1
    APSL-1.2
    APSL-2.0
    Artistic-1.0
    Artistic-1.0-cl8
    Artistic-1.0-Perl
    Artistic-2.0
    Bahyph
    Barr
    Beerware
    BitTorrent-1.0
    BitTorrent-1.1
    blessing
    BlueOak-1.0.0
    Borceux
    BSD-1-Clause
    BSD-2-Clause
    BSD-2-Clause-FreeBSD
    BSD-2-Clause-NetBSD
    BSD-2-Clause-Patent
    BSD-3-Clause
    BSD-3-Clause-Attribution
    BSD-3-Clause-Clear
    BSD-3-Clause-LBNL
    BSD-3-Clause-No-Nuclear-License
    BSD-3-Clause-No-Nuclear-License-2014
    BSD-3-Clause-No-Nuclear-Warranty
    BSD-3-Clause-Open-MPI
    BSD-4-Clause
    BSD-4-Clause-UC
    BSD-Protection
    BSD-Source-Code
    BSL-1.0
    bzip2-1.0.5
    bzip2-1.0.6
    Caldera
    CATOSL-1.1
    CC-BY-1.0
    CC-BY-2.0
    CC-BY-2.5
    CC-BY-3.0
    CC-BY-4.0
    CC-BY-NC-1.0
    CC-BY-NC-2.0
    CC-BY-NC-2.5
    CC-BY-NC-3.0
    CC-BY-NC-4.0
    CC-BY-NC-ND-1.0
    CC-BY-NC-ND-2.0
    CC-BY-NC-ND-2.5
    CC-BY-NC-ND-3.0
    CC-BY-NC-ND-4.0
    CC-BY-NC-SA-1.0
    CC-BY-NC-SA-2.0
    CC-BY-NC-SA-2.5
    CC-BY-NC-SA-3.0
    CC-BY-NC-SA-4.0
    CC-BY-ND-1.0
    CC-BY-ND-2.0
    CC-BY-ND-2.5
    CC-BY-ND-3.0
    CC-BY-ND-4.0
    CC-BY-SA-1.0
    CC-BY-SA-2.0
    CC-BY-SA-2.5
    CC-BY-SA-3.0
    CC-BY-SA-4.0
    CC-PDDC
    CC0-1.0
    CDDL-1.0
    CDDL-1.1
    CDLA-Permissive-1.0
    CDLA-Sharing-1.0
    CECILL-1.0
    CECILL-1.1
    CECILL-2.0
    CECILL-2.1
    CECILL-B
    CECILL-C
    CERN-OHL-1.1
    CERN-OHL-1.2
    ClArtistic
    CNRI-Jython
    CNRI-Python
    CNRI-Python-GPL-Compatible
    Condor-1.1
    copyleft-next-0.3.0
    copyleft-next-0.3.1
    CPAL-1.0
    CPL-1.0
    CPOL-1.02
    Crossword
    CrystalStacker
    CUA-OPL-1.0
    Cube
    curl
    D-FSL-1.0
    diffmark
    DOC
    Dotseqn
    DSDP
    dvipdfm
    ECL-1.0
    ECL-2.0
    EFL-1.0
    EFL-2.0
    eGenix
    Entessa
    EPL-1.0
    EPL-2.0
    ErlPL-1.1
    etalab-2.0
    EUDatagrid
    EUPL-1.0
    EUPL-1.1
    EUPL-1.2
    Eurosym
    Fair
    Frameworx-1.0
    FreeImage
    FSFAP
    FSFUL
    FSFULLR
    FTL
    GFDL-1.1-only
    GFDL-1.1-or-later
    GFDL-1.2-only
    GFDL-1.2-or-later
    GFDL-1.3-only
    GFDL-1.3-or-later
    Giftware
    GL2PS
    Glide
    Glulxe
    gnuplot
    GPL-1.0-only
    GPL-1.0-or-later
    GPL-2.0-only
    GPL-2.0-or-later
    GPL-3.0-only
    GPL-3.0-or-later
    gSOAP-1.3b
    HaskellReport
    HPND
    HPND-sell-variant
    IBM-pibs
    ICU
    IJG
    ImageMagick
    iMatix
    Imlib2
    Info-ZIP
    Intel
    Intel-ACPI
    Interbase-1.0
    IPA
    IPL-1.0
    ISC
    JasPer-2.0
    JPNIC
    JSON
    LAL-1.2
    LAL-1.3
    Latex2e
    Leptonica
    LGPL-2.0-only
    LGPL-2.0-or-later
    LGPL-2.1-only
    LGPL-2.1-or-later
    LGPL-3.0-only
    LGPL-3.0-or-later
    LGPLLR
    Libpng
    libpng-2.0
    libselinux-1.0
    libtiff
    LiLiQ-P-1.1
    LiLiQ-R-1.1
    LiLiQ-Rplus-1.1
    Linux-OpenIB
    LPL-1.0
    LPL-1.02
    LPPL-1.0
    LPPL-1.1
    LPPL-1.2
    LPPL-1.3a
    LPPL-1.3c
    MakeIndex
    MirOS
    MIT
    MIT-0
    MIT-advertising
    MIT-CMU
    MIT-enna
    MIT-feh
    MITNFA
    Motosoto
    mpich2
    MPL-1.0
    MPL-1.1
    MPL-2.0
    MPL-2.0-no-copyleft-exception
    MS-PL
    MS-RL
    MTLL
    MulanPSL-1.0
    Multics
    Mup
    NASA-1.3
    Naumen
    NBPL-1.0
    NCSA
    Net-SNMP
    NetCDF
    Newsletr
    NGPL
    NLOD-1.0
    NLPL
    Nokia
    NOSL
    Noweb
    NPL-1.0
    NPL-1.1
    NPOSL-3.0
    NRL
    NTP
    NTP-0
    OCCT-PL
    OCLC-2.0
    ODbL-1.0
    ODC-By-1.0
    OFL-1.0
    OFL-1.0-no-RFN
    OFL-1.0-RFN
    OFL-1.1
    OFL-1.1-no-RFN
    OFL-1.1-RFN
    OGL-Canada-2.0
    OGL-UK-1.0
    OGL-UK-2.0
    OGL-UK-3.0
    OGTSL
    OLDAP-1.1
    OLDAP-1.2
    OLDAP-1.3
    OLDAP-1.4
    OLDAP-2.0
    OLDAP-2.0.1
    OLDAP-2.1
    OLDAP-2.2
    OLDAP-2.2.1
    OLDAP-2.2.2
    OLDAP-2.3
    OLDAP-2.4
    OLDAP-2.5
    OLDAP-2.6
    OLDAP-2.7
    OLDAP-2.8
    OML
    OpenSSL
    OPL-1.0
    OSET-PL-2.1
    OSL-1.0
    OSL-1.1
    OSL-2.0
    OSL-2.1
    OSL-3.0
    Parity-6.0.0
    PDDL-1.0
    PHP-3.0
    PHP-3.01
    Plexus
    PostgreSQL
    PSF-2.0
    psfrag
    psutils
    Python-2.0
    Qhull
    QPL-1.0
    Rdisc
    RHeCos-1.1
    RPL-1.1
    RPL-1.5
    RPSL-1.0
    RSA-MD
    RSCPL
    Ruby
    SAX-PD
    Saxpath
    SCEA
    Sendmail
    Sendmail-8.23
    SGI-B-1.0
    SGI-B-1.1
    SGI-B-2.0
    SHL-0.5
    SHL-0.51
    SimPL-2.0
    SISSL
    SISSL-1.2
    Sleepycat
    SMLNJ
    SMPPL
    SNIA
    Spencer-86
    Spencer-94
    Spencer-99
    SPL-1.0
    SSH-OpenSSH
    SSH-short
    SSPL-1.0
    SugarCRM-1.1.3
    SWL
    TAPR-OHL-1.0
    TCL
    TCP-wrappers
    TMate
    TORQUE-1.1
    TOSL
    TU-Berlin-1.0
    TU-Berlin-2.0
    UCL-1.0
    Unicode-DFS-2015
    Unicode-DFS-2016
    Unicode-TOU
    Unlicense
    UPL-1.0
    Vim
    VOSTROM
    VSL-1.0
    W3C
    W3C-19980720
    W3C-20150513
    Watcom-1.0
    Wsuipa
    WTFPL
    X11
    Xerox
    XFree86-1.1
    xinetd
    Xnet
    xpp
    XSkat
    YPL-1.0
    YPL-1.1
    Zed
    Zend-2.0
    Zimbra-1.3
    Zimbra-1.4
    Zlib
    zlib-acknowledgement
    ZPL-1.1
    ZPL-2.0
    ZPL-2.1)
  "SPDX License list.")

(defun license--user-name ()
  "Try to get the `user-full-name`."
  user-full-name)

(defun license--project-detect ()
  (when license-project-detection
    (let ((loaded
           (append (when (fboundp 'ffip-get-project-root-directory) '(ffip))
                   (when (fboundp 'projectile-project-root) '(projectile))
                   (when (fboundp 'project-current) '(project)))))
      (cond ((equal 'auto license-project-detection)
             (car loaded))
            ((member license-project-detection loaded)
             license-project-detection)
            (t
             (error "license-project-detection method %S not loaded"
                    license-project-detection))))))

(defun license--project-name ()
  "Try to get the project name. Otherwise nil is returned."
  (pcase (license--project-detect)
    ;; FIXME: any better way?
    ('ffip
     (directory-file-name (funcall (symbol-function 'ffip-project-root))))
    ('projectile
     (funcall (symbol-function 'projectile-project-name)))
    ;; FIXME: any better way?
    ('project
     (let ((proj (funcall (symbol-function 'project-current))))
       (and proj (directory-file-name
                  (if (stringp proj) proj (cdr proj))))))
    (_ nil)))

(defun license-copyright-format ()
  "Copyright format."
  (format "Copyright (C) %s  %s"
          (format-time-string "%Y")
          (let* ((user (license--user-name))
                 (proj (license--project-name))
                 (proj-authors (when proj (concat proj " Authors"))))
            (pcase license-copyright-holder
              ('auto (or proj-authors user))
              ('user user)
              ('project proj)
              (_ (error "Unknown license-copyright-holder: %S"
                        license-copyright-holder))))))

(defun license-license-format ()
  "License format."
  (concat "SPDX-License-Identifier: "
          (completing-read "License: " license-spdx-identifiers)))

(tempo-define-template "license"
  '(comment-start
    (license-copyright-format)
    comment-end > n>
    comment-start
    (license-license-format)
    comment-end > n>)
  'license
  "Insert a SPDX license.")

;;;###autoload
(defun license-insert ()
  "Insert licenseiand header."
  (interactive)
  (tempo-template-license))

(provide 'license)
;;; license.el ends here
