# spdx.el

`spdx.el` provides SPDX license header insertion.

## Installation

Put `spdx.el` in your Emacs system. Add the following to your `.emacs`:

```elisp
(require 'spdx)
(define-key prog-mode-map (kbd "C-c i l") #'license-insert)
```

Or using [straight.el](https://github.com/raxod502/straight.el) with
[use-package](https://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package spdx
  :ensure t
  :straight (:host github :repo "condy0919/spdx.el")
  :bind (:map prog-mode-map
         ("C-c i l" . license-insert))
  :custom
  (license-copyright-holder 'auto)
  (license-project-detection 'auto))
```

Then you can press `C-c i l` to trigger `license-insert`

Or manual run:

    M-x license-insert

Then, `spdx.el` will ask you to select a license. It's done by
`completing-read`.

After that, the copyright and license header will be written. An example
follows.

``` emacs-lisp
;Copyright (C) 2020  spdx.el Authors
;SPDX-License-Identifier: MIT
```

## Customization

- `license-copyright-holder`
- `license-project-detection`
