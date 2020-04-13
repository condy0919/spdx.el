# license.el

`license.el` provides SPDX license header insertion.

## Installation

Put `license.el` in your Emacs system. Add the following to your `.emacs`:

```elisp
(require 'license)
(define-key prog-mode-map (kbd "C-c C-s l") #'tempo-template-license)
```

Or manual run:

    M-x tempo-template-license

Or using [straight.el](https://github.com/raxod502/straight.el) with
[use-package](https://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package license
  :ensure t
  :straight (:host github :repo "condy0919/license.el")
  :custom
  (license-copyright-holder 'auto)
  (license-project-detection 'projectile))
```

Then, `license.el` will ask user to select a license. It's done by
`completing-read'.

After that, the copyright and license header will be written. An example
follows.

``` emacs-lisp
;Copyright (C) 2020  license.el Authors
;SPDX-License-Identifier: MIT
```

## Customization

- `license-copyright-holder`
- `license-project-detection`
