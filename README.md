# spdx.el

`spdx.el` provides SPDX license header insertion.

## Installation

Put `spdx.el` in your Emacs system. Add the following to your `.emacs`:

```elisp
(require 'spdx)
(define-key prog-mode-map (kbd "C-c i l") #'spdx-insert)
```

Or use [use-package](https://github.com/jwiegley/use-package) with
[straight.el](https://github.com/raxod502/straight.el):

``` emacs-lisp
(use-package spdx
  :ensure t
  :straight (:host github :repo "condy0919/spdx.el")
  :bind (:map prog-mode-map
         ("C-c i l" . spdx-insert))
  :custom
  (spdx-copyright-holder 'auto)
  (spdx-project-detection 'auto))
```

Then you can press `C-c i l` to trigger `spdx-insert`

Or manual run:

    M-x spdx-insert

Then, `spdx.el` will ask you to select a license. It's done by
`completing-read`.

After that, the copyright and license header will be written. An example
follows.

``` emacs-lisp
;Copyright (C) 2020  spdx.el Authors
;SPDX-License-Identifier: MIT
```

If you use [tempo](https://www.emacswiki.org/emacs/TempoMode), add `spdx-tempo-setup` to the mode specific hook. Type `spdx` then <kbd>M-x</kbd>` tempo-expand-if-complete`, you will get the same result as above.

It's recommanded to combine `tempo-expand-if-complete` with `hippie-expand`:

``` emacs-lisp
(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (defun try-expand-tempo (_old)
    (require 'tempo)
    (tempo-expand-if-complete))
  :custom
  (hippie-expand-try-functions-list '(try-expand-tempo
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))
```

Let <kbd>M-/</kbd> rule the world.

## Customization

- `spdx-copyright-holder`
- `spdx-project-detection`
