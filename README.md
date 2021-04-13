# spdx.el

![Latest](https://github.com/condy0919/spdx.el/workflows/Check%20for%20license%20list%20version/badge.svg)
[![](https://melpa.org/packages/spdx-badge.svg)](https://melpa.org/#/spdx)

`spdx.el` provides SPDX license header and copyright insertion.

## Installation

Please install `spdx` from [melpa](https://melpa.org/#/spdx).

Put `spdx.el` in your Emacs system. Add the following to your `.emacs`:

```elisp
(require 'spdx)
(define-key prog-mode-map (kbd "C-c i l") #'spdx-insert-spdx)
```

Or use [use-package][use-package] with [straight.el][straight.el]:

``` emacs-lisp
(use-package spdx
  :ensure t
  :straight (:host github :repo "condy0919/spdx.el")
  :bind (:map prog-mode-map
         ("C-c i l" . spdx-insert-spdx))
  :custom
  (spdx-copyright-holder 'auto)
  (spdx-project-detection 'auto))
```

## Usage

you can press `C-c i l` to trigger `spdx-insert-spdx`

Or manually run <kbd>M-x spdx-insert-spdx</kbd>

Then, `spdx.el` will ask you to select a license. It's done by `completing-read`.

After that, the license header will be written. An example follows.

``` emacs-lisp
;; SPDX-License-Identifier: MIT
```

If you use [tempo][tempo], add `spdx-tempo-setup` to the mode specific hook.
Type `spdx` then <kbd>M-x</kbd>` tempo-expand-if-complete`, you will get the
same result as above.

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

## Available functions

- `spdx-insert-spdx` inserts a SPDX license header.
- `spdx-insert-spdx-only` inserts a SPDX license header without comments.
- `spdx-insert-copyright` inserts a copyright header.
- `spdx-insert-spdx-copyright`inserts a SPDX license and copyright header.

## Available tempo snippets

The following tempo snippets can be expanded via <kbd>M-x tempo-expand-if-complete</kbd>. Make sure `spdx-tempo-setup` is called to access these tempo snippets.

- `spdx` expands to a SPDX license header.
- `cpy` expands to a copyright header.
- `spdxcpy` expands to a SPDX license and copyright header.

## Customization

- `spdx-copyright-holder` (`'auto` by default)
- `spdx-copyright-sign` (`'ascii` by default)
- `spdx-project-detection` (`'auto` by default)
- `spdx-ignore-deprecated` (`nil` by default)

## Other projects

### lice-el

[lice-el][lice-el] provides license template management and file header insertion.

The content of license is inserted while `spdx.el` only inserts two lines.

``` emacs-lisp
;; Copyright (C)  spdx.el Authors
;; SPDX-License-Identifier: MIT
```

If you use `lice-el`, you may also need to hide the massive headers. Take a look
at the builtin [elide-head][elide-head] package.

### license-snippets

[license-snippets][license-snippets] is a license snippet collection of
[yasnippet][yasnippet].

`spdx.el` uses the builtin completion system [tempo][tempo] without dependencies
of third party libraries.

### license-template

[license-template][license-template] creates LICENSE file using GitHub API. It's
NOT used to insert license header.

### copyright

Emacs has a builtin [copyright][copyright] package providing copyright header
insertion. However, the copyright `elisp-mode` has only one leading semicolon.

``` emacs-lisp
;Copyright (C) 2020 by Me

;; M-x copyright
```

`spdx.el` inserts with `;;` with one space following.

``` emacs-lisp
;; Copyright (C) 2020  spdx.el Authors

;; M-x spdx-insert-copyright
```

## Contribution

If you found the `spdx-spdx-identifiers` is out of date, don't hesitate to raise a PR.

## FAQ

1. `spdx-insert-spdx-copyright` failed to work in `text-mode`.

   Now, it works. Even though `text-mode` hasn't defined comment syntax, `spdx.el` will ask
   user to define it.

[lice-el]: https://github.com/buzztaiki/lice-el
[tempo]: https://www.emacswiki.org/emacs/TempoMode
[yasnippet]: https://github.com/joaotavora/yasnippet
[straight.el]: https://github.com/raxod502/straight.el
[use-package]: https://github.com/jwiegley/use-package
[license-snippets]: https://github.com/sei40kr/license-snippets
[license-template]: https://github.com/jcs-elpa/license-templates
[elide-head]: https://github.com/emacs-mirror/emacs/blob/master/lisp/elide-head.el
[copyright]: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/copyright.el
