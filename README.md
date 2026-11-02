# spdx.el

![Latest](https://github.com/condy0919/spdx.el/workflows/Check%20for%20license%20list%20version/badge.svg)
[![](https://melpa.org/packages/spdx-badge.svg)](https://melpa.org/#/spdx)

`spdx.el` provides SPDX license header and copyright insertion.

## Installation

Requires Emacs 29.1 or later.

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
- `spdx-insert-spdx-identifier-only` inserts a SPDX identifier only.
- `spdx-insert-copyright` inserts a copyright header.
- `spdx-insert-spdx-copyright` inserts a SPDX license and copyright header.

When called from Lisp, these commands accept optional arguments in this order:

| Command                            | Optional arguments                  |
|------------------------------------|-------------------------------------|
| `spdx-insert-spdx`                 | `license`, `no-prompt`              |
| `spdx-insert-spdx-only`            | `license`, `no-prompt`              |
| `spdx-insert-spdx-identifier-only` | `license`, `no-prompt`              |
| `spdx-insert-copyright`            | `copyright`, `no-prompt`            |
| `spdx-insert-spdx-copyright`       | `license`, `copyright`, `no-prompt` |

Supplying a license skips the license prompt. Supplying copyright text skips
the copyright prompt and default-value lookup:

```emacs-lisp
(spdx-insert-spdx "MIT")
(spdx-insert-copyright "(C) 2026 Alice")
(spdx-insert-spdx-copyright "MIT" "(C) 2026 Alice")
```

A license must be a non-empty string matching an available SPDX identifier.
Deprecated identifiers are rejected when `spdx-ignore-deprecated` is non-nil.
Copyright text must be a non-blank, single-line string without the prefix;
`spdx-copyright-prefix` is added automatically. Invalid arguments signal an
error before prompting or inserting text.

Omitting an argument or passing `nil` retains the corresponding prompt unless
`no-prompt` is non-nil. For example, `(spdx-insert-spdx-copyright "MIT")` still
asks for copyright information. Interactive commands and Tempo snippets retain
their existing prompts, including asking for comment syntax when the mode has
none.

Pass non-nil `no-prompt` to prohibit prompts:

```emacs-lisp
(spdx-insert-spdx-copyright "MIT" "(C) 2026 Alice" t)
(spdx-insert-spdx-only "MIT" t)
```

In this mode, every required license or copyright argument must be supplied.
Commands that insert comments also require an existing comment syntax. Missing
inputs or comment syntax signal an error before insertion; defaults are not
chosen automatically.

`spdx-identifier-only`, `spdx-license-format`, and `spdx-copyright-format`
accept the corresponding optional value and `no-prompt`, and return strings
without inserting them.

## Available tempo snippets

The following tempo snippets can be expanded via <kbd>M-x tempo-expand-if-complete</kbd>. Make sure `spdx-tempo-setup` is called to access these tempo snippets.

- `spdx` expands to a SPDX license header.
- `spdxo` expands to a SPDX license header without comments.
- `spdxi` expands to a SPDX identifier only.
- `cpy` expands to a copyright header.
- `spdxcpy` expands to a SPDX license and copyright header.

## Customization

- `spdx-copyright-holder` (`'auto` by default)
- `spdx-copyright-sign` (`'ascii` by default)
- `spdx-copyright-prefix` (`"Copyright "` by default)
- `spdx-project-detection` (`'auto` by default)
- `spdx-ignore-deprecated` (`nil` by default)

With `spdx-project-detection` set to `auto`, available backends are tried in
order: ffip, projectile, then Emacs project.el. A backend returning no project
name or a blank name is skipped; errors and `C-g` are propagated.

`spdx-copyright-holder` set to `auto` prefers the project authors and falls back
to the user name. Setting it explicitly to `project` requires a project;
otherwise, generating a default copyright signals an error. Explicit copyright
text bypasses this lookup.

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

CI runs strict byte compilation and the ERT suite on Emacs 29.1 and 30.1 for
pushes and pull requests.

Run the tests with:

```sh
emacs -Q --batch -L . -L admin \
  -l test/spdx-test.el -l test/spdx-update-test.el \
  -f ert-run-tests-batch-and-exit
```

Run `./admin/spdx-update.sh` to refresh the bundled license data. It downloads
the upstream JSON once and validates it before replacing the database. Invalid
data leaves the existing database untouched; unchanged data exits successfully
without creating a commit. A change to the release date alone leaves the file
untouched and does not create a commit. License list changes are detected even
when the release date stays the same.

If `spdx-data-license-identifiers` is out of date, don't hesitate to raise a PR.

## FAQ

1. `spdx-insert-spdx-copyright` failed to work in `text-mode`.

   Now, it works. Even though `text-mode` hasn't defined comment syntax, `spdx.el` will ask
   the user to define it. Calls with non-nil `no-prompt` instead signal an error;
   configure comment syntax first or use a command that inserts without comments.

[lice-el]: https://github.com/buzztaiki/lice-el
[tempo]: https://www.emacswiki.org/emacs/TempoMode
[yasnippet]: https://github.com/joaotavora/yasnippet
[straight.el]: https://github.com/raxod502/straight.el
[use-package]: https://github.com/jwiegley/use-package
[license-snippets]: https://github.com/sei40kr/license-snippets
[license-template]: https://github.com/jcs-elpa/license-templates
[elide-head]: https://github.com/emacs-mirror/emacs/blob/master/lisp/elide-head.el
[copyright]: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/copyright.el
