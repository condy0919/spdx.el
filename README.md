# spdx.el

[![Tests](https://github.com/condy0919/spdx.el/actions/workflows/test.yml/badge.svg)](https://github.com/condy0919/spdx.el/actions/workflows/test.yml)
[![License data update](https://github.com/condy0919/spdx.el/actions/workflows/update.yml/badge.svg)](https://github.com/condy0919/spdx.el/actions/workflows/update.yml)
[![MELPA](https://melpa.org/packages/spdx-badge.svg)](https://melpa.org/#/spdx)

Insert SPDX license identifiers and copyright notices in Emacs, using the
current major mode's comment syntax. For example, in Emacs Lisp:

```emacs-lisp
;; Copyright (C) 2026 Alice
;; SPDX-License-Identifier: MIT
```

Choose a license with `completing-read`, expand a Tempo snippet, or supply the
values directly from Lisp. License identifiers are bundled with the package;
normal insertion does not require a network connection.

## Installation

Requires **Emacs 29.1 or later**. Runtime dependencies are built into Emacs.

### MELPA

With [MELPA](https://melpa.org/#/getting-started) configured, run
`M-x package-install RET spdx RET`. Then add a binding to your init file:

```emacs-lisp
(require 'spdx)
(define-key prog-mode-map (kbd "C-c i l") #'spdx-insert-spdx)
```

Alternatively, use `use-package`:

```emacs-lisp
(use-package spdx
  :ensure t
  :bind (:map prog-mode-map
         ("C-c i l" . spdx-insert-spdx)))
```

## Quick start

Move point to the insertion location and run `M-x spdx-insert-spdx`, or press
`C-c i l` with the binding above. Choose a license such as `MIT` to insert:

```emacs-lisp
;; SPDX-License-Identifier: MIT
```

Run `M-x spdx-insert-spdx-copyright` to insert both lines. It first asks for
copyright text, offering a generated default, then asks for a license.

Comments follow the current major mode. In C mode, the same license becomes:

```c
/* SPDX-License-Identifier: MIT */
```

The commands insert at point. They do not find, replace, or deduplicate existing
headers, or update years in existing copyright notices.

If insertion or indentation fails, the text inserted by the template is rolled
back. This also applies to Tempo expansion; Tempo's preceding removal of the
typed tag keeps its usual behavior.

## Commands and Lisp API

All five commands can be called interactively with `M-x` or from Lisp:

| Command | Inserts | Optional Lisp arguments |
| --- | --- | --- |
| `spdx-insert-spdx` | A license header with comments | `license`, `no-prompt` |
| `spdx-insert-spdx-only` | A license header without comments | `license`, `no-prompt` |
| `spdx-insert-spdx-identifier-only` | The license ID alone | `license`, `no-prompt` |
| `spdx-insert-copyright` | A copyright notice with comments | `copyright`, `no-prompt` |
| `spdx-insert-spdx-copyright` | Copyright and license headers with comments | `license`, `copyright`, `no-prompt` |

Supplying a value skips its prompt. Omitting it or passing `nil` retains the
prompt unless `no-prompt` is non-nil:

```emacs-lisp
(spdx-insert-spdx "MIT")
(spdx-insert-copyright "(C) 2026 Alice")
(spdx-insert-spdx-copyright "MIT" "(C) 2026 Alice")
(spdx-insert-spdx-copyright "MIT") ; Still asks for copyright text.
```

A license must match a single ID in the bundled database. Deprecated IDs are
excluded when `spdx-ignore-deprecated` is non-nil. Compound expressions such as
`MIT OR Apache-2.0` and custom IDs are not currently supported.

Copyright text must be a non-blank, single-line string. Supply the body only,
including any desired year and copyright sign: `spdx-copyright-prefix` is added
automatically. Explicit text bypasses the generated copyright defaults.
Copyright text entered at a prompt follows the same validation rules.
Invalid explicit arguments signal an error before prompting or inserting text.

### Calls without prompts

Pass a non-nil final `no-prompt` argument to prohibit prompts:

```emacs-lisp
(spdx-insert-spdx-copyright "MIT" "(C) 2026 Alice" t)
(spdx-insert-spdx-only "MIT" t)
```

Every required license or copyright argument must be supplied. Commands that
insert comments also require comment syntax to be configured in the current
buffer. Missing inputs or comment syntax signal an error before insertion;
no defaults are chosen automatically.

### Formatting strings for snippets

For a snippet engine or other Lisp code that expects a string, use the formatting
functions instead of an insertion command. They accept an optional value and
`no-prompt`, and return text without modifying the buffer:

```emacs-lisp
(spdx-identifier-only "MIT" t)
;; => "MIT"

(spdx-license-format "MIT" t)
;; => "SPDX-License-Identifier: MIT"

(spdx-copyright-format "(C) 2026 Alice" t)
;; => "Copyright (C) 2026 Alice" with the default copyright prefix
```

## Tempo snippets

Enable the bundled templates in programming buffers:

```emacs-lisp
(add-hook 'prog-mode-hook #'spdx-tempo-setup)
```

For an existing buffer, run `M-x spdx-tempo-setup` once. Type a tag and run
`M-x tempo-expand-if-complete`:

| Tag | Expansion |
| --- | --- |
| `spdx` | License header with comments |
| `spdxo` | License header without comments |
| `spdxi` | License ID only |
| `cpy` | Copyright notice with comments |
| `spdxcpy` | Copyright and license headers with comments |

To include Tempo in `hippie-expand` without replacing its other expansion
functions:

```emacs-lisp
(defun my-try-expand-tempo (_old)
  (require 'tempo)
  (tempo-expand-if-complete))

(with-eval-after-load 'hippie-exp
  (add-to-list 'hippie-expand-try-functions-list #'my-try-expand-tempo))

(global-set-key (kbd "M-/") #'hippie-expand)
```

## Customization

Use `M-x customize-group RET spdx RET`, or set these options in your init file:

| Option | Default | Values and behavior |
| --- | --- | --- |
| `spdx-copyright-holder` | `'auto` | `auto`: project name plus ` Authors`, falling back to the user name; `user`: user name only; `project`: project name only, with an error if none is found |
| `spdx-copyright-sign` | `'ascii` | `ascii`: `(C)`; `unicode`: `©`; `none`: no sign |
| `spdx-copyright-prefix` | `"Copyright "` | Text prepended to the copyright body |
| `spdx-project-detection` | `'auto` | `auto`, `ffip`, `projectile`, `project`, or `nil` to disable project detection |
| `spdx-ignore-deprecated` | `nil` | Non-nil excludes deprecated IDs from completion and explicit arguments |

Generated copyright defaults use the current year. The user name comes from
Git's `user.name` configuration when available, falling back to `user-full-name`.
With `spdx-project-detection` set to `auto`, available backends are tried in
order: ffip, projectile, then Emacs project.el. An empty result falls through to
the next backend; errors and `C-g` propagate to the caller.

For a personal copyright notice with an SPDX-style prefix:

```emacs-lisp
(setq spdx-copyright-holder 'user
      spdx-copyright-prefix "SPDX-FileCopyrightText: "
      spdx-copyright-sign 'none
      spdx-ignore-deprecated t)
```

The generated copyright default then produces a line such as:

```emacs-lisp
;; SPDX-FileCopyrightText: 2026 Alice
```

## Troubleshooting

- **No comment syntax in `text-mode`:** interactive insertion asks for a comment
  prefix. With `no-prompt`, configure `comment-start` and `comment-end` for the
  buffer first, or use a command that inserts without comments.
- **No project found for the copyright holder:** `spdx-copyright-holder` is set
  to `project`. Use `auto` or `user`, configure project detection, or supply
  explicit copyright text.
- **A license is rejected:** check that the ID exists in the bundled database
  and is not excluded by `spdx-ignore-deprecated`. Empty IDs are not accepted.
- **A Tempo tag does not expand:** run `spdx-tempo-setup` in the buffer and make
  sure point is immediately after a complete tag.

## Development

CI runs strict byte compilation and ERT on Emacs 29.1 and 30.1 for pushes and
pull requests. Run the same checks locally from the repository root:

```sh
emacs -Q --batch -L . -L admin \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile spdx-data.el spdx.el admin/spdx-update.el

emacs -Q --batch -L . -L admin \
  -l test/spdx-test.el -l test/spdx-update-test.el \
  -f ert-run-tests-batch-and-exit
```

### Updating the license database

`spdx-data.el` is generated from the upstream SPDX license list. To refresh it:

```sh
./admin/spdx-update.sh
```

The script downloads the JSON once, validates it, and replaces the database only
if there is a change beyond the release date. Invalid data leaves the database
untouched. Unchanged data, including a date-only update, exits successfully
without modifying the file or creating a commit.

**When the data changes, the script creates a local Git commit.** It does not
push; the scheduled GitHub Actions workflow handles that separately. Set `EMACS`
to use a different Emacs executable. Please use the update script rather than
editing the generated license lists by hand.

## Related projects

- [lice-el][lice-el]
- [license-snippets][license-snippets] for [YASnippet][yasnippet]
- [license-templates][license-template]
- Emacs's built-in [copyright][copyright] library

## License

The package is licensed under [GPL-3.0-or-later](LICENSE). The generated
`spdx-data.el` database is marked MIT; see its file header.

[lice-el]: https://github.com/buzztaiki/lice-el
[yasnippet]: https://github.com/joaotavora/yasnippet
[license-snippets]: https://github.com/sei40kr/license-snippets
[license-template]: https://github.com/jcs-elpa/license-templates
[copyright]: https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/copyright.el
