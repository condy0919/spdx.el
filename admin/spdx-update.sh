#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
spdx_update_file=$(mktemp ../.spdx-data.XXXXXX)
trap 'rm -f "$spdx_update_file"' 0
trap 'exit 1' HUP INT TERM
SPDX_UPDATE_OUTPUT="$spdx_update_file" "${EMACS:-emacs}" -Q -batch -L . \
    -l spdx-update --eval '(spdx-update (getenv "SPDX_UPDATE_OUTPUT"))'
# A new upstream release date alone does not change our license database.
if diff -I '^[(]defconst spdx-data-release-date ' \
    "$spdx_update_file" ../spdx-data.el >/dev/null; then
    echo "SPDX license data is unchanged (ignoring the release date)"
    exit 0
fi
chmod 644 "$spdx_update_file"
mv -f "$spdx_update_file" ../spdx-data.el
if git diff --quiet HEAD -- ../spdx-data.el; then
    echo "SPDX data matches the current commit"
    exit 0
fi
git commit --message "Update SPDX data" ../spdx-data.el
git --no-pager show
