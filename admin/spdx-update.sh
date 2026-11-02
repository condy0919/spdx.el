#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
spdx_update_file=$(mktemp ../.spdx-data.XXXXXX)
spdx_head_file=
trap 'rm -f "$spdx_update_file"; if [ -n "$spdx_head_file" ]; then rm -f "$spdx_head_file"; fi' 0
trap 'exit 1' HUP INT TERM
SPDX_UPDATE_OUTPUT="$spdx_update_file" "${EMACS:-emacs}" -Q -batch -L . \
    -l spdx-update --eval '(spdx-update (getenv "SPDX_UPDATE_OUTPUT"))'
# A new upstream release date alone does not change our license database.
if diff -I '^[(]defconst spdx-data-release-date ' \
    "$spdx_update_file" ../spdx-data.el >/dev/null; then
    echo "SPDX license data is unchanged (ignoring the release date)"
    exit 0
else
    spdx_diff_status=$?
    if [ "$spdx_diff_status" -ne 1 ]; then
        echo "Unable to compare SPDX license data" >&2
        exit "$spdx_diff_status"
    fi
fi
# Compare the candidate with HEAD before replacing the file.  --no-index
# leaves the real Git index untouched, including any staged user changes.
spdx_head_file=$(mktemp ../.spdx-data.XXXXXX)
git show HEAD:spdx-data.el > "$spdx_head_file"
if git diff --no-index --quiet -- "$spdx_head_file" "$spdx_update_file"; then
    echo "SPDX data matches the current commit"
    exit 0
else
    spdx_diff_status=$?
    if [ "$spdx_diff_status" -ne 1 ]; then
        echo "Unable to compare SPDX data with the current commit" >&2
        exit "$spdx_diff_status"
    fi
fi
chmod 644 "$spdx_update_file"
mv -f "$spdx_update_file" ../spdx-data.el
git commit --message "Update SPDX data" ../spdx-data.el
git --no-pager show
