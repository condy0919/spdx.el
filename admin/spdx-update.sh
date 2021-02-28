#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
${EMACS:-emacs} -Q -batch -L . -l spdx-update -f spdx-update
mv -f spdx-data.el.new ../spdx-data.el
git commit --message "Update SPDX data" ../spdx-data.el
git --no-pager show
