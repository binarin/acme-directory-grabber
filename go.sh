#!/usr/bin/env bash
set -euo pipefail

now() {
  date +'%Y%m%d%H%M'
}

ROOT=$(readlink -f $(dirname $0))

last_date=
while true; do
    this_dt=$(now)
    if [[ $this_dt != $last_date ]]; then
        echo "============"
        echo "Going to run grabber for $this_dt"
        $ROOT/dist/build/acme-dir-grabber/acme-dir-grabber "$@" || true
        last_date=$this_dt
        pkill -9 -f -- --user-data-dir=/tmp/.org. || true
        git add .
        git commit -m "$this_dt"
        git push origin
    fi
    sleep 30
done
