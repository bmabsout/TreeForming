#!/usr/bin/env bash
ramtmp="$(mktemp -p /dev/shm/)"
cat diagram.svg > $ramtmp
feh $ramtmp &
inotifywait -r -m -e close_write . | 
    while read file_path file_event file_name; do
        if [[ $file_name =~ ^.*\.hs$ ]]; then
            echo ${file_path}${file_name} event: ${file_event}
            runhaskell Main.hs
            echo "Success"
        elif [[ $file_name =~ ^.*\.svg$ ]]; then
            echo "new svg!" $file_name
            cat ${file_path}${file_name} > $ramtmp
        fi
    done
