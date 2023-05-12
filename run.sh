#!/usr/bin/env bash

inotifywait -r -m -e close_write . | 
    while read file_path file_event file_name; do
        if [[ $file_name =~ ^.*\.hs$ ]]; then
            echo ${file_path}${file_name} event: ${file_event}
            runhaskell Main.hs
            echo "Success"
        fi
    done