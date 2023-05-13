#!/usr/bin/env bash
ramtmp="$(mktemp -p /dev/shm/)"
cat diagram.svg > $ramtmp
feh $ramtmp &
# x =  &
exec 3> >(cabal repl --offline)
inotifywait -r -m -e close_write . | 
    while read file_path file_event file_name; do
        if [[ $file_name =~ ^.*\.hs$ ]]; then
            echo ${file_path}${file_name} event: ${file_event}
            # runhaskell -- -XLambdaCase -XPatternSynonyms -XTemplateHaskell -XTypeFamilies -XPartialTypeSignatures -XRecordWildCards ${file_path}${file_name}
            echo ":l ${file_path}${file_name}" >&3
            echo "main" >&3
            echo "Success"
        elif [[ $file_name =~ ^.*\.svg$ ]]; then
            echo "new svg!" $file_name
            cat ${file_path}${file_name} > $ramtmp
        fi
    done
