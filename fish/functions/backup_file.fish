function backup_file -d "Make a backup of a file (with timestamp)"
    cp -a "$1" "${1}_$(date --iso-8601=seconds)"
end

