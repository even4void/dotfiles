function flow_all -d 'Lists all files with Flow issues'
    flow | grep -Ee '^Error --' | rev | cut -d' ' -f1 | rev | cut -d: -f1 | sort -u
end

