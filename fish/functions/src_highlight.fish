function src_highlight
    if [ -z "$2" ]
    then src="pbpaste"
    else
        src="cat $2"
    fi
    $src | highlight -O rtf --syntax $1 --line-numbers -j 3 --font Inconsolata --style solarized-dark --font-size 22 | pbcopy
end

