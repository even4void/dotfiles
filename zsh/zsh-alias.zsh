# Listing dir
alias lls='ls -lh --sort=size --reverse'
alias llt='ls -l -snew'
alias ll="ls -l"
alias lh="ls -lh"
alias la="ls -a"
alias l="exa --long --git"
alias lk="ls -lhSr"

# More dir actions
alias dud="du -sh ./* | sort -h"
alias jl="jobs -l"
alias tree="tree -NC"
alias perms="stat -c '%A %a %n'"
alias cx="chmod +x"

# History
alias history='history 1'
alias hs='history | grep '

alias c="clear"

# Python stuff
alias venv="python3 -m venv"
alias pip-upgrade-all="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"

# Editors
alias ec="emacsclient -n -t -a ''"
alias ew="env TERM=xterm-24bit emacs -nw"
alias edit="open -e"
alias vim="nvim"

alias confd="nvim ~/.config"
alias sz='source $HOME/.config/zsh/.zshrc'
alias reload="exec ${SHELL} -l"

# Default programs
alias awk=gawk
alias sed=gsed
alias ccl=ccl64
alias R="R -q --no-save --no-restore"
alias math="/Applications/Mathematica.app/Contents/MacOS/MathKernel"

# Misc
alias -g G="| grep --color=auto"
alias nonascii="LC_CTYPE=C ggrep --color='auto' -n -P '[\x80-\xFF]'"
alias ccat="pygmentize -g"
alias notes='rg "TODO|NOTE|FIXME"'
alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "
alias mergepdf='gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=_merged.pdf'
alias inbox="mbsync -a && mu index --quiet; mu find flag:unread -u --fields 'd f s m' --sortfield=date --reverse -n 5 2>/dev/null || echo '\033[1mNo new mail\033[0m'"
alias statadocs="open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias rhelp="Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"
alias week='date +%V'
alias update='brew update; brew upgrade; brew cleanup -s; npm install npm -g; npm update -g'
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"
alias ifactive="ifconfig | pcregrep -M -o '^[^\t:]+:([^\n]|\n\t)*status: active'"
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
alias path='echo -e ${PATH//:/\\n}'

# Kitty
alias icat="kitty +kitten icat"
alias kdiff="kitty +kitten diff"

# Use rsync with ssh and show progress
alias rsh='rsync -Pr --rsh=ssh'
alias rcp='rsync -vaP --delete'
alias rmr='rsync -rtvu --delete'

# Git
g.() { git add -p "${@}" }
alias gc="git commit -m"
alias gd="git d"
alias gg="git status -s -b"
alias gu="git diff @{upstream}"

# tmux
alias tma="tmux attach -t"
alias tmk="tmux kill-session -t"
alias tmn="tmux new -s"

# cd'ing stuff
alias ..='cd ../'
alias ...=' cd ../..'
alias cd..='cd ../'

# launch app
alias -s log=vim
alias -s pdf="open -a skim"
