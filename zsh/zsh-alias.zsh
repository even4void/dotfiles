# Listing dir
alias lls='ls -lh --sort=size --reverse'
alias llt='ls -l -snew'
alias ll="ls -l"
alias lh="ls -lh"
alias la="ls -a"
alias l="exa --long --header --git"
alias lk="ls -lhSr"

# More dir actions
alias dud="du -sh ./* | sort -h"
alias jl="jobs -l"
alias mkdir="mkdir -p"
alias tree="tree -NC"
alias perms="stat -c '%A %a %n'"
alias cx="chmod +x"

# History
alias history='history 1'
alias hs='history | grep '

alias c="clear"

# Python stuff
alias mkhttp="python3 -m http.server"
alias venv="python3 -m venv"
alias pip-upgrade-all="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"

# Editors
alias ec="emacsclient -n"
alias ew="emacs -nw"
alias edit="open -e"
alias vim="nvim"

alias confd="nvim ~/.config"
alias sz='source $HOME/.config/zsh/.zshrc'

# Default programs
alias awk=gawk
alias sed=gsed
alias ccl=ccl64
alias R="R -q --no-save --no-restore"
alias math="/Applications/Mathematica.app/Contents/MacOS/MathKernel"

# Misc
alias -g G="| grep --color=auto"
alias nonascii="LC_CTYPE=C grep --color='auto' -n -P '[\x80-\xFF]'"
alias ccat="pygmentize -g"
alias notes='rg "TODO|NOTE|FIXME|HACK"'
alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "
alias inbox="mbsync -a && mu index --quiet; mu find flag:unread -u --fields 'd f s m' --sortfield=date --reverse -n 5 2>/dev/null || echo 'No new mail'"
alias statadocs="open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias rhelp="Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"

# Use rsync with ssh and show progress
alias rsh='rsync -Pr --rsh=ssh'
alias rcp='rsync -vaP --delete'
alias rmr='rsync -rtvu --delete'

# Git
alias g.="git add -p"
alias ga="git commit --amend --reuse-message=HEAD"
alias gb="git checkout -b"
alias gc="git commit -S -m"
alias gd='git diff'
alias gg="git status -s -b"
alias gm="git fetch upstream && git merge upstream/master"
alias gp="git pull --ff-only"
alias gt="git show | tig"
alias gu="git fetch && git diff master origin/master" # or gd @{upstream}

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
