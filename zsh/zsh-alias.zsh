alias ls=exa
alias lls='ls -lh --sort=size --reverse'
alias llt='ls -l -snew'
alias ls="ls -G"
alias ll="ls -G -l"
alias lh="ls -G -lh"
# alias ll="ls -la"
# alias ls="ls -FG"
alias la="ls -a"
alias l="exa --long --header --git"
alias lk="ls -lhSr"

# More dir actions
alias dud="du -sh ./* | sort -h"
alias j="jobs -l"
alias md="mkdir -p"
alias tree="tree -NC"
alias perms="stat -c '%A %a %n'"
alias cx="chmod +x"

# History
alias history='history 1'
alias hs='history | grep '
alias nonascii="LC_CTYPE=C ggrep --color='auto' -n -P '[\x80-\xFF]'"

# Python stuff
alias mkhttp="python3 -m http.server"
alias venv="python3 -m venv"
alias pip-upgrade-all="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"

# Misc
alias ccat="pygmentize -g"
alias notes='rg "TODO|NOTE|FIXME"'
alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "
alias inbox='mbsync -a && mu index -m "~/.mail"'

# Handy shortcuts
alias awk=gawk
alias sed=gsed
alias ccl=ccl64
alias stata="stata-mp"
alias statadocs="open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias ipy="jupyter console --simple-prompt"
alias qpy="jupyter qtconsole"
alias R="R -q --no-save --no-restore"
alias rhelp="Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"
alias ete3="~/Library/Python/3.7/bin/ete3"
alias c="clear"
alias e="emacsclient -n"
alias edit="open -e"
alias math="/Applications/Mathematica.app/Contents/MacOS/MathKernel"
alias vim="nvim"

# Use rsync with ssh and show progress
alias rsyncssh='rsync -Pr --rsh=ssh'

# Edit/Source vim config
alias sz='source ~/.zshrc'

# git
alias gg="git show | tig"
alias gd='git diff'
alias gdc='git diff --cached'
alias gst="git status -s"
alias gci="git commit -S -m"
alias gco="git checkout"
alias gcb="git checkout -b"
alias gaa="git add --all"
alias gfu="git fetch upstream"
alias gmu="git merge upstream/master"

# tmux
alias tma='tmux attach -t'
alias tmn='tmux new -s'

# ceedee dot dot dot
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

