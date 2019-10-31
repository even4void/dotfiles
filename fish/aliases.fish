alias confd 'nvim ~/.config/'

alias eg 'emacsclient'
alias et 'emacsclient -nw -s server'

alias j 'jobs -l'
alias l 'exa --long --header --git'
alias ll 'ls -la'
alias ls 'ls -FG'
alias lk 'ls -lhSr'
alias md 'mkdir -p'
abbr -a -- - 'cd -'

alias c clear
alias vim nvim
alias m make
alias g git
alias awk gawk
alias sed gsed
alias ccl ccl64
alias ipy ipython
alias hk heroku
alias R 'R -q --no-save --no-restore'
alias cx 'chmod +x'
alias cat bat
alias ccat 'pygmentize -g'
alias notes 'rg "TODO|NOTE|FIXME"'
alias tree 'tree -NC'
alias gg 'git show | tig'
alias perms "stat -c '%A %a %n'"
alias mkhttp "python3 -m http.server"
alias dud "du -sh ./* | sort -h"
alias venv "python3 -m venv"

alias inbox 'mbsync -a && mu index -m "~/.mail"'
alias podo 'ssh podospora@172.27.34.115'
alias statadocs "open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias rhelp "Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"
alias pip-upgrade-all "pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"
alias julia "/Applications/Julia-1.2.app/Contents/Resources/julia/bin/julia"

# See plenty of other nice aliases at https://bit.ly/2OYA8qW
abbr -a gst git status
abbr -a gaa git add --all
abbr -a gco git checkout
abbr -a gcb git checkout -b
abbr -a gfu git fetch upstream
abbr -a gmu git merge upstream/master
abbr -a gci git commit -S -m
