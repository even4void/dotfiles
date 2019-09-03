alias confd 'nvim ~/.config/'

alias e 'emacs -nw'
alias ec="emacsclient"
alias eq="emacs -nw -Q"
alias eg="emacs &"

alias j 'jobs -l'
alias l ls
alias ll 'ls -la'
alias ls 'ls -FG'
alias md 'mkdir -p'
abbr -a -- - 'cd -'

alias c clear
alias vim nvim
alias m make
alias g git
alias awk gawk
alias sed gsed
alias ccl ccl64
alias h heroku
alias cx 'chmod +x'
alias ccat 'pygmentize -g'
alias notes 'rg "TODO|NOTE|FIXME"'
alias tree 'tree -NC'
alias gg 'git show | tig'
alias perms "stat -c '%A %a %n'"
alias mkhttp "python3 -m http.server"
alias dud "du -sh ./* | sort -h"

alias artemis "/Users/chl/Applications/Artemis/Artemis.app/Contents/art"
alias inbox 'mbsync -a && mu index -m "~/.mail"'
alias podo 'ssh podospora@172.27.34.115'
alias bioawk '/Users/chl/git/sandbox/bioawk/bioawk'
alias statadocs "open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias rhelp "Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"
alias pip-upgrade-all "pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"

# See plenty of other nice aliases at https://bit.ly/2OYA8qW
abbr -a gst git status
abbr -a gaa git add --all
abbr -a gco git checkout
abbr -a gcb git checkout -b
abbr -a gfu git fetch upstream
abbr -a gmu git merge upstream/master
abbr -a gci git commit -S -m
