alias ea 'vim ~/.config/fish/aliases.fish'
alias ef 'vim ~/.config/fish/config.fish'
alias eg 'vim ~/.gitconfig'
alias ev 'vim ~/.vimrc'

alias e 'emacs -nw'
alias ec="emacsclient"
alias eq="emacs -nw -Q"

alias j jobs
alias l ls
alias ll 'ls -la'
alias ls 'ls -FG'
alias md 'mkdir -p'

alias ccat 'pygmentize -g'

alias c clear
alias nv nvim
alias notes 'rg "TODO|HACK|FIXME|OPTIMIZE"'
alias m make
alias tree 'tree -NC'
alias g git
alias cx 'chmod +x'
alias h heroku
alias gg 'git show | tig'

alias inbox='mbsync -a && mu index -m "~/.mail"'
alias bioawk='~/git/sandbox/bioawk/bioawk'
alias statadocs="open -a 'Adobe Acrobat Reader DC' /Applications/Stata/docs/i.pdf"
alias rhelp="Rscript -e 'args <- commandArgs(TRUE); help(args[2], package=c(\"base\", \"stats\"), help_type=\"text\")' --args"
alias pip-upgrade-all="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"

# See plenty of other nice aliases at https://bit.ly/2OYA8qW
abbr -a gst git status
abbr -a gaa git add --all
abbr -a gco git checkout
abbr -a gfu git fetch upstream
abbr -a gci git commit -S

