# Mostly stolen from Thorsten Ball's config
# https://github.com/mrnugget/dotfiles/blob/master/zshrc

##############
# BASIC SETUP
##############

typeset -U PATH
autoload colors; colors;

# autoload -U promptinit; promptinit
# prompt pure

#############
## PRIVATE ##
#############
# Include private stuff that's not supposed to show up
# in the dotfiles repo
local private="${HOME}/.zsh.d/private.sh"
if [ -r ${private} ]; then
  . ${private}
fi

##########
# HISTORY
##########

HISTFILE=$HOME/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

setopt EXTENDED_HISTORY
setopt HIST_VERIFY
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Dont record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Dont record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Dont write duplicate entries in the history file.

setopt inc_append_history
setopt share_history

#############
# COMPLETION
#############

# Add completions installed through Homebrew packages
# See: https://docs.brew.sh/Shell-Completion
if type brew &>/dev/null; then
  FPATH=/usr/local/share/zsh/site-functions:$FPATH
fi

autoload -U compinit
compinit -i

unsetopt menu_complete
unsetopt flowcontrol
setopt prompt_subst
setopt auto_menu
setopt complete_in_word
setopt always_to_end
setopt auto_pushd
zmodload -i zsh/complist

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' special-dirs true
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH/cache/
zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# use /etc/hosts and known_hosts for hostname completion
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.ssh/config ] && _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config=()
[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
  "$_ssh_config[@]"
  "$_global_ssh_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  "$HOST"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts
zstyle ':completion:*' users off

###############
# KEY BINDINGS
###############

# Vim Keybindings
bindkey -e

# CTRL-R to search through history
bindkey '^R' history-incremental-search-backward
# CTRL-S to search forward in history
bindkey '^S' history-incremental-search-forward
# Accept the presented search result
bindkey '^Y' accept-search

# Use the arrow keys to search forward/backward through the history,
# using the first word of what's typed in as search word
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward

# Use the same keys as bash for history forward/backward: Ctrl+N/Ctrl+P
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# Backspace working the way it should
bindkey '^?' backward-delete-char
bindkey '^[[3~' delete-char

#########
# Aliases
#########

alias ls=exa
alias lls='ls -lh --sort=size --reverse'
alias llt='ls -l -snew'
alias ls="ls -G"
alias ll="ls -G -l"
alias lh="ls -G -lh"

alias history='history 1'
alias hs='history | grep '

# Use rsync with ssh and show progress
alias rsyncssh='rsync -Pr --rsh=ssh'

# Edit/Source vim config
alias ez='vim ~/.zshrc'
alias sz='source ~/.zshrc'

# git
alias gst='git status'
alias gaa='git add -A'
alias gd='git diff'
alias gdc='git diff --cached'

# tmux
alias tma='tmux attach -t'
alias tmn='tmux new -s'

# ceedee dot dot dot
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

alias g='git'

alias inbox='mbsync -a && mu index -m "~/.mail"'

##########
# FUNCTIONS
##########

startpostgres() {
  local pidfile="/usr/local/var/postgres/postmaster.pid"
  if [ -s $pidfile ] && kill -0 $(cat $pidfile | head -n 1) > /dev/null 2>&1; then
    echo "Already running"
  else
    pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start
  fi
}

stoppostgres() {
  pg_ctl -D /usr/local/var/postgres stop
}

mkdircd() {
  mkdir -p $1 && cd $1
}

serve() {
  local port=${1:-8000}
  local ip=$(ipconfig getifaddr en0)
  echo "Serving on ${ip}:${port} ..."
  python -m SimpleHTTPServer ${port}
}

beautiful() {
  while
  do
    i=$((i + 1)) && echo -en "\x1b[3$(($i % 7))mo" && sleep .2
  done
}

spinner() {
  while
  do
    for i in "-" "\\" "|" "/"
    do
      echo -n " $i \r\r"
      sleep .1
    done
  done
}

s3() {
  local route="s3.thorstenball.com/${1}"
  aws s3 cp ${1} s3://${route}
  echo http://${route} | pbcopy
}

f() {
  find . -iname "*${1}*"
}

#########
# PROMPT
#########

git_prompt_info() {
  local dirstatus=" ✓"
  local dirty="%{$fg_bold[red]%} X%{$reset_color%}"

  if [[ ! -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
    dirstatus=$dirty
  fi

  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo " %{$fg_bold[green]%}${ref#refs/heads/}$dirstatus%{$reset_color%}"
}

# local dir_info_color="$fg_bold[black]"

# This just sets the color to "bold".
# Future me. Try this to see what's correct:
#   $ print -P '%fg_bold[black] black'
#   $ print -P '%B%F{black} black'
#   $ print -P '%B black'
local dir_info_color="%B"

local dir_info_color_file="${HOME}/.zsh.d/dir_info_color"
if [ -r ${dir_info_color_file} ]; then
  source ${dir_info_color_file}
fi

local dir_info="%{$dir_info_color%}%(5~|%-1~/.../%2~|%4~)%{$reset_color%}"
local promptnormal="%{$fg_bold[green]%}λ %{$reset_color%}"
local promptjobs="%{$fg_bold[yellow]%}λ %{$reset_color%}"

PROMPT='${dir_info}$(git_prompt_info) %(1j.$promptjobs.$promptnormal)'

# Taken from here: https://zenbro.github.io/2015/07/23/show-exit-code-of-last-command-in-zsh
function check_last_exit_code() {
  local LAST_EXIT_CODE=$?
  if [[ $LAST_EXIT_CODE -ne 0 ]]; then
    local EXIT_CODE_PROMPT=' '
    EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%}"
    EXIT_CODE_PROMPT+="%{$fg_bold[red]%}$LAST_EXIT_CODE%{$reset_color%}"
    EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%}"
    echo "$EXIT_CODE_PROMPT"
  fi
}

# RPROMPT='$(check_last_exit_code)'

########
# ENV
########
# export TERM=xterm-256color

export PATH="$HOME/local/bin:$PATH"

# export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Reduce delay for key combinations in order to change to vi mode faster
# See: http://www.johnhawthorn.com/2012/09/vi-escape-delays/
# Set it to 10ms
export KEYTIMEOUT=1

# homebrew
export PATH="/usr/local/bin:$PATH"

# Encoding problems with gem
# export LC_ALL=en_US.UTF-8
# export LANG=en_US.UTF-8

# node.js
export NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"

#heroku
export PATH="/usr/local/heroku/bin:$PATH"

# direnv
if which direnv &> /dev/null; then
  eval "$(direnv hook zsh)"
fi

# rust
export PATH="$HOME/.cargo/bin:$PATH"
if which rustc &> /dev/null; then
  export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

# python
if which pyenv &> /dev/null; then
  eval "$(pyenv init -)"
  export PATH="$HOME/.local/bin:$PATH"
fi

export BAT_THEME=ansi-light
if [ ! -n "$INSIDE_EMACS" ]; then
  alias cat=bat
fi

alias la="ls -a"
alias dud="du -sh ./* | sort -h"
alias awk=gawk
alias sed=gsed
alias ccl=ccl64
alias stata="stata-mp"
alias ipy=ipython
alias qpy="jupyter qtconsole"
alias R="R -q --no-save --no-restore"
alias ete3="~/Library/Python/3.7/bin/ete3"
alias cx="chmod +x"
alias ccat="pygmentize -g"
alias notes='rg "TODO|NOTE|FIXME"'
alias tree="tree -NC"
alias gg="git show | tig"
alias perms="stat -c '%A %a %n'"
alias mkhttp="python3 -m http.server"
alias venv="python3 -m venv"
alias inbox='mbsync -a && mu index -m "~/.mail"'
alias gst="git status -s"
alias gci="git commit -S -m"
alias gco="git checkout"
alias gcb="git checkout -b"
alias gaa="git add --all"
alias j="jobs -l"
alias l="exa --long --header --git"
alias lk="ls -lhSr"
alias md="mkdir -p"
alias pip-upgrade-all="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"
alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "
