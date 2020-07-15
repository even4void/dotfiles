# Partly inspired by Thorsten Ball's config
# https://github.com/mrnugget/dotfiles/blob/master/zshrc

typeset -U PATH
autoload colors
colors

setopt autocd

# autoload -U promptinit; promptinit

### CUSTOM FILES ###
source ~/.config/zsh/zsh-comp.zsh   # completions
source ~/.config/zsh/zsh-alias.zsh  # alias
source ~/.config/zsh/zsh-func.zsh   # functions
source ~/.config/zsh/zsh-prompt.zsh # prompt
source ~/.config/zsh/private.zsh    # private stuff

### HISTORY ###
HISTFILE=$HOME/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

HISTORY_IGNORE="l[alsh]*:htop:ps ax:ew*:[bc]at*:less*:which*:inbox:cd ..:cd -"

setopt EXTENDED_HISTORY
setopt HIST_VERIFY
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Dont record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_IGNORE_SPACE      # Dont record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS      # Dont write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Delete empty lines from history file
setopt HIST_NO_STORE          # Don't store history (fc -l) command
setopt HIST_NO_FUNCTIONS      # Don't store function definitions

setopt inc_append_history
setopt share_history

### KEY BINDINGS ###
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

### ENV ###
# export TERM=xterm-256color

export PATH="$HOME/local/bin:$HOME/.config/bin:$PATH"

# rg
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgreprc"

# export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Reduce delay for key combinations in order to change to vi mode faster
# See: http://www.johnhawthorn.com/2012/09/vi-escape-delays/
# Set it to 10ms
export KEYTIMEOUT=1

# homebrew
export PATH="/usr/local/bin:$PATH"

# export LC_ALL=en_US.UTF-8
# export LANG=en_US.UTF-8

# node.js
export NODE_PATH="/usr/local/lib/node_modules"

# rust
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

# python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH="$HOME/.poetry/bin:$PATH"

export PATH="$HOME/Library/Python/3.8/bin:$PATH"

export PYSPARK_PYTHON="python3"

source $HOME/.zsh/zsh-autoswitch-virtualenv/autoswitch_virtualenv.plugin.zsh

# pandoc
export PATH="$HOME/Library/Python/3.7/bin:$PATH"

# Doom and local repos
export PATH="$HOME/.emacs.d/bin:$HOME/local/bioinfo/bin:$PATH"
# export JAVA_HOME=(/usr/libexec/java_home -v 1.8)'/jre'
export ARTEMIS_JAVA_JRE="/Library/Java/JavaVirtualMachines/openjdk-11.0.2.jdk/Contents/Home"
export PSQLRC="$HOME/.config/psql/psqlrc"
export HOMEBREW_NO_EMOJI=1

export BAT_THEME=ansi-light
if [ ! -n "$INSIDE_EMACS" ]; then
	alias cat=bat
fi

# show available tmux sessions
if [[ -z $TMUX ]]; then
	sessions=$(tmux ls 2>/dev/null | awk '! /attached/ { sub(":", "", $1); print $1; }' | xargs echo)
	if [[ ! -z $sessions ]]; then
		echo "⚠  Available tmux session(s): \033[34m$sessions\033[0m"
	fi
	unset sessions
fi
