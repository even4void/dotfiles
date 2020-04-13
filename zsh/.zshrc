# Partly inspired by Thorsten Ball's config
# https://github.com/mrnugget/dotfiles/blob/master/zshrc

typeset -U PATH
autoload colors; colors;

# autoload -U promptinit; promptinit

### CUSTOM FILES ###
source ~/.config/zsh/zsh-comp.zsh      # completions
source ~/.config/zsh/zsh-alias.zsh     # alias
source ~/.config/zsh/zsh-func.zsh      # functions
source ~/.config/zsh/zsh-prompt.zsh    # prompt

### HISTORY ###
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

### KEY BINDINGS ###
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

### ENV ###
# export TERM=xterm-256color

export PATH="$HOME/local/bin:$HOME/.config/bin:$PATH"

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
