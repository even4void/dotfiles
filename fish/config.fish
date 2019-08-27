source $HOME/.config/fish/aliases.fish
source $HOME/.config/fish/env.fish

set fish_greeting ""

# Python autoenv (https://is.gd/VmxDRJ) + Pyenv
# source $HOME/.config/fish/activate.fish
# source $HOME/.pyenv/plugins/pyenv-autoenv/bin/pyenv-autoenv

# OCaml
. $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# iTerm2
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

