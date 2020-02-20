source $HOME/.config/fish/aliases.fish
source $HOME/.config/fish/env.fish
source $HOME/.config/fish/private.fish

set fish_greeting ""

function fish_title
    true
end

# set pure_symbol_prompt "" # because iTerm2 shell integration

# Python autoenv (https://is.gd/VmxDRJ) + Pyenv
# source $HOME/.config/fish/activate.fish
# source $HOME/.pyenv/plugins/pyenv-autoenv/bin/pyenv-autoenv

# OCaml
# source $HOME/.opam/opam-init/init.fish >/dev/null 2>/dev/null
# or true

# iTerm2
test -e {$HOME}/.iterm2_shell_integration.fish
and source {$HOME}/.iterm2_shell_integration.fish

# Virtual Fish
eval (python3 -m virtualfish auto_activation global_requirements)
