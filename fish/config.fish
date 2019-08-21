. ~/.config/fish/aliases.fish
. ~/.config/fish/env.fish

set fish_greeting ""

. /Users/chl/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

