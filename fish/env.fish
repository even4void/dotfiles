set -x EDITOR nvim

# basedir defaults (at least, some of them)
# http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
set -x XDG_DATA_HOME "/Users/chl/.local/share"
set -x XDG_CONFIG_HOME "/Users/chl/.config"

set -x PATH '/Users/chl/.config/bin' $PATH
set -x PATH '/Users/chl/.local/bin' $PATH
set -x PATH '/Users/chl/.emacs.d/bin' $PATH
set -x PATH '/Users/chl/.cargo/bin' $PATH
set -x PATH '/usr/local/opt/sqlite/bin' $PATH
set -x PATH '/Users/chl/.stack/programs/x86_64-osx/ghc-8.6.5/bin/' $PATH
set -x PATH '/Users/chl/.pyenv/bin' $PATH

set -g fish_user_paths '/usr/local/sbin' $fish_user_paths

set -x PYENV_ROOT $HOME/.pyenv
set -x PATH $PYENV_ROOT/bin $PATH
status --is-interactive; and . (pyenv init -|psub)

set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)'/jre'
set -x NODE_PATH '/usr/local/lib/node_modules'
set -x RUST_SRC_PATH (rustc --print sysroot)'/lib/rustlib/src/rust/src'

