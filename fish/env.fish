set -x EDITOR nvim

set -x HOMEBREW_GITHUB_API_TOKEN '90ca58618d8e91ee87329213679cf1d1a05efb3e'

set -x PATH '/Users/chl/.config/bin' $PATH
set -x PATH '/Users/chl/.emacs.d/bin' $PATH
set -x PATH '/Users/chl/.cargo/bin' $PATH
set -x PATH '/usr/local/opt/sqlite/bin' $PATH
set -x PATH '/Users/chl/.stack/programs/x86_64-osx/ghc-8.6.5/bin/' $PATH
set -x PATH '/Users/chl/.pyenv/bin' $PATH

set -x PYENV_ROOT '~/.pyenv'
set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)'/jre'
set -x NODE_PATH '/usr/local/lib/node_modules'
set -x RUST_SRC_PATH (rustc --print sysroot)'/lib/rustlib/src/rust/src'

