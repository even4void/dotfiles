## The following is adapted from Throsten Ball's own config. At some point
## I should probably rewrite it and use Zsh's built-in facilities to
## manage Git status, see:
## https://scriptingosx.com/2019/07/moving-to-zsh-06-customizing-the-zsh-prompt/
git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(git rev-parse --short HEAD 2> /dev/null) || return
    local dirstatus=" %F{242}${ref#refs/heads/}%f"
    if [[ ! -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
        dirstatus=" %F{242}${ref#refs/heads/}*%f"
    fi
    echo $dirstatus
}

local dir_info="%{$fg_bold[blue]%}%(5~|%-1~/.../%2~|%4~)%{$reset_color%}"
local promptnormal="%(?.%{$fg_bold[magenta]%}.%{$fg_bold[red]%})❯ %{$reset_color%}"
local promptjobs="%(?.%{$fg_bold[yellow]%}.%{$fg_bold[red]%})❯ %{$reset_color%}"
local ret_code="%(?..%{$fg_bold[red]%} %?)%{$reset_color%}"

PROMPT='${dir_info}$(git_prompt_info) %(1j.$promptjobs.$promptnormal)'

preexec() {
    timer=${timer:-$SECONDS}
    # printf "\x1b]0;%s\x07" "$1";
}

_is_ssh() {
    [[ -n ${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-} ]]
}

precmd() {
    ## on remote host only
    # if _is_ssh || (( EUID == 0 )); then
    #     remote="@${(%):-%m}"
    # else
    #     remote=""
    # fi
    if [[ $VIRTUAL_ENV ]]; then
        venv=`basename $VIRTUAL_ENV`
    else
        venv=""
    fi
    if [ $timer ]; then
        toc=$(($SECONDS - $timer))
        if [ ${toc} -ge 5 ]; then
            export RPROMPT="${ret_code} %{$fg_bold[yellow]%}${toc}s%{$reset_color%} %F{242}${venv}%f"
        else
            export RPROMPT="${ret_code} %F{242}${venv}%f"
        fi
        unset timer
    fi
}
