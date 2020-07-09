git_prompt_info() {
  local dirstatus=""
  local dirty="%{$fg_bold[yellow]%} %{$reset_color%}"

  if [[ ! -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
    dirstatus=$dirty
  fi

  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo " %{$fg_bold[yellow]%}${ref#refs/heads/}$dirstatus%{$reset_color%}"
}

local dir_info="%{$fg_bold[blue]%}%(5~|%-1~/.../%2~|%4~)%{$reset_color%}"
local promptnormal="%(?.%{$fg_bold[blue]%}.%{$fg_bold[red]%}) %{$reset_color%}"
local promptjobs="%(?.%{$fg_bold[cyan]%}.%{$fg_bold[red]%}) %{$reset_color%}"

PROMPT='${dir_info}$(git_prompt_info) %(1j.$promptjobs.$promptnormal)'

function preexec() {
  timer=${timer:-$SECONDS}
}

_is_ssh() {
  [[ -n ${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-} ]]
}

function precmd() {
  if _is_ssh || (( EUID == 0 )); then
    remote="@${(%):-%m}"
  else
    remote=""
  fi
  if [ $timer ]; then
    toc=$(($SECONDS - $timer))
    if [ ${toc} -ge 5 ]; then
      export RPROMPT="%F{blue}${remote} %F{cyan}${toc}s %{$reset_color%}"
    else
      export RPROMPT="%F{blue}${remote} %{$reset_color%}"
    fi
    unset timer
  fi
}
