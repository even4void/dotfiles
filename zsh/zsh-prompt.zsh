git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  local dirstatus=" %{$fg_bold[green]%}${ref#refs/heads/}%{$reset_color%}"
  if [[ ! -z $(git status --porcelain 2> /dev/null | tail -n1) ]]; then
    dirstatus=" %{$fg_bold[yellow]%}${ref#refs/heads/}%{$reset_color%}"
  fi

  echo $dirstatus
}

local dir_info="%{$fg_bold[blue]%}%(5~|%-1~/.../%2~|%4~)%{$reset_color%}"
local promptnormal="%(?.%{$fg_bold[blue]%}.%{$fg_bold[red]%}) %{$reset_color%}"
local promptjobs="%(?.%{$fg_bold[yellow]%}.%{$fg_bold[red]%}) %{$reset_color%}"

PROMPT='${dir_info}$(git_prompt_info) %(1j.$promptjobs.$promptnormal)'

preexec() {
  timer=${timer:-$SECONDS}
}

_is_ssh() {
  [[ -n ${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-} ]]
}

precmd() {
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
