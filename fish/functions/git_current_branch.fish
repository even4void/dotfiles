function git_current_branch
    set -l ref (git symbolic-ref --quiet HEAD 2> /dev/null)
    set -l ret $status
    if [ $ret != 0 ]
        [ $ret == 128 ]; and return  # no git repo.
        set -l ref (git rev-parse --short HEAD 2> /dev/null); or return
    end
    string replace 'refs/heads/' "" $ref
end

