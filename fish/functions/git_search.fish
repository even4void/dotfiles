function git_search
    git log -S"$argv" --pretty=format:%H | xargs -n1 git show 
end

