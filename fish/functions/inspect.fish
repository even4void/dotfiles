function inspect
    (head -n 3; tail -n 3) < "$argv" | column -t
end

