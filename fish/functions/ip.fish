function ip
    ifconfig | grep "inet " | awk '{ print $2 }' | grep -v "^127"
    curl -Ss icanhazip.com
end
