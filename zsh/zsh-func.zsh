startpostgres() {
  local pidfile="/usr/local/var/postgres/postmaster.pid"
  if [ -s $pidfile ] && kill -0 $(cat $pidfile | head -n 1) > /dev/null 2>&1; then
    echo "Already running"
  else
    pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start
  fi
}

stoppostgres() {
  pg_ctl -D /usr/local/var/postgres stop
}

mkdircd() {
  mkdir -p $1 && cd $1
}

serve() {
  local port=${1:-8000}
  local ip=$(ipconfig getifaddr en0)
  echo "Serving on ${ip}:${port} ..."
  python -m SimpleHTTPServer ${port}
}

beautiful() {
  while
  do
    i=$((i + 1)) && echo -en "\x1b[3$(($i % 7))mo" && sleep .2
  done
}

spinner() {
  while
  do
    for i in "-" "\\" "|" "/"
    do
      echo -n " $i \r\r"
      sleep .1
    done
  done
}

f() {
  find . -iname "*${1}*"
}

enc64() {
  openssl base64 -in "${1}" | awk -v ext=(get_ext ${1}) '{ str1=str1 $0 }END{ print "background:url(data:image/"ext";base64,"str1");" }'|pbcopy
  echo "${1} encoded to clipboard"
}

bkp() {
  cp -a "${1}" "${1}_(date --iso-8601=seconds)"
}

cl() {
  history --max=1 | sed -e 's/^ +//' | pbcopy
  echo "Last stdout encoded to clipboard"
}

dash() {
  open "dash://${1}"
}

day() {
  echo (date -j -f '%Y-%m-%d' ${1} +'%A')
}

flow_all() {
  flow | grep -Ee '^Error --' | rev | cut -d' ' -f1 | rev | cut -d: -f1 | sort -u
}

git_search() {
  git log -S"$argv" --pretty=format:%H | xargs -n1 git show 
}

ip() {
  ifconfig | grep "inet " | awk '{ print $2 }' | grep -v "^127"
  curl -Ss icanhazip.com
}

snif() {
  sudo lsof -iTCP -sTCP:LISTEN -P -n  
}

stata_help() {
  links https://www.stata.com/help.cgi\?"${1}"
}

wget_single() {
  wget --no-parent --timestamping --convert-links --page-requisites --no-directories --no-host-directories --span-hosts --adjust-extension ${1}
}

