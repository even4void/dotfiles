# This is a mix of personal stuff, Torsten Ball's zsh functions and
# Mathias Bynens's bash functions.

function expand-alias() {
    zle _expand_alias
    zle self-insert
}

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

e() {
  emacsclient -a '' -n "$@" 2>/dev/null || command emacs;
}

function mkd() {
	mkdir -p "$@" && cd "$_";
}

function cdf() { # short for `cdfinder`
	cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')";
}

function gz() {
	local origsize=$(wc -c < "$1");
	local gzipsize=$(gzip -c "$1" | wc -c);
	local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l);
	printf "orig: %d bytes\n" "$origsize";
	printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio";
}

function digga() {
	dig +nocmd "$1" any +multiline +noall +answer;
}

function tre() {
	tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX;
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

# credit: http://nparikh.org/notes/zshrc.txt
extract () {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)  tar -jxvf $1                        ;;
            *.tar.gz)   tar -zxvf $1                        ;;
            *.bz2)      bunzip2 $1                          ;;
            *.dmg)      hdiutil mount $1                    ;;
            *.gz)       gunzip $1                           ;;
            *.tar)      tar -xvf $1                         ;;
            *.tbz2)     tar -jxvf $1                        ;;
            *.tgz)      tar -zxvf $1                        ;;
            *.zip)      unzip $1                            ;;
            *.ZIP)      unzip $1                            ;;
            *.pax)      cat $1 | pax -r                     ;;
            *.pax.Z)    uncompress $1 --stdout | pax -r     ;;
            *.rar)      unrar x $1                          ;;
            *.Z)        uncompress $1                       ;;
            *)          echo "'$1' cannot be extracted/mounted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

webmp4() {
    if [ -z "$(command -v ffmpeg)" ]; then
        printf "%s\n" "ffmpeg is not installed" >&2
        return 1
    fi
    while [ $# -ne 0 ]; do
        ffmpeg -i "$1" "${1%.*}.mp4"
        shift
    done
}
