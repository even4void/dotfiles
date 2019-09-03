# Usage: select_jdk <primary jdk dir> [<secondary jdk dirs>]
#            Sets the JAVA_HOME environment variable to <primary jdk dir> and puts
#            $JAVA_HOME/bin on PATH (removing old $JAVA_HOME/bin as well). The
#            specified JDK directories are also saved to ~/.jdk_home_cache.
#
#        select_jdk
#            This prompts you to select 1 or more JDKs from ~/.jdk_home_cache.
#            The selected JDKs are then passed as arguments to a recursive call
#            to select_jdk to match the first form described above.
#
function select_jdk
  if test (count $argv) -ne 0
    set OLD_JH $JAVA_HOME
    set -x JAVA_HOME $argv[1]
    set NEW_PATH $JAVA_HOME/bin
    for i in $PATH
      if test $i != $OLD_JH/bin
        set NEW_PATH $NEW_PATH $i
      end
    end
    set -x PATH $NEW_PATH
    if not set -q __SELECT_JDK_RECURSIVE__
      echo $JAVA_HOME >> $HOME/.jdk_home_cache
    end
    if test (count $argv) -gt 1
      set -x EXTRA_JAVA_HOMES (echo $argv[2..(count $argv)] | tr ' ' ':')
    else
      set -e EXTRA_JAVA_HOMES
    end
  else if test -e $HOME/.jdk_home_cache
    set index 1
    set candidates
    set pid %self
    set new_cache (echo $HOME/.jdk_home_cache.$pid)
    echo >$new_cache
    for jdkhome in (cat $HOME/.jdk_home_cache | sort | uniq)
      if test -d $jdkhome
        echo "[$index] $jdkhome"
        set candidates $candidates "$jdkhome"
        set index (expr $index + 1)
        echo $jdkhome >> $new_cache
      end
    end
    mv $new_cache $HOME/.jdk_home_cache
    if test $index != 1
      read -p 'echo "Select JDK(s) (separate multiple choices by whitespace)> "' indexes_raw
      eval set indexes $indexes_raw
      set jdks
      for index in $indexes
        set jdks $jdks $candidates[$index]
      end
      set __SELECT_JDK_RECURSIVE__ true
      select_jdk $jdks
    end
  end
end