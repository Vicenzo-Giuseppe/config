export ZPLUGINDIR=$ZDOTDIR/../

red='\033[1;31m'
rset='\033[0m'
grn='\033[1;32m'
blue='\033[1;34m'

function add_file() {
    [ -f "$ZDOTDIR/$1.sh" ] && source "$ZDOTDIR/$1.sh"
}

function add() {
    [ -f "$ZPLUGINDIR/$1" ] && source "$ZPLUGINDIR/$1"
}

function add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    if [ -d "$ZPLUGINDIR/plugins/$PLUGIN_NAME" ]; then 
        # For plugins
       add "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
       add "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    else
      echo -e 
        clear
        echo -e "$grn  ----------------------------------------------------------------$rset"
        echo -e "$red             Installing Plugin :: $blue $PLUGIN_NAME              $rset"
        echo -e "$grn  ----------------------------------------------------------------$rset"
        git clone "https://github.com/$1.git" "$ZPLUGINDIR/plugins/$PLUGIN_NAME"
    fi
}

function extract {  
  if [ -z "$1" ]; then
    echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz>"
    echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
  else 
    for n in "$@"
    do       
      if [ -f "$n" ] ; then
        case "${n%,}" in
          *.cbt|*.tar.bz2|*.tar.gz|*.tar.xz|*.tbz2|*.tgz|*.txz|*.tar)
            tar xvf "$n"       ;;             *.lzma)      unlzma ./"$n"      ;;
            *.bz2)       bunzip2 ./"$n"     ;;             *.cbr|*.rar)       unrar x -ad ./"$n" ;;
            *.gz)        gunzip ./"$n"      ;;             *.cbz|*.epub|*.zip)       unzip ./"$n"       ;;
            *.z)         uncompress ./"$n"  ;; *.zst)      unzstd ./"$n"      ;;
            *.7z|*.arj|*.cab|*.cb7|*.chm|*.deb|*.dmg|*.iso|*.lzh|*.msi|*.pkg|*.rpm|*.udf|*.wim|*.xar)
              7z x ./"$n"        ;;             *.xz)        unxz ./"$n"        ;;
              *.exe)       cabextract ./"$n"  ;;             *.cpio)      cpio -id < ./"$n"  ;;
              *.cba|*.ace)      unace x ./"$n"      ;;             *)                          
                echo "extract: '$n' - unknown archive method"                          return 1                          ;;
            esac       
          else
            echo "'$n' - file does not exist"           return 1       
            fi    
          done 
          fi 
        }

