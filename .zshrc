# users generic .zshrc file for zsh(1)

alias vim=emacs
alias emacs=vim

alias ls=~/.bin/sl

## Environment variable configuration
#
# LANG
# http://curiousabt.blog27.fc2.com/blog-entry-65.html
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8


## Backspace key
#
bindkey "^?" backward-delete-char

## Default shell configuration
#
# set prompt
# colors enables us to idenfity color by $fg[red].
autoload colors
colors
case ${UID} in
0)
    PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
    PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
    SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
*)
#
# Color
#
DEFAULT=$'%{\e[1;0m%}'
RESET="%{${reset_color}%}"
#GREEN=$'%{\e[1;32m%}'
GREEN="%{${fg[green]}%}"
BLUE=$'%{\e[1;35m%}'
RED="%{${fg[red]}%}"
CYAN="%{${fg[cyan]}%}"
#
# Prompt
#
setopt prompt_subst
PROMPT='${RED}${USER}@${HOST} ${GREEN}%~${RESET}
${GREEN}%(5~,%-2~/.../%2~,%~)% ${RED} $ ${RESET}'


# Show git branch when you are in git repository
# http://blog.s21g.com/articles/1159

_set_env_git_current_branch() {
  GIT_CURRENT_BRANCH=$( git branch 2> /dev/null | grep '^\*' | cut -b 3- )
}

_update_rprompt () {
  if [ "`git ls-files 2>/dev/null`" ]; then
    RPROMPT='${CYAN}git[branch:$GIT_CURRENT_BRANCH]${RESET}'
  else
    RPROMPT=""
  fi
}

precmd()
{
  _set_env_git_current_branch
  _update_rprompt
}


chpwd()
{
  _set_env_git_current_branch
  _update_rprompt
}



#    PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
#    PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
#    SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
#    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
#        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"


    ;;
esac

#case "$TERM" in
#    xterm*|kterm*|rxvt*)
#    PROMPT=$(print "%B%{\e[34m%}%m:%(5~,%-2~/.../%2~,%~)%{\e[33m%}%# %b") PROMPT=$(print "%{\e]2;%n@%m: %~\7%}$PROMPT") # title bar
#    ;;
#    *)
#    PROMPT='%m:%c%# '
#    ;;
#esac

# auto change directory
#
setopt auto_cd

# auto directory pushd that you can get dirs list by cd -[tab]
#
setopt auto_pushd

# command correct edition before each completion attempt
#
setopt correct

# compacked complete list display
#
setopt list_packed

# no remove postfix slash of command line
#
setopt noautoremoveslash

# no beep sound when complete list displayed
#
setopt nolistbeep

# Match without pattern
# ex. > rm *~398
# remove * without a file "398". For test, use "echo *~398"
setopt extended_glob

## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes 
#   to end of it)
#
bindkey -e

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end


## Command history configuration
#
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data


## Completion configuration
#
fpath=(~/.zsh/functions/Completion ${fpath})
autoload -U compinit
compinit -u


## zsh editor
#
autoload zed


## Prediction configuration
#
autoload predict-on
#predict-off


## Alias configuration
#
# expand aliases before completing
#
setopt complete_aliases     # aliased ls needs if file/dir completions work

alias where="command -v"
alias j="jobs -l"
alias 'ps?'='ps alx |grep '

case "${OSTYPE}" in
freebsd*|darwin*)
    alias ls="ls -G"
    zle -N expand-to-home-or-insert
    bindkey "@"  expand-to-home-or-insert
    ;;
linux*)
    alias ls="ls --color"
    ;;
esac

alias la="ls -a"
alias lf="ls -F"
alias ll="ls -l"

alias du="du -h"
alias df="df -h"

alias su="su -l"

case "${OSTYPE}" in
darwin*)
    alias updateports="sudo port selfupdate; sudo port outdated"
    alias portupgrade="sudo port upgrade installed"
    export PATH=$PATH:/opt/local/bin:/opt/local/sbin/
    export PATH=$PATH:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/
    export PATH=$PATH:"/Applications/Adobe Flex Builder 3/sdks/3.2.0/bin"
    ;;
freebsd*)
    case ${UID} in
    0)
        updateports() 
        {
            if [ -f /usr/ports/.portsnap.INDEX ]
            then
                portsnap fetch update
            else
                portsnap fetch extract update
            fi
            (cd /usr/ports/; make index)

            portversion -v -l \<
        }
        alias appsupgrade='pkgdb -F && BATCH=YES NO_CHECKSUM=YES portupgrade -a'
        ;;
    esac
    ;;
esac


## terminal configuration
# http://journal.mycom.co.jp/column/zsh/009/index.html
unset LSCOLORS

case "${TERM}" in
xterm)
    export TERM=xterm-color

    ;;
kterm)
    export TERM=kterm-color
    # set BackSpace control character

    stty erase
    ;;

cons25)
    unset LANG
	export LSCOLORS=ExFxCxdxBxegedabagacad

    export LS_COLORS='di=01;32:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30'
    zstyle ':completion:*' list-colors \
        'di=;36;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
    ;;

kterm*|xterm*)
   # Terminal.app
#    precmd() {
#        echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
#    }
   # export LSCOLORS=exfxcxdxbxegedabagacad
		export LSCOLORS=gxfxcxdxbxegedabagacad
    export LS_COLORS='di=1;34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30'

    zstyle ':completion:*' list-colors \
        'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
    ;;

dumb)
    echo "Welcome Emacs Shell"
    alias man="man -P cat"
    # http://en.yummy.stripper.jp/?eid=561843
    # C-c C-r 最後のコマンドの出力の1行目へジャンプ
    # C-c C-o 最後のコマンドの出力を削除（キルリングへ）
    # C-c C-p ひとつ前の出力グループの先頭へジャンプ
    # C-c C-n ひとつ後の出力グループの先頭へジャンプ
    ;;
esac


## load user .zshrc configuration file
#
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine


## Easy directory change and setup
#  Suzuki Tomohiro

alias 'vi'='vim'
alias 'src'='exec zsh' 
alias 'm'='make'
alias 'g'='~/dotfiles/bin/grep.sh'
alias 'mn'='make native-code'
alias 'mc'='make clean'
alias s='screen -S main'
alias pon='predict-on'
alias poff='predict-off'
alias p='ping -c 4'
alias om='omake'

export EDITOR=vim
export PATH=$PATH:$HOME/local/bin:/usr/local/git/bin
export PATH=$PATH:$HOME/dotfiles/bin:/work/tsuzuki/local/bin
export PATH=$PATH:/sbin:usr/local/bin
export MANPATH=$MANPATH:/opt/local/man:/usr/local/share/man
export PYTHONPATH=./third_party:$PYTHONPATH

expand-to-home-or-insert () {
        if [ "$LBUFFER" = "" -o "$LBUFFER[-1]" = " " ]; then
                LBUFFER+="~/"
        else
                zle self-insert
        fi
}

##
# ls color for black background.
export LSCOLORS=gxfxcxdxbxegedabagacad


function rmf(){
   for file in $*
   do
      __rm_single_file $file
   done
}

function __rm_single_file(){
       if ! [ -d ~/.Trash/ ]
       then
               command /bin/mkdir ~/.Trash
       fi

       if ! [ $# -eq 1 ]
       then
               echo "__rm_single_file: 1 argument required but $# passed."
               exit
       fi

       if [ -e $1 ]
       then
               BASENAME=`basename $1`
               NAME=$BASENAME
               COUNT=0
               while [ -e ~/.Trash/$NAME ]
               do
                       COUNT=$(($COUNT+1))
                       NAME="$BASENAME.$COUNT"
               done

               command /bin/mv $1 ~/.Trash/$NAME
       else
               echo "No such file or directory: $file"
       fi
}

#alias rm='rmf'

alias ssmake="cd ~/git/simple_server;make; popd"

if [ -e ~/.zshrc_local ]
then
  source ~/.zshrc_local
fi
