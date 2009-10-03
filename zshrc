# TODO: Split into a Tradelink specific subfile

if [[ -a ~/.profile ]]; then
	# Need to run as ksh to work correctly
	SHELL=/bin/ksh source ~/.profile
fi

# Add stuff to path from /opt if .xsession hasn't already
#if [[ $DISPLAY = '' ]]; then
#    export PATH=`~/etc/build_path.py`
#fi

# Timestamps for when commands occurred make diagnosing problems easier
setopt EXTENDED_HISTORY

# Try to use .zhistory on local box before resorting to using the network
if [[ -a /home/udesktop178/joeg/.zhistory ]]; then
	export HISTFILE=/home/udesktop178/joeg/.zhistory
else
	if [[ -a /home/titan/joeg/.zhistory ]]; then
		export HISTFILE=/home/titan/joeg/.zhistory
	fi
fi

if [[ -d ~/opt/android-sdk-linux_x86-1.5_r2/tools ]]; then
 	export PATH=~/opt/android-sdk-linux_x86-1.5_r2/tools:$PATH
fi

# number of lines kept in history
export HISTSIZE=100000

# number of lines saved in the history after logout
export SAVEHIST=100000

# append command to history file once executed
setopt inc_append_history


# system beep is irritating for tab completion
unsetopt beep

# Push the current folder onto the stack everytime I switch folders
setopt autopushd

# Give some convenient shortcuts for pushing and popping folder stack
alias -r b='pushd +1 > /dev/null'
alias -r f='pushd -0 > /dev/null'

# Ack is a nice replacement for grep, just does the right thing
alias -r ack='~/etc/ack'

alias -r up='cd ..'

alias -r cdl='cd /home/udesktop178/joeg'

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# 'ls' output is easier to read when colored
# Doesn't work on solaris
#alias -r ls='ls --color=auto'
if which gls &> /dev/null # Use GNU ls if available
then
	alias -r ls='gls --color=auto'
else
	alias -r ls='ls --color=auto'
fi

# Intuitively, searches current folder and subfolders
search () {
	find \( -type f -o -type d \) -name \*$1\*
}

# Add mime support for opening files
autoload -U zsh-mime-setup
zsh-mime-setup

# Enable completions
zmodload zsh/complist
autoload -U compinit
compinit

# Pick up new stuff in $path
_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1	# Because we didn't really complete anything
}
zstyle ':completion:::::' completer _force_rehash _complete _approximate
# VERY fancy completion
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

# bash function to decompress archives - http://www.shell-fu.org/lister.php?id=375  
extract () {  
    if [ -f $1 ] ; then  
        case $1 in  
            *.tar.bz2)   tar xvjf $1        ;;  
            *.tar.gz)    tar xvzf $1     ;;  
            *.bz2)       bunzip2 $1       ;;  
            *.rar)       unrar x $1     ;;  
            *.gz)        gunzip $1     ;;  
            *.tar)       tar xvf $1        ;;  
            *.tbz2)      tar xvjf $1      ;;  
            *.tgz)       tar xvzf $1       ;;  
            *.zip)       unzip $1     ;;  
            *.Z)         uncompress $1  ;;  
            *.7z)        7z x $1    ;;  
            *)           echo "'$1' cannot be extracted via >extract<" ;;  
        esac  
    else  
        echo "'$1' is not a valid file"  
    fi  
}

alarm() {
    echo 'xmessage $1' | at $2
}

# Need in order to get color on solaris
if [[ -d "/home/udesktop178" ]]; then
	if [[ $COLORTERM = "gnome-terminal" ]]; then
		export TERM=dtterm
	fi

	if [[ $TERM = "xterm" ]]; then
		export TERM=dtterm
	fi
fi

################################
#BEGIN SUPER FANCY PROMPT
#Source: http://aperiodic.net/phil/prompt/
################################
function precmd {

    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))


    ###
    # Truncate the path if it's too long.
    
    PR_FILLBAR=""
    PR_PWDLEN=""
    
    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}
    
    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	    ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
	PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi


    ###
    # Get APM info.

    if which ibam > /dev/null; then
	PR_APM_RESULT=`ibam --percentbattery`
    elif which apm > /dev/null; then
	PR_APM_RESULT=`apm`
    fi
}


setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	echo -n "\ek$CMD\e\\"
    fi
}


setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst


    ###
    # See if we can use colors.

    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	#eval PR_$color='%{$terminfo[bold]%}%F{(L)color}'
	#eval PR_$color='%{$terminfo[bold]%F{${(L)color}}%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	#eval PR_LIGHT_$color='%{$f[${(L)color}]%}'
	(( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"


    ###
    # See if we can use extended characters to look nicer.
    
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}

    
    ###
    # Decide if we need to set titlebar text.
    
    case $TERM in
	xterm*)
	    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
	    ;;
	screen)
	    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
	*)
	    PR_TITLEBAR=''
	    ;;
    esac
    
    
    ###
    # Decide whether to set a screen title
    if [[ "$TERM" == "screen" ]]; then
	PR_STITLE=$'%{\ekzsh\e\\%}'
    else
	PR_STITLE=''
    fi
    
    
    ###
    # APM detection
    
    if which ibam > /dev/null; then
	PR_APM='$PR_RED${${PR_APM_RESULT[(f)1]}[(w)-2]}%%(${${PR_APM_RESULT[(f)3]}[(w)-1]})$PR_LIGHT_BLUE:'
    elif which apm > /dev/null; then
	PR_APM='$PR_RED${PR_APM_RESULT[(w)5,(w)6]/\% /%%}$PR_LIGHT_BLUE:'
    else
	PR_APM=''
    fi
    
    
    ###
    # Finally, the prompt.

    PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_CYAN$PR_SHIFT_IN$PR_ULCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m:%l\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_HBAR${(e)PR_FILLBAR}$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_MAGENTA%$PR_PWDLEN<...<%~%<<\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_URCORNER$PR_SHIFT_OUT\

$PR_CYAN$PR_SHIFT_IN$PR_LLCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?$PR_BLUE:)\
${(e)PR_APM}$PR_YELLOW%D{%H:%M}\
$PR_LIGHT_BLUE:%(!.$PR_RED.$PR_WHITE)%#$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_NO_COLOUR '

    RPROMPT=' $PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_HBAR$PR_SHIFT_OUT\
($PR_YELLOW%D{%a,%b%d}$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

    PS2='$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

# To help emacs tramp mode, all the fancy terminal stuff confuses it
if [ "$TERM" = "dumb" ]
then
  unsetopt zle
  unsetopt prompt_cr
  #unsetopt prompt_subst
  setopt prompt_subst
  unfunction precmd
  unfunction preexec
  unfunction setprompt
  PS1='$ '
  PS2='$ '
  PROMPT='$ '
  RPROMPT='$ '
else
# 	clear
 	cat /etc/motd
	setprompt
fi
