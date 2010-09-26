# TODO: Split into a Tradelink specific subfile

# Need to run as ksh to work correctly
if [[ -a ~/.profile ]]; then
	SHELL=/bin/ksh source ~/.profile
fi


################################################################################
# History settings
################################################################################
# Timestamps for when commands occurred make diagnosing problems easier
setopt EXTENDED_HISTORY

# Try to use .zhistory on local box before resorting to using the network
if [[ "$HOST" == "udesktop178" ]]; then
	export HISTFILE=/export/home/joeg/.zhistory
else
	if [[ -a /home/udesktop178/joeg/.zhistory ]]; then
		export HISTFILE=/home/udesktop178/joeg/.zhistory
	else
		export HISTFILE=$HOME/.zhistory
	fi
fi

export ANDROID_PATH=~/opt/android-sdk-linux_x86-1.6_r1;
if [[ -d $ANDROID_PATH ]]; then
	export PATH=$ANDROID_PATH/tools:$PATH
fi

if [[ -d /home/udesktop178/joeg/opt/bin ]]; then
	export PATH=/home/udesktop178/joeg/opt/bin:$PATH
	export SCONS_LIB_DIR=/home/udesktop178/joeg/opt/lib
fi

# number of lines kept in history
export HISTSIZE=100000

# number of lines saved in the history after logout
export SAVEHIST=100000

# append command to history file once executed
setopt inc_append_history

if [[ `uname -s` = "SunOS" ]]; then
	# Without this I don't get psrinfo
	export PATH=$PATH:/usr/sbin
fi

###############################################################################
# vcs_info settings
###############################################################################
# comment out until I find where to get this
#autoload -Uz vcs_info && vcs_info

# Enable git cvs and svn magic
zstyle ':vcs_info:*' enable git cvs svn darcs hg

_vcs_fmt_action="${BLD}${MAG}[${CYA}%b${NOR}-${BLD}${WHT}%a${MAG}]${NOR}"
_vcs_fmt_branch="${BLD}${MAG}[${CYA}%b${MAG}]${NOR}"
_vcs_fmt_branch_svn="${CYA}%b${RED}:${YLW}%r${NOR}"
_vcs_fmt_type="${BLD}${MAG}(${GRN}%s${MAG})${NOR}"

# CVS formats
zstyle ':vcs_info:cvs:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:cvs:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}

# Git formats
zstyle ':vcs_info:git:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:git:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}

# Hg formats
zstyle ':vcs_info:hg:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:hg:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}

# Git-svn formats
zstyle ':vcs_info:git-svn:*' branchformat ${_vcs_fmt_branch_svn}
zstyle ':vcs_info:git-svn:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:git-svn:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}

# Darcs formats
zstyle ':vcs_info:darcs:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:darcs:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}

# SVN-alike formats
zstyle ':vcs_info:svn:*' branchformat ${_vcs_fmt_branch_svn}
zstyle ':vcs_info:svn:*' actionformats \
        ${_vcs_fmt_action} \
        ${_vcs_fmt_type}
zstyle ':vcs_info:svn:*' formats \
        ${_vcs_fmt_branch} \
        ${_vcs_fmt_type}


###############################################################################
# preferred app settings
###############################################################################
if [[ -a /proc/cpuinfo ]]; then
	export MAKEFLAGS="-j"`cat /proc/cpuinfo | grep processor | wc -l`
else
	# We have to pipe to tr at the end to get rid of spaces and tabs
	# that solaris wc introduces that prevents shell string concatenation
	export MAKEFLAGS="-j"`psrinfo -v | grep virtual | wc -l | tr -d ' \\t'`
fi

export DS_DOMAIN="joegtest"
#export DS_SERVICES="~/.services"
export EDITOR="~/etc/launchemacs -n"


###############################################################################
# misc zsh settings
###############################################################################

setopt autopushd             # Push things onto the directory stack
setopt autocd                # cd to a dir when just its name is given
setopt chasedots             # Resolve symlinks before resolving parent dirs
setopt autolist              # Do a list on ambiguous completion
setopt automenu              # Do a menu after <Tab><Tab>
setopt autoparamslash        # Append a slash for directory completion
setopt completealiases       # Treat aliases as commands
setopt listpacked            # Use variable column widths
setopt globdots              # Assume leading . for hidden files

# Use terminfo from the last century
if [[ -d "/opt/tradelink/share/terminfo" ]]; then
	export TERMINFO=/opt/tradelink/share/terminfo
else
	if [[ -d "/opt/app/nonc++/ncurses-5.7/share/terminfo" ]]; then
		export TERMINFO=/opt/app/nonc++/ncurses-5.7/share/terminfo
	else
		if [[ -d "/export/home/joeg/ncurses-install/share/terminfo" ]]; then
			export TERMINFO=/export/home/joeg/ncurses-install/share/terminfo
		fi
	fi
fi

# # Need in order to get color on solaris
# if [[ -d "/net/udesktop178" ]]; then
# 	if TERM=xterm infocmp &> /dev/null # Use GNU ls if available
# 	then
# 		export TERM=xterm
# 	else
# 		export TERM=xterm
# 	fi
# fi

# system beep is irritating for tab completion
unsetopt beep

# Add mime support for opening files
autoload -U zsh-mime-setup
zsh-mime-setup


###############################################################################
# utilities
###############################################################################

# Give some convenient shortcuts for pushing and popping folder stack
alias -r b='pushd +1 > /dev/null'
alias -r f='pushd -0 > /dev/null'

# Ack is a nice replacement for grep, just does the right thing
# Really should be able to add to the existing make type, but will make mk a separate
# type for now.
alias -r ack='~/etc/ack --type-set incl=.incl --type-set tc=.tc --sort-files --type-set mk=.mk --type-set bejunk=.ii,.utii,.P --type=nobejunk --type-set pagedisplay=.page --type-add hh=.ipp'

alias -r cdl='cd /home/udesktop178/joeg'

alias -r recent='ls -l -r --sort=time'

alias -r e='~/etc/launch-emacs -n'

if [[ `uname -s` = "Linux" ]]; then
	alias -r tsk='ps aux | grep -i $@'
fi
if [[ `uname -s` = "SunOS" ]]; then
	alias -r tsk='ps -ef | grep -i $@'
fi

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# 'ls' output is easier to read when colored
if which gls &> /dev/null # Use GNU ls if available
then
	alias -r ls='gls --color=auto'
else
	alias -r ls='ls --color=auto'
fi

if which gfind &> /dev/null # Use GNU ls if available
then
	alias -r find='gfind'
fi

if which gmake &> /dev/null # Use GNU make if available
then
	alias -r make='gmake'
fi

if which ggrep &> /dev/null # Use GNU grep if available
then
	alias -r grep='ggrep --color=auto'
fi

if which gawk &> /dev/null # Use GNU awk if available
then
	alias -r awk='gawk'
fi

if which gsed &> /dev/null # Use GNU sed if available
then
	alias -r sed='gsed'
fi

if which gsleep &> /dev/null # Use GNU sed if available
then
	alias -r sleep='gsleep'
fi

if which gtar &> /dev/null # Use GNU tar if available
then
	alias -r tar='gtar'
fi

if which python &> /dev/null # Script requires python, and cat is rather essential
then
	alias -r cat='python ~/etc/safecat.py'
fi

# if which perl &> /dev/null # Script requires perl, and rm is rather essential
# then
# 	alias -r rm='~/etc/safe-rm-0.8/safe-rm'
# fi

alias l.='ls -d .*'     #list hidden files
alias -r up="cd .."
alias -r upup="cd ../.."
alias -r upupup="cd ../../.."
alias -r upupupup="cd ../../../.."
alias cl="clear;ls"

# Intuitively, searches current folder and subfolders
search () {
	find \( -type f -o -type d \) -name \*$1\*
}

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

alias linecount="find -regextype posix-extended -regex \".*\.(h|cpp|c|cc|py)\" | xargs wc -l"




################################################################################
# Completion settings
################################################################################

autoload -Uz compinit && compinit

#zstyle ':completion:*' completions 1
#zstyle ':completion:*' prompt '%e errors found:'

zstyle ':completion:*' completer \
    _expand \
    _complete \
    _match \
    _approximate \
    _prefix

# Cache completion results
zstyle ':completion:*' use-cache on

# Try to use .zhistory on local box before resorting to using the network
if [[ -a /home/udesktop178/joeg/.zshcache ]]; then
	zstyle ':completion:*' cache-path /home/udesktop178/joeg/.zshcache
else
	zstyle ':completion:*' cache-path $HOME/.zshcache
fi

# Make lower-case input case insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Better process completion
zstyle ':completion:*:processes' command 'ps -U $USERNAME -u $USERNAME -o pid,s,nice,stime,args'
zstyle ':completion:*:processes-names' command 'ps -U $USERNAME -u $USERNAME -o args'

# Ignore VCS directories when completing
zstyle ':completion:*:(all-|)files' ignored-patterns \
    '(|*/)CVS' \
    '(|*/).git' \
    '(|*/).svn'
zstyle ':completion:*:cd:*' ignored-patterns \
    '(|*/)#CVS' \
    '(|*/)#.git' \
    '(|*/)#.svn'

# Fix errors
zstyle ':completion:*:approximate:*' max-errors 2 numeric

# Ignore internal functions
zstyle ':completion:*:functions' ignored-patterns \
    '_*'

# Remove trailing slashes from directories
zstyle ':completion:*' squeeze-slashes true

# Attempt to complete many parts at once
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'

# Substitute things when tab completing
zstyle ':completion:*:expand:*' substitute true

# Insert as much as possible
zstyle ':completion:*:match:*' glob true
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:match:*' insert-unambiguous true

# Ignore where we are when completing directory names
zstyle ':completion:*' ignore-parents parent pwd directory

# Completion list settings
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

# Separate groups
zstyle ':completion:*' group-name ''

# Highlight current choice
zstyle ':completion:*' menu select



################################################################################
# Prompt settings
# Source: http://aperiodic.net/phil/prompt/
################################################################################

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

    if apm &> /dev/null; then
	PR_APM_RESULT=`apm`
    fi
}


setopt extended_glob
# preexec () {
#     if [[ "$TERM" == "screen" ]]; then
# 	local CMD=${1[(wr)^(*=*|sudo|-*)]}
# 	echo -n "\ek$CMD\e\\"
#     fi
# }


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
	# screen)
	#     PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	#     ;;
	*)
	    PR_TITLEBAR=''
	    ;;
    esac


    ###
    # # Decide whether to set a screen title
    # if [[ "$TERM" == "screen" ]]; then
	# PR_STITLE=$'%{\ekzsh\e\\%}'
    # else
	# PR_STITLE=''
    # fi


    ###
    # APM detection

    if which apm &> /dev/null; then
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
	if [[ 1 -ne $GNU_SCREEN_IS_RUNNING && $TERM == "screen" ]]; then
		export TERM=xterm
	fi

	# Creating new shells in GNU screen is jacked up on Solaris until you do this.
	if which stty &> /dev/null
	then
		stty sane

		# This disable Ctrl+s sending the STOP signal. This allows forward search in
		# zsh to work, and it prevents me from accidentally freezing all of my screen
		# instances (sending STOP to one sends it to all of them).
		stty -ixon
	fi

	# And this
	if which resize &> /dev/null
	then
		resize >& /dev/null
	fi

 	if [[ -s /etc/motd ]]; then
 	    cat /etc/motd
 	fi
	setprompt
fi
