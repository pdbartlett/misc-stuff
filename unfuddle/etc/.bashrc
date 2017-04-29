# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

########################################################################
# User specific aliases and functions go here (override system defaults)
########################################################################

# Set shell prompt to something pretty if terminal supports it
function updatePrompt {
  if [ "$TERM" == "dumb" ]; then
    export PS1="\u@\h: \w\n$ "
  elif [ "$TERM" == "screen" ]; then
    export PS1="\[\033[0;31m\][$WINDOW] \[\033[0;37m\]\u@\h: \[\033[0;36m\]\w\[\033[0m\]\n\$ "
  else
    export PS1="\[\033[0;37m\]\u@\h: \[\033[0;36m\]\w\[\033[0m\]\n\$ "
  fi
}
export PROMPT_COMMAND=updatePrompt

# My functions - general
function emacs {
  /usr/bin/emacs --no-splash $@ >/tmp/emacs.log 2>&1 &
}

# My vars - general
export ACOC=`which acoc`
export EDITOR="/usr/bin/emacs --no-splash -nw"
export USERNAME=$USER

# My aliases - acoc-only
alias df='$ACOC df'
alias ping='$ACOC ping'
alias top='$ACOC top'
alias traceroute='$ACOC traceroute'

# My aliases - general
alias cp='cp -ip'
alias df='df -h'
alias edd=$EDITOR
alias grep='grep --color=auto'
alias iftop='sudo iftop'
alias l='$ACOC ls -hlF'
alias la='$ACOC ls -ahlF'
alias lr='$ACOC ls -hlFR'
alias log='script ~/script.txt && vi ~/script.txt'
alias mv='mv -i'
alias p='$ACOC ps -ef'
alias rebash='. ~/.bashrc'
alias saar='sudo apt-get autoremove'
alias sagi='sudo $ACOC apt-get install'
alias sagr='sudo $ACOC apt-get remove'
alias scr='screen -d -R -e^Zz -p ='
alias ss='xscreensaver --no-splash &'
alias sspn='sudo shutdown -P now'
alias ssrn='sudo shutdown -r now'
alias testxmonad='echo ":t main" | ghci ~/.xmonad/xmonad.hs'
alias vi='vi -X'
alias virc='$EDITOR ~/.bashrc && . ~/.bashrc'

# Go stuff
export GOROOT=$HOME/go
export GOOS=linux
export GOARCH=386
