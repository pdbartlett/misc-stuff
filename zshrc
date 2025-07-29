# .zshrc extract for common setup.
# vim: syn=sh

# Variables.
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
PROMPT='%(?.%F{green}[OK].%F{red}[%?])%f %B%F{240}%3~%f %F{white}%#%f%b '

# Set zsh options
setopt auto_cd
setopt no_case_glob
setopt extended_history
setopt share_history
setopt append_history
setopt inc_append_history
setopt hist_ignore_all_dups
setopt hist_reduce_blanks

# Use vi keybindings
bindkey -v

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Get aliases to a known, clean state before we start
unalias -a

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Editing
export EDITOR=vim
alias edd="${EDITOR}"

# Shell config
export RCNAME=zshrc
export RCPATH="${HOME}/.${RCNAME}"
alias edrc="${EDITOR} ${RCPATH} && source ${RCPATH}"

# Misc. aliases
alias wordle="${HOME}/src/misc-stuff/other/wordle.sh"

# Shell utils
alias df='df -h'
function md() {
  mkdir -p ${1} && cd ${1}
}
function qwhich() {
  which "$@" >/dev/null 2>&1
}
function witch() {
  if qwhich ${1}; then ls -lhF $(which ${1}); fi
}
function psgrep() {
  ps -ef | grep $@ | grep -v grep
}

# Homebrew (first, so installed tools are visible)
for d in "/opt" "${HOME}"; do
  local maybe="${d}/homebrew/bin/brew"
  if [[ -x "${maybe}" ]]; then
    local prefix="$(${maybe} --prefix)"
    PATH="${prefix}/sbin":"${prefix}/bin":$PATH
    break
  fi
done
if qwhich brew; then
  export HOMEBREW_AUTO_UPDATE_SECS=14400
  export HOMEBREW_CLEANUP_MAX_AGE_DAYS=28
  export HOMEBREW_CLEANUP_PERIODIC_FULL_DAYS=7
  export HOMEBREW_FORBIDDEN_LICENSES="AGPL-1.0-or-later	OSL-1.0+ SSPL-1.0+ CAL-1.0 CPAL-1.0+ CPOL-1.02 EUPL-1.0+ SISSL SISSL-1.2+ Watcom-1.0+ CC-BY-NC-1.0+ CC-BY-NC-ND-1.0+ CC-BY-NC-SA-1.0+"
  export HOMEBREW_FORCE_BREWED_CURL=1
  export HOMEBREW_FORCE_BREWED_GIT=1
  export HOMEBREW_NO_INSECURE_REDIRECT=1
  export HOMEBREW_UPGRADE_GREEDY=1
  function buu() {
    brew update && brew upgrade
  }
  if [[ -f $(brew --prefix)/etc/bash_completion ]]; then
    . $(brew --prefix)/etc/bash_completion
  fi
fi
if [[ -d /opt/homebrew/opt/curl/lib/pkgconfig ]]; then
  export PKG_CONFIG_PATH="/opt/homebrew/opt/curl/lib/pkgconfig"
fi

# "Alternatives"
if qwhich bat; then
  alias cat=bat
fi
if qwhich lsd; then
  alias l='lsd -l'
  alias la='lsd -al'
  alias ls='lsd'
elif qwhich exa; then
  alias l='exa -l --git --colour-scale'
  alias la='exa -al --git --colour-scale'
  alias ls='exa'
else
  alias l='ls -lhFG'
  alias la='ls -alhFG'
fi

# ABC
if qwhich abc2midi; then
  if qwhich m4; then
    function a4() {
      local stem=${1%\.abc4}
      local temp_abc=${stem}_gen.abc
      m4 $1 >$temp_abc
      abc $temp_abc ${2:-mid}
    }
  fi
  function abc() {
    local stem=${1%\.abc}
    local suffix=${2:-mid}
    case $suffix in
      mid) abc2midi $1 -o ${stem}.mid ;;
      pdf) abcm2ps  $1 -O - | ps2pdf -sPAPERSIZE=a4 - > ${stem}.pdf ;;
      ps)  abcm2ps  $1 -O ${stem}.ps ;;
    esac
  }
fi

# Android
if [[ -d "$HOME/Library/Android/sdk" ]]; then
  export ANDROID_HOME=$HOME/Library/Android/sdk
  export PATH=$PATH:$ANDROID_HOME/platform-tools
fi

# (Ana|Mini)conda
if qwhich conda; then
  alias cua='conda update --all'
fi

# Git(hub)
function gitall() {
  local root="$(pwd)"
  for gitdir in $(find . -name .git); do
    cd "${gitdir}/.."
    pwd
    git "$@"
    cd "${root}"
  done
}

# Go
if qwhich go; then
  if qwhich brew; then
    PATH="${PATH}:$(brew --prefix)/opt/go/libexec/bin"
  fi
  export GOPATH="${HOME}/go"  # Default, but be explicit.
  PATH="${PATH}:${GOPATH}/bin"
fi

# iterm2 shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Micromamba
if qwhich micromamba; then
# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
export MAMBA_EXE="$(brew --prefix)/opt/micromamba/bin/micromamba";
export MAMBA_ROOT_PREFIX="${HOME}/micromamba";
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias micromamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<
  alias mm='micromamba'
  alias mmu='micromamba update'
  export MM_DEF=data
  micromamba activate ${MM_DEF}
fi

# Node / npm
if qwhich nodenv; then
  eval "$(nodenv init -)"
fi

# R
if qwhich R; then
  alias R='R --no-save'
fi

# Rbenv
if qwhich rbenv; then
  eval "$(rbenv init -)"
fi

# Scala
if qwhich scala; then export SBT_OPTS='-XX:MaxPermSize=128M -Xmx8192M'; fi

# Sqlite (keg-only install)
_SQL=/Users/pdbartlett/homebrew/opt/sqlite/bin/sqlite3
if [[ -x "${_SQL}" ]]; then alias sql="${_SQL}"; fi

# Stable (pony dependency management)
if qwhich stable; then alias pony='stable env ponyc'; fi

# TeX
if qwhich tlmgr; then
  function tlu() {
    local TLM=$(which tlmgr)
    sudo -v
    sudo "${TLM}" update --self --all
  }
fi

# tmx2
if qwhich tmx2; then alias tm='tmx2 new -A -s main'; fi

# Utilities
function banner() {
  echo
  if qwhich hr; then hr; fi
  echo "### $@"
  if qwhich hr; then hr; fi
}

function utd() {
  if qwhich tlmgr; then
    banner 'tlmgr (needs sudo)'
    tlu
  fi
  if qwhich brew; then
    banner 'brew'
    buu
  fi
  if qwhich conda; then
    banner 'conda'
    cua --yes
  fi
  if qwhich micromamba; then
    for e in data disc; do
      banner "micromamba ($e)"
      mm activate "$e"
      mmu -a
      echo "Installed: $(python3 -V)\nAvailable:"
      mm search python -v 2>/dev/null | \
        egrep '^\s+(python )?3\.1[3-9]\.[0-9]+\s' | \
        sed 's/ python//'
    done
  fi
  if qwhich nodenv; then
    banner 'nodenv'
    nodenv update
  fi
  if qwhich npm; then
    banner 'npm'
    npm update
  fi
  if qwhich R; then
    banner 'R'
    R -q -e 'update.packages(ask=F)'
    #R -q -e 'withr::with_makevars(c(OBJCXXFLAGS = "${CXX17STD}"), \
    #         update.packages(ask=F))'
  fi
  if qwhich rbenv; then
    banner 'rbenv'
    local oldrubies=/Users/pdbartlett/.rubies.old
    local newrubies=/Users/pdbartlett/.rubies.new
    rbenv install --list | grep '^\s*[0-9]' >${newrubies}
    if [[ -f ${oldrubies} ]]; then
      diff -s ${oldrubies} ${newrubies};
    fi
    mv -f ${newrubies} ${oldrubies}
    echo 'Installed:'
    rbenv versions
    (rbenv version | grep -qv system) && gem update
  fi
  if qwhich rustup; then
    banner 'rustup'
    rustup update
  fi
  local here="$(pwd)"
  local root="${HOME}/src"
  if [[ -d $root ]]; then
    banner 'github'
    cd "${root}"
    gitall pull
    gitall status -s
    cd "${here}"
  fi
  banner "Up-to-date as of $(date)"
}

# Tidy up path
PATH="${PATH}:${HOME}/bin"
PATH=$(printf "%s" "${PATH}" | /usr/bin/awk -v RS=: -v ORS=: '!($0 in a) {a[$0]; print}')
