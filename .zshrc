#
# Author: Peter P. Neuromantic <peter.brovchenko@gmail.com>
# This configuration file is based on the .zshrc file of my brother.
#

export LANG=en_US.UTF-8
export LESS=eFRX
export EDITOR="nano" # Default console editor

export JAVA_HOME=/usr/lib64/jdk1.8.0_152
export SCALA_HOME=/usr/lib64/scala
export PATH=$PATH:$JAVA_HOME/bin:$SCALA_HOME/bin

bindkey -e # Emacs-like shortcuts
bindkey '\e[3~' delete-char # now del character works ok

# History commans settings
HISTFILE="$HOME/.zsh_history"
SAVEHIST=2000
HISTSIZE=2000
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt SHARE_HISTORY # One history between all terminals

# Prompt settings
autoload -U promptinit colors && colors && promptinit

# Enable minimal vcs info (full doc - http://www.zsh.org/mla/users/2008/msg00842.html)
autoload -Uz vcs_info && vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' formats "(%s:%b)" # vcs name and current branch
precmd () { vcs_info }
setopt prompt_subst
VCS_PROMPT="%{$fg_bold[blue]%}\$vcs_info_msg_0_%{$reset_color%}"

# Set spec symbol for uid (for root is a bold, red #, for other uids - bold, green $).
UID_SYM="%{$fg_bold[green]%}$%{$reset_color%}%b"
if [[ $EUID == 0 ]]; then UID_SYM="%{$fg_bold[red]%}#%{$reset_color%}%b" fi

# hostname prompt
HP="%{$fg_bold[green]%}%m%{$reset_color%}"
if [[ $EUID == 0 ]]; then HP="%{$fg_bold[red]%}%m%{$reset_color%}" fi

# Set prompt.
PS1="[$HP:%2~]$VCS_PROMPT$UID_SYM "

# Autocomplete settings
autoload -U compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true

# Aliases settings
alias df="df -h"
alias lsa="ls -lAh --color=auto --group-directories-first"
alias ls="ls --color=auto --group-directories-first"
alias grep="grep --colour=auto"
alias mocp="mocp -T nightly_theme"
alias get_bat="(acpi | cut -d ',' -f 2)"

# Work alias
# alias berest="ssh -p 12322 peter@c.calamar.ga"
# alias remote="ssh -p 12322 www@test.uid.me"
# alias upages="ssh -p 12322 www@upages.io"
# alias cuid1="ssh -l www -p 12322 195.216.243.140"
# alias cuid2="ssh -l www -p 12322 195.216.243.141"
alias 21stf="ssh -p 16622 web@21stf.ru"
alias dslife="ssh peter.brovchenko@cbroker-deploy.reg.ru"
alias morph="ssh peter.brovchenko@31.31.205.31"
alias logs="ssh peter.brovchenko@logs.reg.ru"

alias bright_night="\
echo 160 > /sys/class/backlight/amdgpu_bl0/brightness && \
redshift -o -l 47.235714:39.701505 -b 0.8 -t 3600:3600 > /dev/null"

alias bright_dev="\
echo 180 > /sys/class/backlight/amdgpu_bl0/brightness && \
redshift -o -l 47.235714:39.701505 -b 0.8 -t 4000:4000 > /dev/null"

alias bright_normal="\
echo 200 > /sys/class/backlight/amdgpu_bl0/brightness && \
redshift -o -l 47.235714:39.701505 -b 0.8 -t 4500:4500 > /dev/null"

alias bright_film="\
echo 220 > /sys/class/backlight/amdgpu_bl0/brightness && \
redshift -o -l 47.235714:39.701505 -b 0.9 -t 5000:5000 > /dev/null"

alias bright_max="\
echo 255 > /sys/class/backlight/amdgpu_bl0/brightness && \
redshift -o -l 47.235714:39.701505 -b 1.0 -t 6000:6000 > /dev/null"

alias bright0="redshift -o -l 47.235714:39.701505 -b 0.8 -t 4000:4000 > /dev/null"
alias bright1="redshift -o -l 47.235714:39.701505 -b 0.8 -t 5250:5250 > /dev/null"
alias bright2="redshift -o -l 47.235714:39.701505 -b 0.8 -t 6500:6500 > /dev/null"
alias bright3="redshift -o -l 47.235714:39.701505 -b 0.9 -t 6500:6500 > /dev/null"
alias bright4="redshift -o -l 47.235714:39.701505 -b 1.0 -t 6500:6500 > /dev/null"

PATH=$PATH:~/perl5/bin
PERL5LIB=$PERL5LIB:~/perl5/lib/perl5

export ANDROID_HOME=/home/peter/opt/android
export _JAVA_OPTIONS='-Dsun.java2d.opengl=true'
export _JAVA_AWT_WM_NONREPARENTING=1

export TERM=xterm-256color
export COLORTERM=trueolor
