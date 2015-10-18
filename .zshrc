#
# Author Peter P. Neuromantic <peter.brovchenko@gmail.com>
# This configuration is based on the .zshrc config of my brother
#

export LANG=en_US.UTF-8
export EDITOR="emacs -b"

bindkey -e # Emacs-like shortcuts
bindkey '\e[3~' delete-char # now del character work

# History commans settings
HISTFILE="$HOME/.zsh_history"
SAVEHIST=2000
HISTSIZE=2000
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS

# Prompt settings
autoload -U promptinit colors
colors
promptinit

# Set prompt for users and root
if [[ $EUID == 0 ]]; then
	PS1="[%~]%{$fg_bold[red]%}#%{$reset_color%}%b "
else
	PS1="[%~]%{$fg_bold[green]%}$%{$reset_color%}%b "
fi

# Autocomplete settings
autoload -U compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true

# Aliases settings
alias mc="mc -b"
alias ls="ls --color=auto"
alias grep='grep --colour=auto'
alias mocp="mocp -T nightly_theme"

# EOF
