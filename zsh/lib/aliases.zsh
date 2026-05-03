### ZSH aliases.

# Development directories.
alias dev='cd ~/dev'
alias devh='cd ~/dev/home'
alias devw='cd ~/dev/work'
alias devc='cd ${XDG_CONFIG_HOME}'
alias devg='cd ${GOPATH}'

# Emacs aliases.
alias ec=emacsclient
alias ecsudo='SUDO_EDITOR=emacsclient sudo -e'
alias ff='emacsclient -n'

# Reload the shell.
alias reload='exec $(which zsh)'

# Common tools.
alias g=git
alias j=just
alias k=kubectl
alias kctx=kubectx
alias kns=kubens
alias tf=tofu
alias terraform=tofu

# Eza aliases.
alias l="eza --group-directories-first"
alias ll="eza -l --group-directories-first"
alias la="eza -a --group-directories-first"
alias lla="eza -la --group-directories-first"
alias l.="eza -d --group-directories-first .*"
alias ll.="eza -ld --group-directories-first .*"

# Watch every 1 second by default.
alias watch='watch -n 1 '

# Search hidden files/directories except .git.
alias rg="rg --hidden --glob '!.git'"
