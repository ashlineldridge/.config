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

# Reload the shell.
alias reload='exec $(which zsh)'

# Common tools.
alias bz=bazel
alias g=git
alias h=helm
alias hf=helmfile
alias j=just
alias k=kubectl
alias kb=kubebuilder
alias kctx=kubectx
alias kns=kubens
alias ku=kustomize
alias tf=terraform

# Replace ls with eza.
alias ls="eza --group-directories-first"
alias l="eza --group-directories-first"
alias ll="eza -l --group-directories-first"
alias l.="eza -d --group-directories-first .*"
alias ll.="eza -ld --group-directories-first .*"
alias la="eza -a --group-directories-first"
alias lla="eza -la --group-directories-first"

# Watch every 1 second by default.
alias watch='watch -n 1 '

# Search hidden files/directories except .git.
alias rg="rg --hidden --glob '!.git'"

# Grafana/Prometheus/Alertmanager.
# Following alias starts Grafana in the same way that it gets started when
# running `brew services start grafana` (recommended way of running Grafana
# on macOS: https://grafana.com/docs/grafana/latest/installation/mac/). See
# /opt/homebrew/opt/grafana/homebrew.grafana.service.
alias grafana='grafana-server --config=/opt/homebrew/etc/grafana/grafana.ini --homepath /opt/homebrew/share/grafana --packaging=brew cfg:default.paths.logs=/opt/homebrew/var/log/grafana cfg:default.paths.data=/opt/homebrew/var/lib/grafana cfg:default.paths.plugins=/opt/homebrew/var/lib/grafana/plugins'
alias prom='prometheus --config.file=/opt/homebrew/etc/prometheus.yml'
alias am='alertmanager --config.file=/opt/homebrew/etc/alertmanager.yml'
