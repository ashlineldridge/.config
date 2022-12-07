# Shortcuts
alias dev="cd ~/dev"
alias devh="cd ~/dev/home"
alias devw="cd ~/dev/work"
alias devc="cd ${XDG_CONFIG_HOME}"
alias devg="cd ${GOPATH}"
alias dev0="cd ~/dev/home/00rg"
alias g="git"
alias j="jobs"

# Detect which `ls` flavor is in use
if command ls --color > /dev/null 2>&1; then
  # GNU `ls`
  alias ls="command ls --color"
else
  # OS X `ls`
  alias ls="command ls -G"
fi

# Use exa for common file listing commands.
alias l="exa"
alias ll="exa -l"
alias l.='exa -d .*'
alias ll.='exa -ld .*'
alias la="exa -a"
alias lla="exa -la"

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Enhanced WHOIS lookups
alias whois="whois -h whois-servers.net"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Lock the screen (when going AFK)
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec zsh -l && source ~/.zshenv"

# Terraform
alias tf="terraform"

# GUID generator with clipboard copy
alias guid="uuidgen | tr '[:upper:]' '[:lower:]' | pbcopy && pbpaste"

# Hub (Github helper) aliased over standard git command
#eval "$(hub alias -s)"

# Kubernetes tools
alias k=kubectl
alias kctx=kubectx
alias kns=kubens
alias h=helm
alias kb=kubebuilder
alias ku=kustomize
alias prom='prometheus --config.file=/opt/homebrew/etc/prometheus.yml'
alias am='alertmanager --config.file=/opt/homebrew/etc/alertmanager.yml'
# Following alias starts Grafana in the same way that it gets started when
# running `brew services start grafana` (recommended way of running Grafana
# on macOS: https://grafana.com/docs/grafana/latest/installation/mac/). See
# /opt/homebrew/opt/grafana/homebrew.grafana.service.
alias grafana='grafana-server --config=/opt/homebrew/etc/grafana/grafana.ini --homepath /opt/homebrew/share/grafana --packaging=brew cfg:default.paths.logs=/opt/homebrew/var/log/grafana cfg:default.paths.data=/opt/homebrew/var/lib/grafana cfg:default.paths.plugins=/opt/homebrew/var/lib/grafana/plugins'

# Emacs
alias e=emacs
alias et='emacs --no-window-system --quick'
alias ec='emacsclient --no-wait'
# Following deletes LSP/DAP session files and prevents zsh complaining if none exist.
alias lsp-clean="zsh -c 'setopt +o nomatch; rm -f ~/.config/emacs/.lsp-session-*'"
alias dap-clean="zsh -c 'setopt +o nomatch; rm -f ~/.config/emacs/.dap-*'"

# Make the clear command clear all data from the buffer in vterm (like it normally does in other terminals).
# See: https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
if [[ "${INSIDE_EMACS:-}" = 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# Force isync/mbsync to follow XDG conventions
alias mbsync="mbsync -c ${XDG_CONFIG_HOME}/isync/mbsyncrc"

# Watch every 1 second by default.
alias watch='watch -n 1 '

# Bazel.
alias bz=bazel

# Search hidden files/directories except .git.
alias rg="rg --hidden --glob '!.git'"

# Opinionated shfmt diff.
alias shfmt_diff='shfmt -i 2 -ci -sr -bn -d'

# Dockerfile linting.
alias dockerfile_lint='docker run -it --rm -v "${PWD}:/root" projectatomic/dockerfile-lint dockerfile_lint -p -f'
