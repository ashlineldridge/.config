# Shortcuts
alias dev="cd ~/Development"
alias devh="cd ~/Development/home"
alias devw="cd ~/Development/work"
alias devc="cd ${XDG_CONFIG_HOME}"
alias devg="cd ${GOPATH}"
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
#alias kpt='kpt --k8s-schema-source=builtin'
alias prom='prometheus --config.file=/usr/local/etc/prometheus.yml'
alias am='alertmanager --config.file=/usr/local/etc/alertmanager.yml'
alias grafana='grafana-server --config=/usr/local/etc/grafana/grafana.ini --homepath /usr/local/share/grafana --packaging=brew cfg:default.paths.logs=/usr/local/var/log/grafana cfg:default.paths.data=/usr/local/var/lib/grafana cfg:default.paths.plugins=/usr/local/var/lib/grafana/plugins'

# Because I still accidentally launch Vim and start type Emacs commands.
alias vim='echo No. Use Emacs.; false'

# Emacs
alias e=emacs
alias et='emacs --no-window-system --quick'
alias ec='emacsclient --no-wait'

# Others
alias watch='watch -n 1 '
alias bz=bazel
