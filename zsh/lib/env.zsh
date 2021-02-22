# This file must be symlinked to ~/.zshenv to bootstrap zsh to follow XDG conventions.

# XDG base directories.
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

# XDG-based zsh home.
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

# Force GnuPG to use XDG data home. Don't use config home as GnuPG
# intermingles config with secrets.
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"

# Force Terraform to use XDG
export TF_CLI_CONFIG_FILE="${XDG_CONFIG_HOME}/terraform/config"

# Force Starship to use XDG
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}"/starship/config.toml

# Make Emacs the default editor
export EDITOR=emacsclient

# History control
export HISTFILE="${XDG_DATA_HOME}/zsh/history"
export HISTSIZE=32768
export SAVEHIST="${HISTSIZE}"
export HISTFILESIZE="${HISTSIZE}"

# Prefer AU English and use UTF-8
export LANG="en_AU.UTF-8"
export LC_ALL="en_AU.UTF-8"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

# Home directories
export DEVELOPMENT_HOME="${HOME}/Development"

# Golang variables
export GOPATH="${DEVELOPMENT_HOME}/go"
export GOROOT="/usr/local/opt/go/libexec"

# Path
export PATH="/usr/local/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/opt/gettext/bin:/usr/local/opt/llvm/bin:${GOROOT}/bin:${GOPATH}/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:${HOME}/bin:${HOME}/.cargo/bin:/usr/local/kubebuilder/bin"

# Kubernetes
export KUBECONFIG="${HOME}/.kube/config"

# Required for GPG signing of Git commits
export GPG_TTY="$(tty)"
