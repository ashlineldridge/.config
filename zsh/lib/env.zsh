# This file must be symlinked to ~/.zshenv to bootstrap zsh to follow XDG conventions.

# XDG base directories.
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

# XDG-based zsh home.
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

# Force GnuPG to use XDG data home. Don't use config home as GnuPG
# intermingles config with secrets.
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"

# Required for GPG signing of Git commits
export GPG_TTY="$(tty)"

# Force pass to use XDG
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# Force Terraform to use XDG
# Commented out for now because it breaks `terraform login`.
# export TF_CLI_CONFIG_FILE="${XDG_CONFIG_HOME}/terraform/config"

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
export DEV_HOME="${HOME}/dev"

# Golang variables
export GOPATH="${DEV_HOME}/go"
export GOROOT="/opt/homebrew/opt/go/libexec"

# Kubernetes
export KUBECONFIG="${HOME}/.kube/config"

# Docker
export DOCKER_SCAN_SUGGEST="false"

# Variables used by tooling that depend on Xcode.
export DEVELOPER_DIR="/Applications/Xcode.app/Contents/Developer"
export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"

# Java (using latest https://docs.aws.amazon.com/corretto).
export JAVA_HOME="$(/usr/libexec/java_home 2> /dev/null || true)"

# Path.
export PATH="${HOME}/bin:/opt/homebrew/bin:/opt/homebrew/opt/llvm/bin:${GOROOT}/bin:${GOPATH}/bin:${JAVA_HOME}/bin:/usr/local/bin:/usr/bin:/bin:/opt/homebrew/sbin:/usr/sbin:/sbin:${HOME}/.cargo/bin:${HOME}/.krew/bin:${HOME}/.local/bin"
