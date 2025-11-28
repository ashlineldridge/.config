### ZSH environment variables.

# This file must be symlinked to ~/.zshenv to bootstrap zsh to follow XDG.

# XDG base directories.
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

# XDG-based zsh home.
export ZDOTDIR="${XDG_CONFIG_HOME}/zsh"

# History control.
export HISTFILE="${XDG_DATA_HOME}/zsh/history"
export HISTSIZE=10000
export SAVEHIST="${HISTSIZE}"
export HISTFILESIZE="${HISTSIZE}"

# Force GnuPG to use XDG data home. Don't use config home as GnuPG
# intermingles config with secrets.
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"

# Force pass to use XDG.
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# Golang variables.
export GOPATH="${HOME}/dev/go"
export GOROOT="/opt/homebrew/opt/go/libexec"

# Variables used by tooling that depend on Xcode.
export DEVELOPER_DIR="/Applications/Xcode.app/Contents/Developer"
export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"

# Java (using latest https://docs.aws.amazon.com/corretto).
export JAVA_HOME="/Library/Java/JavaVirtualMachines/current/Contents/Home"

# Path.
export PATH="${HOME}/bin:${HOME}/.local/bin:${HOME}/.cargo/bin:${HOME}/.ghcup/bin:/opt/homebrew/bin:/opt/homebrew/opt/curl/bin:/opt/homebrew/opt/llvm/bin:${GOROOT}/bin:${GOPATH}/bin:${JAVA_HOME}/bin:/usr/local/bin:/usr/bin:/bin:/opt/homebrew/sbin:/usr/sbin:/sbin"

# Make Emacs the default editor.
export EDITOR=emacsclient

# Prompt.
# See prompt sequences here: https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html.
# See hex colors here: https://www.ditig.com/256-colors-cheat-sheet.
export PROMPT='%B%F{#ffafaf}%~ ❯ %f%b'

# Undo Emacs-specific overrides (for when zsh is run from within Emacs).
unset PAGER
unset KUBECTX_IGNORE_FZF
