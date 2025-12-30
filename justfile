mac-user      := "ae"
mac-group     := "admin"
emacs-version := "31"

default:
    @just --list

brew-install:
    @echo ">>> Installing Brew packages"
    brew bundle

brew-dump:
    @echo ">>> Dumping list of Brew packages"
    brew bundle dump -f

emacs-install: emacs-clean
    @echo ">>> Installing Emacs"
    brew install emacs-plus@{{emacs-version}} --with-savchenkovaleriy-big-sur-icon
    # The entire application is copied rather than creating an alias as that
    # seems to screw with the icon that gets shown by Raycast/Spotlight.
    sudo rm -rf /Applications/Emacs.app
    sudo cp -r /opt/homebrew/opt/emacs-plus@{{emacs-version}}/Emacs.app /Applications/
    sudo chown -R {{mac-user}}:{{mac-group}} /Applications/Emacs.app
    @echo "-----"
    @echo "Emacs has been installed."
    @echo "Remember to enable Emacs notifications and update Raycast hotkey."

emacs-uninstall:
    @echo ">>> Uninstalling Emacs"
    brew uninstall emacs-plus@{{emacs-version}}
    sudo rm -rf /Applications/Emacs.app

emacs-clean:
    @echo ">>> Cleaning all Emacs packages"
    rm -rf emacs/var/elpaca
    rm -rf emacs/var/eln-cache
    rm -rf emacs/eln-cache
    rm -rf emacs/var/treesit

ai-upgrade:
    #!/usr/bin/env bash
    set -euo pipefail
    echo ">>> Upgrading AI agents"
    # Upgrade agent binaries.
    goose update
    claude update
    brew upgrade gemini-cli
    brew upgrade opencode
    # Upgrade ACP adapters.
    npm install -g @zed-industries/claude-code-acp
    codex_acp_url="$(curl -s https://api.github.com/repos/zed-industries/codex-acp/releases/latest |
        jq -r '.assets[] | select(.name | contains("aarch64-apple-darwin")) | .browser_download_url')"
    curl -sL "${codex_acp_url}" | tar -xz -C ~/bin/

symlink-install:
    # As not all programs respect XDG conventions, this recipe creates symlinks
    # back to files in this repo so that they get source controlled.
    @echo ">>> Installing symlinks"
    ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
    mkdir -p ~/.local/share/gnupg
    ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf

install: symlink-install brew-install emacs-install
