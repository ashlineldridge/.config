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
    brew install emacs-plus@{{emacs-version}}
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
    @echo ">>> Upgrading AI agents"
    @echo "\n----- Upgrading Claude Code"
    claude update
    @echo "\n----- Upgrading Cursor Agent CLI"
    brew upgrade cursor-cli
    @echo "\n----- Upgrading Gemini CLI"
    brew upgrade gemini-cli
    @echo "\n---- Upgrading OpenCode"
    brew upgrade opencode

symlink-install:
    # Not all programs respect XDG conventions and this recipe creates symlinks
    # back to files in this repo so that they get source controlled. It also
    # symlinks source-controlled bin files into ~/bin.
    @echo ">>> Installing symlinks"
    ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
    mkdir -p ~/.local/share/gnupg
    mkdir -p ~/.claude
    ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf
    ln -sf ~/.config/claude/*.json ~/.claude/
    ln -sf ~/.config/cursor/*.json ~/Library/Application\ Support/Cursor/User/
    mkdir -p ~/bin
    for f in ~/.config/bin/*; do ln -sf "$f" ~/bin/; done

install: symlink-install brew-install emacs-install
