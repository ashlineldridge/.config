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
    # Create aliases (see: https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#usage)
    osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@{{emacs-version}}/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
    osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@{{emacs-version}}/Emacs Client.app" at posix file "/Applications" with properties {name:"Emacs Client.app"}'
    @echo "-----"
    @echo "Emacs has been successfully installed"

emacs-uninstall:
    @echo ">>> Uninstalling Emacs"
    brew uninstall emacs-plus@{{emacs-version}}
    sudo rm -rf /Applications/Emacs.app
    @echo "-----"
    @echo "Emacs has been successfully uninstalled"

emacs-clean:
    @echo ">>> Cleaning all Emacs packages"
    rm -rf emacs/var/elpaca
    rm -rf emacs/var/eln-cache
    rm -rf emacs/eln-cache
    rm -rf emacs/var/treesit
    @echo "-----"
    @echo "Emacs packages have cleaned"

agent-upgrade:
    @echo ">>> Upgrading AI agents"
    @echo "\n----- Upgrading Claude Code"
    claude update
    @echo "\n----- Upgrading Cursor Agent CLI"
    brew upgrade cursor-cli
    @echo "\n----- Upgrading Pi"
    npm install -g @mariozechner/pi-coding-agent
    @echo "-----"
    @echo "AI agents have been ugpraded"

symlink-install:
    # Not all programs respect XDG conventions and this recipe creates symlinks
    # back to files in this repo so that they get source controlled. It also
    # symlinks source-controlled bin files into ~/bin.
    @echo ">>> Installing symlinks"
    ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
    mkdir -p ~/.local/share/gnupg
    ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf
    # Claude and Cursor config now managed via spacejunk/aeldridge
    mkdir -p ~/bin
    for f in ~/.config/bin/*; do ln -sf "$f" ~/bin/; done
    @echo "-----"
    @echo "Symlinks have been installed"

install: symlink-install brew-install emacs-install
