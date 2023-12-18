mac-user      := "ae"
mac-group     := "staff"
emacs-version := "29"
cargo-bin-dir := "~/.cargo/bin"
go-bin-dir    := "~/dev/go/bin"

default:
    @just --list

install-brew-pkgs:
    @echo ">>> Installing Brew packages"
    brew bundle

install-cargo-bins:
    @echo ">>> Installing Cargo binaries"
    # TODO

install-go-bins:
    @echo ">>> Installing Go binaries"
    # TODO

alias update-emacs := install-emacs
install-emacs: clean-emacs
    @echo ">>> Installing/updating Emacs"
    brew install emacs-plus@{{emacs-version}} \
        --with-imagemagick \
        --with-poll \
        --with-native-comp \
        --with-modern-black-variant-icon
    # The entire application is copied rather than creating an alias as that
    # seems to screw with the icon that gets shown by Raycast/Spotlight.
    sudo rm -rf /Applications/Emacs.app
    sudo cp -r /opt/homebrew/Cellar/emacs-plus@{{emacs-version}}/Emacs.app /Applications/
    sudo chown -R {{mac-user}}:{{mac-group}} /Applications/Emacs.app

uninstall-emacs:
    @echo ">>> Uninstalling Emacs"
    brew uninstall emacs-plus@{{emacs-version}}
    sudo rm -rf /Applications/Emacs.app

clean-emacs:
    @echo ">>> Cleaning all Emacs packages"
    rm -rf emacs/var/elpaca
    rm -rf emacs/var/eln-cache
    rm -rf emacs/eln-cache

dump-brew-pkgs:
    @echo ">>> Saving list of Brew packages"
    brew bundle dump -f

dump-cargo-bins:
    @echo ">>> Saving list of Cargo binaries"
    ls -1 {{cargo-bin-dir}} > cargo-bins.txt

dump-go-bins:
    @echo ">>> Saving list of Go binaries"
    ls -1 {{go-bin-dir}} > go-bins.txt

# As not all programs respect XDG conventions, this recipe creates symlinks
# back to files in this repo so that they get source controlled.
create-symlinks:
    @echo ">>> Installing symlinks"
    ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
    mkdir -p ~/.local/share/gnupg
    ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf

dump-all: dump-cargo-bins dump-go-bins dump-brew-pkgs
install-all: create-symlinks install-brew-pkgs install-cargo-bins install-go-bins install-emacs
