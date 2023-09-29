### ZSH completion configuration.

# Enable completion system. The fpath array variable stores the names of the
# directories that contain completion scripts (e.g., run `echo "${fpath[@]}"`).
# By default, Homebrew seems to install zsh completion scripts under
# /opt/homebrew/share/zsh/site-functions (these will appear as symlinks to
# Homebrew cellar files). When we install tooling that isn't managed by Homebrew,
# we can install the completion scripts directly into the site-functions directory;
# e.g., `rustup completions zsh > /opt/homebrew/share/zsh/site-functions/_rustup`.
autoload -U +X compinit; compinit

# Configure fzf. These commands are taken from the changes fzf makes when you
# run `$(brew --prefix)/opt/fzf/install` as per the installation instructions
# here: https://github.com/junegunn/fzf#installation.
if [[ -f /opt/homebrew/opt/fzf/shell/completion.zsh ]]; then
    [[ $- == *i* ]] && source /opt/homebrew/opt/fzf/shell/completion.zsh 2> /dev/null
    source /opt/homebrew/opt/fzf/shell/key-bindings.zsh
fi

# Manually source the gcloud completion configuration if it exists. Apparently the
# file is missing a directive which prevents it from being autoloaded. See:
# https://github.com/Homebrew/homebrew-cask/issues/143596.
if [[ -f /opt/homebrew/share/zsh/site-functions/_google_cloud_sdk ]]; then
    source /opt/homebrew/share/zsh/site-functions/_google_cloud_sdk
fi
