# .config

My XDG base directory configuration.

1. Clone this repo into ~/.config
2. In ~/.config/homebrew run: brew bundle
3. Run: chsh -s /usr/local/bin/zsh
4. Run in ~: ln -s .config/zsh/zshenv ~/.zshenv
5. Install iTerm2
6. Set iTerm2 to load settings from ~/.config/iterm
7. mkdir -p $XDG_DATA_HOME/zsh (put this in Makefile)
8. ln -s ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf
