# .config

My XDG base directory configuration.

```
mkdir -p /etc/zsh
cat << EOF > /etc/zsh/zshrc
XDG_CONFIG_HOME="${HOME}/.config"
XDG_CACHE_HOME="${HOME}/.cache"
XDG_DATA_HOME="${HOME}/.local/share"
ZDOTDIR="${XDG_CONFIG_HOME}/zsh"
EOF
```

1. Clone this repo into ~/.config
2. In ~/.config/homebrew run: brew bundle
3. Run: chsh -s /usr/local/bin/zsh
4. Run in ~: ln -s .config/zsh/zshenv ~/.zshenv
5. Done
