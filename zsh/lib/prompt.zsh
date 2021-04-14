# This file initialises the zsh prompt. I was using Starship but switched back to Pure Prompt
# as Starship starts to become unreasonably slow on large Git repositories. This file assumes
# that Pure has been installed using `npm install --global pure-prompt`.

autoload -U promptinit; promptinit

# Show Git stash status.
zstyle :prompt:pure:git:stash show yes

prompt pure
