# ZSH configuration

# Shell options
setopt extended_glob
setopt hist_find_no_dups
setopt inc_append_history
setopt share_history

libs=(
  env.zsh
  aliases.zsh
  colors.zsh
  functions.zsh
  fzf.zsh
  secrets.zsh
)

# Load zsh configuration files. Note: this intentionally sources lib/env.zsh
# even though it will have already been sourced via the ~/.zshenv symlink. This
# is due to https://www.zsh.org/mla/users/2003/msg00600.html.
for f in "${libs[@]}"; do
  lib="${XDG_CONFIG_HOME}/zsh/lib/${f}"
  if [[ ! -f "${lib}" ]]; then
    echo >&1 "Expected zsh lib file ${lib} not found"
    # exit 1
  fi
  source "${lib}"
done

# Initialize Starship prompt
eval "$(starship init zsh)"

# Shortcuts
bindkey "<ESC>b" backward-word
bindkey "<ESC>f" forward-word
bindkey "^[b" backward-word
bindkey "^[f" forward-word

# Enables completion system for sourced scripts below
autoload -U +X compinit; compinit

#
compinit -d "${XDG_CACHE_HOME}/zsh/zcompdump-${ZSH_VERSION}"

# Enable broot shell integration
source /Users/aeldridge/.config/broot/launcher/bash/br

# Workaround for https://github.com/kubernetes/kubectl/issues/125
# source <(kubectl completion zsh | sed '/_bash_comp/ s/^#*/#/') || true

