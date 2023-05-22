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
  prompt.zsh
  vterm.zsh
  completion.zsh
  shortcuts.zsh
)

# Load zsh configuration files. Note: this intentionally sources lib/env.zsh
# even though it will have already been sourced via the ~/.zshenv symlink. This
# is due to https://www.zsh.org/mla/users/2003/msg00600.html.
for f in "${libs[@]}"; do
  lib="${XDG_CONFIG_HOME}/zsh/lib/${f}"
  if [[ ! -f "${lib}" ]]; then
    echo >&1 "Expected zsh lib file ${lib} not found"
    exit 1
  fi
  source "${lib}"
done

if [[ -f "${XDG_CONFIG_HOME}/zsh/lib/secrets.zsh" ]]; then
    source "${XDG_CONFIG_HOME}/zsh/lib/secrets.zsh"
fi
