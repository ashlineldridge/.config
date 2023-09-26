# ZSH configuration

# Shell options
setopt extended_glob
setopt extended_history
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt inc_append_history

libs=(
  env.zsh
  aliases.zsh
  functions.zsh
  completion.zsh
)

# Load zsh configuration files. Note: this intentionally sources lib/env.zsh
# even though it will have already been sourced via the ~/.zshenv symlink as
# some variables such as HISTFILE don't carry across (quite annoying).
for f in "${libs[@]}"; do
  lib="${XDG_CONFIG_HOME}/zsh/lib/${f}"
  if [[ ! -f "${lib}" ]]; then
    echo >&1 "Expected zsh lib file ${lib} not found"
    exit 1
  fi
  source "${lib}"
done

if [[ -v INSIDE_EMACS ]]; then
  # Mirror the Eshell welcome message.
  echo "Welcome to the Emacs terminal"
  echo
fi
