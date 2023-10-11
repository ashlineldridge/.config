### ZSH top-level configuration.

# Shell options.
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

# Configure vterm if we're running it.
if [[ "${INSIDE_EMACS:-}" == vterm ]]; then
  libs+=(vterm.zsh)
fi

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

# Load private.zsh if it exists. This is where I keep work-specific config.
if [[ -f "${XDG_CONFIG_HOME}/zsh/lib/private.zsh" ]]; then
  source "${XDG_CONFIG_HOME}/zsh/lib/private.zsh"
fi
