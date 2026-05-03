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

# See: https://elpa.nongnu.org/nongnu-devel/doc/eat.html#Shell-Integration
if [[ "${INSIDE_EMACS:-}" == "eat" ]]; then
  source "${EAT_SHELL_INTEGRATION_DIR}/zsh"
fi

# See: https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
if [[ "${INSIDE_EMACS:-}" == "vterm" ]]; then
  source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
fi

# See: https://github.com/dakra/ghostel#shell-integration
if [[ "${INSIDE_EMACS:-}" == "ghostel" ]]; then
  source "${EMACS_GHOSTEL_PATH}/etc/shell/ghostel.zsh"
fi

# Load private.zsh if it exists.
if [[ -f "${XDG_CONFIG_HOME}/zsh/lib/private.zsh" ]]; then
  source "${XDG_CONFIG_HOME}/zsh/lib/private.zsh"
fi
