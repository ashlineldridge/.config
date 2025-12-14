### ZSH Vterm configuration.

# For more details, see: https://github.com/akermu/emacs-libvterm/blob/master/README.md.

if [[ ! -v EMACS_VTERM_PATH ]]; then
  echo >&2 "Error: expected EMACS_VTERM_PATH to be set."
  return
fi

# Source the canned Zsh script provided by the emacs-libvterm package.
source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
