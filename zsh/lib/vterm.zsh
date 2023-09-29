### ZSH Vterm configuration.

# For more details, see: https://github.com/akermu/emacs-libvterm/blob/master/README.md.

if [[ ! -v EMACS_VTERM_PATH ]]; then
  echo >&2 "Error: expected EMACS_VTERM_PATH to be set."
  return
fi

# Source the canned Zsh script provided by the emacs-libvterm package.
source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

# Call the Emacs find-file command.
ff() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

# Call the Emacs find-file-other-window command.
ffo() {
    vterm_cmd find-file-other-window "$(realpath "${@:-.}")"
}

printf "Welcome to Vterm\n\n"
