### ZSH Eat configuration.

# For more details, see: https://elpa.nongnu.org/nongnu-devel/doc/eat.html#Shell-Integration

if [[ ! -v EAT_SHELL_INTEGRATION_DIR ]]; then
  echo >&2 "Error: expected EAT_SHELL_INTEGRATION_DIR to be set."
  return
fi

source "${EAT_SHELL_INTEGRATION_DIR}/zsh"

printf "Welcome to Eat\n\n"
