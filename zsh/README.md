# Zsh Configuration

## JetBrains Terminal Configuration

Because JetBrains applications (GoLand, CLion, etc) have their own weird
terminal init process, the integrated terminal doesn't respect XDG based
Zsh configuration out of the box. Add the following lines to
`/Applications/GoLand.app/Contents/plugins/terminal/.zshrc` (or equivalent)
to get the terminal to load this configuration.

```bash
if [[ -f "${ZDOTDIR}/zshenv" ]]; then
  source "${ZDOTDIR}/zshenv"
fi

if [[ -f "${ZDOTDIR}/config" ]]; then
  source "${ZDOTDIR}/config"
fi
```
