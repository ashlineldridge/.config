### Nushell Vterm configuration.

# See: https://github.com/akermu/emacs-libvterm/issues/679.

module vterm {
  # Escape a command for outputting by vterm send.
  def escape-for-send [
    to_escape: string # The data to escape
  ] {
    $to_escape | str replace --all '\\' '\\' | str replace --all '"' '\"'
  }

  # Send an Emacs command to vterm.
  export def send [
    command: string # Command to pass to vterm
    ...args: string # Arguments to pass to vterm
  ] {
    print --no-newline "\e]51;E"
    print --no-newline $"\"(escape-for-send $command)\" "
    for arg in $args {
      print --no-newline $"\"(escape-for-send $arg)\" "
    }
    print --no-newline "\e\\"
  }

  # Clear the terminal window including scrollback.
  export def clear [] {
    send vterm-clear-scrollback
    tput clear
  }

  # Call the Emacs find-file command.
  export def ff [
    filepath: path # File to open
  ] {
    send "find-file" $filepath
  }

  # Call the Emacs find-file-other-window command.
  export def ffo [
    filepath: path # File to open
  ] {
    send "find-file" $filepath
  }
}

module vprompt {
  # Complete escape sequence based on environment.
  def complete-escape-by-env [
    arg: string # argument to send
  ] {
    let tmux: string = (if ($env.TMUX? | is-empty) { '' } else { $env.TMUX })
    let term: string = (if ($env.TERM? | is-empty) { '' } else { $env.TERM })
    if $tmux =~ "screen|tmux" {
      # tell tmux to pass the escape sequences through
      $"\ePtmux;\e\e]($arg)\a\e\\"
    } else if $term =~ "screen.*" {
      # GNU screen (screen, screen-256color, screen-256color-bce)
      $"\eP\e]($arg)\a\e\\"
    } else {
      $"\e]($arg)\e\\"
    }
  }

  # Output text prompt that vterm can use to track current directory.
  export def left-prompt-track-cwd [] {
    $"(create_left_prompt)(complete-escape-by-env $'51;A(whoami)@(hostname):(pwd)')"
  }
}

use vterm
use vprompt

$env.PROMPT_COMMAND = {|| vprompt left-prompt-track-cwd }