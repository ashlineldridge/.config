if status is-interactive
    # Commands to run in interactive sessions can go here...

    # This sets up full completion for AWS CLI which eshell can also leverage.
    # See https://github.com/aws/aws-cli/issues/1079#issuecomment-242923826
    complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
end
