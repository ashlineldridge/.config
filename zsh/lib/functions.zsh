# AWS auths using the credentials in the specified file
function aws_auth_with() {
  if [ $# -ne 1 ]; then
    printf "You must specify the AWS credentials CSV file.\n"
    return
  fi;
  creds=$(tail -1 "$1")
  arr=("${(@s/,/)creds}")
  if [ $#arr -ne 3 ]; then
    printf "The AWS credentials CSV file you specified is invalid.\n"
    printf "The final line must consist of 'UserName,AccessKeyId,SecretAccessKey'.\n"
    return
  fi
  export AWS_ACCESS_KEY_ID="${arr[2]}"
  export AWS_SECRET_ACCESS_KEY="${arr[3]}"
  export AWS_SESSION_TOKEN=
  export AWS_SECURITY_TOKEN=
  export AWS_ROLE_IDENTIFIER=
  printf "Done.\n"
}

# Uses https://github.com/remind101/assume-role to assume the AWS IAM role
# that corresponds to the specified profile name within the context of the
# current shell. If no argument is specified, the function prints the profile
# name associated with the currently assumed role.
function aws_assume_role() {
  if [[ $# == 0 ]]; then
    echo "Current role: ${ASSUMED_ROLE}"
  else
    eval $(command assume-role $@)
    export AWS_DEFAULT_REGION=ap-southeast-2
    printf "Assumed role: ${ASSUMED_ROLE}"
  fi
}

# Clears any assumed role environment credentials.
function aws_clear_role() {
  local role="${ASSUMED_ROLE}"
  unset AWS_ACCESS_KEY_ID
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_SESSION_TOKEN
  unset AWS_SECURITY_TOKEN
  unset AWS_DEFAULT_REGION
  unset ASSUMED_ROLE
  printf "Cleared role: ${role}"
}

# Useful for decoding JSON that has been gzipped and base-64 encoded
function decode_b64_gz_json() {
  if [ $# -ne 1 ]; then
    printf "You must specify a string to decode.\n"
    return
  fi;
  echo "$1" | base64 -d | gunzip | jq .
}

function macdown() {
  "$(mdfind kMDItemCFBundleIdentifier=com.uranusjr.macdown | head -n1)/Contents/SharedSupport/bin/macdown" $@
}

# Installs the latest rust-analyzer into ~/bin which overrides the one installed by Homebrew.
# Sometimes useful if I perhaps temporarily want a different version than the one managed by Homebrew.
function update_rust_analyzer() {
  # Use URL below to switch to nightly.
  # https://github.com/rust-lang/rust-analyzer/releases/download/nightly/rust-analyzer-aarch64-apple-darwin.gz

  curl -Lsf \
    https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-aarch64-apple-darwin.gz \
    | gunzip -c - > ~/bin/rust-analyzer
  chmod +x ~/bin/rust-analyzer
  rust-analyzer --version
}

# Show all processes listening on open ports.
function open_ports() {
  sudo lsof -iTCP -sTCP:LISTEN -n -P
}
