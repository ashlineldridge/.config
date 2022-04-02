# AWS auths using the credentials in the specified file
function awsauth_with() {
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
