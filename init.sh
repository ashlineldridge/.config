#!/usr/bin/env bash

yq r modules.txt -j | jq -re '.modules[] | [.path, .url, .branch] | @tsv' \
  | while IFS=$'\t' read -r path url branch; do
      echo "Read $path, $url, |$branch|"
      if [[ -z "${branch:-}" ]]; then
        git submodule add "$url" "$path"
      else
        git submodule add -b "$branch" "$url" "$path"
      fi
    done
