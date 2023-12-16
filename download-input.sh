#!/bin/bash

year="$1"
day="$2"
cookie=$(cat session.cookie)

if [[ -z "$year" ]]; then
  echo "Usage: ./download-input <year> [day]"
  exit 1
fi

if [ -z "$cookie" ]; then
  echo "Missing 'session.cookie' file"
  exit 1
fi

download_day() {
  local day="$1"
  local out_file=$(printf "%02d" $day)
  local out_dir="$year/inputs"
  local out_path="$out_dir/day$out_file"

  if [ ! -d "$out_dir" ]; then
    mkdir -p "$out_dir"
  fi

  if [ -f "$out_path" ]; then
    echo "'$out_path' already exists"
  else
    echo "Downloading to $out_path"
    wget \
      --header="Cookie: session=$cookie" "https://adventofcode.com/$year/day/$day/input" \
      --output-document="$out_path" \
      --quiet
  fi
}

if [ -n "$day" ]; then
  download_day "$day"
else
  for i in $(seq 1 25); do
    download_day "$i"
  done
fi
