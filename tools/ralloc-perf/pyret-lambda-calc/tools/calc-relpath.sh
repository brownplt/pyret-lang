#!/usr/bin/env bash
# POSIX-compliant version of realpath --relative-to shamelessly
# copied from http://stackoverflow.com/a/12498485
# and http://stackoverflow.com/a/3915420

if [ "$#" -ne 2 ]; then
    >&2 echo "Usage: $0 source target"
    exit 1
fi

source="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
target="$(cd "$(dirname "$2")"; pwd)/$(basename "$2")"

common_part=$source # for now
result="" # for now

while [[ "${target#$common_part}" == "${target}" ]]; do
    # no match, means that candidate common part is not correct
    # go up one level (reduce common part)
    common_part="$(dirname $common_part)"
    # and record that we went back, with correct / handling
    if [[ -z $result ]]; then
        result=".."
    else
        result="../$result"
    fi
done

if [[ $common_part == "/" ]]; then
    # special case for root (no common path)
    result="$result/"
fi

# since we now have identified the common part,
# compute the non-common part
forward_part="${target#$common_part}"

# and now stick all parts together
if [[ -n $result ]] && [[ -n $forward_part ]]; then
    result="$result$forward_part"
elif [[ -n $forward_part ]]; then
    # extra slash removal
    result="${forward_part:1}"
fi

echo $result
