#/bin/bash

for file in check/*.arr; do
  echo "Testing $file"
  raco pyret --check $file;
done

