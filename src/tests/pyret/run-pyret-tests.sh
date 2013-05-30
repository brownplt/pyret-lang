#/bin/bash

for file in check/*.arr; do
  racket ../../cmdline.rkt --check $file;
done

