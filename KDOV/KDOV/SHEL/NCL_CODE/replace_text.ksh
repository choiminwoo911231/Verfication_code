#!/usr/bin/ksh

for file in *.sh; do
  sed -i 's/DRAW_3deg/NCL_CODE/g' "$file"
done
wait



