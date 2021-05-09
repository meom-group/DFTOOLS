#!/bin/bash

for f in *.nc ; do
  g=${f%.nc}_1m.nc
  cdo -O monmean $f $g
done
