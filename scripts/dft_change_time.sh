#!/bin/bash


for f in *.nc ; do
   # look for year
   year=$( echo ${f%.nc}  | awk -F_ '{ print $NF}' | sed -e 's/y//')

   # look for frequency 
   ntime=$( ncdump -h $f | grep UNLIMITED | awk -F\( '{print $NF}' | awk '{print $1}' )
#   echo $f $year $ntime

  if [ $ntime -gt 366 ] ; then
    cf_time=../time_counter_${year}_03h.nc
  else
    cf_time=../time_counter_${year}_24h.nc
  fi

  ncks -A -v time $cf_time $f 
done


exit






q2_ERA_BSAS_interim_y1992.nc
	time = UNLIMITED ; // (2928 currently)

