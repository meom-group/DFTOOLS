#!/bin/bash
#
#  dft_ch_unit_precip.sh
# this script takes the precip, snow in m/s and transforms them to kg/m2/s

usage()  {
       echo
       echo " USAGE : $(basename $0 ) go "
       echo 
       echo "  PURPOSE:"
       echo "    Scan the local precip and snow file in m/s and transforms them in kg/m2/s."
       echo 
       echo "  ARGUMENTS:"
       echo "    any word works, no argument display this message."
       echo 
       echo "  OUTPUT: "
       echo "    '_chunit' is appended to the original name."
       echo 
       exit 0
         }

if [ $# = 0 ] ; then
   usage
fi

for typ in snow precip ; do
   for f in ${typ}*.nc_daily ; do
     o_unit=$( ncdump -h $f | grep ${typ}:units | awk -F\" '{print $2}' )
     if [ $o_unit = 'm/s' ] ; then
        n_unit='kg/m2/s'
        cmd="ncap2 -O -s '$typ=float(${typ}*1000.)' $f ${f%_daily}_chunit"
        eval $cmd
        ncatted -O -a units,$typ,m,c,$n_unit ${f%_daily}_chunit
     else
       echo Bad units in $f : $o_unit
       echo Need to be 'm/s'
       echo skip $f ...
     fi
   done
done
