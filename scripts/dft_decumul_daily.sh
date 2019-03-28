#!/bin/bash

# dft_decumul_daily.sh script
# This is a wrapper to the dft_decumul_daily tool 

usage()  {
       echo
       echo " USAGE : $(basename $0 ) go "
       echo 
       echo "  PURPOSE:"
       echo "    This script apply dft_decumul_daily procedure on all local cumulated"
       echo "    variables files."
       echo 
       echo "  ARGUMENTS:"
       echo "    go or any word ... no argument show this message "
       echo 
       echo "  OUTPUT:"
       echo "    Processed file have the string '_daily' appended to their name."
       echo "    Variable name is the same but units are changed accordingly."
       echo
       exit 0
         }

if [ $# = 0 ] ; then
   usage
fi

BINDIR=$(DEVGIT)/DFTOOLS/bin

for typ in radlw radsw precip snow ; do

   case $typ in 
   ( radlw ) var=radlw  ; cunit='W/m2' ;;
   ( radsw ) var=radsw  ; cunit='W/m2' ;;
   ( precip) var=precip ; cunit='m/s'  ;;
   ( snow  ) var=snow   ; cunit='m/s'  ;;
   esac
   
  for f in ${typ}*.nc ; do
     echo $f being processed :
     ${BINDIR}/dft_decumul_daily -f $f -v $var -u $cunit
  done
done
