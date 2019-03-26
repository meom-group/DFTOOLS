#!/bin/bash

# This script is part of the DFTOOLS.
# It renames ECMWF variable names to those used historically in DRAKKAR

typlist="d2 msl radlw radsw snow precip t2 tcc u10 v10 "

if [ $# = 0 ] ; then
   echo "USAGE : $(basename $0) go "
   echo
   echo "   PURPOSE:"
   echo "     Rename ECMWF variable for atmospheric fields to DRAKKAR used names."
   echo "     This script scans all the files downloaded from ECMWF  and changes "
   echo "     the variable names for :" $typlist
   echo "     This script must be used in the directory where the files are."
   echo
   echo "   ARGUMENT:"
   echo "     In fact any character does the job. Without arg, display this message."
   echo
   exit 0
fi

for typ in $typlist ; do
   for f in ${typ}*nc ; do
      case $typ in
      ( d2  )  var=D2M ;;
      ( msl )  var=MSL ;;
      ( precip )  var=TP ;;
      ( radlw )  var=STRD ;;
      ( radsw )  var=SSRD ;;
      ( snow )  var=SF ;;
      ( t2 )  var=T2M ;;
      ( tcc )  var=TCC ;;
      ( u10 )  var=U10M ;;
      ( v10 )  var=V10M ;;
      esac

      echo action : ncrename -v $var,$typ $f
      ncrename -v $var,$typ $f
    done
done
