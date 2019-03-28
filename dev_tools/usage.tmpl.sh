usage()  {
       echo
       echo " USAGE : $(basename $0 ) ............"
       echo 
       echo "  PURPOSE:"
       echo "    .............."
       echo 
       echo "  ARGUMENTS:"
       echo "    .............."
       echo 
       echo "  OPTIONS:"
       echo "    .............."
       echo 
       echo "  OUTPUT: "
       echo "    .............."
       echo 
       exit 0
         }

if [ $# = 0 ] ; then
   usage
fi

while getopts :h  opt
  do
    case $opt in
       (h)  usage ;;
       (*) ;;
    esac
  done

shift $(($OPTIND-1))

dirs="*"


