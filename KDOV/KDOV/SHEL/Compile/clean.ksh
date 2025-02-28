#!/usr/bin/ksh

 echo "!!------------- Clean ------------- !!"

 export HOME=${PWD}
 export exe_path="$HOME/../../SRC/FORTRAN_3deg"
 export und_path="$HOME/../../.UNDEF"
 cd $exe_path

 rm -f $exe_path/*.o 
 rm -f $exe_path/*.mod
 rm -f $exe_path/*.exe
 echo "!!--------------------- COMPLETE ------------------- !!"
