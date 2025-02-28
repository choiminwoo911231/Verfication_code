#!/usr/bin/ksh
#source /usr/share/Modules/init/ksh

 echo "" 
 echo "!!------------- START Calculate STAT ------------- !!"

 export HOME=$1

 #DATE
 export SDATE=$2
 export EDATE=$3
 export INTV=$4

 #EXPERIMENT NAME
 export LOG_DIR=$5
 export OUT_DIR=$6
 export OBSNAME=$7
 export src_path=$8
 export MDL=$9

 NT=0 
 CDATE=$SDATE
 while (( $CDATE <= $EDATE)); do
   ymd=`echo $CDATE | cut -c1-8`
   hh=`echo $CDATE  | cut -c9-10`
   CDATE=`date --date="+$INTV hours $ymd $hh" +%Y%m%d%H`
   let NT=NT+1
 done
 export NT

 make -C ${HOME}/SRC/FORTRAN_3deg/ all_map
 wait
 echo "*** finish ./run_exe  ***"


 echo "!!--------------------- COMPLETE ------------------- !!"
