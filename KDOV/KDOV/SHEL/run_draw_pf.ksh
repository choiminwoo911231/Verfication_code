#!/usr/bin/ksh

  export HOME=$1
  export SDATE=$2
  export EDATE=$3   
  export FIG_DIR=$4
  export OUT_DIR=$5
  export OUT_DIR1=$6
  export OUT_DIR2=$7
  export DRAW_DIR=$HOME/SHEL/NCL_CODE
  export CTLNAME=$8
  export EXPNAME=$9


##==========================================================
#PRESSURE
#  list=("amv" "aircraft" "gpsro" "sonde")
#CHANNEL
#  list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2")
#SURFACE
#  list=("scatwind" "surface" "grndgnss")
##==========================================================

list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2"\
		"gpsro" "amv" "aircraft" "sonde" "surface" "grndgnss" "scatwind")


file_list1=($(find ${OUT_DIR1}/ -type f -name "OB4DA_PF_${SDATE}_${EDATE}*" | sort -n))
file_list2=($(find ${OUT_DIR2}/ -type f -name "OB4DA_PF_${SDATE}_${EDATE}*" | sort -n))
file_num=${#file_list1[@]}

nfile=0
while [ $nfile -le $file_num-1 ];do
 SET_OBS1=${file_list1[$nfile]}
 SET_OBS2=${file_list2[$nfile]}
 for ilist in "${list[@]}";do
  if [[ $SET_OBS1 == *$ilist* ]];then
   SET_OBS11=$SET_OBS1
   SET_OBS22=$SET_OBS2

#------------------------------------------------------------------------------------
#Only Channel level related
if [ "${ilist}" == "amsua" ];then
  sel_chn="5, 6, 7, 8, 9, 10, 11, 12, 13, 14"

elif [ "${ilist}" == "amsr2" ];then
  sel_chn="7, 8, 9, 10, 11"

elif [ "${ilist}" == "atms" ];then
  sel_chn="6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22"

elif [ "${ilist}" == "cris" ];then
  sel_chn="27, 37, 49, 51, 53, 59, 61, 63, 64, 65, 67, 69, 71, 73, 75, 79, 80, 81, 83, 85, 87, 88, 89, 93, 95, 96, 99, 143"

elif [ "${ilist}" == "iasi" ];then
  org_sel_chn="38, 51, 57, 63, 109, 116, 122, 128, 135, 141, 148, 154, 161, 167, \
    173, 179, 180, 185, 187, 193, 199, 205, 207, 210, 212, 214, 217, 219, \
	222, 224, 226, 230, 232, 236, 239, 242, 243, 246, 249, 252, 254, 260, \
	262, 265, 267, 269, 275, 280, 282, 294, 296, 299, 306, 323, 327, 329, \
	335, 345, 347, 350, 354, 356, 360, 366, 371, 373, 375, 377, 379, 381, \
    383, 386, 389, 398, 401, 407, 414, 426, 432, 439, 445, 2889, 2958, 2993, \
    3002, 3049, 3105, 3110, 5381, 5399, 5480" 
  sel_chn=$(echo $org_sel_chn | tr -d ' ' | awk -F',' '{for(i=NF; i>1; i--) printf $i","; print $1}')

elif [ "${ilist}" == "mhs" ];then
  org_sel_chn="3, 4, 5"
  sel_chn=$(echo $org_sel_chn | tr -d ' ' | awk -F',' '{for(i=NF; i>1; i--) printf $i","; print $1}')

elif [ "${ilist}" == "mwhs2" ];then
  sel_chn="11, 12, 13, 15"
fi
#------------------------------------------------------------------------------------


   if [[ ${ilist} == *"grndgnss"* ]] || [[ ${ilist} == *"scatwind"* ]] || [[ ${ilist} == *"surface"* ]]; then
    ksh $DRAW_DIR/draw_PF_sfc.sh  $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $SET_OBS11 $SET_OBS22 $ilist ${CTLNAME} ${EXPNAME}
   echo ""
   elif [[ ${ilist} == *"amv"* ]] || [[ ${ilist} == *"gpsro"* ]] || [[ ${ilist} == *"sonde"* ]] || [[ ${ilist} == *"aircraft"* ]]; then
    ksh $DRAW_DIR/draw_PF_prs.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $SET_OBS11 $SET_OBS22 $ilist ${CTLNAME} ${EXPNAME}
   echo ""
   else
    ksh $DRAW_DIR/draw_PF_chn.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $SET_OBS11 $SET_OBS22 $ilist "${sel_chn}" ${CTLNAME} ${EXPNAME}
    echo ""
   fi 

  fi
 done
 let nfile=nfile+1
done
