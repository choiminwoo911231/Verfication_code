#!/usr/bin/ksh

  export HOME=$1

  export SDATE=$2
  export EDATE=$3   
  export FIG_DIR=$4
  export OUT_DIR=$5
  export OUT_DIR1=$6
  export OUT_DIR2=$7
  export CTL_NAME=$8
  export EXP_NAME=$9
  export DRAW_DIR=$HOME/SHEL/NCL_CODE


##==========================================================
#PRESSURE
#  list=("gpsro" "amv" "aircraft" "sonde")
#CHANNEL
#  list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2")
#SURFACE
#  list=("scatwind" "surface" "grndgnss")
##==========================================================

list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2"\
		"gpsro" "amv" "aircraft" "sonde" "surface" "grndgnss" "scatwind")


file_list1=($(find ${OUT_DIR1}/ -type f -name "OB4DA_MAP_${SDATE}_${EDATE}*" | sort -n))
file_list2=($(find ${OUT_DIR2}/ -type f -name "OB4DA_MAP_${SDATE}_${EDATE}*" | sort -n))
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
#Only Channel
if [ "${ilist}" == "amsua" ];then
  sel_chn=" 5, 6, 7, 8, 9, 10, 11, 12, 13, 14" 
  chn_prs=" 700, 400, 250, 200, 100, 50, 25, 10, 5, 2.5"

elif [ "${ilist}" == "amsr2" ];then
  sel_chn=" 7, 8, 9, 10, 11"
  chn_prs=" sfc, sfc, sfc, sfc, sfc"

elif [ "${ilist}" == "atms" ];then
  sel_chn=" 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22"
  chn_prs=" 700, 400, 250, 200, 100, 50, 25, 10, 5, 2.5, 700, 600, 500, 400, 300"

elif [ "${ilist}" == "iasi" ];then
  sel_chn=" 161, 199, 236, 226, 239, 260, 265, 347, 350, 381, 432, 407, 3002, 2993, 3049, 2889, 5381, 5399, 5480"
  chn_prs=" 100up, 100, 200, 250, 300, 400, 500, 600, 700, 800, 850, 900, 300, 400, 500, 600, 700, 800, 900"

elif [ "${ilist}" == "mhs" ];then
  sel_chn=" 5, 4, 3"
  chn_prs=" 700, 500, 300"

elif [ "${ilist}" == "mwhs2" ];then
  sel_chn=" 15, 13, 12, 11"
  chn_prs=" 700, 500, 400, 300"

elif [ "${ilist}" == "cris" ];then
  sel_chn=" 143, 95, 80, 61"
  chn_prs=" 700, 300, 100, 100up"

elif [ "${ilist}" == "csrgk2a" ];then
  sel_chn=" 13, 14, 15, 10, 9, 8"
  chn_prs=" 1000, 1000, 950, 600, 400, 300"

elif [ "${ilist}" == "csrhima" ];then
  sel_chn=" 13, 14, 15, 16, 10, 9, 8"
  chn_prs=" 1000, 1000, 950, 900, 600, 400, 300"

elif [ "${ilist}" == "csrmsg" ];then
  sel_chn=" 9, 10, 11, 6, 5"
  chn_prs=" 1000, 950, 900, 600, 400"

fi
#------------------------------------------------------------------------------------


   if [[ ${ilist} == *"grndgnss"* ]] || [[ ${ilist} == *"scatwind"* ]] || [[ ${ilist} == *"surface"* ]]; then
    ksh $DRAW_DIR/draw_MAP_sfc_omb.sh        $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
    ksh $DRAW_DIR/draw_MAP_sfc_cmb.sh        $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
    ksh $DRAW_DIR/draw_MAP_sfc_mean_diff.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist
    ksh $DRAW_DIR/draw_MAP_sfc_std_diff.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
  echo "-"
  elif [[ ${ilist} == *"amv"* ]] || [[ ${ilist} == *"gpsro"* ]] || [[ ${ilist} == *"sonde"* ]] || [[ ${ilist} == *"aircraft"* ]]; then
    ksh $DRAW_DIR/draw_MAP_prs_omb.sh       $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
    ksh $DRAW_DIR/draw_MAP_prs_mean_diff.sh  $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
    ksh $DRAW_DIR/draw_MAP_prs_std_diff.sh  $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist 
  echo "-"
  else
    ksh $DRAW_DIR/draw_MAP_chn_omb.sh        $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist "${sel_chn}" "${chn_prs}" 
    ksh $DRAW_DIR/draw_MAP_chn_cmb.sh        $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist "${sel_chn}" "${chn_prs}" 
    ksh $DRAW_DIR/draw_MAP_chn_mean_diff.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist "${sel_chn}" "${chn_prs}" 
    ksh $DRAW_DIR/draw_MAP_chn_std_diff.sh   $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $CTL_NAME $EXP_NAME $ilist "${sel_chn}" "${chn_prs}" 
   echo "-"
   fi 

  fi
 done
 let nfile=nfile+1
done
