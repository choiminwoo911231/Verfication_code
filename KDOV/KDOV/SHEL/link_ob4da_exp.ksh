#!/usr/bin/ksh

 ext=.nc
 sdate=$1
 edate=$2
 
 intlt='h'; inttm=$3;

 exp_src_dir=$4
 exp_des_dir=$5
 log_dir=$6
 list=$7

 EXP_NAME=$8

 undef_dir=${PWD}"/../.UNDEF"
 KMA_OPER="/ARCV/NWP/RAWD/MODL/GDPS/NE36/"

 ESUBDIR="/"
 if [[ "${exp_src_dir}" == *"share/cycle"* ]];then; ESUBDIR="/KPOP/LATE/";fi
 if [ ${exp_src_dir:0:30} = ${KMA_OPER}  ];then;  ESUBDIR="/LATE/KPOP/";fi

 echo "*!!! Checking the EXP OB4DA ${list} FILE"
 echo "*!!! START OF EXP_LINK (OB4DA_${list}) PROCESS"
 fname_exp=EXP_ob4da

 ## --------------- CTL_LINK --------------##
 udf_links() {
  ln -sf ${undef_dir}/ob4da_undef_EXP_${list}.nc ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
  printf "%s\n" ${list}","1 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt 
 }
 
 EXP_links() {
  if [ -e $exp_src_dir/${wcurrent}${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $exp_src_dir/${wcurrent}${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_EXP_${list}.nc ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  fi #4
 }

 EXP_links1() {
  if [ -e $exp_src_dir/${ymd}T${hh}00Z${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $exp_src_dir/${ymd}T${hh}00Z${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_EXP_${list}.nc ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  fi #4
 }

 EXP_links2() {
  if [ -e $exp_src_dir/${ym}/${dd}/${hh}/${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $exp_src_dir/${ym}/${dd}/${hh}/${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_EXP_${list}.nc ${exp_des_dir}/${fname_exp}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${EXP_NAME}_ob4da_undef_${list}.txt
  fi #4
 }

 # LINK EXIST FILE & AND & LINK from maked exist list for not exist file
   if grep -q "${list}" "./namelist.txt"; then #1
    current=$sdate
    while [[ $current -le $edate ]] ; do #2
     export current
     wcurrent=$(echo $current | cut -c1-10)
     ymd=`echo $wcurrent | cut -c1-8`
     ym=`echo $wcurrent | cut -c1-6`
     dd=`echo $wcurrent | cut -c7-8`
     hh=`echo $wcurrent | cut -c9-10`
 
	 if [ -d $exp_src_dir/${ymd}T${hh}00Z ]; then
	  EXP_links1
	 elif [ -d $exp_src_dir/${wcurrent}/Ctrl ];then 
	  ESUBDIR="/Ctrl/"
 	  EXP_links
	 elif [ -d $exp_src_dir/$wcurrent ];then
	  EXP_links
     elif [ -d $exp_src_dir/${ym}/${dd}/${hh} ];then 
      EXP_links2
     else
      udf_links
     fi 
     current=`./maketim $current -$intlt $inttm | cut -c1-10 `
    done #2
   fi #1
 
 ##------------------------------------------



