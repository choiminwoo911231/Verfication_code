#!/usr/bin/ksh

 ext=.nc
 sdate=$1
 edate=$2
 
 intlt='h'; inttm=$3;

 ctl_src_dir=$4
 ctl_des_dir=$5
 log_dir=$6
 list=$7

 CTL_NAME=$8

 undef_dir=${PWD}"/../.UNDEF"
 KMA_OPER="/ARCV/NWP/RAWD/MODL/GDPS/NE36/"

 ESUBDIR="/"
 if [[ "${ctl_src_dir}" == *"share/cycle"* ]];then; ESUBDIR="/KPOP/LATE/";fi
 if [ ${ctl_src_dir:0:30} = ${KMA_OPER}  ];then;  ESUBDIR="/LATE/KPOP/";fi

 echo "*!!! Checking the CTL OB4DA ${list} FILE"
 echo "*!!! START OF CTL_LINK (OB4DA_${list}) PROCESS"
 fname_ctl=CTL_ob4da

 ## --------------- CTL_LINK --------------##
 udf_links() {
  ln -sf ${undef_dir}/ob4da_undef_CTL_${list}.nc ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
  printf "%s\n" ${list}","1 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt 
 }

 CTL_links() {
  if [ -e $ctl_src_dir/${wcurrent}${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $ctl_src_dir/${wcurrent}${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_CTL_${list}.nc ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
  fi #4
 }

 CTL_links1() {
  if [ -e $ctl_src_dir/${ymd}T${hh}00Z${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $ctl_src_dir/${ymd}T${hh}00Z${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_CTL_${list}.nc ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
  fi #4
 }

 CTL_links2() {
  if [ -e $ctl_src_dir/${ym}/${dd}/${hh}/${ESUBDIR}ob4da_${list}_${wcurrent}$ext ];then #4
   ln -sf $ctl_src_dir/${ym}/${dd}/${hh}/${ESUBDIR}ob4da_${list}_${wcurrent}$ext ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","0 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
  else
   ln -sf ${undef_dir}/ob4da_undef_CTL_${list}.nc ${ctl_des_dir}/${fname_ctl}_${list}${wcurrent}${ext}
   printf "%s\n" ${list}","1 >> $log_dir/${CTL_NAME}_ob4da_undef_${list}.txt
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

	 if [ -d $ctl_src_dir/${ymd}T${hh}00Z ]; then
	  CTL_links1
	 elif [ -d $ctl_src_dir/${wcurrent}/Ctrl ];then 
	  ESUBDIR="/Ctrl/"
 	  CTL_links
	 elif [ -d $ctl_src_dir/$wcurrent ];then
	  CTL_links
   	 elif [ -d $ctl_src_dir/${ym}/${dd}/${hh} ];then
	  CTL_links2
     else
      udf_links
	 fi

     current=`./maketim $current -$intlt $inttm | cut -c1-10 `
    done #2
   fi #1
 
 ##------------------------------------------



