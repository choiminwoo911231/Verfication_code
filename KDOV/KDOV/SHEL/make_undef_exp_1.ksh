#!/usr/bin/ksh
#BSUB -J OBS_UNDEF_CTL
#BSUB -q normal_p@maru                                                          
#BSUB -o UNDEF_CTL.out                                                         
#BSUB -e UNDEF_CTL.err
#BSUB -M 50GB                                                                  
source /usr/share/Modules/init/ksh

 cd $PWD
 export exe_path=$5
 list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2")

 # INPUT_DIR preprocess
 export mdl_dir=$1
 export stdate=$2
 export etdate=$3
 export intv=$4

 ESUBDIR="/"        
 if [[ "${mdl_dir}" == *"share/cycle"* ]];then; ESUBDIR="/KPOP/LATE/";fi

 for ilist in "${list[@]}"; do
  echo ${ilist}
  export obslist=${ilist}
  FLAG=0

  if [ -e ${exe_path}/ob4da_undef_EXP_${obslist}.nc ]; then
   echo " "
  else
   cdate=$stdate
   while (( $cdate <= $etdate)); do
    ymd=`echo $cdate | cut -c1-8`
    ym=`echo $cdate | cut -c1-6`
    dd=`echo $cdate | cut -c7-8`
    hh=`echo $cdate | cut -c9-10`

    if [ -d $mdl_dir/${ymd}T${hh}00Z ]; then
     export exp_dir=${mdl_dir}/${ymd}T${hh}00Z/${ESUBDIR}
    elif [ -d ${mdl_dir}/${cdate}/Ctrl ];then
     export exp_dir=${mdl_dir}/${cdate}/Ctrl
    elif [ -d ${mdl_dir}/${cdate} ];then
     export exp_dir=${mdl_dir}/${cdate}
    elif [ -d ${mdl_dir}/${ym}/${dd}/${hh} ];then
     export exp_dir=${mdl_dir}/${ym}/${dd}/${hh}
    fi
  
    if [ -e ${exp_dir}/ob4da_${obslist}_${cdate}.nc ]; then #2
     export MDL=EXP
     export sdate=${cdate}
     ifort -mcmodel=large -o $exe_path/make_undef_exp_${obslist}.exe $exe_path/make_undef_obs.f90 -I/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/include -L/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/lib -lnetcdf -lnetcdff
     ${exe_path}/make_undef_exp_${obslist}.exe
     wait
     FLAG=1
	 echo $FLAG
     if [ $FLAG -eq 1 ];then
      continue 2
     fi
    else
     echo " "
     cdate=`date --date="+$intv hours $ymd $hh" +%Y%m%d%H`
    fi #2
   done

#   if [ $FLAG -eq 1 ];then
#    continue
#   fi

  fi
 done

 wait
 rm -rf $exe_path/*exp*.exe  
