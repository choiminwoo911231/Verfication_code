#!/usr/bin/ksh
#BSUB -J OBS_UNDEF_CTL
#BSUB -q normal_p@maru                                                          
#BSUB -o UNDEF_CTL.out                                                         
#BSUB -e UNDEF_CTL.err
#BSUB -M 50GB                                                                  
source /usr/share/Modules/init/ksh

 cd $PWD
 export exe_path=$3
 list=("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2")
 list1=("aircraft" "amv" "gpsro" "grndgnss" "scatwind" "sonde" "surface")

 # INPUT_DIR preprocess
 export mdl_dir=$1
 export sdate=$2
 export MDL=CTL

 ESUBDIR="/"        
 if [[ "${mdl_dir}" == *"share/cycle"* ]];then; ESUBDIR="/KPOP/LATE/";fi
 ymd=`echo $sdate | cut -c1-8`                                           
 ym=`echo $sdate | cut -c1-6`                                            
 dd=`echo $sdate | cut -c7-8`                                            
 hh=`echo $sdate | cut -c9-10`

 if [ -d $mdl_dir/${ymd}T${hh}00Z ]; then
  export exp_dir=${mdl_dir}/${ymd}T${hh}00Z/${ESUBDIR}
 elif [ -d ${mdl_dir}/${sdate}/Ctrl ];then
  export exp_dir=${mdl_dir}/${sdate}/Ctrl
 elif [ -d ${mdl_dir}/${sdate} ];then
  export exp_dir=${mdl_dir}/${sdate}
 elif [ -d ${mdl_dir}/${ym}/${dd}/${hh} ];then
  export exp_dir=${mdl_dir}/${ym}/${dd}/${hh}
 fi


 echo ' '
 echo "!! Start Make obs undef file --------------------------- !!"

 for ilist in "${list[@]}"; do
 export obslist=${ilist}
  ifort -mcmodel=large -o $exe_path/make_undef_ctl_${obslist}.exe $exe_path/make_undef_obs.f90 -I/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/include -L/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/lib -lnetcdf -lnetcdff
  ${exe_path}/make_undef_ctl_${obslist}.exe
 done

 wait

 for ilist1 in "${list1[@]}"; do                                                  
 export obslist=${ilist1}                                                        
  ifort -mcmodel=large -o $exe_path/make_undef_ctl_${obslist}.exe $exe_path/make_undef_obs_nosat.f90 -I/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/include -L/opt/kma/kma_lib/apps/netcdf-fortran/4.5.2/INTEL/200/lib -lnetcdf -lnetcdff
  ${exe_path}/make_undef_ctl_${obslist}.exe                                     
 done

 rm -rf $exe_path/*ctl*.exe  


 echo "!!--------------------- MAKE UNDEF COMPLETE --------------------!!"      
