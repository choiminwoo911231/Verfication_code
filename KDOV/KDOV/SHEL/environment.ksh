#!/usr/bin/ksh
  #MODULE SETTING
  module purge
  module load prg_env/intel
#  module load python/2.7.14-lenovo
#  module load wgrib
#  module load wgrib2
  module load netcdf-fortran/4.5.2
  module load pnetcdf/1.8.1
#  module load ncview/2.1.7
  module load ncl/6.6.2
#  module load nco/4.9.1
#  module load xxdiff/4.0
  module load imagemagick/6.9.10-83


  #MAKE DIRECTORY
  export HOME=$1
  export CHECK_DIR=${HOME}/DATA/MDL

  if [ -d $CHECK_DIR ];then
   echo "*FIRST ENVIRONMENT SETTING :: it's already made, so it passes"
  else
   mkdir -p ${HOME}/DATA/MDL
   mkdir -p ${HOME}/DATA/OUT
   mkdir -p ${HOME}/LOG
   mkdir -p ${HOME}/FIG
   echo "*FIRST ENVIRONMENT SETTING :: !!NOW COMPLETE!!"
  fi
  
