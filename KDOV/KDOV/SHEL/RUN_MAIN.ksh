## .bash_profile
#!/usr/bin/ksh
cd $PWD

### Module SET-----------------------------------------------------------------
  export HOME=$PWD/../
  source ${HOME}/SHEL/environment.ksh $HOME
### ---------------------------------------------------------------------------

### USER SET-------------------------------------------------------------------
  export SDATE=2024010100   #[Analysis cycle start time]
  export EDATE=2024010218   #[Analysis cycle   end time]
  export INTV=12            #[06, 12, 18, 24 UTC]-[interval, unit :: hr]

  #ORIGINAL FILE LOCATION 
  export CTL_ORIDIR="/$HOME/example/CTL/" #CTL Dir location
  export EXP_ORIDIR="/$HOME/example/EXP/" #EXP Dir location

  #EXPERIMENT NAME
  export CTL_NAME=CTL	            	    #[Control     Directory name]
  export EXP_NAME=EXP     	     	#[Experiment  Directory name]

  #OBS_LIST
  gpsro=N
  aircraft=N
  sonde=N
  amv=N
  grndgnss=N
  scatwind=N
  surface=N
  amsr2=N
  amsua=N
  atms=Y
  cris=N
  csrgk2a=N
  csrhima=N
  csrmsg=N
  iasi=N
  mhs=N
  mwhs2=N

  #RUN SHEL condition
  FULL_RUN="on"			#Full-run (Retrieval-> processing-> verification -> visualization)
  DRAW_RUN="off"		#Draw-run (Already calculated so just -> visualization)
###----------------------------------------------------------------------------


### NON-SETTING PART ---------------------------------------------------------- plz don't touch
  #LINK DIRECTORY                              
  export CTL_DIR=$HOME/DATA/MDL/$EXP_NAME'_'$CTL_NAME'_'${SDATE}'_'${EDATE}'_'${INTV}'h'/${CTL_NAME}/        #LINK FILE(CTL_ORIDIR --> CTL_DIR)
  export EXP_DIR=$HOME/DATA/MDL/$EXP_NAME'_'$CTL_NAME'_'${SDATE}'_'${EDATE}'_'${INTV}'h'/${EXP_NAME}/        #LINK FILE(EXP_ORIDIR --> EXP_DIR)
  export FIG_DIR=$HOME/FIG/$EXP_NAME'_'$CTL_NAME'_'${SDATE}'_'${EDATE}'_'${INTV}'h'/						 #FINAL OUTPUT (Visualization PNG FILES)

  #Middel stat OUTPUT (NetCDF)
  export OUT_DIR=$HOME/DATA/OUT/$EXP_NAME'_'${SDATE}'_'${EDATE}'_'$CTL_NAME'_'${INTV}'h'/
  export OUT_DIR1=$OUT_DIR/${CTL_NAME}/
  export OUT_DIR2=$OUT_DIR/${EXP_NAME}/

  export SRC_DIR=$HOME/SRC/
  export LOG_DIR=$HOME/LOG/$EXP_NAME'_'$CTL_NAME'_'${SDATE}'_'${EDATE}'_'${INTV}'h'/
  export UDF_DIR=$HOME/.UNDEF/
### ---------------------------------------------------------------------------

## RUN FULL SHEL --------------------------------------------------
  if [ $FULL_RUN == "on" ];then
 
  if [ -d $CTL_DIR ];then
    rm -rf $CTL_DIR/*
  else 
    mkdir -p $CTL_DIR
  fi
  if [ -d $EXP_DIR ];then
    rm -rf $EXP_DIR/*
  else
    mkdir -p $EXP_DIR
  fi
  if [ -d $LOG_DIR ];then
    rm -rf $LOG_DIR/*
  else
    mkdir -p $LOG_DIR
  fi
  if [ -e "./namelist.txt" ];then
    rm -f ./namelist.txt
  fi



  if [ -d $OUT_DIR1 ];then
   echo "exist CTLDIR"
  else
    mkdir -p $OUT_DIR1
  fi
  if [ -d $OUT_DIR2 ];then
   echo "exist EXPDIR"
  else
    mkdir -p $OUT_DIR2
  fi

  if [ -d $FIG_DIR ];then
    echo ""
  else
    mkdir -p $FIG_DIR/MAP
    mkdir -p $FIG_DIR/MAP_COUNT
    mkdir -p $FIG_DIR/PF
    mkdir -p $FIG_DIR/TS
  fi
 
 #-----------------------------------------------------------------------------------------------------
 #PRESSURE level obs ("gpsro" "amv" "aircraft" "sonde")													 ||||
 #CHANNEL  level obs ("amsr2" "amsua" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "iasi" "mhs" "mwhs2")	 ||||
 #SURFACE			 ("scatwind" "surface" "grndgnss")												    \\  //
 #-----------------------------------------------------------------------------------------------------   \/
  export turn_obs=("$aircraft" "$amsr2" "$amsua" "$amv" "$atms" "$cris" "$csrgk2a" "$csrhima" "$csrmsg" "$gpsro" "$grndgnss"\
        "$iasi" "$mhs" "$mwhs2" "$scatwind" "$sonde" "$surface")

  list=("aircraft" "amsr2" "amsua" "amv" "atms" "cris" "csrgk2a" "csrhima" "csrmsg" "gpsro" "grndgnss"\
         "iasi" "mhs" "mwhs2" "scatwind" "sonde" "surface")

  ilist=0
  while [[ $ilist -le ${#list[@]}-1 ]] ;do
   if [ "${turn_obs[$ilist]}" = "Y" ]; then
    echo "${list[$ilist]}" >> ./namelist.txt
   fi
  let ilist=ilist+1
  done

  # MAKE UNDEF(missing) FILE
  ksh ./make_undef_ctl.ksh ${CTL_ORIDIR} ${SDATE} ${UDF_DIR} &
  wait
  ksh ./make_undef_exp.ksh ${EXP_ORIDIR} ${SDATE} ${UDF_DIR} &
  wait
  ksh ./make_undef_ctl_1.ksh ${CTL_ORIDIR} ${SDATE} ${EDATE} ${INTV} ${UDF_DIR}
  ksh ./make_undef_exp_1.ksh ${EXP_ORIDIR} ${SDATE} ${EDATE} ${INTV} ${UDF_DIR}
  wait
 
  #LINK Original KPOP outputs (ob4da files)
  for ilist in "${list[@]}"; do
    iobs=${!ilist}
    if [ "${iobs}" = "Y" ]; then
     ksh ./link_ob4da_ctl.ksh $SDATE $EDATE $INTV $CTL_ORIDIR $CTL_DIR $LOG_DIR $ilist $CTL_NAME &
     ksh ./link_ob4da_exp.ksh $SDATE $EDATE $INTV $EXP_ORIDIR $EXP_DIR $LOG_DIR $ilist $EXP_NAME &
    fi
  done
  wait&&
 
  #MAKE LIST linked files
  for ilist in "${list[@]}"; do
    iobs=${!ilist}
    if [ "${iobs}" = "Y" ]; then
     ksh ./makefile_list_ob4da.ksh ${CTL_DIR} ${EXP_DIR} ${LOG_DIR} ${SDATE} ${EDATE} ${INTV} $ilist $CTL_NAME $EXP_NAME &
    fi
  done
  wait&&
 
  #COMPILE FORTRAN CODE (statistics calculations & Make NetCDF files)
  if [ -e $LOG_DIR/success_ob4da_file ];then
  echo "!!--------------- FINISH MAKING FILE LIST ------------- !!"
   echo ""
   echo "!!--------*** COMPILE & RUN FORTRAN CODE ***---------- !!"
   wait
   echo "!!---------------------------------------------------- !!"
   echo ""

   MDL=$CTL_NAME                                                                
   for ilist in "${list[@]}"; do                                                
   iobs=${!ilist}                                                               
    if [ "${iobs}" = "Y" ]; then                                                
     exec ./Compile/compile_pf.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_pf_${ilist}.log&
    fi                                                                          
   done                                                                         
   wait&&                                                                       
                                                                                
                                                                                
   MDL=$EXP_NAME                                                                
   for ilist in "${list[@]}"; do                                                
   iobs=${!ilist}                                                               
    if [ "${iobs}" = "Y" ]; then                                                
     exec ./Compile/compile_pf.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_pf_${ilist}.log&
    fi                                                                          
   done                                                                         
   wait&&  

   MDL=$CTL_NAME
   for ilist in "${list[@]}"; do
   iobs=${!ilist}
	if [ "${iobs}" = "Y" ]; then
     exec ./Compile/compile_map.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_map_${ilist}.log&
    fi
   done
   wait&&


   MDL=$EXP_NAME
   for ilist in "${list[@]}"; do
   iobs=${!ilist}
	if [ "${iobs}" = "Y" ]; then
     exec ./Compile/compile_map.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_map_${ilist}.log&
    fi
   done
   wait&&

   MDL=$CTL_NAME                                                                
   for ilist in "${list[@]}"; do                                                
   iobs=${!ilist}                                                               
    if [ "${iobs}" = "Y" ]; then                                                
     exec ./Compile/compile_map_count.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_map_count_${ilist}.log&
    fi                                                                          
   done                                                                         
   wait&&                                                                       
                                                                                
                                                                                
   MDL=$EXP_NAME                                                                
   for ilist in "${list[@]}"; do                                                
   iobs=${!ilist}                                                               
    if [ "${iobs}" = "Y" ]; then                                                
     exec ./Compile/compile_map_count.ksh $HOME $SDATE $EDATE $INTV $LOG_DIR $OUT_DIR $ilist $SRC_DIR $MDL > ${LOG_DIR}ob4da_${MDL}_map_count_${ilist}.log&
    fi                                                                          
   done                                                                         
   wait&& 

   # IF FULL RUN --> Visualization ---------------------------------------------
   echo "!!------------------- DRAW ------------------------- !!"
   ksh ./run_draw_pf.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME > ${LOG_DIR}fig_pf.log 
   wait
   ksh ./run_draw_map.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME > ${LOG_DIR}fig_map.log 
   wait
   ksh ./run_draw_map_count.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME > ${LOG_DIR}fig_map_count.log
   wait
   ksh ./run_draw_ts.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME $INTV > ${LOG_DIR}fig_ts.log
   wait
   rm -f ${UDF_DIR}/*.nc
	echo " "
   # ---------------------------------------------------------------------------
  else
    exit 7
  fi
 fi
 echo "!!------------------- FINISH ------------------------- !!"
##------------------------------------------------------------------------------


### IF DRAW_RUN Visualization --------------------------------------------------
 if [ $DRAW_RUN == "on" ];then
   ksh ./run_draw_pf.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME> ${LOG_DIR}fig_pf_re.log 
   wait
   ksh ./run_draw_map.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME > ${LOG_DIR}fig_map_re.log 
   wait
   ksh ./run_draw_map_count.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME > ${LOG_DIR}fig_map_count_re.log 
   wait
   ksh ./run_draw_ts.ksh $HOME $SDATE $EDATE $FIG_DIR $OUT_DIR $OUT_DIR1 $OUT_DIR2 $CTL_NAME $EXP_NAME $INTV > ${LOG_DIR}fig_ts_re.log 
   wait
  exit 0 
 fi
 echo "!!------------------- FINISH ------------------------- !!"
### ----------------------------------------------------------------------------
