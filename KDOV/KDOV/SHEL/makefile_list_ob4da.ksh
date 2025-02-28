#!/usr/bin/ksh

  #DIRECTORY
  export ctl_dir=$1
  export exp_dir=$2
  export log_dir=$3
  export sdate=$4
  export edate=$5
  export intv=$6
  export list=$7
  export CTL_NAME=$8
  export EXP_NAME=$9

  ctl_nfile=$(ls ${ctl_dir}*ob4da_${list}* | wc -l)
  exp_nfile=$(ls ${exp_dir}*ob4da_${list}* | wc -l)


 #MAKE FILE LIST
  ndate=0
  cdate=$sdate
  while (( $cdate <= $edate)); do
    ymd=`echo $cdate | cut -c1-8`
    hh=`echo $cdate | cut -c9-10`
    cdate=`date --date="+$intv hours $ymd $hh" +%Y%m%d%H`
    let ndate=ndate+1
  done

  obs_num=$(wc -l ./namelist.txt | cut -c 1-3)
  data_num=$(($ndate))
  array=($ctl_dir $exp_dir)
  filen=(${CTL_NAME}_ob4da_${list} ${EXP_NAME}_ob4da_${list})
  lenfln=${#filen[@]}
  ext=(ob4da_${list} ob4da_${list})
    echo "!!--------------  START MAKE FILELIST :: ob4da_${list}  ------------- !!"

    index=0
    while [ $index -le $lenfln-1 ];do
    for fname in `ls ${array[index]}*${ext[index]}*`;do
      echo "'"$fname"'" >> $log_dir/${filen[index]}list.txt
    done
    let index=index+1
    done

    nuctl=$(grep "1" ${log_dir}/${CTL_NAME}_ob4da_undef_${list}.txt | wc -l)
    nuexp=$(grep "1" ${log_dir}/${EXP_NAME}_ob4da_undef_${list}.txt | wc -l)

    let ctl_nfile=ctl_nfile-${nuctl}
    let exp_nfile=exp_nfile-${nuexp}

  if [ data_num -eq ctl_nfile ] && [ data_num -eq exp_nfile ];then
     printf "success to run make_filelist_ob4da.ksh \n"                  >> $log_dir/success_ob4da_file
  else
     mkdir -p $log_dir/FAIL
     printf "*!!!!! ------------------- PLEASE CEHCK THE ORIGINAL FILE or LINKED FILE !!!!!*"
     printf "*!!!!! ------------------- WARNING :: The number of files doesn't match !!!!!*"
     printf "*!!!!! OB4DA DATA FILE NUM  :: %d (ntime:%d X obs:%s)\n" $data_num ${ndate} ${list}>> $log_dir/FAIL/fail_ob4da_${list}_file
     printf "*!!!!! CTL_OB4DA_FILE NUM   :: %d\n" $ctl_nfile					>> $log_dir/FAIL/fail_ob4da_${list}_file
     printf "*!!!!! EXP_OB4DA_FILE NUM   :: %d\n" $exp_nfile					>> $log_dir/FAIL/fail_ob4da_${list}_file
     printf "fail to run make_filelist_ob4da.ksh \n"							>> $log_dir/FAIL/fail_ob4da_${list}_file
  fi

  wait
