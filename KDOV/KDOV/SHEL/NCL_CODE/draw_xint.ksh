#!/usr/bin/ksh

# TYPE = filename

SD=$1
ED=$2

## Set -1 = Automaticaly set xint, If set XINT_OP=## then set xint=## hour (##=number, Ex=XINT_OP=48)
XINT_OP=12

SDs=`date --date="${SD:0:8} ${SD:8:2}" +%s`
EDs=`date --date="${ED:0:8} ${ED:8:2}" +%s`
cylcN=`echo "((((${EDs} - ${SDs})/3600)/6)+1)" | bc`
if [ ${XINT_OP} = -1 ];then
  xint=6;cylc=8;loop=0
  while [ $cylcN -gt $cylc ];do
    let loop=loop+1;  let cylc=cylc*2;  let xint=xint*2
  done
else
  xint=${XINT_OP}
fi

return

