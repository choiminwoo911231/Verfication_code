#!/usr/bin/bash

HOME=$1                                                                         
SDATE=$2                                                                        
EDATE=$3                                                                        
INTV=${12}
                                                                                
FIG_DIR=$4                                                                      
OUT_DIR=$5                                                                      
                                                                                
SET_OBS1=$6                                                                     
SET_OBS2=$7                                                                     
list=$8                                                                         
list1=$(echo "$list" | tr '[:lower:]' '[:upper:]')                              
sel_chn=$9                                                                      
CTLNAME=${10}                                                                   
EXPNAME=${11}                                                                   
echo "${list1}"                                                                 
echo "@@@@@-- "$CTLNAME                                                         
echo "@@@@@-- "$EXPNAME     

source ${HOME}/SHEL/NCL_CODE/draw_xint.ksh ${SDATE} ${EDATE}                    

##----------------------------------------------------------                    
idx=( "OmB" "CmB")                                                              
idx_name=( "OmB" "CmB" )                                                        
varidx=( "std" "mean" )                                                        
lenidx=${#idx[@]}                                                               
lenvar=${#varidx[@]}
                                                                                
nidx=0    
while [ $nidx -lt ${lenidx} ];do                  

nvar=0
while [ $nvar -lt ${lenvar} ];do                  
  echo "====================================="                                  
  echo "${list1} ${idx_name[$nidx]} ${varidx[$nvar]}"
##==========================================================                    
##==========================================================         
mkdir -p ${HOME}/NCL                                                            
cat > ${HOME}/NCL/Draw_plot_ts_chn.ncl << EOF 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"
load "${HOME}/SHEL/NCL_CODE/FUNC/sub_axis_ts_chn.ncl" 

  err = NhlGetErrorObjectId()
  setvalues err
    "errLevel" : "Fatal"
  end setvalues

;************************************************
;begin
;************************************************
;;; make time ;;;
 sd_dum=stringtochar(tostring(${SDATE}))
 ed_dum=stringtochar(tostring(${EDATE}))

 sd_y4=toint(chartostring(sd_dum(0:3)))
 sd_m2=toint(chartostring(sd_dum(4:5)))
 sd_d2=toint(chartostring(sd_dum(6:7)))
 sd_h2=toint(chartostring(sd_dum(8:9)))
 
 ed_y4=toint(chartostring(ed_dum(0:3)))
 ed_m2=toint(chartostring(ed_dum(4:5)))
 ed_d2=toint(chartostring(ed_dum(6:7)))
 ed_h2=toint(chartostring(ed_dum(8:9)))

 unit="hours since 1900-01-01 0:0:0"
 sd2=cd_inv_calendar(sd_y4,sd_m2,sd_d2,sd_h2,0,0,unit,0)
 ed2=cd_inv_calendar(ed_y4,ed_m2,ed_d2,ed_h2,0,0,unit,0)

 time = ispan(toint(sd2),toint(ed2),${INTV})
 time@units = unit
 xtime1=(cd_calendar(time,-3))
 xtime=(cd_calendar(time(::(${xint}/${INTV})),-3))

 ;***** D.H.S by edit *****
 case=xtime
 nct=dimsizes(case)
 ct = stringtochar(case)

 y4 = chartostring(ct(:,0:3))
 m2 = chartostring(ct(:,4:5))
 d2 = chartostring(ct(:,6:7))
 h2 = chartostring(ct(:,8:9))
 mn = toint(h2)
 mn = 0

 case_inv = cd_inv_calendar(toint(y4),toint(m2),toint(d2),toint(h2),mn,mn,unit,0)
 month = (/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)

 times = cd_calendar(case_inv,0)
 yy = toint(times(:,0))
 mm = toint(times(:,1))
 dd = toint(times(:,2))
 hh = toint(times(:,3))

; str = sprinti("%0.2iZ",hh) +" "+ sprinti("%0.2i",dd) +" "+ month(mm) +" " +sprinti("%0.4i",yy)
; str  = sprinti("%0.2i",dd)+'.'+sprinti("%0.2iZ",hh)+'z'
 str  = sprinti("%0.2i",dd)+month(mm) 
;**************************

;************************************************
; read in data
;************************************************
stat_ctl = "${SET_OBS1}"                                                        
stat_exp = "${SET_OBS2}"  

print(" Stat. CTL file ")                                                       
print(" Stat. EXP file ")                                                       
print("==============================================")                         
                                                                                
f1 = addfile(stat_ctl, "r")                                                     
cb_avg_stat_ctl0 = f1->${idx[$nidx]}(:,:,${nvar})                                     
f2 = addfile(stat_exp, "r")                                                     
cb_avg_stat_exp0 = f2->${idx[$nidx]}(:,:,${nvar})                                     
dim=dimsizes(cb_avg_stat_exp0)                                                  
                                                                                
cb_avg_stat_ctl0@_FillValue = -999.                                             
cb_avg_stat_exp0@_FillValue = -999.                                             
cb_avg_stat_ctl0 = where (cb_avg_stat_ctl0 .gt. -998., cb_avg_stat_ctl0, cb_avg_stat_ctl0@_FillValue)
cb_avg_stat_ctl0 = where (cb_avg_stat_ctl0 .eq. 0.,    cb_avg_stat_ctl0@_FillValue, cb_avg_stat_ctl0) 
cb_avg_stat_exp0 = where (cb_avg_stat_exp0 .gt. -998., cb_avg_stat_exp0, cb_avg_stat_exp0@_FillValue)
cb_avg_stat_exp0 = where (cb_avg_stat_exp0 .eq. 0.,    cb_avg_stat_exp0@_FillValue, cb_avg_stat_exp0)

 all_chn = f2->channel                                                          
 ch_sat = (/all_chn/)                                                           
 ch_sat_da0 = (/all_chn/)                                                       
                                                                                
llist = (/"cris","amsua","iasi","mhs","atms","amsr2","mwhs2"/)                                 
if (any("${list}" .eq. llist)) then                                             
 delete(ch_sat_da0)                                                             
 ch_sat = (/all_chn/)                                                           
 ch_sat_da0 = (/${sel_chn}/)                                                    
end if

cb_avg_stat_exp = cb_avg_stat_exp0(:, {ch_sat_da0})                            
cb_avg_stat_ctl = cb_avg_stat_ctl0(:, {ch_sat_da0})                            
;cb_avg_sat = cb_avg_sat0({ch_sat_da0})                                         
;igood = ind(cb_avg_sat .gt. -999.)                                             
;nmi = num(cb_avg_sat .gt. -999.)                                               
;cb_avg_sat2 = new(nmi, "float")                                                
;cb_avg_sat2 = cb_avg_sat(igood)                                                
;ch_sat_da   = new(nmi, "float")                                                
;ch_sat_da   = ch_sat_da0(igood)                      
 
avg_ts_ctl = dim_avg_n_Wrap(cb_avg_stat_ctl,1)  ; O-B, C-B mean
avg_ts_exp = dim_avg_n_Wrap(cb_avg_stat_exp,1)  ; O-B, C-B mean

;************************************************
; plotting parameters
;************************************************
 fign = "${FIG_DIR}/TS/OB4DA_TS_${list}_${SDATE}_${EDATE}_${idx_name[$nidx]}_${varidx[$nvar]}"
 wks = gsn_open_wks("png", fign)

 res                    = True                 ; plot mods desired
 res@gsnMaximize        = True
 res@tiMainFont        = "helvetica-bold"

 res@vpHeightF=0.4
 res@vpWidthF=0.75

; max1 = dim_max(avg_ts_exp)
; min1 = dim_min(avg_ts_exp)

 ytick_v=getchn_v("${list}","${idx[$nidx]}","${varidx[$nvar]}")
 res@trYMinF           = stringtofloat(ytick_v(0))
 res@trYMaxF           = stringtofloat(ytick_v(1))


 res@xyMarkLineMode = "MarkLines"              ; Markers *and* lines
 res@xyMarkers      = (/16,16/)      ; filled circle(md2), hollow squre(md1)
 res@xyMarkerColors  = (/"black","red"/)
 res@xyDashPatterns = (/0,0/)        ; 0 is solid line
 res@xyLineColors   = (/"black","red"/)
 res@xyLineThicknessF =6.0
  
 res@tmXBMode   = "Explicit"
 xmax=dimsizes(xtime1) -1
 res@tmXBValues = ispan(0,xmax,(${xint}/${INTV}))
 res@tmXBLabels = str 
 res@tmXBLabelFontHeightF = 0.015
 res@tmYLLabelFontHeightF = 0.015
 res@tmXBLabelAngleF      = 0
 res@tmXBLabelFont        = "helvetica"

; res@tmYLMode   = "Explicit"
; res@tmYLValues = (/ytick_v(0),ytick_v(1)/)
; res@tmYUseBottom = True
; res@tmYUseTop = True
 res@tmYLLabelFont = "helvetica"

 res@tiMainString  = "${list1}"
 res@gsnLeftString  = "${idx_name[$nidx]} ${varidx[$nvar]}"   ; add center string               
; res@tiYAxisString = "${idx_name0[$nidx]}"
 res@tiXAxisString = "DATE"

 res@pmLegendDisplayMode    = "Always"
 res@pmLegendSide           = "Top"               ; Change location of
 res@pmLegendParallelPosF   = .86                 ; move units right
 res@pmLegendOrthogonalPosF = -0.5               ; move units down
 res@pmLegendWidthF         = 0.12                ; Change width and
 res@pmLegendHeightF        = 0.1                 ; height of legend.

 res@lgLabelFontHeightF     = (/.014,.014/)       ; change font height
 res@lgLabelFont            = "helvetica"         ; Font
 res@lgPerimOn              = False               ; no box around
 res@xyExplicitLegendLabels = (/"${CTLNAME}","${EXPNAME}"/)

 data=new((/2,dimsizes(avg_ts_ctl)/),float)
 data(0,:)=avg_ts_ctl(:)
 data(1,:)=avg_ts_exp(:)
 xaxis=ispan(0,xmax,1)+0.005
 plot=gsn_csm_xy(wks,xaxis,data,res)
 
 system("convert -trim " + fign + ".png " + fign + ".png" )

EOF
ncl -Q -n ${HOME}/NCL/Draw_plot_ts_chn.ncl
rm -f ${HOME}/NCL/Draw_plot_ts_chn.ncl

let nvar=nvar+1
done

let nidx=nidx+1
done

