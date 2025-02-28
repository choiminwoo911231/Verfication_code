#!/usr/bin/bash

HOME=$1
SDATE=$2
EDATE=$3

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

##----------------------------------------------------------

idx=( "OmB" "CmB")
idx_name=( "OmB" "CmB" )
lenidx=${#idx[@]}
let lenidx=lenidx-1
  
nidx=0
while [ $nidx -le ${lenidx} ];do
  echo "====================================="
  echo "          === Draw "${idx_name[${nidx}]}" ==="
  echo "====================================="
##==========================================================
##==========================================================
mkdir -p ${HOME}/NCL
cat > ${HOME}/NCL/Draw_plot_pf_chn.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "${HOME}/SHEL/NCL_CODE/FUNC/sub_axis.ncl"

begin
print(" ")

stat_ctl = "${SET_OBS1}"
stat_exp = "${SET_OBS2}"
outfig   = "${FIG_DIR}/PF/OB4DA_PF_${list}_${SDATE}_${EDATE}_${idx_name[${nidx}]}"

print("==============================================")
print(" ")
print(" Date = " + "${SDATE}" + " ~ " + "${EDATE}")
print(" ")
print(" Stat. CTL file ")
print(" Stat. EXP file ")
print("==============================================")

f1 = addfile(stat_ctl, "r")
cb_std_stat_ctl0 = f1->${idx[$nidx]}(:,:,0)
f2 = addfile(stat_exp, "r")
cb_std_stat_exp0 = f2->${idx[$nidx]}(:,:,0)
dim=dimsizes(cb_std_stat_exp0)

cb_std_stat_ctl0@_FillValue = -999.
cb_std_stat_exp0@_FillValue = -999.
cb_std_stat_ctl0 = where (cb_std_stat_ctl0 .gt. -998., cb_std_stat_ctl0, cb_std_stat_ctl0@_FillValue)
cb_std_stat_ctl0 = where (cb_std_stat_ctl0 .eq. 0.,    cb_std_stat_ctl0@_FillValue, cb_std_stat_ctl0) 
cb_std_stat_exp0 = where (cb_std_stat_exp0 .gt. -998., cb_std_stat_exp0, cb_std_stat_exp0@_FillValue)
cb_std_stat_exp0 = where (cb_std_stat_exp0 .eq. 0.,    cb_std_stat_exp0@_FillValue, cb_std_stat_exp0)

; all_chn  = f2->channel
; all_chn0 = f1->channel
; ch_sat = (/all_chn/)
; ch_sat_da0 = (/all_chn/)

;reverse channel profile (row2high)
rvs_pf = (/"csrgk2a","csrhima","csrmsg"/)
if (any("${list}" .eq. rvs_pf)) then
 all_chn  = f2->channel
 all_chn0 = f1->channel
 all_chn  = all_chn(::-1)
 all_chn0 = all_chn0(::-1)
 ch_sat = (/all_chn/)                                                           
 ch_sat_da0 = (/all_chn/)
else
 all_chn  = f2->channel                                                         
 all_chn0 = f1->channel                                                         
 ch_sat = (/all_chn/)                                                           
 ch_sat_da0 = (/all_chn/) 
end if

lllist = (/"cris","mwhs2"/)
llist  = (/"cris","amsua","iasi","mhs","atms","amsr2","mwhs2"/)
if (any("${list}" .eq. llist)) then
 delete(ch_sat_da0)
 ch_sat  = (/all_chn/)
 ch_sat0 = (/all_chn0/)
 ch_sat_da0 = (/${sel_chn}/)
 if (any("${list}" .eq. lllist)) then
  ch_sat_da0 = ch_sat_da0(::-1)
 end if

 cb_std_stat_exp0!1 = "channel"
 cb_std_stat_exp0&channel = ch_sat
 cb_std_stat_ctl0!1 = "channel"
 cb_std_stat_ctl0&channel = ch_sat0
                                             
 cb_std_sat0 = dim_avg_n_Wrap(cb_std_stat_exp0(:,{ch_sat_da0})/cb_std_stat_ctl0(:,{ch_sat_da0}), 0)  ; Normalized FG std (EXP/CTL)
 cb_std_sat0 = cb_std_sat0 * 100 ; %                                             
                                                                                
 cb_std_sat0!0 = "channel"                                                       
 cb_std_sat0&channel = ch_sat_da0

 else

 cb_std_stat_exp0!1 = "channel"
 cb_std_stat_exp0&channel = ch_sat
 cb_std_stat_ctl0!1 = "channel"
 cb_std_stat_ctl0&channel = ch_sat

 cb_std_sat0 = dim_avg_n_Wrap(cb_std_stat_exp0/cb_std_stat_ctl0, 0)  ; Normalized FG std (EXP/CTL)
 cb_std_sat0 = cb_std_sat0 * 100 ; %

 cb_std_sat0!0 = "channel"
 cb_std_sat0&channel = ch_sat

end if

 cb_std_stat_exp = cb_std_stat_exp0(:, {ch_sat_da0})
 cb_std_stat_ctl = cb_std_stat_ctl0(:, {ch_sat_da0})
 cb_std_sat = cb_std_sat0({ch_sat_da0})
 igood = ind(cb_std_sat .gt. -999.)
 nmi = num(cb_std_sat .gt. -999.)
 cb_std_sat2 = new(nmi, "float")
 cb_std_sat2 = cb_std_sat(igood)
 ch_sat_da   = new(nmi, "float")
 ch_sat_da   = ch_sat_da0(igood)


print("######## STDV ########")
print("sat:         " + cb_std_sat2 + "(" + (100.-(cb_std_sat2)) + ")" )
print("######################")
print(" ")
;################################ error bars
z=1.96

sigma_sat = dim_stddev_n_Wrap(cb_std_stat_exp/cb_std_stat_ctl, 0)
error_sat = z*sigma_sat/sqrt(dimsizes(cb_std_stat_exp(:, 0)))
error_sat = error_sat * 100
error_sat2 = new(nmi, "float")
error_sat2 = error_sat(igood)



print("######## Error ########")
print("sat_e:      " + error_sat2)
print("#######################")

;#####################################################
wks_type = "png"
;wks_type@wkWidth = 1600
;wks_type@wkHeight = 1600
wks   = gsn_open_wks (wks_type, outfig)

res = True
res@gsnDraw = False
res@gsnFrame = False

res@tmYLMode = "Explicit"

res@vpHeightF = 0.6 ; ori
res@vpWidthF = 0.25 ; ori

res@tmXTOn = False
res@tmYROn = False
res@tmYLMinorLengthF = 0

res@xyMarkLineModes = "MarkLines"
res@xyMarkers = 16
res@xyMarkerSizeF = 0.007
res@xyMarkerColor = "black"
res@xyLineColor = "black"
res@xyLineThicknessF = 4


res@tmXMajorGrid = True
res@tmXMajorGridThicknessF = 1.0
res@tmXMajorGridLineDashPattern = 2
res@tmXMajorGridLineColor = "gray70"

res@tmYMajorGrid = True
res@tmYMajorGridThicknessF = 1.0
res@tmYMajorGridLineDashPattern = 2
res@tmYMajorGridLineColor = "gray70" 


res@tiYAxisString = "Channel (hPa)"
res@tiXAxisString = "FG std. dev. (%, normalized)"

;;; sub_axis
if ("${list}" .eq. "iasi") then
 tt = ispan(0, dimsizes(ch_sat_da)-1, 1)
 res@tmYLValues = tt(::3)
 prs_v=getprs_value("${list}")
 res@tmYLLabels = " " + ch_sat_da(::3) + prs_v
else
 res@tmYLValues = ispan(0, dimsizes(ch_sat_da)-1, 1)
 prs_v=getprs_value("${list}")
 res@tmYLLabels = " " + ch_sat_da + prs_v
end if

res@tmXBLabelFontHeightF = 0.0125
res@tmYLLabelFontHeightF = 0.0125

  res@tiMainString      = "${list1}"
  res@tiMainFont        = "helvetica-bold"
  res@tiMainFontHeightF = 0.02
 
 xmin=95.
 xmax=105.
 res@trXMinF = xmin-0.2
 res@trXMaxF = xmax+0.2
 res@trYMinF = 0-0.15
 res@trYMaxF = dimsizes(ch_sat_da)-1+0.15
 
 res@tmXBMode = "Explicit"
 res@tmXBValues = (/xmin, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105/)
 res@tmXBLabels = (/"95", "96", "97", "98", "99", "100", "", "102", "", "104", ""/)
 ;res@tmXBValues = (/xmin, 50.0, 60., 70., 80., 90., 100.0, 110., xmax/)
 ;res@tmXBLabels = " " + res@tmXBValues
 
 res@gsnLeftStringFontHeightF = 0.012
 res@gsnLeftStringOrthogonalPosF = 0.02
 res@gsnXRefLine = 100
 res@gsnLeftString  = "${idx[$nidx]}" +""+"(TB)"
 res@gsnRightStringFontHeightF = 0.012
 res@gsnRightString = "CTL: ${CTLNAME} ~C~EXP: ${EXPNAME}" 
 

plot_sat = gsn_csm_xy(wks,  cb_std_sat2, ispan(0, dimsizes(cb_std_sat2)-1, 1), res)

;################################ error bars

polyres = True
polyres@gsMarkerIndex = 1
polyres@gsLineThicknessF = 2

ebar_sat = new(dimsizes(ch_sat_da), graphic)
ebar_sat_vert = new(dimsizes(ch_sat_da)*2, graphic)

vertd = 0.02

do n=0, dimsizes(ch_sat_da)-1
    ebar_sat(n) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)+error_sat2(n)/),(/n, n/), polyres)
    ebar_sat_vert(2*n) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)-error_sat2(n)/),(/n-vertd, n+vertd/), polyres)
    ebar_sat_vert(2*n+1) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)+error_sat2(n), cb_std_sat2(n)+error_sat2(n)/),(/n-vertd, n+vertd/), polyres)
end do

;############################### TEMP or HUMID channel tick
  txres = True
  txres@txFontHeightF = 0.015

 if ("${list}" .eq. "atms") then
  plus=0.425
  gsn_text_ndc(wks, "Temp",   0.49 , 0.45, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.207, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.372+plus, txres)

  gsn_text_ndc(wks, "Humid",  0.49 , 0.28+plus, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.2 +plus, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.37+plus, txres)
 end if

 if ("${list}" .eq. "iasi") then
;  gsn_text_ndc(wks, "Temp",   0.49 , 0.46,  txres)
;  gsn_text_ndc(wks, "-----",  0.48 , 0.207, txres)
;  gsn_text_ndc(wks, "-----",  0.48 , 0.745, txres)

;  gsn_text_ndc(wks, "Humid",  0.49 , 0.77, txres)
;  gsn_text_ndc(wks, "-----",  0.48 , 0.795,  txres)

  gsn_text_ndc(wks, "Temp",   0.49 , 0.5,   txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.257, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.795, txres)

  gsn_text_ndc(wks, "Humid",  0.49 , 0.234, txres)
  gsn_text_ndc(wks, "-----",  0.48 , 0.207,  txres)
 end if



draw(plot_sat)
frame(wks)
system("convert -trim " + outfig + ".png " + outfig + ".png")

end

EOF

ncl -Q -n ${HOME}/NCL/Draw_plot_pf_chn.ncl


let nidx=nidx+1
done
