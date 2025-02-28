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

##----------------------------------------------------------

idx=( "OmB" "CmB" )
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
cat > ${HOME}/NCL/Draw_plot_pf.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
print(" ")


stat_ctl = "${SET_OBS1}"
stat_exp = "${SET_OBS2}"
;outfig   = "${FIG_DIR}/PF/OB4DA_PF_${list}_${SDATE}_${EDATE}_${idx_name[${nidx}]}"

print("==============================================")
print(" ")
print(" Date = " + "${SDATE}" + " ~ " + "${EDATE}")
print(" ")
print(" Stat. CTL file ")
print(" Stat. EXP file ")
print(" ${list1} ")
print("==============================================")

f1 = addfile(stat_ctl, "r")
f2 = addfile(stat_exp, "r")
ctl_var = f1->${idx[$nidx]}
dim=dimsizes(ctl_var)
dimv=tointeger(dim(1))
var_name = (/"U","V","T","Q","RH","PS"/)
all_chn = f2->variable

 cb_std_stat_ctl0 = f1->${idx[$nidx]}(:,:,0)
 cb_std_stat_exp0 = f2->${idx[$nidx]}(:,:,0)
 cb_std_stat_ctl0@_FillValue = -999.
 cb_std_stat_exp0@_FillValue = -999.
 
 ch_sat = (/all_chn/)
 ch_sat_da0 = ch_sat
 cb_std_stat_exp0!1 = "variable"
 cb_std_stat_exp0&variable = ch_sat
 cb_std_stat_ctl0!1 = "variable"
 cb_std_stat_ctl0&variable = ch_sat
 
 cb_std_sat0 = dim_avg_n_Wrap(cb_std_stat_exp0/cb_std_stat_ctl0, 0)  ; Normalized FG std (EXP/CTL)
 cb_std_sat0 = cb_std_sat0 * 100 ; %

 cb_std_sat0!0 = "variable"
 cb_std_sat0&variable = ch_sat

 
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
 
 if ("${list}" .eq. "grndgnss") then
  sigma_sat = dim_stddev_n_Wrap(cb_std_stat_exp/cb_std_stat_ctl, 0)
  error_sat = z*sigma_sat/sqrt(dimsizes(cb_std_stat_exp(:)))
  error_sat = error_sat * 100
  error_sat2 = new(nmi, "float")
  error_sat2 = error_sat(igood)
 else
  sigma_sat = dim_stddev_n_Wrap(cb_std_stat_exp/cb_std_stat_ctl, 0)
  error_sat = z*sigma_sat/sqrt(dimsizes(cb_std_stat_exp(:,0)))
  error_sat = error_sat * 100
  error_sat2 = new(nmi, "float")
  error_sat2 = error_sat(igood)
 end if


 print("######## Error ########")
 print("sat_e:      " + error_sat2)
 print("#######################")
 
 outfig= "${FIG_DIR}/PF/OB4DA_PF_${list}_${SDATE}_${EDATE}_${idx_name[${nidx}]}"
 ;#####################################################
 wks_type = "png"
 ;wks_type@wkWidth = 1600
 ;wks_type@wkHeight = 1600
 wks   = gsn_open_wks (wks_type, outfig)
 
 res = True
 res@gsnDraw = False
 res@gsnFrame = False
 
 res@tmYLMode = "Explicit"
 
 res@vpHeightF = 0.25 ; ori
 res@vpWidthF = 0.6 ; ori
 
 res@tmXTOn = False
 res@tmYROn = False
 res@tmYLMinorLengthF = 0
 
 res@xyMarkLineModes = "MarkLines"
 res@xyMarkers = 16
 res@xyMarkerSizeF = 0.007
; res@tmXBLabels = " " + ch_sat_da
 res@xyMarkerColor = "black"
 res@xyLineColor = "white"
 res@xyLineThicknessF = 0
 
 
 res@tmXMajorGrid = True
 res@tmXMajorGridThicknessF = 1.0
 res@tmXMajorGridLineDashPattern = 2
 res@tmXMajorGridLineColor = "gray70"
 
 res@tmYMajorGrid = True
 res@tmYMajorGridThicknessF = 1.0
 res@tmYMajorGridLineDashPattern = 2
 res@tmYMajorGridLineColor = "gray70"
 
 
 res@tiXAxisString = "Variable"
 res@tiYAxisString = "FG std. dev. (%, normalized)"
 res@tmXBMode = "Explicit"
 res@tmXBValues = ispan(0, dimsizes(ch_sat_da)-1, 1)
 if ("${list}" .eq. "grndgnss") then
  res@tmXBLabels = " " + "ZTD"
 else
  res@tmXBLabels = " " + var_name(0:dimv-1)
 end if
 res@tmYLLabelFontHeightF = 0.0125
 res@tmXBLabelFontHeightF = 0.0125

  res@tiMainString      = "${list1}"
  res@tiMainFont        = "helvetica-bold"
  res@tiMainFontHeightF = 0.02
 
 xmin=95.
 xmax=105.
 res@trYMinF = xmin-0.2
 res@trYMaxF = xmax+0.2
 res@trXMinF = 0-0.15
 res@trXMaxF = dimsizes(ch_sat_da)-1+0.15
 res@trXMaxF = dimsizes(ch_sat_da)-1+0.15
 
 res@tmYLMode = "Explicit"
 res@tmYLValues = (/xmin, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105/)        
 res@tmYLLabels = (/"95", "96", "97", "98", "99", "100", "", "102", "", "104", ""/)

; res@tmYLValues = (/ymin, 98.0, 99, 100.0, ymax/)
; res@tmYLLabels = " " + res@tmYLValues
 
 res@gsnLeftStringOrthogonalPosF = 0.03
 res@gsnYRefLine = 100
 res@gsnLeftStringFontHeightF = 0.012
 res@gsnLeftString = "${idx[$nidx]}"
 res@gsnRightStringFontHeightF = 0.012
 res@gsnRightString = "CTL: ${CTLNAME} ~C~EXP: ${EXPNAME}" 

 
 if ("${list}" .eq. "grndgnss") then
  cb_std_sat3 = new(2, "float")
  cb_std_sat3(:) = cb_std_sat2
  plot_sat = gsn_csm_xy(wks,  ispan(0, dimsizes(cb_std_sat2), 1), cb_std_sat3, res)
 else
  plot_sat = gsn_csm_xy(wks,  ispan(0, dimsizes(cb_std_sat2)-1, 1), cb_std_sat2, res)
 end if
 
 ;################################ error bars
 
 polyres = True
 polyres@gsMarkerIndex = 1
 polyres@gsLineThicknessF = 2
 
 ebar_sat = new(dimsizes(ch_sat_da), graphic)
 ebar_sat_vert = new(dimsizes(ch_sat_da)*2, graphic)
 
 vertd = 0.02

 do n=0, dimsizes(ch_sat_da)-1
    if (ismissing(error_sat2(n))) then
     print("-")
    else
     ebar_sat(n) = gsn_add_polyline(wks, plot_sat, (/n, n/), (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)+error_sat2(n)/), polyres)
     ebar_sat_vert(2*n) = gsn_add_polyline(wks, plot_sat, (/n-vertd, n+vertd/), (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)-error_sat2(n)/), polyres)
     ebar_sat_vert(2*n+1) = gsn_add_polyline(wks, plot_sat, (/n-vertd, n+vertd/), (/cb_std_sat2(n)+error_sat2(n), cb_std_sat2(n)+error_sat2(n)/), polyres)
    end if
 end do
 

 draw(plot_sat)
 frame(wks)
 system("convert -trim " + outfig + ".png " + outfig + ".png")
 

end

EOF

ncl -Q -n ${HOME}/NCL/Draw_plot_pf.ncl


let nidx=nidx+1
done
