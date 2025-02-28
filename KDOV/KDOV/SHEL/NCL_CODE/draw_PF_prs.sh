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

idx=("OmB")
idx_name=('OmB')
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
ctl_var = f1->${idx}
dim=dimsizes(ctl_var)
dimv=tointeger(dim(3))
var_name = (/"U","V","T","Q"/)
all_chn = f2->pressure

do iv = 0,dimv-1
 print(iv)
 cb_std_stat_ctl0 = f1->${idx}(:,:,0,iv)
 cb_std_stat_exp0 = f2->${idx}(:,:,0,iv)
 cb_std_stat_ctl0@_FillValue = 0.
 cb_std_stat_exp0@_FillValue = 0.
 
 ch_sat = (/all_chn/)
 ch_sat_da0 = ch_sat
 cb_std_stat_exp0!1 = "pressure"
 cb_std_stat_exp0&pressure = ch_sat
 cb_std_stat_ctl0!1 = "pressure"
 cb_std_stat_ctl0&pressure = ch_sat
 
 cb_std_sat0 = dim_avg_n_Wrap(cb_std_stat_exp0/cb_std_stat_ctl0, 0)  ; Normalized FG std (EXP/CTL)
 cb_std_sat0 = cb_std_sat0 * 100 ; %

 cb_std_sat0!0 = "pressure"
 cb_std_sat0&pressure = ch_sat

 
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
 error_sat = z*sigma_sat/sqrt(dimsizes(cb_std_stat_exp(:,0)))
 error_sat = error_sat * 100
 error_sat2 = new(nmi, "float")
 error_sat2 = error_sat(igood)


 print("######## Error ########")
 print("sat_e:      " + error_sat2)
 print("#######################")
 
 if ("${list}" .eq. "gpsro") then
  outfig= "${FIG_DIR}/PF/OB4DA_PF_${list}_${SDATE}_${EDATE}_${idx_name[${nidx}]}"
 else
  outfig= "${FIG_DIR}/PF/OB4DA_PF_${list}_"+var_name(iv)+"_${SDATE}_${EDATE}_${idx_name[${nidx}]}"
 end if
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
 
 
 res@tiYAxisString = "Pressure"
 
 res@tiXAxisString = "FG std. dev. (%, normalized)"
 res@tmYLValues = ispan(0, dimsizes(ch_sat_da)-1, 1)
 res@tmYLLabels = " " + ch_sat_da
 
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
 res@tmXBValues = (/xmin, 96, 97, 98, 99, 100, 101, 102, 103, 104, xmax/)        
 res@tmXBLabels = (/"95", "96", "97", "98", "99", "100", "", "102", "", "104", ""/)

 res@gsnLeftStringFontHeightF = 0.012
 res@gsnLeftStringOrthogonalPosF = 0.02
 res@gsnXRefLine = 100
 if ("${list}" .eq. "gpsro") then
  res@gsnLeftString = "${idx}" +""+"(BA)"
 else
  res@gsnLeftString = "${idx}" +""+"("+var_name(iv)+")"
 end if
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
 delete(cb_std_sat0)

 do n=0, dimsizes(ch_sat_da)-1
    if (ismissing(error_sat2(n))) then
     print("-")
    else
     ebar_sat(n) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)+error_sat2(n)/),(/n, n/), polyres)
     ebar_sat_vert(2*n) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)-error_sat2(n), cb_std_sat2(n)-error_sat2(n)/),(/n-vertd, n+vertd/), polyres)
     ebar_sat_vert(2*n+1) = gsn_add_polyline(wks, plot_sat, (/cb_std_sat2(n)+error_sat2(n), cb_std_sat2(n)+error_sat2(n)/),(/n-vertd, n+vertd/), polyres)
    end if
 end do
 

 draw(plot_sat)
 frame(wks)
 system("convert -trim " + outfig + ".png " + outfig + ".png")
 
end do

end

EOF

ncl -Q -n ${HOME}/NCL/Draw_plot_pf.ncl


let nidx=nidx+1
done
