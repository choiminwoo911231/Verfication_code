#===============================================================================================
#!/bin/ksh -E
#===============================================================================================
# Plot the background in the global map
#
# Model index (25 levels).
# hPa   : 1 2 3 5 7 10 20 30 50 70 100 150 200 250 300 400 500 600 700 800 850 900 925 950 1000
# index : 0 1 2 3 4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23   24
#===============================================================================================
# Experimental Settings
!
# export OUTDIR="/h3/home/kiaps/k-da-de05/DIAG/DATA/OUT/"
 export HOME=$1
 export SDATE=$2   #[Analysis cycle start time]
 export EDATE=$3   #[Analysis cylce   end time]
 export FIGDIR=$4
 export OUTDIR=$5
 export CTL_NAME=$6
 export EXP_NAME=$7
 export list=$8
 list1=$(echo "$list" | tr '[:lower:]' '[:upper:]')

 mdl=( ${CTL_NAME} ${EXP_NAME} )
 mdl_name=( "CTL" "EXP" )
 idx=( "OmB" "CmB" )
 var_name=( "OmB_std_DIFF" "CmB_std_DIFF" )
 var_idx=( "U" "V" "T" "Q" "RH" "PS" )

 lenmdl=${#mdl[@]}
 lenvar=${#idx[@]}
 lenvdx=${#var_idx[@]}
#===============================================================================================

echo "!-------------------------------------------------------"
echo "*** DRAW_MAP ***"
echo "!-------------------------------------------------------"


file_name0="${OUTDIR}/${mdl[0]}/OB4DA_MAP_${SDATE}_${EDATE}_STAT_${list}.nc"
file_name1="${OUTDIR}/${mdl[1]}/OB4DA_MAP_${SDATE}_${EDATE}_STAT_${list}.nc"
echo $file_name0
echo $file_name1

nvdx=0
while [ $nvdx -le $lenvdx-1 ];do

nvar=0
while [ $nvar -le $lenvar-1 ];do


cat > draw_map_sfc_omb_diff.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

if ("${list}" .eq. "grndgnss") then
 outfig  = "${FIGDIR}/MAP/OB4DA_MAP_${list}_${SDATE}_${EDATE}_${var_name[$nvar]}"
else
 outfig  = "${FIGDIR}/MAP/OB4DA_MAP_${list}_${var_idx[$nvdx]}_${SDATE}_${EDATE}_${var_name[$nvar]}"
end if
print("outfig :"+outfig)

 model_file0 = "${file_name0}"
 model_file1 = "${file_name1}"
 ctl0 = addfile(model_file0,"r")
 exp0 = addfile(model_file1,"r")

 latm = ctl0->lat
 lonm = ctl0->lon(0:359)
 variable = ctl0->variable
 varn=dimsizes(variable)

 if ("$nvdx" .lt. varn) then
 ctl1 = ctl0->${idx[$nvar]}(:,${nvdx},:,0:359)
 exp1 = exp0->${idx[$nvar]}(:,${nvdx},:,0:359)
 ctl1@_FillValue = -999.
 ctl1 = where (ctl1 .gt. -999., ctl1, ctl1@_FillValue)
 ctl1 = where (ctl1 .eq. 0., ctl1@_FillValue, ctl1)
 exp1@_FillValue = -999.
 exp1 = where (exp1 .gt. -999., exp1, exp1@_FillValue)
 exp1 = where (exp1 .eq. 0., exp1@_FillValue, exp1)

 ctl_std = dim_stddev_n_Wrap(ctl1,0)
 exp_std = dim_stddev_n_Wrap(exp1,0)
 diff = exp_std - ctl_std 

  diff1 = diff(:,:)

  diff1!0   = "lat"
  diff1!1   = "lon"
  diff1&lat = latm
  diff1&lon = lonm
  diff1&lat@units = "degrees_north"
  diff1&lon@units = "degrees_east"

  diffmax  = max(diff1)
  diffmin  = min(diff1)
  diffavg  = avg(diff1)

;-------------------------------------------------------
; Plot
;-------------------------------------------------------
  wks = gsn_open_wks("png", outfig)


  res                  = True                ; plot mods desired
;===================================
; you can have the contour lines on in raster mode, but their thickness
; actually make the plot look like is was contoured normally.
  res@cnFillOn         = True               ; color Fill 
  res@cnFillMode       = "RasterFill"       ; Raster Mode
  res@cnLinesOn        = False             ; Turn off contour lines
  res@cnLineLabelsOn   = False
  res@gsnDraw  = False
  res@gsnFrame = False
  res@lbLabelBarOn = False
  res@cnInfoLabelOn = False
;================================
; these three resources make the continents look nice. The first two
; make them color, and the later adds continental outlines when
; raster mode is used.

  res@cnLineDrawOrder  = "Predraw"          ; Draw lines and filled
  res@cnFillDrawOrder  = "Predraw"          ; areas before map gets set
;=================================
  res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
   res@cnMinLevelValF   = -0.4               ; set min contour level
   res@cnMaxLevelValF   =  0.4               ; set max contour level
   res@cnLevelSpacingF  =  0.05              ; set contour spacing

  res@mpCenterLonF     = 180                ; set map center at 180
  res@mpFillOn = False
  res@mpGeophysicalLineThicknessF = 4.0

  res@tiMainString   = "${list1}     ${EXP_NAME}-${CTL_NAME}"
  res@gsnLeftString  = "${var_name[$nvar]}"   ; add center string
  res@gsnRightString    = "Ave="+sprintf("%6.3f",diffavg)+", "+"Min="+sprintf("%6.3f",diffmin)+", "+"Max="+sprintf("%6.3f",diffmax)
  gsn_define_colormap(wks,"BlueDarkRed18")

  plot = new(2,graphic)
  plot(0) = gsn_csm_contour_map_ce(wks,diff1, res) ; create plot

;----- HISTOGRAM ---------------------------------------------------
  hres = True
  hres@gsnDraw  = False
  hres@gsnFrame = False

  data_1d = ndtooned(diff1)
  data_1d@_FillValue = -999.

  xtick_st  = res@cnMinLevelValF-0.05
  xtick_int = res@cnLevelSpacingF

  tn=20
  xtick=new((/tn/),float)
  xtick(0)=-99.
  xtick(tn-1)=99.
  xtick(1:tn-2)= xtick_int * ispan(0,tn-3,1) + xtick_st
  data_1d = where (data_1d .lt. xtick(2), xtick(2)-0.001, data_1d)
  data_1d = where (data_1d .gt. xtick(tn-2), xtick(tn-2)+0.001, data_1d)
  hres@gsnHistogramBinIntervals = (/xtick(1:tn-1)/)

  ;hres@gsnYRefLine = 0.
  hres@vpWidthF  = 0.8
  hres@vpHeightF = 0.15
  ;hres@tmXBMajorInwardLengthF = 0.021
  hres@tmXTBorderOn = False
  hres@tmXBBorderOn = False
  hres@tmYRBorderOn = False    
  hres@tmYLBorderOn = False
  hres@tmXTOn = False
  hres@tmXBOn = True
  hres@tmYLOn = False 
  hres@tmYROn = False
  hres@tmXBLabelsOn  = False
  hres@tiXAxisString = " "
  hres@tiYAxisString = " "

  hres@gsnHistogramBarWidthPercent=95
  plot(1) = gsn_histogram(wks, data_1d, hres)

;----- TEXT ---------------------------------------------------
  txres = True
  txres@txFontHeightF = 0.015

  ndc_xst  = 0.185
  ndc_xst1 = 0.195
  ndc_int  = 0.164
  ndc_yst  = 0.28
  gsn_text_ndc(wks, "-0.4", ndc_xst,                 ndc_yst, txres)
  gsn_text_ndc(wks, "-0.3", ndc_xst+(ndc_int*0.5),   ndc_yst, txres)
  gsn_text_ndc(wks, "-0.2", ndc_xst+(ndc_int*1.0),   ndc_yst, txres)
  gsn_text_ndc(wks, "-0.1", ndc_xst+(ndc_int*1.5),   ndc_yst, txres)
  gsn_text_ndc(wks, "0",    ndc_xst+(ndc_int*2.05),  ndc_yst, txres)
  gsn_text_ndc(wks, "0.1",  ndc_xst1+(ndc_int*2.5),  ndc_yst, txres)
  gsn_text_ndc(wks, "0.2",  ndc_xst1+(ndc_int*3.0),  ndc_yst, txres)
  gsn_text_ndc(wks, "0.3",  ndc_xst1+(ndc_int*3.5),  ndc_yst, txres)
  gsn_text_ndc(wks, "0.4",  ndc_xst1+(ndc_int*4.0),  ndc_yst, txres)

;----- Panel plot ----------------------------------------------
  pres = True
  pres@gsnMaximize = True 
  pres@gsnPanelYWhiteSpacePercent = 1.0
  pres@gsnPanelYF = (/0.9,0.45/)
  gsn_panel(wks, plot, (/2,1/), pres)


system("convert -trim " + outfig + ".png " + outfig + ".png")

  end if
EOF
ncl -Q -n draw_map_sfc_omb_diff.ncl
rm -f draw_map_sfc_omb_diff.ncl

let nvar=nvar+1
done

let nvdx=nvdx+1
done


