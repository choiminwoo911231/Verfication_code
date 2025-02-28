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
 idx="OmB"
 var_name=( "OmB_mean" "OmB_std" )
 if [ ${list} = "gpsro" ]; then
  prs_idx=( "500" "300" "200" "100" "50" "25" "10" )
 else
  prs_idx=( "1000" "925" "850" "700" "500" "300" "200" "100" "50" ) 
 fi
  var_idx=( "U" "V" "T" "Q" "RH" "PS")

 lenvar=${#var_name[@]}
 lenmdl=${#mdl[@]}
 lenprs=${#prs_idx[@]}
 lenvdx=${#var_idx[@]}
#===============================================================================================

echo "!-------------------------------------------------------"
echo "*** DRAW_MAP ***"
echo "!-------------------------------------------------------"


nmdl=0
while [ $nmdl -le $lenmdl-1 ];do
file_name="${OUTDIR}/${mdl[$nmdl]}/OB4DA_MAP_${SDATE}_${EDATE}_STAT_${list}.nc"
echo $file_name

nvar=0
while [ $nvar -le $lenvar-1 ];do

nprs=0
while [ $nprs -le $lenprs-1 ];do

nvdx=0
while [ $nvdx -le $lenvdx-1 ];do


cat > draw_map_prs.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
if ("${list}" .eq. "gpsro") then
 outfig  = "${FIGDIR}/MAP/OB4DA_MAP_${mdl_name[$nmdl]}_${list}_"+"BA"+"_${prs_idx[$nprs]}hPa_${SDATE}_${EDATE}_${var_name[$nvar]}"
else
 outfig  = "${FIGDIR}/MAP/OB4DA_MAP_${mdl_name[$nmdl]}_${list}_${var_idx[$nvdx]}_${prs_idx[$nprs]}hPa_${SDATE}_${EDATE}_${var_name[$nvar]}"
end if
print("outfig :"+outfig)

 model_file = "${file_name}"
 opt1 = addfile(model_file,"r")

 latm = opt1->lat
 lonm = opt1->lon(0:359)
 variable = opt1->variable
 varn=dimsizes(variable)

 if ("$nvdx" .lt. varn) then
 omb = opt1->${idx}(:,${nprs},${nvdx},:,0:359)
 omb@_FillValue = -999.
 omb = where (omb .gt. -999., omb, omb@_FillValue)
 omb = where (omb .eq. 0., omb@_FillValue, omb)

 if ("$nvar" .eq. "0") then
  diff = dim_avg_n_Wrap(omb,0)
 end if
 if ("$nvar" .eq. "1") then
  diff = dim_stddev_n_Wrap(omb,0)
 end if
 
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

;  gsn_define_colormap(wks,"BlueDarkRed18")

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
  if ("${nvar}" .eq. "0") then
   res@cnMinLevelValF   = -5.0               ; set min contour level
   res@cnMaxLevelValF   =  5.0               ; set max contour level
   res@cnLevelSpacingF  =  0.5               ; set contour spacing
   res@cnFillPalette = "NCV_jet"
   gsn_define_colormap(wks,"NCV_jet")
  end if
  if ("${nvar}" .eq. "1") then
   res@cnMinLevelValF   =  0.4               ; set min contour level
   res@cnMaxLevelValF   =  4.0               ; set max contour level
   res@cnLevelSpacingF  =  0.4  
   res@cnFillPalette = "MPL_GnBu"
   gsn_define_colormap(wks,"MPL_GnBu")
  end if

  res@mpCenterLonF     = 180                ; set map center at 180
  res@mpFillOn = False
  res@mpGeophysicalLineThicknessF = 4.0

  ;res@mpLandFillColor  = "white"     ; choose color of continents.
                                            ; must be in colormap  
   
  res@tiMainString   = "${list1}     ${mdl[$nmdl]}"
  if ("${list1}" .eq. "GPSRO") then                                             
   res@tiMainString  = "${list1} (10~S~4~N~)     ${mdl[$nmdl]}"   ; add center string  
  end if  
  res@gsnLeftString  = "${var_name[$nvar]}"   ; add center string
  res@gsnRightString    = "Ave="+sprintf("%6.3f",diffavg)+", "+"Min="+sprintf("%6.3f",diffmin)+", "+"Max="+sprintf("%6.3f",diffmax)

  plot = new(2,graphic)
  plot(0) = gsn_csm_contour_map_ce(wks,diff1, res) ; create plot

;----- HISTOGRAM ---------------------------------------------------
  hres = True
  hres@gsnDraw  = False
  hres@gsnFrame = False

  data_1d = ndtooned(diff1)
  data_1d@_FillValue = -999.

 if ("${nvar}" .eq. "0") then
  xtick_st  = -5.5
  xtick_int = 0.5

  xtick=new((/24/),float)
  xtick(0)=-99.
  xtick(23)=99.
  xtick(1:22)= xtick_int * ispan(0,21,1) + xtick_st
  data_1d = where (data_1d .lt. xtick(2), xtick(2)-0.001, data_1d)
  data_1d = where (data_1d .gt. xtick(22), xtick(22)+0.001, data_1d)
  hres@gsnHistogramBinIntervals = (/xtick(1:23)/)
 else
  xtick_st  = 0.0
  xtick_int = 0.4

  xtick=new((/12/),float)
  xtick(11)=99.
  xtick(0:10)=xtick_int * ispan(0,10,1) + xtick_st
  data_1d = where (data_1d .gt. xtick(10), xtick(10)+0.001, data_1d)
  hres@gsnHistogramBinIntervals = (/xtick/)
   end if  

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

 if ("${nvar}" .eq. "0") then
  ndc_xst  = 0.177
  ndc_xst1 = 0.18
  ndc_int  = 0.1365
  ndc_yst  = 0.28
  gsn_text_ndc(wks, "-5", ndc_xst,                 ndc_yst, txres)
  gsn_text_ndc(wks, "-4", ndc_xst+(ndc_int*0.5),   ndc_yst, txres)
  gsn_text_ndc(wks, "-3", ndc_xst+(ndc_int*1.0),   ndc_yst, txres)
  gsn_text_ndc(wks, "-2", ndc_xst+(ndc_int*1.5),   ndc_yst, txres)
  gsn_text_ndc(wks, "-1", ndc_xst+(ndc_int*2.0),   ndc_yst, txres)
  gsn_text_ndc(wks, "0",  ndc_xst1+(ndc_int*2.5),  ndc_yst, txres)
  gsn_text_ndc(wks, "1",  ndc_xst1+(ndc_int*3.0),  ndc_yst, txres)
  gsn_text_ndc(wks, "2",  ndc_xst1+(ndc_int*3.5),  ndc_yst, txres)
  gsn_text_ndc(wks, "3",  ndc_xst1+(ndc_int*4.0),  ndc_yst, txres)
  gsn_text_ndc(wks, "4",  ndc_xst1+(ndc_int*4.5),  ndc_yst, txres)
  gsn_text_ndc(wks, "5",  ndc_xst1+(ndc_int*5.0),  ndc_yst, txres)
 else
  ndc_xst  = 0.15
  ndc_int  = 0.1363
  ndc_yst  = 0.28
  gsn_text_ndc(wks, "0",   ndc_xst,               ndc_yst, txres)
  gsn_text_ndc(wks, "0.8", ndc_xst+(ndc_int*1.0), ndc_yst, txres)
  gsn_text_ndc(wks, "1.6", ndc_xst+(ndc_int*2.0), ndc_yst, txres)
  gsn_text_ndc(wks, "2.4", ndc_xst+(ndc_int*3.0), ndc_yst, txres)
  gsn_text_ndc(wks, "3.2", ndc_xst+(ndc_int*4.0), ndc_yst, txres)
  gsn_text_ndc(wks, "4.0", ndc_xst+(ndc_int*5.0), ndc_yst, txres)
 end if


;----- Panel plot ----------------------------------------------
  pres = True
  pres@gsnMaximize = True 
  pres@gsnPanelYWhiteSpacePercent = 1.0
  pres@gsnPanelYF = (/0.9,0.45/)
  gsn_panel(wks, plot, (/2,1/), pres)


system("convert -trim " + outfig + ".png " + outfig + ".png")

  end if
EOF
ncl -Q -n draw_map_prs.ncl
rm -f draw_map_prs.ncl

let nvdx=nvdx+1
done

let nprs=nprs+1
done

let nvar=nvar+1
done

let nmdl=nmdl+1
done

rm -f draw_map_prs.ncl
