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
 idx="Data_num"                                                                 
 var_name=( "Data_count" )  
 var_idx=( "U" "V" "T" "Q" "RH" "PS" )

 lenvar=${#var_name[@]}
 lenvdx=${#var_idx[@]}
#===============================================================================================

echo "!-------------------------------------------------------"
echo "*** DRAW_MAP ***"
echo "!-------------------------------------------------------"


file_name1="${OUTDIR}/${mdl[0]}/OB4DA_MAP_${SDATE}_${EDATE}_COUNT_${list}.nc"
file_name2="${OUTDIR}/${mdl[1]}/OB4DA_MAP_${SDATE}_${EDATE}_COUNT_${list}.nc"
echo $file_name1
echo $file_name2

nvar=0
while [ $nvar -le $lenvar-1 ];do

nvdx=0
while [ $nvdx -le $lenvdx-1 ];do


cat > draw_map_sfc_count.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"


if ("${list}" .eq. "grndgnss") then
 outfig1  = "${FIGDIR}/MAP_COUNT/OB4DA_MAP_COUNT_DIFF_${list}_${SDATE}_${EDATE}"
else
 outfig1  = "${FIGDIR}/MAP_COUNT/OB4DA_MAP_COUNT_DIFF_${list}_${var_idx[$nvdx]}_${SDATE}_${EDATE}"
end if
print("outfig1 :"+outfig1)

 model_file1 = "${file_name1}"
 model_file2 = "${file_name2}"
 opt1 = addfile(model_file1,"r")
 opt2 = addfile(model_file2,"r")

 latm = opt1->lat(:)
 lonm = opt1->lon(0:359)
 variable = opt1->variable
 varn=dimsizes(variable)

 if ("$nvdx" .lt. varn) then
 dc = opt1->${idx}(:,${nvdx},:,0:359)
 dc@_FillValue = -999.                                                          
 dc = where (dc .gt. -999., dc, dc@_FillValue)                                  
 dc = where (dc .le. 0., dc@_FillValue, dc)                                     
 diff = dim_sum_n(dc,0)                                                         
 diff1 = diff(:,:)  

 diff1!0   = "lat"
 diff1!1   = "lon"
 diff1&lat = latm
 diff1&lon = lonm
 diff1&lat@units = "degrees_north"
 diff1&lon@units = "degrees_east"

 dc1 = opt2->${idx}(:,${nvdx},:,0:359)                                           
 dc1@_FillValue = -999.                                                          
 dc1 = where (dc1 .gt. -999., dc1, dc1@_FillValue)                                  
 dc1 = where (dc1 .le. 0., dc1@_FillValue, dc1)                                     
 diff3 = dim_sum_n(dc1,0)                                                         
 diff2 = diff3(:,:)                                                              
                                                                                
 diff2!0   = "lat"                                                              
 diff2!1   = "lon"                                                              
 diff2&lat = latm                                                               
 diff2&lon = lonm                                                               
 diff2&lat@units = "degrees_north"                                              
 diff2&lon@units = "degrees_east" 


 var = ((diff2-diff1)/diff1)*100.                                               
                                                                                
 var!0   = "lat"                                                                
 var!1   = "lon"                                                                
 var&lat = latm                                                                 
 var&lon = lonm                                                                 
 var&lat@units = "degrees_north"                                                
 var&lon@units = "degrees_east"         


;-------------------------------------------------------
; Plot
;-------------------------------------------------------
  wks1 = gsn_open_wks("png", outfig1)

  colormap = "NCV_blue_red"                                                     
  gsn_define_colormap(wks1,colormap)  

  res                  = True                ; plot mods desired
;===================================
; you can have the contour lines on in raster mode, but their thickness
; actually make the plot look like is was contoured normally.

  res@cnFillOn         = True               ; color Fill 
  res@cnFillMode       = "RasterFill"       ; Raster Mode
  res@cnLinesOn        = False             ; Turn off contour lines
  res@cnLineLabelsOn   = False
  res@lbLabelBarOn = True
  res@cnInfoLabelOn = False
;================================
; these three resources make the continents look nice. The first two
; make them color, and the later adds continental outlines when
; raster mode is used.

  res@cnLineDrawOrder  = "Predraw"          ; Draw lines and filled
  res@cnFillDrawOrder  = "Predraw"          ; areas before map gets set
;=================================
  res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels         
   res@cnMissingValFillColor =  "White"    ; set min contour level              
   res@cnMinLevelValF   =  -20.              ; set min contour level              
   res@cnMaxLevelValF   =  20          ; set max contour level          
   res@cnLevelSpacingF  =  2             ; set contour spacing             
   res@cnFillPalette = colormap                                                 


  res@mpCenterLonF     = 180                ; set map center at 180
  res@mpFillOn = False
  res@mpGeophysicalLineThicknessF = 4.0

  ;res@mpLandFillColor  = "white"     ; choose color of continents.
  
  res@tiMainString   = "${list1}"
  res@gsnLeftString  = "Rate of change in data count"   ; add center string
;  res@gsnRightString  = "Min="+sprintf("%6.3f",diffmin1)+", "+"Max="+sprintf("%6.3f",diffmax1)
  plot1 = gsn_csm_contour_map_ce(wks1,var, res) ; create plot

  system("convert -trim " + outfig1 + ".png " + outfig1 + ".png")

  end if
EOF
ncl -Q -n draw_map_sfc_count.ncl
rm -f draw_map_sfc_count.ncl

let nvdx=nvdx+1
done

let nvar=nvar+1
done


rm -f draw_map_sfc_count.ncl
