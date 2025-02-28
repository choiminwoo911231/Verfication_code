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
 export sel_chn=$9
 export chn_prs=${10}

 mdl=( ${CTL_NAME} ${EXP_NAME} )
 mdl_name=( "CTL" "EXP" )
 idx="Data_num"
 var_name=( "Data_count" )

 lenvar=${#var_name[@]}
 lenmdl=${#mdl[@]}
 ch_num=($(echo $sel_chn | tr ',' ' '))
 lench=${#ch_num[@]}
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

nch=0
while [ $nch -le $lench-1 ];do


cat > draw_map_count1.ncl << EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
ch_sat  = (/${sel_chn}/)
prs_sat = (/"${chn_prs}"/)
ch_num = ch_sat(${nch})
prs_num = str_split(prs_sat, ", ")
prs_nnum= prs_num(${nch})
outfig1  = "${FIGDIR}/MAP_COUNT/OB4DA_MAP_COUNT_DIFF_${list}_ch"+ch_num+"_"+prs_nnum+"hPa_${SDATE}_${EDATE}"
print("outfig1 :"+outfig1)


 model_file1 = "${file_name1}"
 model_file2 = "${file_name2}"
 opt1 = addfile(model_file1,"r")
 opt2 = addfile(model_file2,"r")

 latm = opt1->lat
 lonm = opt1->lon(0:119)

 dc = opt1->${idx}(:,{ch_num},:,0:119)
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

 dc1 = opt2->${idx}(:,{ch_num},:,0:119)                                          
 dc1@_FillValue = -999.                                                          
 dc1 = where (dc1 .gt. -999., dc1, dc1@_FillValue)                                  
 dc1 = where (dc1 .le. 0., dc1@_FillValue, dc1)                                     
 diff3 = dim_sum_n(dc1,0)                                                         
 diff2 = diff3(:,:)                                                              

 var = ((diff2-diff1)/diff1)*100.
; var@_FillValue = -999.                                                          
; var = where (var .ge. -1. .and. var .le. 1, var@_FillValue, var)  

 var!0   = "lat"                                                              
 var!1   = "lon"                                                              
 var&lat = latm                                                               
 var&lon = lonm                                                               
 var&lat@units = "degrees_north"                                              
 var&lon@units = "degrees_east" 



;-------------------------------------------------------
; Plot
;-------------------------------------------------------
  wks = gsn_open_wks("png", outfig1)
 
  colormap = "NCV_blue_red"

   gsn_define_colormap(wks,colormap)

  res                  = True                ; plot mods desired
;===================================
; you can have the contour lines on in raster mode, but their thickness
; actually make the plot look like is was contoured normally.

  res@cnFillOn         = True               ; color Fill 
  res@cnFillMode       = "RasterFill"       ; Raster Mode
  res@cnLinesOn        = False             ; Turn off contour lines
  res@cnLineLabelsOn   = False
;  res@gsnDraw  = False
;  res@gsnFrame = False
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
   res@cnMaxLevelValF   =  20.          ; set max contour level
   res@cnLevelSpacingF  =  2             ; set contour spacing
   res@cnFillPalette = colormap

  res@mpCenterLonF     = 180                ; set map center at 180
  res@mpFillOn = False
  res@mpGeophysicalLineThicknessF = 4.0
  ;;res@mpLandFillColor  = "white"     ; choose color of continents.
  res@tiMainString = "${list1} ch"+ch_num
  res@gsnLeftString  = "Rate of change in data count"   ; add center string
;  res@gsnRightString = "Min="+sprintf("%6.3f",diffmin1)+", "+"Max="+sprintf("%6.3f",diffmax1)
  map1 = gsn_csm_contour_map_ce(wks,var, res) ; create plot

  system("convert -trim " + outfig1 + ".png " + outfig1 + ".png")

EOF
ncl -Q -n draw_map_count1.ncl
rm -f draw_map_count1.ncl

let nch=nch+1
done

let nvar=nvar+1
done


rm -f draw_map_count1.ncl
