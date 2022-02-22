#!/bin/sh
#                                            11/2019 Wesley Ebisuzaki
#
# CDAS use to obtain SST from the oisst processing
# now it has to be obtained from the NSST product
#
# use a 7-day average of NSST to replace the OISST because OISST
# was a weekly average (computed every day).  Also reduces noise.
#
# INPUT:
# variable: COM_SST              location of nsst  ${COM_SST}.YYYYMMDD
#           COMOUT               output of sst
#           PDY                  YYYYMMDD
#           cyc                  00/06/12/18
#           DATA                 location for computations
#           sst                  name of output sst file
#
# OUTPUT:
#           $COMOUT/$sst       (grib1)
#           $COMOUT/$sst.grib2 (grib2)
#
# Parameters: INFILE  (name of grib2 sst file)
#
# routines used
#          $WGRIB2, $CNVGRIB, $NDATE
#
# modules used
#          grib_util, prod_util, prod_env
#
# Method: use wgrib2 to create a 7 day average (grib2)
#         use wgrib2 to interpolate to 360x180 grid (use water)
#         use wgrib2 to expand sst to nearby
#         use wgrib2 to describe ave as an analysis (so cnvgrib can handle)
#         use 
#
# returns 0 if ok
# returns 1 if failure
#
# v1.0 11/22/2019               Initial version Wesley Ebisuzaki
# 

# for testing
#export COM_SST=/gpfs/tp2/nco/ops/com/gfs/prod
#export PDY=20191123
#export cyc=00
#export COMOUT=$ptmp
#export DATA=$stmp

set -x
INFILE=rtgssthr_grb_0.083_awips.grib2
TMPFILE=$DATA/sst_tmp

# if file already exists, return
#if [ -f $COMOUT/$sst ] ; then
#   n=`$WGRIB $COMOUT/$sst -s | grep  -c ':TMP:sfc:'`
#   [ "$n" -eq 1 ] && exit 0
#fi

if [ -s ${COM_SST}.$PDY/$INFILE ] ; then
   list="-6 -5 -4 -3 -2 -1 0"
else
   list="-7 -6 -5 -4 -3 -2 -1"
fi

[ -f $TMPFILE ] && rm $TMPFILE

for i in $list
do
  echo $i
  hour=`expr $i \* 24`
  bdate=`$NDATE $hour ${PDY}00 | cut -c1-8`
  echo "bdate=$bdate"
  if [ -s ${COM_SST}.$bdate/$INFILE ] ; then
      $WGRIB2 -match ':TMP:surface:'  ${COM_SST}.$bdate/$INFILE -append -grib $TMPFILE 
  fi
done
n=`$WGRIB2 $TMPFILE | grep -c ":TMP:surface:"`

if [ "$n" -eq 0 ] ; then
  echo "FATAL ERROR - NO SST"
  exit 9
fi
if [ "$n" -ne 7 ] ; then
  echo "WARNING: only $n sst fields found"
fi

# average fields
$WGRIB2 $TMPFILE -ave 1dy ${TMPFILE}.ave

# change date code and make it an analysis
bdate=`$NDATE -24 ${PDY}00`
$WGRIB2 ${TMPFILE}.ave -set_date $bdate -set_ftime 'anl' -grib ${TMPFILE}.ave2

# change to 360x180 grid
$WGRIB2 ${TMPFILE}.ave2 -new_grid_winds earth -new_grid_interpolation budget \
   -new_grid latlon 0.5:360:1 89.5:180:-1 ${TMPFILE}.ave3

# the sst is undefined over land, mask may differ from model.
# to avoid points were SST in undefined in model, must expand SST.
#
# use wgrib2 trick #46 (modified for 4 grid points)
# use wgrib2 -rpn smth9g to expand SST by one grid at coast line
#  (do 4 times)

$WGRIB2 ${TMPFILE}.ave3 -set_scaling -2 0 -rpn smth9g -grib_out ${TMPFILE}.1 \
   -rpn smth9g -grib_out ${TMPFILE}.2 -rpn smth9g -grib_out ${TMPFILE}.3 \
   -rpn smth9g -grib_out ${TMPFILE}.4

# To avoid smoothing the places were the SST were initially defined,
# wgrib2 trick #46 used the original SST where available.
# since the OISST was smoother than the 1x1 degree resolution, use
# the 1st smoothed grid as the SST analysis over water.

$WGRIB2 ${TMPFILE}.1 -rpn sto_1 \
   -import_grib ${TMPFILE}.2 -rpn "rcl_1:merge:sto_1" \
   -import_grib ${TMPFILE}.3 -rpn "rcl_1:merge:sto_1" \
   -import_grib ${TMPFILE}.4 -rpn "rcl_1:merge:sto_1" \
   -set_scaling -2 0 -grib_out ${TMPFILE}.final

cp ${TMPFILE}.final $COMOUT/${sst}.grib2
$CNVGRIB -g21  ${TMPFILE}.final $COMOUT/${sst}

rm ${TMPFILE}.ave ${TMPFILE}.ave2 ${TMPFILE}.ave3 ${TMPFILE}.1 ${TMPFILE}.2 ${TMPFILE}.3 ${TMPFILE}.4 

exit 0
