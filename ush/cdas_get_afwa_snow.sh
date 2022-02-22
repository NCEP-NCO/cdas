#!/bin/sh
#                                            11/2019 Wesley Ebisuzaki
#
# snow cover is obtained from AFWA (old: AF Weather Agency - new: 577th Weather Squadron)
#
#  with conversion to phase-3, obsproc is not making snow cover (based on AFWA snow depth)
#
#
# INPUT:
# variable: DCOM_SNOW        ($DCOMROOT/prod)  source of AFWA data
#           COMOUT               output of snow covert
#           PDY                  YYYYMMDD
#           cyc                  00/06/12/18
#           DATA                 location for computations
#           EXECcdas             location of executables
#           snow                 name of output file
#
# file:     $DCOM_SNOW/YYYYMMDD/wgrbbul/NPR.SNWN.SP.S1200.MESH16
# file:     $DCOM_SNOW/YYYYMMDD/wgrbbul/NPR.SNWS.SP.S1200.MESH16
#
# OUTPUT:
#           $COMOUT/$snow
#
# routines used
#          $COPYGB, $WGRIB, $NDATE, cdas_fix_afwa_snow, cdas_snowd2snowc
#
# modules used
#          grib_util, prod_util, prod_env
#
# returns 0 if ok
# returns 1 if failure
#
# Note: tried converting data to afwa to 720x361 grid using copygb
#          problem on the equator
#       solution: convert to grid without grid points on equator
#          then convert to 720x361 grid
#
# based on  rcdas_get_afwa_snow.sh
# v1.0 11/19/2019               Initial version Wesley Ebisuzaki
# 

# for testing
#export DCOM_SNOW=/gpfs/dell1/nco/ops/dcom/prod
#export PDY=20191116
#export cyc=06
#export COMOUT=$ptmp
#export DATA=$stmp
#export EXECcdas=../exec
#export snow=snogrb

set -x
set -e

if [ "$NDATE" = '' -o "$WGRIB" = '' ] ; then
   echo "modules not loaded"
   exit 1
fi

# if file already exists, return
if [ -f $COMOUT/$snow ] ; then
   n=`$WGRIB $COMOUT/$snow -s | grep -c ":SNOD:"`
   [ "$n" -eq 1 ] && exit 0
fi

for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

do
   hour=`expr $i \* 6`
   date=`$NDATE -$hour ${PDY}${cyc} | cut -c1-8`
   nh=$DCOM_SNOW/$date/wgrbbul/NPR.SNWN.SP.S1200.MESH16
   sh=$DCOM_SNOW/$date/wgrbbul/NPR.SNWS.SP.S1200.MESH16
   [ ! -f $nh ] && continue
   [ ! -f $sh ] && continue

   # check the date code
   snow_date=`$WGRIB -4yr $nh | head -n 1 | cut -f3 -d: | cut -c3-`
   [ "$snow_date" = "" ] && continue
   if [ "$snow_date" -gt "$PDY$cyc" ] ; then
      echo "snow_date ($snow_date) is too late"
      continue
   fi

   # copy snow to $DATA and fix it
   cp $nh $DATA/nh_snow.grb.tmp
   chmod 644 $DATA/nh_snow.grb.tmp
   $EXECcdas/cdas_fix_afwa_snow $DATA/nh_snow.grb.tmp
   cp $sh $DATA/sh_snow.grb.tmp
   chmod 644 $DATA/sh_snow.grb.tmp
   $EXECcdas/cdas_fix_afwa_snow $DATA/sh_snow.grb.tmp

   # remove all but snow depth
   $WGRIB $DATA/nh_snow.grb.tmp | grep ':SNOD:' | $WGRIB -i $DATA/nh_snow.grb.tmp -grib -o $DATA/nh_snow.grb
   $WGRIB $DATA/sh_snow.grb.tmp | grep ':SNOD:' | $WGRIB -i $DATA/sh_snow.grb.tmp -grib -o $DATA/sh_snow.grb
   rm $DATA/nh_snow.grb.tmp $DATA/sh_snow.grb.tmp

   # merge the files together into a T574 grid
   $COPYGB -g129 -M $DATA/sh_snow.grb -x $DATA/nh_snow.grb $DATA/snowdepth.${PDY}${cyc}.T574

   # check file for results
   [ ! -f $DATA/snowdepth.${PDY}${cyc}.T574 ] && continue
   n=`$WGRIB $DATA/snowdepth.${PDY}${cyc}.T574 | grep -c ':SNOD:'`
   [ "$n" -ne 1 ] && continue

   # convert to 720x361 grid
   $COPYGB -g4 -x $DATA/snowdepth.${PDY}${cyc}.T574 $DATA/snowdepth.${PDY}${cyc}

   # convert snowdepth to snow cover 0..1
   $EXECcdas/cdas_snowd2snowc $DATA/snowdepth.${PDY}${cyc} $DATA/snowcover.${PDY}${cyc}

   cat $DATA/snowdepth.${PDY}${cyc} $DATA/snowcover.${PDY}${cyc} > $COMOUT/$snow
#   $CNVGRIB -g12 -p40 $COMOUT/$snow $COMOUT/${snow}.grib2
   exit 0

done

# failed
exit 1
