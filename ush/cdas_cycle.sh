#!/bin/sh
#
# get snow, sst, sea-ice files from databases
#  mod 12/99 for nesdis snowc (change percentage to fractions) wne
#
# sets up files and namelists for cycle program
# and finally runs cycle
#
# for cdas only
#
# input files: $snow, $ice, $sst, $bges
# output file: $sfcanl
#
# $PDY, $cyc
#

cd $DATA
set -x
date

if [ $# -ne 6 ] ; then
  echo "$0 bges sfcanl ice snow sst yyyymmddhh"
  echo "runs cycle"
  export err=8 ; err_check
  exit 8
fi
bges=$1
sfcanl=$2
ice=$3
snow=$4
sst=$5
date=$6

yyyy=`echo $date | cut -c1-4`
mm=`echo $date | cut -c5-6`
dd=`echo $date | cut -c7-8`
hh=`echo $date | cut -c9-10`

export FNMASK=${FIXcdas}/cdas_slm.t62
export FIMASK=${FIXcdas}/cdas_slm.t62.index
export FNOROG=${FIXcdas}/cdas_oro.t62
export FIOROG=${FIXcdas}/cdas_oro.t62.index
export FNALBC=${FIXcdas}/cdas_matthews.albclim
export FIALBC=${FIXcdas}/cdas_matthews.albclim.index
export FNPLRC=${FIXcdas}/cdas_sibresis.one
export FIPLRC=${FIXcdas}/cdas_sibresis.one.index
export FNGLAC=${FIXcdas}/cdas_glacier
export FIGLAC=${FIXcdas}/cdas_glacier.index

export FNMXIC=${FIXcdas}/cdas_maxice
export FIMXIC=${FIXcdas}/cdas_maxice.index

export FNTSFC=${FIXcdas}/cdas_sst
export FITSFC=${FIXcdas}/cdas_sst.index
export FNWETC=${FIXcdas}/cdas_soilwet
export FIWETC=${FIXcdas}/cdas_soilwet.index
export FNSNOC=${FIXcdas}/cdas_snow
export FISNOC=${FIXcdas}/cdas_snow.index
export FNZORC=${FIXcdas}/cdas_sibrough
export FIZORC=${FIXcdas}/cdas_sibrough.index
export FNAISC=${FIXcdas}/cdas_ice
export FIAISC=${FIXcdas}/cdas_ice.index
export FNTG3C=${FIXcdas}/cdas_tg3
export FITG3C=${FIXcdas}/cdas_tg3.index

##################  *** SNOW ***  #####################

daily=snogrb
$USHcdas/cdas_sfc_daily.sh $snow SNOWC $daily $PDY$cyc
export err=$?; export pgm=cdas_sfc_daily.sh; err_chk

#
# convert % (0..100) snowc to fraction (0..1)
#
export pgm=cdas_fix_snowc; . prep_step
startmsg
${EXECcdas}/cdas_fix_snowc $daily $daily.tmp
export err=$?; err_chk

mv $daily.tmp $daily
$GRBINDEX $daily $daily.index


##################  *** SST ***  #####################

daily=sstgrb
$USHcdas/cdas_sfc_daily.sh $sst TMP $daily $PDY$cyc
export err=$?; export pgm=cdas_sfc_daily.sh; err_chk

$GRBINDEX $daily $daily.index


##################  *** SEA-ICE ***  #####################
daily=icegrb
$USHcdas/cdas_sfc_daily.sh $ice ICEC $daily $PDY$cyc
export err=$?; export pgm=cdas_sfc_daily.sh; err_chk

$GRBINDEX $daily $daily.index

daily=sstgrb


# convert bges to double precision
export pgm=cdas_sig2dbl; . prep_step
startmsg
$EXECcdas/cdas_sig2dbl $bges bges.$PDY$cyc.dbl
export err=$?; err_chk


#		*** run CYCLE ***

cat >namtest <<EOF
&NAMTEST
 IDIM=192,JDIM=94,LSOIL=2,
 LUGI=1,LUGB=2,
 IY=$yyyy,IM=$mm,ID=$dd,IH=$hh,
 FH=0.,
/
EOF

cat >namsfc <<EOF
&NAMSFC
 FNBGSI='bges.$PDY$cyc.dbl',
 FNBGSO='sfcanl.$PDY$cyc.dbl',
 FNOROG='$FNOROG',
 FIOROG='$FIOROG',
 FNMASK='$FNMASK',
 FIMASK='$FIMASK',
 FNACNA='icegrb',
 FIACNA='icegrb.index',
 FNSCVA='snogrb'
 FISCVA='snogrb.index'
 FNTSFA='sstgrb',
 FITSFA='sstgrb.index',
 FNPLRC='$FNPLRC',
 FIPLRC='$FIPLRC',
 FNALBC='$FNALBC',
 FIALBC='$FIALBC',
 FNGLAC='$FNGLAC',
 FIGLAC='$FIGLAC',
 FNMXIC='$FNMXIC',
 FIMXIC='$FIMXIC',
 FNTSFC='$FNTSFC',
 FITSFC='$FITSFC',
 FNWETC='$FNWETC',
 FIWETC='$FIWETC',
 FNSNOC='$FNSNOC',
 FISNOC='$FISNOC',
 FNZORC='$FNZORC',
 FIZORC='$FIZORC',
 FNAISC='$FNAISC',
 FIAISC='$FIAISC',
 FNTG3C='$FNTG3C',
 FITG3C='$FITG3C',
/
EOF

export pgm=cdas_cycle; . prep_step
startmsg
cat namtest namsfc | $EXECcdas/cdas_cycle >> $pgmout 2> errfile
export err=$?; err_chk

# get results in single precision
export pgm=cdas_sig2sngl; . prep_step
startmsg
${EXECcdas}/cdas_sig2sngl sfcanl.$PDY$cyc.dbl $sfcanl
export err=$?; err_chk

rm sfcanl.$PDY$cyc.dbl bges.$PDY$cyc.dbl


