#!/bin/sh
#
# excdas.sh.ecf
#
# This is the main driver script for the CDAS model.
# It runs the file prep, analysis (ssi), forecast (mrf),
# and post processing of the model.
#
# At 00z run a prediction to 384 hours
#

#####################################################################
echo
echo "------------------------------------------------"
echo "excdas.sh.ecf - CDAS analysis and forcast"
echo "------------------------------------------------"
echo "History: Jul 20 2000 - Original script."
echo "       : Jul 10 2006 - added fcst to 384 hours"
echo
#####################################################################

set -x
export OMP_NUM_THREADS=1
date
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$msg"

echo "step ############# break ##############################" > $pgmout

msg="CYCLE TIME FOR CDAS ANALYSIS IS $PDY$cyc"
postmsg "$msg"

# clean_unit .. removes old fortran unit number connections
clean_unit() {
  touch fort.1
  rm fort.*
  export FORT1="abc"
  unset `set | grep '^FORT' | sed 's/=.*//'`
}

fatal() {
  echo "fatal error $*"
  exit 8
#  export err=8
#  sh err_chk
}


#                                    CYCLE files

# input files
export snow=cdas.t${cyc}z.snogrb
export sst=cdas.t${cyc}z.sstgrb
export ice=cdas.t${cyc}z.engicegrb

#phase-3 make $snow
$USHcdas/cdas_get_afwa_snow.sh
cp $COMOUT/$snow ./$snow || fatal "missing snow: $COMOUT/$snow"

#phase-3 make $sst
$USHcdas/cdas_get_sst.sh
cp $COMOUT/$sst ./$sst || fatal "missing sst: $COMOUT/$sst"

# Added by XXW
$CNVGRIB -g12 -p40 $sst $sst.grib2
cp $sst.grib2 $COMOUT/
######end add

$USHcdas/cdas_get_seaice.sh
#cp $COMOUT/$ice ./$ice || fatal "missing ice: $COMOUT/$ice"
cp $COMINobsproc/$ice ./$ice || fatal "missing ice: $COMOUT/$ice"

# output file
export sfcanl=cdas.t${cyc}z.sfcanl

#                                    SSI files

# input files
export sges=cdas.t${cycm6hr}z.sf06
export bges=cdas.t${cycm6hr}z.bf06
export oldsig=cdas.t${cycm6hr}z.sanl
cp $COMINm6hr/$sges $sges || fatal "missing sges: $COMINm6hr/$sges"
cp $COMINm6hr/$bges $bges || fatal "missing bges: $COMINm6hr/$bges"
cp $COMINm6hr/$oldsig $oldsig || fatal "missing oldsig: $COMINm6hr/$oldsig"

export prepqm=cdas.t${cyc}z.prepbufr
cp $COMINobsproc/$prepqm $prepqm || fatal "missing prepqm: $COMIN/$prepqm"

# output files
export sanl=cdas.t${cyc}z.sanl
#                                    MODEL files

# output files
export newsges=cdas.t${cyc}z.sf06
export newbges=cdas.t${cyc}z.bf06
export newflx=cdas.t${cyc}z.sfluxgrbf06
export newdg3=cdas.t${cyc}z.dg3f06
export newznl00=cdas.t${cyc}z.znlf00
export newznl06=cdas.t${cyc}z.znlf06
export pgbf00=cdas.t${cyc}z.pgrbf00
export pgbf06=cdas.t${cyc}z.pgrbf06
if [ $cyc = 00 ] ; then
	fhini=0;fhout=12;fhmax=384
	fhr=$fhini; while [ $((fhr+=$fhout)) -le $fhmax ] ;do 
		eval export  sigf$fhr=sigft.$PDY${cyc}ft$fhr
		eval export  pgbf$fhr=cdas.t${cyc}z.pgrbf$fhr
		eval export  flxf$fhr=flx.$PDY${cyc}ft$fhr
	done
fi

########################## CYCLE ##########################

# check bges for right date code
$USHcdas/cdas_sigmatime.sh $bges $PDY$cyc

# run cycle
$USHcdas/cdas_cycle.sh $bges $sfcanl $ice $snow $sst $PDY$cyc
export err=$?; err_chk
ls -l $sfcanl
########################### SSI ###########################

# check files for right date code

$USHcdas/cdas_sigmatime.sh $sges $PDY$cyc

$USHcdas/cdas_sigmatime.sh $oldsig $PDYm6hr$cycm6hr

clean_unit

export pgm=cdas_prepdate; . prep_step
export FORT11="$prepqm"
export FORT51=prepdate.out
startmsg
$EXECcdas/cdas_prepdate >> $pgmout 2> errfile
export err=$?; err_chk
cat errfile >> $pgmout
chkdate=`cat prepdate.out`

if [ "$chkdate" -ne "$PDY$cyc" ] ; then
   echo "bufr file has wrong date"
   export err=10; exit 8
fi

startmsg
$EXECcdas/cdas_prepdate >> $pgmout 2> errfile
export err=$?; err_chk
cat errfile >> $pgmout
chkdate=`cat prepdate.out`
if [ "$chkdate" -ne "$PDY$cyc" ] ; then
   msg="date codes mismatch"
   postmsg "$msg" 
   export err=$?; err_chk
fi
$USHcdas/cdas_ssi.sh
export err=$?; err_chk

set -x
#cp SSI output to COM
cp $sanl $COMOUT/$sanl
cp $sfcanl $COMOUT/$sfcanl

########################### run model ###########################

endhour=6
$USHcdas/cdas_model.sh $sanl $sfcanl $endhour
export err=$?; err_chk

# save output in COMOUT
set -x
cp sigft.$PDY${cyc}ft6 $COMOUT/$newsges
cp sigft.$PDY${cyc}ft6 $newsges
cp bges.$PDY${cyc}ft6 $COMOUT/$newbges
cp bges.$PDY${cyc}ft6 $newbges
cp dg3.$PDY${cyc}ft6 $COMOUT/$newdg3
cp dg3.$PDY${cyc}ft6 $newdg3
cp flx.$PDY${cyc}ft6 $COMOUT/$newflx
cp flx.$PDY${cyc}ft6 $newflx
cp znl.$PDY${cyc}ift0 $COMOUT/$newznl00
cp znl.$PDY${cyc}ift0 $newznl00
cp znl.$PDY${cyc}ft6 $COMOUT/$newznl06
cp znl.$PDY${cyc}ft6 $newznl06

$USHcdas/cdas_pgb.sh $sanl $pgbf00
cp $pgbf00 $COMOUT/$pgbf00
$USHcdas/cdas_pgb.sh $newsges $pgbf06
cp $pgbf06 $COMOUT/$pgbf06

# Convert pgrb and sfluxgrb to grib2
$CNVGRIB -g12 -p40 $newflx ${newflx}.grib2
$WGRIB2 -s ${newflx}.grib2 > ${newflx}.grib2.idx
$CNVGRIB -g12 -p40 $pgbf00 ${pgbf00}.grib2
$WGRIB2 -s ${pgbf00}.grib2 > ${pgbf00}.grib2.idx

# Copy GRIB2 to COMOUT
cp ${newflx}.grib2 $COMOUT/${newflx}.grib2
cp ${pgbf00}.grib2 $COMOUT/${pgbf00}.grib2
cp ${newflx}.grib2.idx $COMOUT/${newflx}.grib2.idx
cp ${pgbf00}.grib2.idx $COMOUT/${pgbf00}.grib2.idx

if [ "$SENDDBN" = "YES" ] ; then
   $DBNROOT/bin/dbn_alert MODEL CDAS_SFLUX $job $COMOUT/$newflx
   $DBNROOT/bin/dbn_alert MODEL CDAS_PGRB $job $COMOUT/$pgbf00
   $DBNROOT/bin/dbn_alert MODEL CDAS_SANL $job $COMOUT/$sanl
   $DBNROOT/bin/dbn_alert MODEL CDAS_SFCANL $job $COMOUT/$sfcanl
   $DBNROOT/bin/dbn_alert MODEL CDAS_SSTGRB $job $COMOUT/$sst.grib2
   $DBNROOT/bin/dbn_alert MODEL CDAS_SFLUX_GB2 $job $COMOUT/${newflx}.grib2
   $DBNROOT/bin/dbn_alert MODEL CDAS_PGRB_GB2 $job $COMOUT/${pgbf00}.grib2
fi

if [ $cyc = 00 ] ; then

	$USHcdas/cdas_model.sh $sanl $sfcanl $fhmax $fhout
	fhr=$fhini; while [ $((fhr+=$fhout)) -le $fhmax ] ;do 
		eval $USHcdas/cdas_pgb.sh \$sigf$fhr \$pgbf$fhr
	done
fi

################### save data in monthly directory ###############

tdir=tmp$PDY$cyc
[ -d "$tdir" ] && rm -rf "$tdir"
mkdir "$tdir"
[ $? -ne 0 ] && err_exit "Unable to make temp directory for mk_2d_3d.sh"
$USHcdas/cdas_mk_2d_3d.sh $DATA/$newflx $DATA/$newdg3 $DATA/grb2d$PDY$cyc \
  $DATA/grb3d$PDY$cyc $tdir
echo "finished mk_2d_3d"

# fix grb2d
$WGRIB grb2d$PDY$cyc -s | grep -v ALBDO | \
  $WGRIB grb2d$PDY$cyc -i -grib \
  -o grb2d$PDY$cyc.tmp -s >/dev/null

$CNVGRIB -g12 -p40 grb2d$PDY$cyc.tmp grb2d$PDY$cyc.grib2
mv grb2d$PDY$cyc.tmp $COMarkv/grb2d$PDY$cyc
mv grb3d$PDY$cyc $COMarkv/grb3d$PDY$cyc

mv $pgbf00 $COMarkv/pgb.f00$PDY$cyc
mv $pgbf06 $COMarkv/pgb.f06$PDY$cyc

if [ $cyc = 00 ] ; then
	fhr=$fhini; while [ $((fhr+=$fhout)) -le $fhmax ] ;do 
		# remove 600mb records in the $COMarkv pgb copy 
        eval pgb=\$pgbf$fhr
		$WGRIB $pgb|grep -v kpds7=600|$WGRIB -i -grib -o $COMarkv/pgb.f$fhr$PDY$cyc $pgb
		rm $pgb
		eval mv  \$flxf$fhr $COMarkv/flx.f$fhr$PDY$cyc
	done
fi

clean_unit
export pgm=cdas_sgb;. prep_step
export FORT11="$sanl"
export FORT51="grbsanl$PDY$cyc"
startmsg
$EXECcdas/cdas_sgb
export err=$?; err_chk
mv grbsanl$PDY$cyc $COMarkv/grbsanl$PDY$cyc

clean_unit
export pgm=cdas_sgb;. prep_step
export FORT11="$newsges"
export FORT51="grbsf06$PDY$cyc"
startmsg
$EXECcdas/cdas_sgb
export err=$?; err_chk
mv grbsf06$PDY$cyc $COMarkv/grbsf06$PDY$cyc

clean_unit
export pgm=cdas_igb;. prep_step
# grib process number
IGEN=180
export FORT11="$sanl"
export FORT51="ipvanl$PDY$cyc"
startmsg
$EXECcdas/cdas_igb <<EOF
&NAMIGB ICEN=7,ICEN2=1,IGEN=$IGEN,
/
EOF
export err=$?; err_chk

$CNVGRIB -g12 -p40 ipvanl$PDY$cyc ipvanl$PDY$cyc.grib2

cp ipvanl$PDY$cyc $COMarkv/ipvanl$PDY$cyc

# cp $prepqm $COMarkv/prepqm$PDY$cyc
cp $sanl $COMarkv/sanl$PDY$cyc

cp $sfcanl $COMarkv/sfcanl$PDY$cyc

cp $newsges $COMarkv/sges$PDYp6hr$cycp6hr
cp $newbges $COMarkv/bges$PDYp6hr$cycp6hr
cp $newznl00 $COMarkv/znl.f00$PDY$cyc
cp $newznl06 $COMarkv/znl.f06$PDY$cyc

# cycle files: save sst, snow and sea-ice and perhaps bges

cp $snow $COMarkv/snogrb$PDY$cyc
cp $sst  $COMarkv/sstgrb$PDY$cyc
cp $ice  $COMarkv/icegrb$PDY$cyc
[ ! -s $COMarkv/bges$PDY$cyc ] && cp $bges $COMarkv/bges$PDY$cyc

# SSI files

[ ! -s $COMarkv/prepqm$PDY$cyc ] && cp -p $prepqm $COMarkv/prepqm$PDY$cyc
[ ! -s $COMarkv/sges$PDY$cyc ] && cp $sges $COMarkv/sges$PDY$cyc

# new v1.1.0 bufr files
cp prepbufer$PDY${cyc} $COMarkv/
cp *$PDY${cyc}.out $COMarkv/
cp tosscat$PDY$cyc $COMarkv/
cp cpc_*$PDY$cyc $COMarkv/
cp *bufr_d $COMarkv/

if [ $SENDCOM = YES ]; then
  cp grb2d$PDY$cyc.grib2 $COMOUT/.
  cp ipvanl$PDY$cyc.grib2 $COMOUT/.
  if [ $SENDDBN = YES ]; then
    $DBNROOT/bin/dbn_alert MODEL CDAS_DGRB2 $job $COMOUT/grb2d$PDY$cyc.grib2
    $DBNROOT/bin/dbn_alert MODEL CDAS_IPVANL $job $COMOUT/ipvanl$PDY$cyc.grib2
  fi
fi

########################################################

# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x


# save standard output
cat break $pgmout break > allout
cat allout
# rm allout

# sleep 10

msg='ENDED NORMALLY.'
postmsg "$msg"

################## END OF SCRIPT #######################

