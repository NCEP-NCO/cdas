#!/bin/sh
#
# args sanl sfcanl endhour [inchour]
#


if [ $# -lt 3 -o $# -gt 4 ] ; then
  echo "runs model: $0 sanl sfcanl endhour [inchour]"
  exit 6
fi

cd $DATA
set -x
date

sanl=$1
sfcanl=$2
endhour=$3

inchour=6
[ $# -eq 4 ] && inchour=$4

#Get Date from sigma analysis file
export pgm=cdas_sigmatime
. prep_step
export FORT11="$sanl"
startmsg
sigdate=`$EXECcdas/cdas_sigmatime`
export err=$?;err_chk


# convert to double precision
$EXECcdas/cdas_sig2dbl $sanl sanl.dbl
$EXECcdas/cdas_sig2dbl $sfcanl sfcanl.dbl

hh=0
while [ $hh -lt $endhour ]
do
  hhp=`expr $hh + $inchour`

  export pgm=cdas_mrf;  . prep_step

  # input files
  if [ $hh -eq 0 ] ; then
    export FORT11="sanl.dbl"
    export FORT12="/dev/null"
    export FORT14="sfcanl.dbl"
  else
    export FORT11="sigft.${sigdate}ft$hh.dbl"
    export FORT12="sigdt.${sigdate}ft$hh.dbl"
    export FORT14="bges.${sigdate}ft$hh.dbl"
  fi

  export FORT15="$FIXcdas/cdas_co2const.dbl"
  export FORT21="heatrate"
  export FORT24="$FIXcdas/cdas_mtnvar.6218.dbl"
  export FORT43="$FIXcdas/cdas_tune1.season.dbl"

  # output files

  export FORT51="sigft.${sigdate}ft$hhp.dbl"
  export FORT52="sigdt.${sigdate}ft$hhp.dbl"
  export FORT53="bges.${sigdate}ft$hhp.dbl"
  export FORT61="znl.${sigdate}ift$hh"
  export FORT62="flx.${sigdate}ift$hh"
  export FORT63="flx.${sigdate}ft$hhp"
  export FORT64="znl.${sigdate}ft$hhp"
  export FORT65="dg3.${sigdate}ift$hh"
  export FORT66="dg3.${sigdate}ft$hhp"
  export FORT67="ken.${sigdate}ft$hhp"
  export FORT68="flxs.${sigdate}ft$hhp"
  export FORT69="znls.ft${sigdate}$hhp"
  export FORT81="diabanl"
  export FORT82="adiages"
  export FORT83="fullges"
  export FORT92="diagscr"
  export FORT98="radscr"
  export FORT99="w3out"
  touch heatrate

  startmsg
  $EXECcdas/cdas_mrf <<EOF >> $pgmout 2> errfile
 &NAMSMF ICEN2=1,NUM(5)=0,CON(7)=${inchour}.,CON(8)=3.,CON(1)=1200., /
EOF
  export err=$?; err_chk

  echo $hh
  hh=`expr $hh + $inchour`
done

#rm sfcanl.dbl sanl.dbl

if [ $endhour -eq 6 ] ; then
	flist="sigft.${sigdate}ft*.dbl sigdt.${sigdate}ft*.dbl bges.${sigdate}ft*.dbl"
else
	flist="sigft.${sigdate}ft*.dbl"
fi
for f in $flist 
do
   ff=`echo $f | sed 's/\.dbl//'`
   $EXECcdas/cdas_sig2sngl $f $ff
#   rm $f
done
