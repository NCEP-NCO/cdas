#!/bin/sh
# runs ssi

# input files: $prepqm $sges $oldsig
# output files: $sanl

# cd $DATA
echo "started ush/cdas_ssi.sh"
set -x
date

divterrs=$FIXcdas/cdas_divterrs2812645
satv28=$FIXcdas/cdas_v28newx
eofs12628=$FIXcdas/cdas_eofs28126

# convert sges, bges and oldsig to double precision

export pgm=cdas_sig2dbl; . prep_step
startmsg
$EXECcdas/cdas_sig2dbl $bges bges.dbl
export err=$?; err_chk
$EXECcdas/cdas_sig2dbl $sges sges.dbl
export err=$?; err_chk
$EXECcdas/cdas_sig2dbl $oldsig oldsig.dbl
export err=$?; err_chk

export pgm=cdas_ssi; . prep_step
# input
export FORT30="$prepqm"
export FORT35="sges.dbl"
export FORT36="oldsig.dbl"
export FORT37="bges.dbl"
export FORT47="$divterrs"
export FORT48="$satv28"
export FORT49="$eofs12628"

# output
export FORT51="sanl.dbl"

IGEN=2
startmsg
$EXECcdas/cdas_ssi <<EOF >> $pgmout 2> errfile
&NAMANAL igen=$IGEN,JCAP=62,NLATH=48,NLON=192,NSIG=28,niter=100,miter=1,
   a=.25,.33,.42,.45,ampdivt=.7,dampdivt=.8,grosst=10.,grossst=10.,
    grossw=10.,grossp=10.,grossq=5.,grosspw=10.,
/
EOF
export err=$?; err_chk

# convert sigma analysis to single precision
export pgm=cdas_sig2sngl; . prep_step
startmsg
$EXECcdas/cdas_sig2sngl sanl.dbl $sanl
export err=$?; err_chk

# clean up
rm bges.dbl sges.dbl oldsig.dbl sanl.dbl

