#!/bin/sh
if [ $# -ne 2 ] ; then
  echo "$0 sanl pgb"
  echo "  converts sigma files to pgb file"
  exit 8
fi

cd $DATA
set -x
date

sanl=$1
pgb=$2

# grib process number
IGEN=180

# PGBPARM=pgb.parm
# cat >$PGBPARM <<EOF
# &NAMPGB IO=144,JO=73,KO=16,NCPUS=1,ICEN2=1,IGEN=$IGEN,
# KO=17,PO=1000.,925.,850.,700.,600.,500.,
#                   400.,300.,250.,200.,150.,100.,
#                        70., 50., 30., 20., 10., 
# /
# EOF

export pgm=cdas_pgb; . prep_step
export FORT11="$sanl"
rm fort.51
ln -s $pgb fort.51
touch fort.51

startmsg
$EXECcdas/cdas_pgb <<EOF >> $pgmout 2> errfile
 &NAMPGB IO=144,JO=73,KO=16,NCPUS=1,ICEN2=1,IGEN=$IGEN,
 KO=17,PO=1000.,925.,850.,700.,600.,500.,
                   400.,300.,250.,200.,150.,100.,
                        70., 50., 30., 20., 10., 
 /
EOF
export err=$?; err_chk

echo "converted $sanl -> $pgb"
exit 0

