#!/bin/sh

cd $DATA
set -x
date

if [ $# -ne 2 ] ; then
  echo "$0 sigma/bges-file PDYCYC"
  echo "returns date of file"
  exit 8
fi

infile=$1
compdate=$2

export pgm=cdas_sigmatime
. prep_step

export FORT11="$infile"
[ -f fort.11 ] && rm fort.11

startmsg
out=`$EXECcdas/cdas_sigmatime`
export err=$?;err_chk

[ `echo $out | cut -c1 | egrep -c '(1|2)'` -ne 1 ] && out="0000000000"

if [ "$out" -ne "$compdate" ] ; then
   echo "$infile file has wrong date"
   export err=10
   err_exit "$infile file has wrong date"
fi

