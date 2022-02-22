#!/bin/sh
# a grtrans clone                  w.ebisuzaki
# v1.1
# different calling sequence
#
# v1.2 9/2017 added export LANG=C LC_COLLATE=C
#      so that ls keeps that same sorted order as on IBM
#
# 2 step process
#  (A) synoptic -> time series by variable type (single process)
#  (B) variable-type time series -> time series by variable and level
#               (up to NMAX processes at once)
#
# NMAX=max number of process at one time
#
# time is limited by step (1) which is comparable to windex1
# when run sequentially
#
# note: this script is much faster than GRTRANS because
#   (1) avoids unneeded file opening/closing
#   (2) step B (above) is multi-tasked
#   (3) code is written in C and has been optimized to
#         minimize i/o
#   (4) code is simplier then GRTRANS
#
# note: does not use index files
#
# bugs:
#  the file name of one sigma level is different than in GRTRANS
#    different rounding/truncation (?)
#  the algorithm used to generate the file names is modelled after
#    GRTRANS, and may not generate unique name for certain perverse
#    layers (a problem with GRTRANS naming convention)
#
set -u

if [ $# -ne 1 ] ; then
    echo "usage: $0 'files-wild-card'"
    err_exit "Incorrect number of arguments passed to $0"
fi

set -x
date

# NMAX=9
# NMAX=8
NMAX=4

filelist=.list.$$

# decompose the list of files by variable
# list of variables is in $filelist
# 9/2017 added export LANG=C, export LC_COLLATE=C

export LANG=C
export LC_COLLATE=C

ls $1 >$filelist.list

export pgm=cdas_ex_grib2;. prep_step
startmsg
$EXECcdas/cdas_ex_grib2 -def <$filelist.list >$filelist
export err=$?; err_chk

if [ ! -s $filelist -eq 0 ] ; then
   echo "no files decoded!!!"
   err_exit "$pgm no files decoded!!!"
fi

# for each variable, decompose by level/layer number
# the number of levels/layers should be less than max
# number of open files

n=0
for f in `cat $filelist`
do
   echo "processing $f"
   echo "$f" >$filelist.$n
   $EXECcdas/cdas_ex_level $FIXcdas/cdas_grib1.kpds6.vsn21 <$filelist.$n >/dev/null &
   n=`expr $n + 1`
   if [ $n -eq $NMAX ] ; then
      n=0
      wait
   fi
done
wait
rm `cat $filelist`
rm $filelist*

exit 0
