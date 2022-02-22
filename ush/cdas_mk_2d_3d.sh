#!/bin/sh
#                                          w. ebisuzaki
# the flux and diag files have fields in common
# in addition, some flux-type fields are in the diag file
# where they logically belong in the flux file
#
# this script (1) eliminates common fields, (2) moves the
# 2-d fields to a new flux file, and the 3-d fields to a new diag file
#
# this script only works on grib
#
# note:  the fields in the 3-D file are hardcoded
#
set -u

if [ $# -ne 5 ] ; then
   echo "$0 [old_flx] [old_diag] [new_flx] [new_diag] [tmp_dir]"
   echo
   echo "  file names must be either fully qualified or relative to [tmp_dir]"
   echo
   echo "  tmp_dir is a directory where all the work of mixing and "
   echo "  merging will be done.  this directory must be empty before"
   echo "  calling and will be cleaned out after finishing"
   err_exit "Incorrect number of arguments passed to $0"
fi

cd $DATA
set -x
date
old_flx=$1
old_diag=$2
new_flx=$3
new_diag=$4
tmp_dir=$5

# cd and check for empty directory
cd $tmp_dir
if [ "`ls`" != "" ] ; then
   echo "not an empty directory: $tmp_dir"
   exit 8
fi

# sh $utilscript/setup.sh

# expand diagnostic file
export pgm=cdas_ex_grib2;. prep_step
startmsg
echo $old_flx | $EXECcdas/cdas_ex_grib2 -def
status=$?
export err=0
[ $status -gt 4 ] && export err=$status
[ $status -eq 1 ] && echo "missing kpds5 defn"
err_chk

# need to rename CDCON to keep from being overwritten if it exists

[ -s CDCON.good ] && rm CDCON.good
[ -s CDCON ] && mv CDCON CDCON.good

# need to save an TMPhag.2 (2 m temp)

# $utilexec/grbindex TMP index
# /wd2/ln/map/GRTRAN F TMP:index
# rm index TMP TMPsfc
# rm namlpre.data print.out stderr

mv TMP tmp
$USHcdas/cdas_grtransm.sh tmp
rm tmp TMPsfc

# expand diag file
# allow diag file to overwrite flux exanpansion except for 2 m TMP

echo $old_diag | $EXECcdas/cdas_ex_grib2 -def
status=$?
[ $status -gt 4 ] && exit $status
[ $status -eq 1 ] && echo "missing kpds5 defn"

[ -f $new_flx ] && rm $new_flx
[ -f $new_diag ] && rm $new_diag

# remove CDCON if found in diag file
[ -s CDCON ] && rm CDCON

diag="CNVHR CNVMR LRGHR LWHR SHAHR SHAMR SWHR VDFHR VDFMR VDFUA VDFVA"

cat $diag >$new_diag
[ $? -ne 0 ] && err_exit "Problem creating $new_diag"

rm $diag
[ $? -ne 0 ] && err_exit "Problem removing $new_diag input files"

cat * >$new_flx
[ $? -ne 0 ] && err_exit "Problem creating $new_flx"

cd $DATA
rm -rf $tmp_dir

exit 0
