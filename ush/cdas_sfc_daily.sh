#!/bin/sh
# sfcanl = gribfile
# field = grib name ex ICEC
# daily = output file
# dtg10 = yyyymmddhh
#
# searchs grib file for field that for latest analysis relative to dtg10

cd $DATA
set -x
date

if [ $# -ne 4 ] ; then
        echo "Usage: $0 sfcanl field daily dtg10"
        exit 8
fi

sfcanl=$1
field=$2
daily=$3
dtg10=$4

$WGRIB -s -4yr $sfcanl | grep $field | \
  awk 'BEGIN {date0=0; FS=":"} \
       {date=substr($3,3); if (date <= dtg10 && date > date0) \
            {date0=date; line=$0} }
       END {print line}' dtg10="$dtg10" | \
  $WGRIB -s -4yr $sfcanl -grib -i -o $daily

# check whether sfc anal is more than 7 days old
dtgsfc=`$WGRIB -s -4yr $daily | head -1 | cut -f3 -d: | cut -c3-`
nhours=$((7*24))
dtg7=`$NDATE $nhours $dtgsfc`
if [ $dtg7 -lt $dtg10 ] ; then
        err_exit "latest $sfcanl $sortdate -gt $nhours past $dtg10"
fi
