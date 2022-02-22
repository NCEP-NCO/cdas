/* version 2 of grib headers  w. ebisuzaki */
/* this version is incomplete */


#define PDSLen1		pds[0]
#define PDSLen2		pds[1]
#define PDSLen3		pds[2]
#define PDSLEN		(pds[0]*256*256+pds[1]*256+pds[2])
#define PDSVsn		pds[3]
#define Center		pds[4]
#define Model		pds[5]
#define Grid		pds[6]
#define GDS_BMS		pds[7]
#define PARAM		pds[8]
#define L_TYPE		pds[9]
#define LEVEL1		pds[10]
#define LEVEL2		pds[11]

#define KPDS5		pds[8]
#define KPDS6		pds[9]
#define KPDS7		(pds[10]*256 + pds[11])

#define Year		pds[12]
#define Month		pds[13]
#define Day		pds[14]
#define Hour		pds[15]
#define Minute		pds[16]
#define ForecastTimeUnit	pds[17]
#define P1		pds[18]
#define P2		pds[19]
#define TimeRange	pds[20]
#define Century		pds[24]
#define Subcenter	pds[25]
