#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "gribwlib.h"
#include "ncepopn.h"

#define NXNY		720*360
#define LOCAL_UNDEF	-999.0

/*
 * gribify an 720x360 evap field
 *  
 * uses PDStool()
 */

int days_in_month[12] = {31,28,31,30,31,30,31,31,30,31,30,31};


int main(int argc, char **argv) {

    unsigned char *pds, *pds_debug, *gds;
    FILE *input, *output;
    int count = 0, yyyymm, i, header, ndays;
    float data[NXNY];

    /* preliminaries .. open up all files */

    if (argc != 4) {
	fprintf(stderr, "%s [in bin-file] [out gribfile] [YYYYMM]\n", argv[0]);
	exit(8);
    }
    if ((input = fopen(argv[1],"rb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[1]);
        exit(7);
    }
    if ((output = fopen(argv[2],"wb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[2]);
        exit(7);
    }
    yyyymm = atoi(argv[3]);
    printf("initial YYYYMM is %d\n", yyyymm);

    /* generate a PDS */

    pds = PDStool(New_PDS, 		/* new PDS */
	NCEP_opn, 			/* center, subcenter */
        P_param_table(2), 		/* parameter table */
	P_process(0),			/* process that made data */

	P_param(ULWRF), P_sfc,		/* variable and level */
	P_date(yyyymm*100+1),		/* initial date yyyymmdd */
	P_hour(0),

	P_ave_mon(0,1),			/* averaged from month 0 to 1 */

	P_dec_scale(1),			/* scale numbers by 10**2 */
	P_end);				/* end of arguments */

    /* generate a GDS */


    /* or make a user defined GDS for a 2.5 x 2.5 grid */

    gds = new_LatLon_GDS(pds,720,360,0.25,-89.75,-0.25,89.75,0.5,0.5);

    /* loop through all the data */

    while (fread(&(data[0]), sizeof(float), NXNY, input) == NXNY) {

	/* number of days in current month */
        month0 = yyyymm % 100;
        year0 = yyyymm / 100;
        ndays = days_in_month[month0-1];
        if (month0 == 2 && year0 % 4 == 0) ndays=29;

	for (i = 0; i < NXNY; i++) {
	    if (data[i] == LOCAL_UNDEF) {
		data[i] = UNDEFINED;
	    }
	    else {
	    }
	}
	pds = PDStool(pds, P_param(DLWRF),P_end);
	wrt_grib_rec(pds, gds, data, NXNY, output);

	/* change date code in the PDS */

	if (++yyyymm % 100 == 13) {
	    yyyymm += (101 - 13);
	}
	pds = PDStool(pds, P_date(yyyymm*100+1),P_end);

	count++;
    }
    free(pds);
    free(gds);

    printf("%d records converted\n", count);

    fclose(input);
    fclose(output);
    return 0;
}
