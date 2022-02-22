#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "gribwlib.h"
#include "ncepopn.h"

#define NXNY		360*180
#define LOCAL_UNDEF	-9999.0

/*
 * gribify an SST field 360x180 
 *  
 * uses PDStool(), NCEP_GDS()
 * v1.1 convert from Celcius to Kelvin
 */


void main(int argc, char **argv) {

    unsigned char *pds, *pds_debug, *gds;
    FILE *input, *output;
    int count = 0, yyyymm, i, header;
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

	P_param(TMP), P_sfc,		/* variable and level */
	P_date(yyyymm*100+1),		/* initial date yyyymmdd */
	P_hour(0),

	P_ave_mon(0,1),			/* averaged from month 0 to 1 */

	P_dec_scale(2),			/* scale numbers by 10**2 */
	P_end);				/* end of arguments */

    /* generate a GDS */

    /* use NCEP grid number 2 */

    /*  gds = NCEP_GDS(pds,GDS_GRID);	/* use 144x73 grid */

    /* or make a user defined GDS for a 2.5 x 2.5 grid */

    gds = new_LatLon_GDS(pds,360,180,-179.5,-89.5,179.5,89.5,1.0,1.0);

    /* loop through all the data */

    while (i=fread(&header, sizeof(int), 1, input) == 1) {
	if (i != 1 || header != NXNY * sizeof(float)) {
	    fprintf(stderr,"missing f77 header\n");
	    exit(8);
	}
        if (i=fread(&(data[0]), sizeof(float), NXNY, input) != NXNY) {
	    fprintf(stderr,"missing data\n");
	    exit(8);
	}

	for (i = 0; i < NXNY; i++) {
		if (data[i] == LOCAL_UNDEF) data[i] = UNDEFINED;
		else data[i] += 273.15;
	}
	wrt_grib_rec(pds, gds, data, NXNY, output);

	/* change date code in the PDS */

	if (++yyyymm % 100 == 13) {
	    yyyymm += (101 - 13);
	}
	pds = PDStool(pds, P_date(yyyymm*100+1),P_end);

	count++;

	i=fread(&header, sizeof(int), 1, input);
	if (i != 1 || header != NXNY * sizeof(float)) {
	    fprintf(stderr,"missing f77 trailer\n");
	    exit(8);
	}

    }
    free(pds);
    free(gds);

    printf("%d records converted\n", count);

    fclose(input);
    fclose(output);
}
