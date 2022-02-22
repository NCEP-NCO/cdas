/*
 * NESDIS SNOWC is in percentage 0..100
 *
 * NCEP expects SNOWC to be 0..1
 *
 * this program returns a grib file with the SNOWC record set 0..1
 *
 * assumption if max value > 5, then it is a percentage
 *  this assumption may fail for regional analyses
 *
 * 12/99 W. Ebisuzaki
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "gribwlib.h"
#include "pds4.h"
#include "ncepopn.h"

int main(int argc, char **argv) {

    unsigned char *pds, *gds;
    FILE *input, *output;
    float *data;
    int ndata, i, count, cnvt, nblank;

    long int pos, len_grib;

    /* preliminaries .. open up all files */

    if (argc != 3) {
	fprintf(stderr, "%s [in gribfile] [out SNOWC]\n", argv[0]);
	exit(8);
    }
    if ((input = fopen(argv[1],"rb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[1]);
        exit(7);
    }
    if ((output = fopen(argv[2],"wb+")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[2]);
        exit(7);
    }

    for(nblank = count = cnvt = pos = 0;;) {

	len_grib = rd_grib_rec(input, pos, &pds, &gds, &data, &ndata);
	if (len_grib <= 0) break;

	if (PDS_PARAM(pds) == SNOWC) {
	    for (i = 0; i < ndata; i++) {
		if (!UNDEFINED_VAL(data[i]) && data[i] > 5.0) break;
	    }
	    if (i < ndata) {
		cnvt++;
		for (i = 0; i < ndata; i++) {
		    if (!UNDEFINED_VAL(data[i])) {
		        data[i] *= 0.01;
		    }
		}
	    }
	    for (i = 0; i < ndata; i++) {
		if (!UNDEFINED_VAL(data[i])) break;
	    }
	    if (i < ndata) {
	        count++;
	        pds = PDStool(pds, P_dec_scale(2), P_end);
	        wrt_grib_rec(pds, gds, data, ndata, output);
	    }
	    else {
		nblank++;
	    }
	}
	else {
	    wrt_grib_rec(pds, gds, data, ndata, output);
	}
	pos += len_grib;
    }
    fclose(output);
    fclose(input);
    printf("%d SNOWC records converted out of %d, %d blank\n", cnvt, count,
         nblank);
    return 0;
}
