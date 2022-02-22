#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#include "cnames.h"
#include "pds2.h"

/*
 * extract grib records
 *
 * take grib file and makes many grib files which are
 * separated by the parameter type
 *
 * can specify which files get written
 *  i.e. can extract HGT fields
 *
 * v1.1 Wesley Ebisuzaki
 * v1.2 Wesley Ebisuzaki - make buffer bigger if needed
 */

/*
 * MSEEK = I/O buffer size
 * LEN_HEADER_PDS = size of section 0 and required PDS
 * note: search distance = (MSEEK-LEN_HEADER_PDS) bytes
 */

#define MSEEK 2048
#define MAXSIZE 100000
#define BUFF_ALLOC0     (100000)


#define LEN_HEADER_PDS  (8 + 28)

#ifndef min
   #define min(a,b)  ((a) < (b) ? (a) : (b))
#endif

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer);

struct {
        FILE *file;
        char *name;
	enum {write, open_write, append_write, nop} action;
} var[256];

int main(int argc, char **argv) {

    unsigned char *buffer;
    int buffer_size;
    int i, count, j, kpds5, error, def_kpds5 = 0;
    long len_grib, pos;
    unsigned char *message, *pds;
    FILE *input;
    char in_name[150];
    int verbose = 0;
    int files_open = 4;

    for (i = 0; i < 256; i++) {
        var[i].action = open_write;
    }

    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i],"-all") == 0) {
    	    for (j = 0; j < 256; j++) {
                var[j].action = open_write;
	    }
	    continue;
	}
	if (strcmp(argv[i],"-none") == 0) {
    	    for (j = 0; j < 256; j++) {
                var[j].action = nop;
	    }
	    continue;
	}
	if (strcmp(argv[i],"-def") == 0) {
	    def_kpds5 = 1;
	    continue;
	}
	if (strcmp(argv[i],"-v") == 0) {
	    verbose = 1;
	    continue;
	}
	break;
    }
    if (def_kpds5) {
	for (j = 0; j < 256; j++) {
            var[j].name = k5toa_def(j);
	}
	error = 0;
    }
    else {
        error = init_kpds5_table(K5DEF_FILE);
	for (j = 0; j < 256; j++) {
            var[j].name = k5toa(j);
	}
    }

    /* continue scan of arguments .. toggle varables */
    for ( ; i < argc; i++) {
	j = atok5(argv[i]);
	if (j >= 0) {
	    var[j].action = (var[j].action == nop) ? open_write : nop;
	}
	else {
	    fprintf(stderr,"huh? %s\n", argv[i]);
	    exit(8);
	}
    }

    if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
        fprintf(stderr,"not enough memory\n");
	exit(8);
    }
    buffer_size = BUFF_ALLOC0;
 
    count = 0;
/* changed to fgets 6/2012 WNE   while (gets(in_name) != NULL ) { */

    while (fgets(in_name, sizeof (in_name) - 1, stdin) != NULL ) {
/*      added 1/2013 linux conversion */
	i = strlen(in_name);
	if (i != 0 && in_name[i-1] == '\n') in_name[i-1] = '\0';

        if ((input = fopen(in_name,"r")) == NULL) {
	    if (in_name[0] == 0) break;
            fprintf(stderr,"missing file: (%s)\n", in_name);
            exit(7);
        }

	fprintf(stderr, "reading from %s\n", in_name);
        pos = 0; 
        while ((message = seek_grib(input, &pos, &len_grib, 
		buffer)) != NULL) {

            /* see if need to increase buffer size */
            if (len_grib + message - buffer > buffer_size) {
                buffer_size = len_grib + message - buffer + 1000;
                buffer = (unsigned char *) realloc((void *) buffer, buffer_size);
                if (buffer == NULL) {
                    fprintf(stderr,"ran out of memory\n");
                    exit(8);
                }
		/* buffer may have been moved */
                message = seek_grib(input, &pos, &len_grib, buffer);
		if (message == NULL) {
		    fprintf(stderr,"logic error\n");
		    exit(8);
		}
            }
 
            pds = (message + 8);
            kpds5 = PARAM;

	    if (var[kpds5].action == nop) {
                pos += len_grib;
		continue;
	    }

	    /* read the whole message */

            j = len_grib + (message - buffer) - MSEEK;
            if (j > 0) {
                i = fread(buffer+MSEEK, sizeof (unsigned char), j, input);
                if (i != j) exit(9);
            }

	    /* open file if necessary */

	    if (var[kpds5].action == open_write) {
                var[kpds5].file = fopen(var[kpds5].name,"w");
		var[kpds5].action = write;
                files_open++;
		printf("%s\n", var[kpds5].name);
		if (verbose) fprintf(stderr,
		    "opening %s\n", var[kpds5].name);
	    }
	    if (var[kpds5].action == append_write) {
                var[kpds5].file = fopen(var[kpds5].name,"a");
		var[kpds5].action = write;
                files_open++;
		if (verbose) fprintf(stderr, "reopening %s\n", var[kpds5].name);
	    }

	    /* write data */
	    if (var[kpds5].action == write) {
                i=fwrite(message, sizeof (unsigned char), len_grib, 
       	            var[kpds5].file);
                if (i != len_grib) {
                    fprintf(stderr,"fatal error - write to %s\n",var[kpds5].name);
                    return 13;
                }
		if (verbose) fprintf(stderr,"writing %ld bytes in %s\n",
			len_grib, var[kpds5].name);
	    }

	    /* free a file if necessary */
            if (files_open == (FOPEN_MAX-4)) {
                fclose(var[kpds5].file);
                var[kpds5].action = append_write;
		--files_open;
		if (verbose) fprintf(stderr,"closing %s\n", var[kpds5].name);
            }   

            pos += len_grib;
            count++;
        }
        fclose(input);
    }
    fprintf(stderr,"number of records %d\n", count);
    return error;
}

/*
 * find next grib header
 *
 * file = what do you think?
 * pos = initial position to start looking at  ( = 0 for 1st call)
 *       returns with position of next grib header (units=bytes)
 * len_grib = length of the grib record (bytes)
 * buffer[MSEEK] = buffer for reading/writing
 *
 * returns (char *) to start of GRIB header+PDS
 *         NULL if not found
 *
 * adapted from SKGB (Mark Iredell)
 *
 * v1.0 5/94 Wesley Ebisuzaki
 *
 */

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer) {

    int i, len;

    /* read data */
    if (fseek(file, *pos, SEEK_SET) == -1) {
        *len_grib = 0;
        return NULL;
    }

    i = fread(buffer, sizeof (unsigned char), MSEEK, file);

    len = min(i, MSEEK) - LEN_HEADER_PDS;

    for (i = 0; i < len; i++) {
        if (buffer[i] == 'G' && buffer[i+1] == 'R' && buffer[i+2] == 'I'
            && buffer[i+3] == 'B' && buffer[i+7] == 1) {
                *pos = i + *pos;
                *len_grib = (buffer[i+4] << 16) + (buffer[i+5] << 8) + 
                        buffer[i+6];
                return (buffer+i);
        }
    }
    *len_grib = 0;
    return NULL;
}
