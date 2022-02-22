#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "cnames.h"
#include "pds2.h"

/*
 * extract grib by level
 *
 * v1.1 Wesley Ebisuzaki
 *
 * v1.1: 2/95 dynamically allocated buffer[]
 */

/*
 * MSEEK = I/O buffer size
 * LEN_HEADER_PDS = size of section 0 and required PDS
 * note: search distance = (MSEEK-LEN_HEADER_PDS) bytes
 */

#define MSEEK 1024
#define BUFF_ALLOC0     40000
#define MAXFILES	58

#define LEN_HEADER_PDS  (8 + 28)

#ifndef min
   #define min(a,b)  ((a) < (b) ? (a) : (b))
#endif

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer);

struct {
        FILE *file;
        char name[32];
	int param, level_t, level1, level2;
	enum {write, open_write, append_write, nop, not_alloc} action;
} files[MAXFILES];

struct {
	char *lev_name;
	enum {do_write, do_not_write} action;
	} level_type[256];


int main(int argc, char **argv) {

    unsigned char *buffer;
    int i, j, kpds6, buffer_size;
    int param, level_t, level1, level2;
    long len_grib, pos;
    unsigned char *message, *pds;
    FILE *input;
    char in_name[180], file_name[180], string[180];
    int verbose = 0;
    int files_open = 0, count = 0;

    for (i = 0; i < MAXFILES; i++) {
        files[i].action = not_alloc;
    }

    if (argv[1] == NULL || argv[1][0] == '-') {
	printf("opening %s\n",LEVDEF_FILE);
        init_lev_type(LEVDEF_FILE);
    }
    else {
	printf("opening %s\n",argv[1]);
        init_lev_type(argv[1]);
    }

    if (strcmp(argv[argc-1],"-v") == 0) verbose = 1;

    for (j = 0; j < 256; j++) {
        level_type[j].lev_name = k6toa(j);
	level_type[j].action = do_write;
    }

    

    if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
        fprintf(stderr,"not enough memory\n");
	exit(8);
    }
    buffer_size = BUFF_ALLOC0;


    count = 0;
//    while (gets(in_name) != NULL ) {
    while (fgets(in_name,sizeof(in_name) - 1, stdin) != NULL ) {
	/* added 1/2013 linux conversion */
	i = strlen(in_name);
	if (i && in_name[i-1] == '\n') in_name[i-1] = '\0';

	/* open file -- quit if no input or bad filename */
        if ((input = fopen(in_name,"r")) == NULL) {
	    if (in_name[0] == 0) break;
            fprintf(stderr,"missing file: %s\n", in_name);
            exit(7);
        }

	/* close any open files */
        for (i = 0; i < files_open; i++) {
	    if (files[i].action == write) fclose(files[i].file);
            files[i].action = not_alloc;
        }
	files_open = 0;

        pos = 0; 
        while ((message = seek_grib(input, &pos, &len_grib, buffer)) != NULL) {
    
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
	    param = PARAM;
	    level_t = L_TYPE;
	    level1 = LEVEL1;
	    level2 = LEVEL2;

	    /* read the data */

            j = len_grib + (message - buffer) - MSEEK;
            if (j > 0) {
                i = fread(buffer+MSEEK, sizeof (unsigned char), j, input);
                if (i != j) exit(9);
            }

	    /* find out where to write the data */
	    for (i = 0; i < files_open; i++) {
		if (level_t == files[i].level_t &&
			level1 == files[i].level1 &&
			level2 == files[i].level2) break;
	    }
	    if (i == files_open) {
		if (i == MAXFILES) {
		    fprintf(stderr,"***too many open files - fatal error***\n");
		    return 9;
		}
		files_open++;
		files[i].param = param;
		files[i].level_t = level_t;
		files[i].level1 = level1;
		files[i].level2 = level2;

		strcpy(file_name, in_name);
		level_name(pds, string);
		strcat(file_name, string);
		files[i].file = fopen(file_name,"w");
		if (files[i].file == NULL) {
		    fprintf(stderr,"bad open %s\n", file_name);
		    exit(8);
		}
		files[i].action = write;
	    }

	    if (i < files_open) {
                j=fwrite(message, sizeof (unsigned char), len_grib, 
       	            files[i].file);
                if (j != len_grib) {
                    fprintf(stderr,"fatal error - write\n");
                    return 13;
                }
	    }
            pos += len_grib;
            count++;
        }
        fclose(input);
    }
    return 0;
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
