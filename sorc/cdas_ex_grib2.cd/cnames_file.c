#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include "cnames.h"

/* read doc file of kpds(5) and dynamically create table */


static char cname5[256][16];
static int init = 0;

int init_kpds5_table(char *name) {

        char line[100];
        int i, j;
	FILE *file;

	file = fopen(name,"r");

	if (file == NULL) {
		/* use default values */
		for (i = 0; i < 255; i++) {
			strcpy(cname5[i], k5toa_def(i));
		}
		init = 1;
		return 1;
	}

        for (i = 0; i < 256; i++) {
                if (fgets(line,100,file) == NULL) return 8;
		line[99] = 0;

                j = atoi(line);
                if (j != i) {
                        fprintf(stderr,"number mismatch:%s\n", line);
                        return 8;
                }

                j = strlen(line);
                if (j < 58) {
			sprintf(cname5[i], "var%d", i);
                        continue;
                }
                if (line[57] != ':') {
                        fprintf(stderr,"bad data:%s\n", line);
                        fprintf(stderr,"bad char:%c\n", line[57]);
                        return 8;
                }
		line[j-1] = 0;
		if (line[j-2] == ' ') line[j-2] = 0;

		sprintf(cname5[i], "%s", line+58);
        }
	init = 1;
        return 0;
}

char *k5toa(int i) {

	if (init == 0) exit(8);
        return cname5[i];
}
 
int atok5(char *name) {
 
	int i;

	if (init == 0) exit(8);
        for (i = 0; i <= 255; i++) {
                if (strcmp(name,cname5[i]) == 0) return i;
        }
        return -1;
}
