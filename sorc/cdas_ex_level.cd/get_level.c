#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef AMIGA
#include <m68881.h>
#endif
#ifdef MSDOS
#pragma msdos programers are wimps
#endif

#include "cnames.h"
#include "pds2.h"

struct LevelType {
	char name[8];
	enum {slevel, level, layer} type;
	enum {yes, none} units;
	float factor;
	float offset;
} lev_type[256], *lpnt;

static int init = 0;


int init_lev_type(char *filename) {
    char line[100], *pnt[8], *p;
    int i, j;
    FILE *file;

    for (i = 0, lpnt = lev_type; i < 256; i++, lpnt++) {
	lpnt->name[0] = 0;
	lpnt->name[7] = 0;
	lpnt->factor = 1.0;
	lpnt->offset = 0.0;
	lpnt->type = slevel;
    }

    if ((file = fopen(filename,"r")) == NULL) {
        fprintf(stderr,"Error could not open %s\n", filename);
        return 1;
    }
 
    for (i = 0, lpnt = lev_type; i < 256; i++, lpnt++) {
        if (fgets(line,100,file) == NULL) return  8;
        line[99] = 0;
        j = strlen(line);
	if (j > 0) line[j-1] = 0;
 
        j = atoi(line);
        if (j != i) {
            fprintf(stderr,"number mismatch:%s\n", line);
            return 8;
	}

	for (j = 0, p = line; *p && j < 7; p++) {
	    if (*p == ':') {
		*p = 0;
	        pnt[j++] = p+1;
	    }
	}
	pnt[j] = NULL;

	/* name */
	if (pnt[0] == NULL) continue;

	/* units */
	if (pnt[1] == NULL) continue;

	/* factor */
	if (pnt[2] == NULL) break;
	lpnt->factor = atof(pnt[2]);

	/* offset */
	if (pnt[3] == NULL) break;
	lpnt->offset = atof(pnt[3]);

	/* ? */
	if (pnt[4] == NULL) break;
	if (strcmp("-", pnt[4]) == 0) {
	    lpnt->units = none;
	}
	else if (strcmp("surface", pnt[4]) == 0) {
	    lpnt->units = none;
	}
	else {
	    lpnt->units = yes;
	}

	/* type */

	if (pnt[5] == NULL) break;
        if (strcmp("level",pnt[5]) == 0) {
            lpnt->type = level;
        }
	else if (strcmp("slevel",pnt[5]) == 0) {
            lpnt->type = slevel;
        }
	else if (strcmp("layer",pnt[5]) == 0) {
            lpnt->type = layer;
        }
	else {
	    fprintf(stderr,"level type? %s\n", pnt[5]);
	    break;
	}

	/* abrev */
	if (pnt[6] == NULL) break;
	strncpy(lpnt->name, pnt[6], 7);

    }
    if (i != 256) {
	fprintf(stderr,"abnormal termination\n");
	exit(9);
    }
    init = 1;
    return 0;
}


int get_level(unsigned char *pds, float *level1, float *level2) {

	int i;

	if (init == 0) {
		init_lev_type(LEVDEF_FILE);
		init = 1;
	}
	i = L_TYPE;
	lpnt = &(lev_type[i]);

	if (lpnt->type == slevel) {
		*level1 = *level2 = 0.0;
		return 1;
	}
	if (lpnt->type == level) {
		i = (LEVEL1) * 256 + (LEVEL2);
		*level1 = (float) i * lpnt->factor + lpnt->offset;
		*level2 = 0.0;
		return 1;
	}
	if (lpnt->type == layer) {
		i = (LEVEL1);
		*level1 = (float) i * lpnt->factor + lpnt->offset;

		i = (LEVEL2);
		*level2 = (float) i * lpnt->factor + lpnt->offset;
		return 1;
	}
	return 0;
}

int level_name(unsigned char *pds, char *name) {
        int i;
	float level, level1, level2;
 
        if (init == 0) {
                init_lev_type(LEVDEF_FILE);
                init = 1;
        }
        i = L_TYPE;
        lpnt = &(lev_type[i]);

	/* base name */ 
	strcpy(name, lpnt->name);

	/* no units .. no suffix */
	if (lpnt->units == none) return 0;

	get_level(pds, &level1, &level2);

	level = level1;
	if (lpnt->type == layer) level = level2;

	if (level < 0.0) level = -level;

	if (level >= 1.0) {
	    sprintf(name+strlen(name), ".%d",(int) level);
	}
	else if (level == 0.0) {
	    strcat(name,".0");
	}
	else {
	    while(level < 100.0) {
		level *= 10.0;
	    }
	    sprintf(name+strlen(name), ".%d",(int) level);
	}
	return 0;
}

char *k6toa(int i) {
 
	return &(lev_type[i].name[0]);
}
 
int atok6(char *name) {
	int i;
	for (i = 0; i < 256; i++) {
		if (strcmp(name,lev_type[i].name) == 0) return i;
	}
	return -1;
}
