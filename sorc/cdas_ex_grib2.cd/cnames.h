#define K5DEF_FILE	"/wd2/ln/map/data/grib1.kpds5.vsn21"
#define LEVDEF_FILE	"/wd2/ln/map/data/grib1.kpds6.vsn21"

/* cnames_file.c */
/* tries to read K5DEF_FILE first -- then default */
int init_kpds5_table(char *name);
char *k5toa(int i);
int atok5(char *name);

/* cnames.c */
/* then default values */
char *k5toa_def(int i);
int atok5_def(char *name);

int init_lev_type(char *filename);
char *k6toa(int i);
int atok6(char *name);

