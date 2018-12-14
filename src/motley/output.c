#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "motley.h"

static FILE *out;
static const char *cur_fname; // for debugging

extern int verbose;

void out_init(void)
{
	out = fopen("motley.out", "w");
	if (!out) {
		perror("motley.out");
		exit(1);
	}
}

void out_file(const char *fname)
{
	cur_fname = fname;
	fprintf(out, "@%s\n", fname);
}

void out_sym(char type, int lineno, const char *sym)
{
	if (!*sym) {
		if (verbose && type != 'e' && type != 'g') // SAM DBG
			fprintf(stderr, "%s:%d empty sym %c\n", cur_fname, lineno, type);
		return;
	}

	fprintf(out, "%s %d %c\n", sym, lineno, type);
}

int out_fini(void)
{
	int rc = 0;

	if (ferror(out)) {
		perror("motley.out");
		rc = 1;
	}

	if (fclose(out)) {
		perror("close motley.out");
		rc = 1;
	}

	return rc;
}
