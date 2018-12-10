#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include "motley.h"

static char *types[] = {
	"$()",
	"# define",
	"t typedef",
	"s struct",
	"e enum",
	"g global",
	"X unknown"
};

int do_lookup(const char *sym)
{
	char fname[256];
	int i, rc = 1, len = strlen(sym);

	FILE *fp = fopen("motley.out", "r");
	if (!fp) {
		perror("motley.out");
		return 1;
	}

	char line[256], *e;
	while (fgets(line, sizeof(line), fp))
		if (*line == '@')
			strcpy(fname, line + 1);
		else if (strncmp(line, sym, len) == 0 && line[len] == ' ') {
			int lineno = strtol(line + len, &e, 10);
			if (lineno > 0) {
				if (*e == ' ') ++e;
				for (i = 0; i < (sizeof(types) / sizeof(char *)) - 1; ++i)
					if (*types[i] == *e)
						break;
				if ((e = strchr(fname, '\n'))) *e = 0;
				printf("%s:%d:1    %s%s\n", fname, lineno, sym, types[i] + 1);
				rc = 0;
			}
		}

	fclose(fp);
	return rc;
}
