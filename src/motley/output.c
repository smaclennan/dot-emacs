#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "motley.h"

static FILE *out;
static const char *cur_fname; // for debugging

// cscope mode only
static char **file_list;
static int file_count;
static int file_size;
static char *dir;


void out_init(void)
{
	if (cscope_mode) {
		out = fopen("cscope.out", "w");
		if (!out) {
			perror("cscope.out");
			exit(1);
		}

		dir = getcwd(NULL, 0);
		if (!dir) {
			fprintf(stderr, "Out of memory\n");
			exit(1);
		}
		fprintf(out, "cscope 15 %s -c 0000000000 0000000000\n", dir);
	} else {
		out = fopen("motley.out", "w");
		if (!out) {
			perror("motley.out");
			exit(1);
		}
	}
}

void out_file(const char *fname)
{
	cur_fname = fname;

	if (!out) return;

	if (cscope_mode) {
		++file_count;

		fprintf(out, "\t@%s\n", fname);

		file_size += strlen(fname) + 1;

		char *new = strdup(fname);
		file_list = realloc(file_list, sizeof(char *) * file_count);
		if (!file_list || !new) {
			fputs("Out of memory\n", stderr);
			exit(1);
		}
		file_list[file_count - 1] = new;
	} else
		fprintf(out, "@%s\n", fname);
}

void out_sym(char type, int lineno, const char *sym)
{
	if (!*sym) {
		if (type != 'e' && type != 'g') // SAM DBG
			fprintf(stderr, "%s:%d empty sym %c\n", cur_fname, lineno, type);
		return;
	}

	if (!out) return;

	if (cscope_mode) {
		switch (type) {
		case '#': // #define
			fprintf(out, "%d #define \n\t#%s\n\n", lineno, sym);
			break;
		case 't': // typedef
			fprintf(out, "%d \n\tt%s\n\n", lineno, sym);
			break;
		case '$': // func
			fprintf(out, "%d \n\t$%s\n()\n\n", lineno, sym);
			break;
		case 's': // struct
			fprintf(out, "%d \n\ts%s\n\n", lineno, sym);
			break;
		case 'e': // enum
			fprintf(out, "%d \n\te%s\n\n", lineno, sym);
			break;
		case 'g': // global
			fprintf(out, "%d \n\tg%s\n\n", lineno, sym);
			break;
		default:
			fprintf(stderr, "Unexpected sym type %c\n", type);
			exit(2);
		}
	} else
		fprintf(out, "%s %d %c\n", sym, lineno, type);
}

void out_fini(void)
{
	if (cscope_mode) {
		fprintf(out, "\t@\n"); // marker
		long size = ftell(out);

		fprintf(out, "1\n.\n0\n");

		fprintf(out, "%d\n", file_count);
		fprintf(out, "%d\n", file_size);

		for (int i = 0; i < file_count; ++i)
			fprintf(out, "%s\n", file_list[i]);

		rewind(out);
		fprintf(out, "cscope 15 %s -c            %010lu\n",
				dir, size);
	}

	fclose(out);
}
