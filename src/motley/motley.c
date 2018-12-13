#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <regex.h>
#include "motley.h"

// If this is defined, try to ignore #if 0 blocks that are outside of bodies.
#define IGNORE_IF0

// The get_line() line
#define LINE_SIZE (16 * 1024)

int cscope_mode;

static const char *cur_fname; // for debugging save the current filename
static int cur_line;
static int lineno;
static int sol;
static int verbose;

static regex_t func_re;

static inline int getch(FILE *fp)
{
	return getc(fp);
}

static inline void ungetch(int c, FILE *fp)
{
	ungetc(c, fp);
}

#undef getc
#define getc bogus

static inline int ishex(int c)
{
	switch (c) {
	case '0' ... '9':
	case 'a' ... 'f':
	case 'A' ... 'F':
		return 1;
	default:
		return 0;
	}
}

// Deal with \c or \nnn
static void quote(FILE *fp)
{
	int c;

again:
	c = getch(fp);

	switch (c) {
	case '0' ... '7':
		while ((c = getch(fp)) != EOF && (c >= '0' && c <= '7')) ;
		ungetch(c, fp); // we read one too far
		break;
	case 'x':
	case 'X':
		while ((c = getch(fp)) != EOF && ishex(c)) ;
		ungetch(c, fp); // we read one too far
		break;
	case '\r':
		goto again;
	case '\n':
		++cur_line;
		break;
	}
}

// Skip /* */ style comments.
static void skip_comments(FILE *fp)
{
	int count = 1;
	int state = 0;
	int c;

	while (count > 0 && (c = getch(fp)) != EOF) {
		if (c == '\n')
			++cur_line;
		switch (state) {
		case 0:
			if (c == '*')
				state = 1;
			else if (c == '/')
				state = 2;
			break;
		case 1:
			if (c == '/') {
				--count;
				state = 0;
			} else if (c != '*')
				state = 0;
			break;
		case 2:
			if (c == '*')
				++count;
			else if(c != '/')
				state = 0;
			break;
		}
	}
}

// This is the lowest level character input and the only one that can
// call getc().
//
//   - It counts the lines.
//   - It removes spaces from the starts and ends of the line.
//   - It converts all sequences of spaces to one space.
//   - It removes empty lines.
//   - It removes CRs from evil dos files.
//   - It deals with the backslash.
//   - It deals with C comments.
//   - It deals with double and single quotes.
static int __getc(FILE *fp)
{
	int c;

again:
	c = getch(fp);
	switch (c) {
	case '\n':
		++cur_line;
		if (sol == 1)
			goto again; // empty line
		sol = 1; // start of line next time
		return c;
	case '\r':
		goto again;
	case ' ':
	case '\t':
		if (sol)
			goto again;
		while ((c = getch(fp)) == ' ' || c == '\t' || c == '\r') ;
		ungetch(c, fp); // we read one too far
		if (c == '\n')
			goto again;
		c = ' ';
		break;
	case '\\':
		quote(fp);
		goto again;
	case '/': // possible comment
		c = getch(fp);
		if (c == '/') {
			while ((c = getch(fp)) != '\n' && c != EOF) ;
			ungetch('\n', fp); // deal with \n
			goto again;
		} else if (c == '*') {
			skip_comments(fp);
			goto again;
		}
		ungetch(c, fp);
		c = '/';
		break;
	case '"':
		while ((c = getch(fp)) != '"' && c != EOF) {
			if (c == '\\') // still must deal with backquotes
				quote(fp);
		}
		goto again;
	case '\'': ;
		c = getch(fp);
		do {
			if (c == '\\')
				quote(fp);
			c = getch(fp);
		} while (c != '\'' && c != EOF);
		goto again;
	case '#':
		// do not clear sol
		return c;
	}

	sol = 0;
	return c;
}

static void maybe_skip_if0(int c, FILE *fp)
{
#ifdef IGNORE_IF0
	if (c != 'i' || __getc(fp) != 'f' || __getc(fp) != ' ' || __getc(fp) != '0')
		return;

	int count = 1;
	while (count > 0 && (c = __getc(fp)) != EOF)
		if (c == '#' && sol) {
			c = __getc(fp);
			if (c == 'i') {
				if (__getc(fp) == 'f')
					++count;
			} else if (c == 'e') {
				// #endif or #elif or #else
				if ((c = __getc(fp) == 'n'))
					--count;
				else if (c == 'l' && __getc(fp) == 's') {
					if (count == 1)
						// our else
						--count;
				}
			}
		}
#endif
}

static void skip_body(FILE *fp)
{
	int count = 1;
	int c;

	while (count > 0 && (c = __getc(fp)) != EOF)
		if (c == '{')
			++count;
		else if (c == '}')
			--count;
	ungetc(c, fp);
}

// This deals with things we can't do in __getc()
//   - preprocessor statements
//   - body {}
static int _getc(FILE *fp)
{
	static int state;
	int c;

again:
	c = __getc(fp);

	switch (c) {
	case '\n':
		// don't clear sol
		state = 0;
		return c;
	case '#':
		if (sol) {
			sol = 0;
			if ((c = __getc(fp)) == ' ')
				c = __getc(fp);
			if (c == 'd') {
				ungetch(c, fp);
				return '#';
			}
			maybe_skip_if0(c, fp);
			// skip to EOL
			while (__getc(fp) != EOF && sol == 0) ;
			goto again;
		}
		break;
	case 'e':
		if (sol)
			state = 1;
		break;
	case '(':
		state = 0;
		break;
	case '{':
		if (state != 5)
			skip_body(fp);
		break;
#if 0
		// Hmmm... problems with typedef (*func)()
	case '(':
		count = 1;
		while (count > 0 && (c = __getc(fp)) != EOF)
			if (c == ')') --count;
			else if (c == '(') ++count;
		ungetch(c, fp);
		c = '(';
		break;
#endif
	}

	switch (state) {
	case 1:
		state = 2;
		break;
	case 2:
		state = c == 'n' ? 3 : 0;
		break;
	case 3:
		state = c == 'u' ? 4 : 0;
		break;
	case 4:
		state = c == 'm' ? 5 : 0;
		break;
	}

	sol = 0;
	return c;
}

static void strip_attributes(char *line, char **p)
{
	char *s = strstr(line, "__attribute__");
	if (!s)
		return; // 97.6% case

	char *e = s + 13;
	if (*e == ' ') ++e;
	if (*e != '(')
		return; // for example #ifndef __attribute__
	++e;

	int count = 1;
	while (count > 0 && *e) {
		if (*e == '(')
			++count;
		else if (*e == ')')
			--count;
		++e;
	}

	int len = e - s;
	memmove(s, e, strlen(s) - len + 1);

	*p -= len;
	**p = 0;
}

// Get a line. Ignore extern lines. Drop bodies.
static char *get_line(char *str, int len, FILE *fp)
{
	int c;
	char *p;
	int plen;

	memset(str, 0, len);

again:
	p = str;
	plen = len;
	c = _getc(fp);
	if (c == EOF)
		return NULL;
	lineno = cur_line + 1;
	while (1) {
		if (c == '\n' || c == EOF) {
			// This is specific to work
			if (strncmp(str, "__SRCVERSION", 12) == 0)
				goto again;
			if (strncmp(str, "__END_DECLS", 11) == 0)
				goto again;
			// End of work specific

			*p = 0;
			if (*str != '#')
				// attributes on defines are generally safe
				strip_attributes(str, &p);

			// This can happen with 'code; // comment'
			if (*(p - 1) == ' ')
				*(--p) = 0;

			if (strncmp(str, "typedef", 7) == 0) {
				// This is to deal with typedef struct foo { bar } foo;
				if (*(p - 1) != ';') {
					// need more
					c = _getc(fp);
					continue;
				}
			}
			if (*str == '#') {
				*p = 0;
				return str;
			}
			if (*(p - 1) == ';' || *(p - 1) == '}') {
				// good line
				*p = 0;
				if (strncmp(str, "extern", 6) == 0)
					goto again;
				return str;
			}
			if (c == EOF)
				return NULL;
			// need more - keep going
			*p++ = ' ';
		} else {
			*p++ = c;
			--plen;
			if (plen == 1) {
				fprintf(stderr, "%s:%d Line truncated\n", cur_fname, lineno);
				*p = 0;
				return str;
			}
		}
		c = _getc(fp);
	}
}

/* Returns 0 if not a function, 1 if function, 2 if reference */
static int try_match_func(char *line, char *func, FILE *fp)
{
	regmatch_t match[2];
	if (regexec(&func_re, line, 2, match, 0))
		return 0;

	int len = match[1].rm_eo - match[1].rm_so;
	memcpy(func, line + match[1].rm_so, len);
	func[len] = 0;

	char *p = line + match[0].rm_eo;

	// skip the args
	int count = 1;
	while (count > 0 && *p) {
		if (*p == ')')
			--count;
		else if (*p == '(')
			++count;
		++p;
	}

	if (*p == ' ') ++p;
	if (*p == ';')
		return 2; //forward reference
	if (*p == '{')
		return 1; // func - normal case

	// Look for {. This is to remove old-school arg definitions
	while (!strchr(line, '{'))
		if (!get_line(line, LINE_SIZE, fp))
			return 1;

	return 1;
}

static void add_file(const char *fname)
{
	cur_fname = fname;
	cur_line = 0;
	lineno = 0;
	sol = 1;

	out_file(fname);
}

/* Returns where it stopped in in */
static char *get_sym(char *in, char *out)
{
	if (*in == ' ') ++in;
	while (*in == '*')
		++in;
	if (*in == ' ') ++in;

	while (isalnum(*in) || *in == '_')
		*out++ = *in++;
	*out = 0;

	return in;
}

static int get_word_backwards(char *start, char *p, char *word)
{
	if (p == NULL) {
		p = strchr(start, ';');
		if (p == NULL) return 0;
		--p;
	}

	if (isspace(*p)) --p;

	if (*p == ')') {
		// pointer to function
		int count = 1;
		--p;
		while (p > start && count > 0) {
			if (*p == ')')
				++count;
			else if (*p == '(')
				--count;
			--p;
		}
		if (isspace(*p)) --p;
		if (*p == ')') --p;
		if (isspace(*p)) --p;
	} else if (*p == ']') {
		--p;
		while (p > start && *p != '[') --p;
		if (*p == '[') --p;
		if (isspace(*p)) --p;
	}

	while ((isalnum(*p) || *p == '_') && p >= start) --p;
	++p;
	while (isalnum(*p) || *p == '_')
		*word++ = *p++;
	*word = 0;
	return 1;
}

static int process_globals(char *line)
{
	char word[128];

	if (strncmp(line, "static", 6) == 0)
		for (line += 6; isspace(*line); ++line) ;

	char *p = strchr(line, ',');
	if (p) {
		process_globals(p + 1);
		*p++ = ';';
		*p = 0;
	}

	p = strchr(line, '=');
	if (p)
		get_word_backwards(line, p - 1, word);
	else {
		p = strchr(line, ';');
		if (!p) return 0;
		get_word_backwards(line, p - 1, word);
	}

	out_sym('g', lineno, word);
	return 1;
}

// Of course enums have to be special.
static void handle_enum(char *line)
{
	char sym[128];

	char *p = get_sym(line + 4, sym);
	if (strchr(p, '{')) {
		// enum definition
		out_sym('e', lineno, sym);

		if (*p == ' ') ++p;
		assert(*p == '{');
		++p;

		while (1) {
			p = get_sym(p, sym);
			out_sym('e', lineno, sym);

			if (*p == ' ') ++p;
			switch (*p) {
			case ',':
				++p;
				break;
			case '=':
				for (++p; *p != ','; ++p)
					if (*p == '}')
						return;
					else if (!*p) {
						fprintf(stderr, "%s:%d: enum error\n", cur_fname, cur_line);
						return;
					}
				++p;
				break;
			case '}':
				return; // done
			default:
				fprintf(stderr, "%s:%d: enum unexpected %c\n", cur_fname, cur_line, *p);
				return;
			}
		}
	} else {
		// global variable of type enum
		get_sym(p, sym);
		out_sym('g', lineno, sym);
	}
}

static void handle_struct(char *line)
{
	char sym[128], *e;

	char *p = get_sym(line + 6, sym);

	if ((e = strchr(p, '{'))) {
		// struct definition
		if (*sym)
			out_sym('s', lineno, sym);
		++e;
		if (*e == '}') ++e;

		get_sym(e, sym);
		if (*sym)
			out_sym('g', lineno, sym);
		return;
	}

	if (*p == ' ') ++p;
	if (*p == ';') {
		// forward declaration
		return;
	}

	process_globals(line);
}

static int process_one(const char *fname)
{
	FILE *fp = fopen(fname, "r");
	if (!fp) {
		perror(fname);
		return 1;
	}

	add_file(fname);
	if (verbose == 1) printf("%s\n", cur_fname);

	int rc;
	char line[LINE_SIZE], func[126];
	while (get_line(line, sizeof(line), fp)) {
		if (verbose > 1) printf(">> %s:%d: %s\n", cur_fname, lineno, line);
		if (*line == '#') {
			assert(strncmp(line, "#define", 7) == 0);
			get_sym(line + 7, func);
			out_sym('#', lineno, func);
		} else if (strncmp(line, "typedef", 7) == 0) {
			get_word_backwards(line, NULL, func);
			out_sym('t', lineno, func);
		} else if ((rc = try_match_func(line, func, fp))) {
			if (rc == 1)
				out_sym('$', lineno, func);
		} else if (strncmp(line, "struct", 6) == 0) {
			handle_struct(line);
		} else if (strncmp(line, "enum", 4) == 0) {
			handle_enum(line);
		} else if (strncmp(line, "namespace", 9) == 0) {
			// silently ignore namespace entries
		} else if (!process_globals(line)) {
			fprintf(stderr, "%s:%d Hmmm %s\n", cur_fname, lineno, line);
		}
	}

	fclose(fp);
	return 0;
}

static void dump_one(const char *fname, int level)
{
	FILE *fp = fopen(fname, "r");
	if (!fp) {
		perror(fname);
		return;
	}

	add_file(fname);

	if (level > 1) {
		int c;
		while ((c = _getc(fp)) != EOF) {
			putchar(c);
		}
	} else {
		char line[LINE_SIZE];
		while (get_line(line, sizeof(line), fp))
			printf("%d: %s\n", lineno, line);
	}

	fclose(fp);
}

int main(int argc, char *argv[])
{
	int c, dump_only = 0, lookup = 0;

	while ((c = getopt(argc, argv, "cDlLv")) != EOF)
		switch (c) {
		case 'c':
			cscope_mode = 1;
			break;
		case 'D':
			++dump_only;
			break;
		case 'l':
		case 'L':
			lookup = 1;
			break;
		case 'v':
			++verbose;
			break;
		default:
			puts("Sorry!");
			exit(1);
		}

	if (lookup) {
		if (optind == argc) {
			fputs("Lookup what?\n", stderr);
			exit(1);
		}
		if (cscope_mode) {
			fputs("For cscope mode use cscope\n", stderr);
			exit(1);
		}
		return do_lookup(argv[optind]);
	}

	// Note: (asctime)(const struct tm *t)
	int rc = regcomp(&func_re, "\\(?(_*[a-zA-Z][a-zA-Z0-9_]*)\\)? ?\\(", REG_EXTENDED);
	if (rc) {
		char err[100];
		regerror(rc, &func_re, err, sizeof(err));
		fprintf(stderr, "regcomp func: %s\n", err);
		exit(1);
	}

	if (dump_only) {
		for (int arg = optind; arg < argc; ++arg)
			dump_one(argv[arg], dump_only);
		exit(0);
	}

	out_init();

	if (optind == argc) {
		// This mimics cscope
		FILE *fp = fopen("cscope.files", "r");
		if (fp == NULL) {
			if (errno != ENOENT) {
				perror("cscope.files");
				exit(1);
			}
		} else {
			char line[1024];
			while (fgets(line, sizeof(line), fp)) {
				strtok(line, "\r\n");
				process_one(line);
			}

			fclose(fp);
		}
	} else
		for (int arg = optind; arg < argc; ++arg)
			process_one(argv[arg]);

	out_fini();

	return 0;
}
